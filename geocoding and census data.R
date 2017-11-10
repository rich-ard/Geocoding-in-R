# Make sure necessary R packages are installed; if not, do so.
packages <- c("ggmap", "httr", "jsonlite", "xlsx", "tcltk", "tools", "stringr")
if (length(setdiff(packages, rownames(installed.packages()))) > 0) {
  install.packages(setdiff(packages, rownames(installed.packages())))
}
library(ggmap)
library(httr)
library(jsonlite)
library(xlsx)
library(tcltk)
library(tools)
library(stringr)

# Import csv with addresses
print("Select .csv with addresses: ")
filename.inputfile <- file.choose()
if(file_ext(filename.inputfile) != "csv"){filename.inputfile <- file.choose()}
inputdata <- read.csv(filename.inputfile)

# Script presumes an address column, "address", with the full street/city/state
# address

# ggmap's "geocode" function may put too many addresses in a short time and overflow
# the Google API limit for free requests. After reviewing Shane Lynn's work here:
# https://www.shanelynn.ie/massive-geocoding-with-r-and-google-maps/
# have decided to run a similar function to make sure that we are able to geocode
# without that limit being a problem, and returning "NA" for those addresses that
# cause the API to choke.

getGeoDetails <- function(address){
  geo_reply <- geocode(address, output='all', messaging=TRUE, override_limit=TRUE)
  answer <- data.frame(lat=NA, long=NA, formatted_address=NA, state_tract=NA,
                       county_tract=NA, census_tract=NA, google_maps_status=NA)
  answer$google_maps_status <- geo_reply$status
  
  # If we are over the query limit, pause for ten seconds
  while(geo_reply$status == "OVER_QUERY_LIMIT"){
    print("Over Google Maps query limit; pausing for ten seconds...")
    Sys.sleep(10)
    geo_reply <- geocode(address, output='all', messaging=TRUE, override_limit=TRUE)
    answer$google_maps_status <- geo_reply$status
  }
  
  # Return NA if the address lookup fails
  if (geo_reply$status != 'OK'){
    return(answer)
  }
  
  # Otherwise, extract info from the geocode reply
  answer$lat <- geo_reply$results[[1]]$geometry$location$lat
  answer$long <- geo_reply$results[[1]]$geometry$location$lng
  answer$formatted_address <- geo_reply$results[[1]]$formatted_address
  
  #...and collect census tract data from the census.gov API
  url <- 'https://geocoding.geo.census.gov'
  path <- 'geocoder/geographies/onelineaddress'
  censusdata <- GET(url = url, path = path, query = list(
    benchmark='9',
    address=answer$formatted_address,
    vintage='910',
    format='json'))
  
  # As above, return NA if the tract lookup fails:
  if (length(content(censusdata)$result$addressMatches) == 0){
    return(answer)
  }
  
  # Otherwise, extract info from the census.gov reply
  answer$state_tract <- content(censusdata)$result$addressMatches[[1]]$geographies$`Census Tracts`[[1]]$STATE
  answer$county_tract <- content(censusdata)$result$addressMatches[[1]]$geographies$Counties[[1]]$COUNTY
  # Tracts seem to be formatted inconsistently - this should, in future iterations, check
  # for situations other than the format 'xx.xx'. Pulling data from a Kansas address
  # returns a tract in the format 'xxxx', where a decimal point should be entered in
  # the middle.
  answer$census_tract <- content(censusdata)$result$addressMatches[[1]]$geographies$`Census Tracts`[[1]]$TRACT
  return(answer)
}

# Data frame to hold geocoded addresses
geocoded <- data.frame()
outputdata <- inputdata

counter <-1

#create temp file name; if a temp file exists - load it up and count the rows!
tempfilename <- paste0(filename.inputfile, '_temp_geocoded.rds')
if (file.exists(tempfilename)){
  print("Found temp file - resuming from index:")
  geocoded <- readRDS(tempfilename)
  counter <- nrow(geocoded)
  print(counter)
}

# Geocode addresses one by one:
for (i in seq(counter, length(inputdata$address))){
  print(paste("Working on index", i, "of", length(inputdata$address)))
  result = getGeoDetails(as.character(inputdata$address[i]))
  print(result$status)
  result$index <- i
  geocoded <- rbind(geocoded, result)
  saveRDS(geocoded, tempfilename)
}

outputdata$index <- inputdata$index
outputdata$formatted_address <- geocoded$formatted_address
outputdata$latitude <- geocoded$lat
outputdata$longitude <- geocoded$long
outputdata$state_tract_number <- geocoded$state_tract
outputdata$county_tract_number <- geocoded$county_tract
outputdata$census_tract <- geocoded$census_tract

# Delete temp file
file.remove(tempfilename)