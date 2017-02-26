#####################################################################################################
#                                             Location tracker                                      #
#####################################################################################################


##load required package
pacman::p_load(jsonlite,lubridate,zoo,ggplot2, ggmap)


###########Set the file dir
setwd("M:/R_Script")
filepath=getwd()
setwd(paste(filepath, "Input", sep="/"))
currentDate = Sys.Date()


#The Google location history can be downloaded from your Google account under https://takeout.google.com/settings/takeout. 
system.time(x <- fromJSON("LocationHistory.json"))


# extracting the locations dataframe
loc = x$locations


# converting time column from posix milliseconds into a readable time scale
loc$time = as.POSIXct(as.numeric(x$locations$timestampMs)/1000, origin = "1970-01-01")

# converting longitude and latitude from E7 to GPS coordinates
loc$lat = loc$latitudeE7 / 1e7
loc$lon = loc$longitudeE7 / 1e7

# calculate the number of data points per day, month and year
loc$date <- as.Date(loc$time, '%Y/%m/%d')
loc$year <- year(loc$date)
loc$month_year <- as.yearmon(loc$date)

points_p_day <- data.frame(table(loc$date), group = "day")
points_p_month <- data.frame(table(loc$month_year), group = "month")
points_p_year <- data.frame(table(loc$year), group = "year")

nepal <- get_map(location = 'Kathmandu', zoom = 14, maptype = "toner-hybrid")

ggmap(nepal) + geom_point(data = loc, aes(x = lon, y = lat), alpha = 0.5, color = "red") + 
  theme(legend.position = "right") + 
  labs(
    x = "Longitude", 
    y = "Latitude", 
    title = "Location history data points in Nepal",
    caption = "\nA simple point plot shows recorded positions.")

#Save the file
loc$activitys = as.character(loc$activitys)
setwd("M:/Daily/Daily")
dir.create(as.character(currentDate))
setwd(paste("M:/Daily/Daily", currentDate, sep="/"))
csvFileName1 = paste("temp",currentDate,".csv",sep=" ") 
write.csv(loc , file=csvFileName1, row.names = F)
############################################Bikram Dahal###################################################

