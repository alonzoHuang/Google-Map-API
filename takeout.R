library(jsonlite)
library(maptools)
library(rgdal)
library(dplyr)
library(sf)
library(data.table)
library(ggplot2)
library(ggmap)
library(leaflet)
library(lubridate)
library(zoo)
system.time(x <- fromJSON("D:/takeout-20211211T082349Z-001/Takeout/takeout/takeout.json"))
# extracting the locations dataframe
loc = x$locations
dimnames(loc)
# converting time column from posix milliseconds into a readable time scale
loc$time = as.POSIXct(as.numeric(x$locations$timestampMs)/1000, origin = "1970-01-01")

# converting longitude and latitude from E7 to GPS coordinates
loc$lat = loc$latitudeE7 / 1e7
loc$lon = loc$longitudeE7 / 1e7
head(loc)
nrow(loc)
min(loc$time)
max(loc$time)
loc$date <- as.Date(loc$time, '%Y/%m/%d')
loc$year <- year(loc$date)
loc$month_year <- as.yearmon(loc$date)
loc2 <- subset(loc, time>"2021-04-12")
my_theme <- function(base_size = 12, base_family = "sans"){
  theme_grey(base_size = base_size, base_family = base_family) +
    theme(
      axis.text = element_text(size = 12),
      axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
      axis.title = element_text(size = 14),
      panel.grid.major = element_line(color = "grey"),
      panel.grid.minor = element_blank(),
      panel.background = element_rect(fill = "aliceblue"),
      strip.background = element_rect(fill = "lightgrey", color = "grey", size = 1),
      strip.text = element_text(face = "bold", size = 12, color = "navy"),
      legend.position = "right",
      legend.background = element_blank(),
      panel.margin = unit(.5, "lines"),
      panel.border = element_rect(color = "grey", fill = NA, size = 0.5)
    )
}
accuracy <- data.frame(accuracy = loc2$accuracy, group = ifelse(loc2$accuracy < 800, "high", ifelse(loc2$accuracy < 5000, "middle", "low")))

accuracy$group <- factor(accuracy$group, levels = c("high", "middle", "low"))

ggplot(accuracy, aes(x = accuracy, fill = group)) + 
  geom_histogram() + 
  facet_grid(group ~ ., scales="free") + 
  my_theme() +
  theme(
    legend.position = "none",
    strip.placement = "outside",
    strip.background = element_blank(),
    axis.text.x = element_text(angle = 0, vjust = 0.5, hjust = 0.5)
  ) +
  labs(
    x = "Accuracy in metres",
    y = "Count",
    title = "How accurate is the location data?",
    subtitle = "Histogram of accuracy of location points",
    caption = "\nMost data points are pretty accurate, 
but there are still many data points with a high inaccuracy.
    These were probably from areas with bad satellite reception."
  )

register_google(key = "AIzaSyDr03JHZRWzEAZPsMBOLaC2ffpY8h9HBPU", write = TRUE) 
taiwan <- get_map(location = 'Taiwan', zoom = 8)

ggmap(taiwan) + geom_point(data = loc2, aes(x = lon, y = lat), alpha = 0.5, color = "red") + 
  theme(legend.position = "right") + 
  labs(
    x = "Longitude", 
    y = "Latitude", 
    title = "Location history data points",
    caption = "\nA simple point plot shows recorded positions.")

# Shifting vectors for latitude and longitude to include end position
shift.vec <- function(vec, shift){
  if (length(vec) <= abs(shift)){
    rep(NA ,length(vec))
  } else {
    if (shift >= 0) {
      c(rep(NA, shift), vec[1:(length(vec) - shift)]) }
    else {
      c(vec[(abs(shift) + 1):length(vec)], rep(NA, abs(shift)))
    }
  }
}

loc2$lat.p1 <- shift.vec(loc2$lat, -1)
loc2$lon.p1 <- shift.vec(loc2$lon, -1)

# Calculating distances between points (in metres) with the function pointDistance from the 'raster' package.
library(raster)
loc2$dist.to.prev <- apply(loc2, 1, FUN = function(row) {
  pointDistance(c(as.numeric(as.character(row["lat.p1"])),
                  as.numeric(as.character(row["lon.p1"]))),
                c(as.numeric(as.character(row["lat"])), as.numeric(as.character(row["lon"]))),
                lonlat = T) # Parameter 'lonlat' has to be TRUE!
})

distance_p_month <- aggregate(loc2$dist.to.prev, by = list(month_year = as.factor(loc2$month_year)), FUN = sum)
distance_p_month$x <- distance_p_month$x*0.001
ggplot(distance_p_month[-1, ], aes(x = month_year, y = x,  fill = month_year)) + 
  geom_bar(stat = "identity")  + 
  guides(fill = FALSE) +
  my_theme() +
  labs(
    x = "",
    y = "Distance in km",
    title = "Distance traveled per month in 2016",
    caption = "This barplot shows the sum of distances between recorded 
    positions for 2016. In September we went to the US and Canada."
  )
loc3 <- subset(loc, time>"2021-11-01")
df <- data.frame(x = loc3$lat, y = loc3$lon);
geocode('taiwan')
map <- get_googlemap('taiwan', markers = df, path = df, scale = 2);
ggmap(map, extent = 'normal');
ggmap(taiwan) +
  geom_point(data = loc3, aes(x=lon, y = lat),
             color = "red", size = 3, alpha = 0.5) +
  geom_density2d(size = 0.3)

activities <- loc2$activity

list.condition <- sapply(activities, function(x) !is.null(x[[1]]))
activities  <- activities[list.condition]

df <- do.call("rbind", activities)
main_activity <- sapply(df$activities, function(x) x[[1]][1][[1]][1])

activities_2 <- data.frame(main_activity = main_activity, 
                           time = as.POSIXct(as.numeric(df$timestampMs)/1000, origin = "1970-01-01"))

head(activities_2)
