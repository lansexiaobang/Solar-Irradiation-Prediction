# get data and package
setwd("I:/R Data/spatial 2014")
lalo = read.csv("SMUD_latlong.csv",header = F)
data = lalo
names(data) = c("lat", "long")
library(ggplot2)
library(ggmap)

#### use the median of the long and lat to get the map
long = unique(lalo[,2])
lat = unique(lalo[,1])
 mapImageData = get_googlemap(
      center = c(lon = median(long), lat = median(lat)),
       zoom = 10,
       maptype = c("satellite")
)


# plot 
ggmap(mapImageData) + geom_point(aes(x = long, y = lat, size = 3),colour = "yellow", data = data, alpha = 0.5)
