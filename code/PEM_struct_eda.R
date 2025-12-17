
library(terra)
library(lubridate)

setwd("C:/Users/brendan.turley/Documents/data/PEM_artifical_structures/ARP FY26")

struc <- read.csv('ARP_update_Nov25.csv')


str_ll <- data.frame(x = struc$Longitude,
                        y = struc$Latitude)

struc_shp <- vect('ARPDPP_112525_final.shp')


struc$Year
struc$year_built <- NA
struc$year_built[which(nchar(struc$Year)==4)] <- as.numeric(struc$Year[which(nchar(struc$Year)==4)])
struc$year_built[which(nchar(struc$Year)>4)] <- struc$Year[which(nchar(struc$Year)>4)] |> mdy() |> year()

table(struc$year_built)


### shapefile
plot(struc_shp)

# struc_rast <- rast(struc_shp, resolution = 1)
struc_rast <- rast(ext(struc_shp), resolution = 0.1, crs=crs(struc_shp))

# test <- rasterize(struc_shp, struc_rast, fun = 'count')
test <- rasterize(str_ll, struc_rast, fun = 'count')
# test[is.na(test)] <- 0
plot(test, col = rev(map.pal('plasma',100)))


polys <- as.polygons(test, values = T, dissolve = F)

plot(polys, add=T, border = 'gray')
