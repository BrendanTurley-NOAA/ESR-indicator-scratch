
# install.packages("marmap")
library(marmap)

# Example: Data for a region in the North Atlantic
# Define the coordinates (lon1, lon2, lat1, lat2) and resolution
my_region <- getNOAA.bathy(lon1 = -87, lon2 = -81, 
                           lat1 = 24, lat2 = 31, 
                           resolution = 5) # Larger numbers (e.g., 5) for a faster, coarser resolution

# You can check a summary of the data
summary(my_region)

# Define the start and end points of your transect (e.g., from lon/lat 1 to lon/lat 2)
# The inputs are: bathy data object, longitude start, latitude start, longitude end, latitude end
transect_data <- get.transect(my_region, 
                              x1 = -82.629, y1 = 27.646, 
                              x2 = -87, y2 = 27.646, 
                              distance = TRUE) # Calculates distance in kilometers

# Plot the depth (y-axis) against the distance (x-axis)
plotProfile(transect_data)
abline(h=c(-200,-150,-100,-75))

plot(-transect_data$dist.km, transect_data$depth, typ = 'l')
abline(h=c(-200,-150,-100,-75))

