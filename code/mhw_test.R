
library(abind)
library(lubridate)
library(ncdf4)
library(terra)
library(sf)
library(heatwave3)


# define years  --------------------------------
styear <- 1982
enyear <- 2025

# define spatial domain  --------------------------------
min_lon <- -98
max_lon <- -80
min_lat <- 18
max_lat <- 31

# load shapefile to subset  --------------------------------
### shapefiles downloaded from marineregions.org (future goal implement mregions2 R package for shapefile)
setwd("~/data/shapefiles/gulf_eez")
eez <- vect('eez.shp') |> makeValid()

setwd("~/data/shapefiles/gulf_iho")
iho <- vect('iho.shp') |> makeValid()

gulf_eez <- terra::intersect(eez, iho)

# gulf_eez <- terra::intersect(eez, iho) |>
#   st_as_sf() |> 
#   st_transform(crs = st_crs(4326))


### load data
setwd("~/R_projects/ESR-indicator-scratch/data/intermediate_files")

for(i in styear:enyear){
  cat(i, '\n')
  tmp <- paste0('sst_',i) |> readRDS()
  
  tmp$sst[which(tmp$sst==-999)] <- NA
  
  if(i==styear){
    sst_a <- tmp$sst
    dates <- tmp$time
  } else {
    sst_a <- abind(sst_a,
                   tmp$sst,
                   along = 3)
    dates <- c(dates,
               tmp$time)
  }
}

sst_a <- aperm(sst_a, c(2,1,3))
sst_r <- rast(sst_a[dim(sst_a)[1]:1,,], crs="EPSG:4326") 
ext(sst_r) <- c(min_lon, max_lon, min_lat, max_lat)
time(sst_r) <- as.Date(dates)

ann_gwide <- crop(sst_r, gulf_eez) |> mask(gulf_eez)
test <- sds(ann_gwide)

setwd("~/R_projects/ESR-indicator-scratch/data/intermediate_files")
writeCDF(ann_gwide, 'oisst_gulf.nc',overwrite=TRUE)
dat <- nc_open('oisst_gulf.nc')
data <- ncvar_get(dat, 'oisst_gulf')


setwd("~/R_projects/ESR-indicator-scratch/data/intermediate_files")
# https://robwschlegel.github.io/heatwave3/reference/detect3.html
# https://robwschlegel.github.io/heatwaveR/reference/ts2clm.html
# https://robwschlegel.github.io/heatwaveR/reference/detect_event.html
mhw_cube <- detect3(file_in = 'oisst_gulf.nc',
                    return_type = "df", clim_period = c("1982-01-01", "2011-12-31"))
save(mhw_cube, file = 'mhw_results.RData')
gc()

yr_mon <- aggregate(cell ~ year(index_start) + month(index_start),
          data = mhw_cube,
          function(x) length(unique(x))) |>
  setNames(c('year','month','cell')) |>
  merge(expand.grid(year=1982:2025,month=1:12),all=T)

mhw_m <- matrix(yr_mon$cell, 12, length(1982:2025)) |> t()

image(1982:2025, 1:12, mhw_m)
