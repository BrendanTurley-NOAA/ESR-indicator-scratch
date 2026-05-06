
library(abind)
library(dplyr)
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
# test <- sds(ann_gwide)
cellsize_km <- cellSize(ann_gwide,unit='km') |> values() |> mean()

setwd("~/R_projects/ESR-indicator-scratch/data/intermediate_files")
writeCDF(ann_gwide, 'oisst_gulf.nc',overwrite=TRUE)
dat <- nc_open('oisst_gulf.nc')
data <- ncvar_get(dat, 'oisst_gulf')
lon <- ncvar_get(dat, 'longitude')
lat <- ncvar_get(dat, 'latitude')
lon_lat <- expand.grid(lon = lon,lat = lat)

dat_m <- apply(data,c(1,2),mean,na.rm=T)
ngrid <- length(which(!is.na(dat_m)))

setwd("~/R_projects/ESR-indicator-scratch/data/intermediate_files")
# https://robwschlegel.github.io/heatwave3/reference/detect3.html
# https://robwschlegel.github.io/heatwaveR/reference/ts2clm.html
# https://robwschlegel.github.io/heatwaveR/reference/detect_event.html
mhw_cube <- detect3(file_in = 'oisst_gulf.nc',
                    return_type = "df", clim_period = c("1982-01-01", "2011-12-31"))
save(mhw_cube, file = 'mhw_results.RData')
gc()


### plots -------------------

setwd("~/R_projects/ESR-indicator-scratch/data/intermediate_files")
load('mhw_results.RData')

par(mfrow=c(2,2),
    mar = c(4,4,1,1))

### yr-month ----------------
yr_mon <- aggregate(cell ~ year(index_start) + month(index_start),
          data = mhw_cube,
          function(x) length(unique(x))) |>
  setNames(c('year','month','cell')) |>
  merge(expand.grid(year=1982:2025,month=1:12),all=T)
yr_mon$cell[is.na(yr_mon$cell)] <- 0
yr_mon$percent <- yr_mon$cell / ngrid
yr_mon$kmsq <- yr_mon$cell * cellsize_km
yr_mon$date <- as.Date(paste(yr_mon$year,yr_mon$month,'01',sep = '-'))

mhw_m <- matrix(yr_mon$cell, 12, length(1982:2025)) |> t()

# image(1982:2025, 1:12, mhw_m)
# barplot(apply(mhw_m,2,mean,na.rm=T))

plot(yr_mon$date, yr_mon$percent, typ = 'l')
# plot(yr_mon$date, yr_mon$kmsq, typ = 'l')


# seasons <- list(c(12,1,2),
#      c(3,4,5),
#      c(6,7,8),
#      c(9,10,11))
# yr_mon$yr_sea <- ifelse(yr_mon$month==12, yr_mon$year+1, yr_mon$year)
# 
# par(mfrow=c(2,2))
# 
# for(i in 1:4){
#   tmp <- subset(yr_mon, month %in% seasons[[i]])
#   yagg <- aggregate(percent ~ year, data = tmp, mean, na.rm = T)
#   plot(yagg$year, yagg$percent, typ = 'l')
# }



### annual ----------------
yr_mhw <- aggregate(cell ~ year(index_start),
                    data = mhw_cube,
                    function(x) length(unique(x))) |>
  setNames(c('year','cell')) |>
  merge(expand.grid(year=1982:2025),all=T)
yr_mhw$cell[is.na(yr_mhw$cell)] <- 0
yr_mhw$percent <- yr_mhw$cell / ngrid
yr_mhw$kmsq <- yr_mhw$cell * cellsize_km

plot(yr_mhw$year, yr_mhw$percent, typ = 'l')
# plot(yr_mhw$year, yr_mhw$kmsq, typ = 'l')


### yr-month-degree days ----------------
yr_mon_dd <- aggregate(intensity_cumulative ~ year(index_start) + month(index_start),
                    data = mhw_cube,
                    mean, na.rm=T) |>
  setNames(c('year','month','intensity_cumulative')) |>
  merge(expand.grid(year=1982:2025,month=1:12),all=T)
yr_mon_dd$intensity_cumulative[is.na(yr_mon_dd$intensity_cumulative)] <- 0
yr_mon_dd$date <- as.Date(paste(yr_mon_dd$year,yr_mon_dd$month,'01',sep = '-'))

mhw_m <- matrix(yr_mon_dd$intensity_cumulative, 12, length(1982:2025)) |> t()

# image(1982:2025, 1:12, mhw_m)
# barplot(apply(mhw_m,2,mean,na.rm=T))

plot(yr_mon_dd$date, yr_mon_dd$intensity_cumulative, typ = 'l')

yr_mhw_dd <- aggregate(cbind(intensity_cumulative) ~ year(index_start),
                    data = mhw_cube,
                    mean, na.rm=T) |>
  setNames(c('year','intensity_cumulative')) |>
  merge(expand.grid(year=1982:2025),all=T)
yr_mhw_dd$intensity_cumulative[is.na(yr_mhw_dd$intensity_cumulative)] <- 0

plot(yr_mhw_dd$year, yr_mhw_dd$intensity_cumulative, typ = 'l')


### seasonal -------------------
mhw_cube <- mhw_cube |> 
  mutate(year = year(index_start),
         month = month(index_start),
         yr_sea = case_when(
           month==12 ~ year+1,
           TRUE ~ year
         ))
seasons <- list(c(12,1,2),
                c(3,4,5),
                c(6,7,8),
                c(9,10,11))
# yr_mon$yr_sea <- ifelse(yr_mon$month==12, yr_mon$year+1, yr_mon$year)

par(mfrow=c(2,2))

for(i in 1:4){
  tmp <- subset(mhw_cube, month %in% seasons[[i]])
  yagg <- aggregate(cell ~ year(index_start),
                    data = tmp,
                    function(x) length(unique(x))) |>
    setNames(c('year','cell')) |>
    merge(expand.grid(year=1982:2025),all=T)
  yagg$cell[is.na(yagg$cell)] <- 0
  yagg$percent <- yagg$cell / ngrid
  yagg$kmsq <- yagg$cell * cellsize_km
  
  mod <- summary(lm(percent ~ year, data = yagg))
  plot(yagg$year, yagg$percent, typ = 'l', lwd = 2)
  if(mod$coefficients[8]<=.05){
    abline(mod, col = 'orange', lwd = 2)
  }
  print(mod)
}


par(mfrow=c(2,2))

for(i in 1:4){
  tmp <- subset(mhw_cube, month %in% seasons[[i]])
  yagg <- aggregate(intensity_cumulative ~ year(index_start),
                    data = tmp,
                    mean, na.rm = T) |>
    setNames(c('year','intensity_cumulative')) |>
    merge(expand.grid(year=1982:2025),all=T)
  yagg$cell[is.na(yagg$intensity_cumulative)] <- 0
  
  mod <- summary(lm(intensity_cumulative ~ year, data = yagg))
  plot(yagg$year, yagg$intensity_cumulative, typ = 'l', lwd = 2)
  if(mod$coefficients[8]<=.05){
    abline(mod, col = 'orange', lwd = 2)
  }
  print(mod)
}



### what/where are the cell #

cell_ll <- unique(mhw_cube[, c("cell", "x", 'y')]) |>
  setNames(c('cell','lon','lat'))

plot(mhw_cube$x, mhw_cube$y)

event_no <- aggregate(event_no ~ cell, data = mhw_cube, length)
hist(event_no$event_no)

gridcell <- cell_ll |>
  merge(event_no) |>
  merge(lon_lat, all = T)

library(fields)
imagePlot(lon, rev(lat),
          t(matrix(gridcell$event_no, 29, 69)), asp = T)


event_no_yr <- aggregate(event_no ~ cell + year(index_start), data = mhw_cube, length) |>
  setNames(c('cell','year','event_no'))
table(event_no_yr$cell) |> hist()
event_no_mean <- aggregate(event_no ~ cell, data = event_no_yr, mean, na.rm = T) |>
  merge(cell_ll, all = T) |>
  merge(lon_lat, all = T)

imagePlot(lon, rev(lat),
          t(matrix(event_no_mean$event_no, 29, 69)), asp = T)

library(data.table)
setDT(event_no_yr)

slopes_dt <- event_no_yr[, 
                     .(slope = coef(lm(event_no ~ year, na.action = na.exclude))[2]), 
                     by = cell]
gridcell_lm <- unique(mhw_cube[, c("cell", "x", 'y')]) |>
  setNames(c('cell','lon','lat')) |>
  merge(slopes_dt) |>
  merge(lon_lat, all = T)
hist(gridcell_lm$slope)
imagePlot(lon, rev(lat),
          t(matrix(gridcell_lm$slope, 29, 69)), asp = T)


results <- event_no_yr[, {
  model <- lm(event_no ~ year, na.action = na.exclude)
  summary_mod <- summary(model)$coefficients
  .(slope = summary_mod["year", "Estimate"], 
    p_val = summary_mod["year", "Pr(>|t|)"])
}, by = cell]

significant_slopes <- results[p_val < 0.05]

gridcell_sig <- unique(mhw_cube[, c("cell", "x", 'y')]) |>
  setNames(c('cell','lon','lat')) |>
  merge(significant_slopes) |>
  merge(lon_lat, all = T)
hist(gridcell_sig$slope)
imagePlot(lon, rev(lat),
          t(matrix(gridcell_sig$slope, 29, 69)), asp = T)


