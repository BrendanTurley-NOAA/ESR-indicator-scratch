
library(abind)
library(dplyr)
library(lubridate)
library(ncdf4)
library(terra)
library(sf)
library(reticulate)
library(heatwave3)

### this adds a fahrenheit axis on the right of the plot by converting the celcius default
ax_convert_c2f <- function(vals, side = 4, n = 5, las = 1, ...){ ### ... used to pass other parameters for interior fxns
  tick_val <- pretty(vals*(9/5)+32, n = n, ...)
  axis(side, (tick_val-32)*(5/9), tick_val, las = las, ...)
}

### area of interest
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

# virtualenv_create(envname = "CopernicusMarineR", packages = c("copernicusmarine"))
# Activate the virtual environment (must be done before importing any Python module)
use_virtualenv("CopernicusMarineR", required = TRUE)

# Optional sanity check: confirm which Python reticulate is using
py_config()

# Import the Python module
copernicusmarine <- import("copernicusmarine")

# The adapted command
result <- copernicusmarine$subset(
  dataset_id = "cmems_mod_glo_phy_my_0.083deg_P1M-m",
  dataset_version="202311",
  variables = list("bottomT"),  # Use list() so reticulate passes a proper Python list
  minimum_longitude = min_lon,
  maximum_longitude = max_lon,
  minimum_latitude = min_lat,
  maximum_latitude = max_lat,
  start_datetime = "1993-01-01T00:00:00",
  end_datetime   = "2025-12-01T00:00:00",
  output_directory = "C:/Users/brendan.turley/Documents/data/copernicusmarine/sbt"
)

# The adapted command
result2 <- copernicusmarine$subset(
  dataset_id = "cmems_mod_glo_phy_anfc_0.083deg_static",
  dataset_version="202211",
  variables = list("deptho"),  # Use list() so reticulate passes a proper Python list
  minimum_longitude = min_lon,
  maximum_longitude = max_lon,
  minimum_latitude = min_lat,
  maximum_latitude = max_lat,
  output_directory = "C:/Users/brendan.turley/Documents/data/copernicusmarine"
)

setwd('C:/Users/brendan.turley/Documents/data/copernicusmarine')
dat <- nc_open('cmems_mod_glo_phy_anfc_0.083deg_static_deptho_98.00W-80.00W_18.00N-31.00N.nc')
deptho <- ncvar_get(dat, 'deptho')
nc_close(dat)


sbt2 <- rast('cmems_mod_glo_phy_my_0.083deg_P1M-m_bottomT_98.00W-80.00W_18.00N-31.00N_1993-01-01-2025-12-01.nc')

setwd('C:/Users/brendan.turley/Documents/data/copernicusmarine/sbt')
dat <- nc_open('cmems_mod_glo_phy_my_0.083deg_P1M-m_bottomT_98.00W-80.00W_18.00N-31.00N_1993-01-01-2025-12-01.nc')
sbt <- ncvar_get(dat, 'bottomT')
time <- ncvar_get(dat, 'time')/24
time <- as.Date(time, origin = '1950-01-01')
nc_close(dat)
# hist(pp)
## 10^mean(log10(x + .0001), na.rm = T)

sbt <- aperm(sbt, c(2,1,3))
sbt_r <- rast(sbt[dim(sbt)[1]:1,,], crs="EPSG:4326")
ext(sbt_r) <- c(min_lon, max_lon, min_lat, max_lat)
time(sbt_r) <- as.Date(time)

### US EEZ
sbt_eez <- crop(sbt_r, gulf_eez) |> mask(gulf_eez)
# test <- sds(ann_gwide)
cellsize_km <- cellSize(sbt_eez, unit='km') |> values() |> mean()

setwd("~/R_projects/ESR-indicator-scratch/data/intermediate_files")
writeCDF(sbt_eez, 'sbt_eez.nc',overwrite=TRUE)
dat <- nc_open('sbt_eez.nc')
data <- ncvar_get(dat, 'oisst_gulf')
lon <- ncvar_get(dat, 'longitude')
lat <- ncvar_get(dat, 'latitude')
lon_lat <- expand.grid(lon = lon,lat = lat)

dat_m <- apply(data,c(1,2),mean,na.rm=T)
ngrid <- length(which(!is.na(dat_m)))



setwd('C:/Users/brendan.turley/Documents/data/copernicusmarine/sbt')# https://robwschlegel.github.io/heatwave3/reference/detect3.html
# https://robwschlegel.github.io/heatwaveR/reference/ts2clm.html
# https://robwschlegel.github.io/heatwaveR/reference/detect_event.html
mhw_cube <- detect3(file_in = 'cmems_mod_glo_phy_my_0.083deg_P1M-m_bottomT_98.00W-80.00W_18.00N-31.00N_1993-01-01-2025-12-01.nc',
                    return_type = "df", 
                    clim_period = c("1993-01-01", "2023-12-31"))



setwd('C:/Users/brendan.turley/Documents/data/copernicusmarine')
deptho2 <- rast('cmems_mod_glo_phy_anfc_0.083deg_static_deptho_98.00W-80.00W_18.00N-31.00N.nc')
msk <- ifel(deptho2 > 100, NA, 1)

setwd('C:/Users/brendan.turley/Documents/data/copernicusmarine/sbt')
sbt <- rast('cmems_mod_glo_phy_my_0.083deg_P1M-m_bottomT_98.00W-80.00W_18.00N-31.00N_1993-01-01-2025-12-01.nc') |>
  mask(msk)
plot(sbt)

### US EEZ
sbt_eez <- crop(sbt, gulf_eez) |> mask(gulf_eez)
# test <- sds(ann_gwide)
cellsize_km <- cellSize(sbt_eez, unit='km') |> values() |> mean()


### annual
# a. annual US Gulf EEZ
year_index <- year(time(sbt_eez))

sbt_eez_layers <- tapp(sbt_eez, index = year_index, fun = mean, na.rm = TRUE)
sbt_eez_ts <- global(sbt_eez_layers, fun = c('mean',"range"), na.rm = TRUE, weighted = T)

sbt_eez_ts <- data.frame(
  year = unique(year_index), # Convert index back to Date
  sbt = sbt_eez_ts$mean,
  min = sbt_eez_ts$min,
  max = sbt_eez_ts$max
)

plot(sbt_eez_ts$year, sbt_eez_ts$sbt, typ = 'o', pch = 16,
     panel.first = grid())


sbt_eez_ts <- global(sbt_eez, fun = c('mean',"range"), na.rm = TRUE, weighted = T)

sbt_eez_ts <- data.frame(
  time = time(sbt_eez), # Convert index back to Date
  sbt = sbt_eez_ts$mean,
  min = sbt_eez_ts$min,
  max = sbt_eez_ts$max
)

plot(sbt_eez_ts$time, sbt_eez_ts$sbt, typ = 'o', pch = 16,
     panel.first = grid())



# add yearmonth column --------------------------
sbt_eez_ts$yrmon <- paste(sbt_eez_ts$time |> year(),
                       sprintf("%02.f", sbt_eez_ts$time |> month()),
                       sep = '-')

### add seasons
sbt_eez_ts$jday <- yday(sbt_eez_ts$time)

sbt_eez_ts <- sbt_eez_ts |>
  mutate(season = case_when(
    month(time)==12 | month(time)<3 ~ 'win',
    month(time)>2 & month(time)<6 ~ 'spr',
    month(time)>5 & month(time)<9 ~ 'sum',
    month(time)>8 & month(time)<12 ~ 'aut'
  )) |>
  arrange(time)

### create season_yr and adjust to make december n-1 part of winter n
sbt_eez_ts$season_yr <- ifelse(month(sbt_eez_ts$time)==12, 
                            year(sbt_eez_ts$time)+1, 
                            year(sbt_eez_ts$time))
sbt_eez_ts$season_yr[which(sbt_eez_ts$season_yr==2026)] <- NA

### alternative to redefine seasons as jfm, amj, jas, ond
# dat_eez <- dat_eez |>
#   mutate(season = case_when(
#     month(time)<4 ~ 'win',
#     month(time)>3 & month(time)<7 ~ 'spr',
#     month(time)>4 & month(time)<10 ~ 'sum',
#     month(time)>9 ~ 'aut'
#   )) |>
#   arrange(time)
# dat_eez$season_yr <- year(dat_eez$time)

### seasonal means
eez_win <- aggregate(sbt ~ season_yr, data = subset(sbt_eez_ts, season=='win'),
                     mean, na.rm = T)
eez_spr <- aggregate(sbt ~ season_yr, data = subset(sbt_eez_ts, season=='spr'),
                     mean, na.rm = T)
eez_sum <- aggregate(sbt ~ season_yr, data = subset(sbt_eez_ts, season=='sum'),
                     mean, na.rm = T)
eez_aut <- aggregate(sbt ~ season_yr, data = subset(sbt_eez_ts, season=='aut'),
                     mean, na.rm = T)

# png(here('figures/plots/sst-seasonal-plot.png'), width = 9, height = 6, units = 'in', res = 300)
par(mfrow = c(2,2), mar = c(3,5,2,3),
    oma = c(0,0,3,0))

plot(eez_win$season_yr, eez_win$sbt, 
     typ = 'o', pch = 16, las = 1,
     panel.first = list(abline(lm(sbt ~ season_yr, data = eez_win), lwd = 4, col = 'orange'),
                        abline(h = mean(eez_win$sbt), col = 'gray', lwd = 2),
                        grid()),
     xlab = '', ylab = 'SBT', main = 'Winter - DJF')
mtext('(°C)', side = 3, adj = -.1, line = .5)
mtext('(°F)', side = 3, adj = 1.1, line = .5)
ax_convert_c2f(eez_win$sbt, n = 4)

plot(eez_spr$season_yr, eez_spr$sbt, 
     typ = 'o', pch = 16, las = 1,
     panel.first = list(abline(lm(sbt ~ season_yr, data = eez_spr), lwd = 4, col = 'orange'),
                        abline(h = mean(eez_spr$sbt), col = 'gray', lwd = 2),
                        grid()),
     xlab = '', ylab = 'SBT', main = 'Spring - MAM')
mtext('(°C)', side = 3, adj = -.1, line = .5)
mtext('(°F)', side = 3, adj = 1.1, line = .5)
ax_convert_c2f(eez_spr$sbt, n = 4)

plot(eez_sum$season_yr, eez_sum$sbt, 
     typ = 'o', pch = 16, las = 1,
     panel.first = list(abline(lm(sbt ~ season_yr, data = eez_sum), lwd = 4, col = 'orange'),
                        abline(h = mean(eez_sum$sbt), col = 'gray', lwd = 2),
                        grid()),
     xlab = '', ylab = 'SBT', main = 'Summer - JJA')
mtext('(°C)', side = 3, adj = -.1, line = .5)
mtext('(°F)', side = 3, adj = 1.1, line = .5)
ax_convert_c2f(eez_sum$sbt, n = 4)

plot(eez_aut$season_yr, eez_aut$sbt,
     typ = 'o', pch = 16, las = 1,
     panel.first = list(abline(lm(sbt ~ season_yr, data = eez_aut), lwd = 4, col = 'orange'),
                        abline(h = mean(eez_aut$sbt), col = 'gray', lwd = 2),
                        grid()),
     xlab = '', ylab = 'SBT', main = 'Fall - SON')
mtext('(°C)', side = 3, adj = -.1, line = .5)
mtext('(°F)', side = 3, adj = 1.1, line = .5)
ax_convert_c2f(eez_aut$sbt, n = 4)

mtext('US Gulf EEZ Bottom Temperatures', side = 3, outer = TRUE, cex = 5/4, font = 2, line = 5/4)
dev.off()




### spatial regression

sbt_m <- app(sbt_eez, mean, na.rm = t)

sbt_eez_anom <- sbt_eez - sbt_m

recent_lyrs <- sbt_eez_anom[[time(sbt_eez_anom) > "2020-01-01"]]
sbt_5yr_t <- regress(recent_lyrs, 1:nlyr(recent_lyrs))
plot(sbt_5yr_t[['x']])

sbt_25 <- sbt_eez_anom[[time(sbt_eez_anom) > "2024-12-31"]] |>
  app(mean, na.rm = t)


### colors and breaks for plotting
t_brks <- seq(-.03,.03,.001)
t_cols <- cmocean('balance')(length(t_brks)-1)
a_brks <- seq(-1.5,1.5,.05)
a_cols <- cmocean('balance')(length(a_brks)-1)


# png(here('figures/plots/sst-spatial-plot.png'), width = 4, height = 6, units = 'in', res = 300)
par(mfrow=c(2,1))
plot(sbt_5yr_t[['x']],
     col = t_cols, range = c(-.03,.03),
     plg = list(tick = 'out', format='g'),
     main = '2021-2025 SST Trend (°C/month)')
plot(world, add= T, col = 'gray')
plot(gulf_eez['geometry'], add = T)

plot(sbt_25, 
     col = t_cols, range = c(-1.5,1.5),
     plg = list(tick = 'out', format='g'),
     main = '2025 SST anomaly (°C)')
plot(world, add= T, col = 'gray')
plot(gulf_eez['geometry'], add = T)
# dev.off()








### mhw -- this needs daily data as a minimum ###

setwd("~/R_projects/ESR-indicator-scratch/data/intermediate_files")
writeCDF(sbt_eez, 'sbt_eez.nc',overwrite=TRUE)
dat <- nc_open('sbt_eez.nc')
data <- ncvar_get(dat, 'bottomT')
lon <- ncvar_get(dat, 'longitude')
lat <- ncvar_get(dat, 'latitude')
lon_lat <- expand.grid(lon = lon,lat = lat)

dat_m <- apply(data,c(1,2),mean,na.rm=T)
ngrid <- length(which(!is.na(dat_m)))


setwd("~/R_projects/ESR-indicator-scratch/data/intermediate_files")
# https://robwschlegel.github.io/heatwave3/reference/detect3.html
# https://robwschlegel.github.io/heatwaveR/reference/ts2clm.html
# https://robwschlegel.github.io/heatwaveR/reference/detect_event.html
mhw_cube <- detect3(file_in = 'sbt_eez.nc',
                    return_type = "df", 
                    clim_period = c("1993-01-01", "2023-12-31"))
save(mhw_cube, file = 'mhw_sbt_results.RData')
gc()


### plots -------------------

setwd("~/R_projects/ESR-indicator-scratch/data/intermediate_files")
load('mhw_sbt_results.RData')

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



