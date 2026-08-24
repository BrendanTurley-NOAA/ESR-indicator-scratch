# File created on 2026-05-06 by B. Turley

#### 0. Setup ####
# Add any packages that are needed for analysis here.
library(IEAnalyzeR)
library(here)
library(ggplot2)
library(abind)
library(data.table)
library(dplyr)
library(lubridate)
library(ncdf4)
library(terra)
library(sf)
library(pak)
library(rnaturalearth)
library(rnaturalearthdata)
# pak::pak("robwschlegel/heatwave3")
library(heatwave3)
library(cmocean)

# File Naming Setup.
root_name <- "mhw-surface"

csv_filename <- here(paste0("data/formatted/formatted_csvs/", root_name, "_formatted.csv"))
object_filename <- here(paste0("data/formatted/final_objects/", root_name, "_object.rds"))
plot_filename <- here(paste0("figures/plots/", root_name, "_plot.png"))

#----------------------------------------------------
#### 1. Read Data ####
# Pull data from its source:
# Manual data: data/unformatted data
# Automated data: Add script for data call (API, package, etc.)
# Confidential data: Store locally in the confidential data folder
#   - This folder is excluded using gitignore and will not push to the GitHub repo
# If intermediate data (shapefiles etc.) are needed, please put them in data>intermediate
#   - Filename should use the syntax rootname_descriptivename


# define years  --------------------------------
styear <- 1982
enyear <- 2025

# define spatial domain  --------------------------------
min_lon <- -98
max_lon <- -80
min_lat <- 18
max_lat <- 31

### bathymetry
# setwd("~/data/bathy")
# burl <- 'etopo1.nc'

burl <- 'https://www.ngdc.noaa.gov/thredds/dodsC/global/ETOPO2022/60s/60s_bed_elev_netcdf/ETOPO_2022_v1_60s_N90W180_bed.nc'
bdat <- nc_open(burl)
# crs <- 'EPSG:4326'

ln <- ncvar_get(bdat, 'lon')
ln_i <- which(ln>=min_lon & ln<=max_lon)
lt <- ncvar_get(bdat, 'lat')
lt_i <- which(lt>=min_lat & lt<=max_lat)

bathy <- ncvar_get(bdat, 'z', 
                   start = c(ln_i[1],lt_i[1]),
                   count = c(length(ln_i), length(lt_i)))
# bathy <- ncvar_get(bdat, 'Band1', 
#                    start = c(ln_i[1],lt_i[1]),
#                    count = c(length(ln_i), length(lt_i)))
nc_close(bdat)

# load shapefile to subset  --------------------------------
### shapefiles downloaded from marineregions.org (future goal implement mregions2 R package for shapefile)
setwd("~/data/shapefiles/gulf_eez")
eez <- vect('eez.shp') |> makeValid()

setwd("~/data/shapefiles/gulf_iho")
iho <- vect('iho.shp') |> makeValid()

gulf_eez <- terra::intersect(eez, iho)


# download by year to avoid timeout errors --------------------

######################################################
#### don't run while reviewing code; takes awhile ####
#### load saved intermediate files below loop ########
######################################################

review_code <- T ### set to F to rerun download loop

if(review_code == F){
  
  ### pull oisst
  
  url <- 'https://www.ncei.noaa.gov/thredds/dodsC/OisstBase/NetCDF/V2.1/AVHRR/198109/oisst-avhrr-v02r01.19810901.nc'
  
  dat <- nc_open(url)
  
  lon <- ncvar_get(dat, 'lon')
  lat <- ncvar_get(dat, 'lat')
  
  i_lon <- which(lon >= (360+min_lon) & lon <= (360+max_lon))
  i_lat <- which(lat <= (max_lat) & lat >= (min_lat))
  lons <- lon[i_lon]
  lats <- lat[i_lat]
  
  # define time domain  --------------------------------
  years <- 1982:2025
  
  
  system.time(
    setwd("C:/Users/brendan.turley/Documents/R_projects/ESR-indicator-scratch/data/intermediate_files"),
    for(h in 1:length(years)){
      cat('\n', years[h], '\n')
      
      dates <- seq(ymd(paste0(years[h],'-01-01')),
                   ymd(paste0(years[h],'-12-31')),
                   by = 'day')
      # dates |> as.character()
      dates <- gsub('-','',dates)
      nyr <- ifelse(leap_year(years[h]),366,365)
      
      sst_a <- array(NA, dim = c(72,52,nyr))
      time_a <- rep(NA,nyr) |> as.Date()
      
      pb <- txtProgressBar(min = 0, max = length(dates), style = 3)
      for(i in 1:length(dates)){
        url <- paste0('https://www.ncei.noaa.gov/thredds/dodsC/OisstBase/NetCDF/V2.1/AVHRR/',
                      substr(dates[i],1,6),
                      '/oisst-avhrr-v02r01.',
                      dates[i],
                      '.nc')
        dat <- nc_open(url)
        time <- ncvar_get(dat, 'time') |>
          as.Date(origin = '1978-01-01')
        sst_grab <- ncvar_get(dat, 'sst', 
                              start = c(i_lon[1], i_lat[1], 1, 1), 
                              count = c(length(i_lon), length(i_lat), -1, -1))
        sst_a[,,i] <- sst_grab
        time_a[i] <- time
        
        nc_close(dat)
        setTxtProgressBar(pb, i)
      }
      
      out <- list(sst = sst_a, time = time_a)
      # assign(paste0('sst_',years[h]), out)
      saveRDS(out, paste0('sst_',years[h]))
      
    }
  )
  
  
  ### load sst data and combine
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
  ### save intermediate file
  setwd("~/R_projects/ESR-indicator-scratch/data/intermediate_files")
  saveRDS(sst_r, 'sst_raster_brick.rds')
  sst_r <- readRDS('sst_raster_brick.rds')
  
  ann_gwide <- crop(sst_r, gulf_eez) |> mask(gulf_eez)
  cellsize_km <- cellSize(ann_gwide,unit='km') |> values() |> mean()
  
  ### save intermediate file
  setwd("~/R_projects/ESR-indicator-scratch/data/intermediate_files")
  writeCDF(ann_gwide, 'oisst_gulf.nc',overwrite=TRUE)
  dat <- nc_open('oisst_gulf.nc')
  data <- ncvar_get(dat, 'oisst_gulf')
  lon <- ncvar_get(dat, 'longitude')
  lat <- ncvar_get(dat, 'latitude')
  lon_lat <- expand.grid(lon = lon,lat = lat)
  
  dat_m <- apply(data,c(1,2),mean,na.rm=T)
  ngrid <- length(which(!is.na(dat_m)))
  
  ### this is the MHW detection function
  setwd("~/R_projects/ESR-indicator-scratch/data/intermediate_files")
  mhw_cube <- detect3(file_in = 'oisst_gulf.nc',
                      return_type = "df", 
                      clim_period = c("1982-01-01", "2011-12-31"))
  ### save intermediate file
  save(mhw_cube, ngrid, cellsize_km, lon_lat,
       file = 'mhw_results.RData')
  gc()
  
  
  ### detrended
  # 1. Create a time vector (e.g., layer indices 1 to n)
  t_vals <- 1:nlyr(sst_r)
  
  # 2. Run cell-level regression to get intercept and slope
  # regress returns a SpatRaster with 2 layers: (Intercept) and x (slope)
  trend_model <- regress(sst_r, t_vals, na.rm = T)
  
  # 3. Calculate the linear trend for each layer
  # trend = intercept + slope * time
  intercept <- trend_model[[1]]
  slope <- trend_model[[2]]
  
  # Generate the trend raster stack
  trend_stack <- intercept + slope * t_vals
  
  # 4. Subtract the trend from the original data (Detrend)
  sst_rdt <- sst_r - trend_stack
  
  rm(trend_model,intercept,slope,trend_stack)
  ### save intermediate file
  setwd("~/R_projects/ESR-indicator-scratch/data/intermediate_files")
  saveRDS(sst_rdt, 'sst_rdetrend_brick.rds')
  sst_rdt <- readRDS('sst_rdetrend_brick.rds')
  
  ann_dt_gwide <- crop(sst_rdt, gulf_eez) |> mask(gulf_eez)
  cellsize_km <- cellSize(ann_dt_gwide, unit='km') |> values() |> mean()
  
  ### save intermediate file
  setwd("~/R_projects/ESR-indicator-scratch/data/intermediate_files")
  writeCDF(ann_dt_gwide, 'oisst_dt_gulf.nc',overwrite=TRUE)
  dat <- nc_open('oisst_dt_gulf.nc')
  data <- ncvar_get(dat, 'oisst_dt_gulf')
  lon <- ncvar_get(dat, 'longitude')
  lat <- ncvar_get(dat, 'latitude')
  lon_lat <- expand.grid(lon = lon,lat = lat)
  
  dat_m <- apply(data,c(1,2),mean,na.rm=T)
  ngrid <- length(which(!is.na(dat_m)))
  
  ### this is the MHW detection function
  setwd("~/R_projects/ESR-indicator-scratch/data/intermediate_files")
  mhw_dt_cube <- detect3(file_in = 'oisst_dt_gulf.nc',
                         return_type = "df", 
                         clim_period = c("1982-01-01", "2011-12-31"))
  ### save intermediate file
  setwd(here('data/intermediate'))
  save(mhw_dt_cube, ngrid, cellsize_km, lon_lat,
       file = 'mhw_dt_results.RData')
  gc()
  
} else {
  
  setwd(here('data/intermediate'))
  load('mhw_results.RData')
  
}


#----------------------------------------------------
#### 2. Clean data and create time series csv ####

#Transform the data to fit the IEA data format.
#For more info on IEA data format go to the IEAnalyzeR vignette (https://gulf-iea.github.io/IEAnalyzeR/articles/How_to_use_IEAnalyzeR.html).
#Once data are formatted with time (annual or monthly) as column 1 and metric values in the remaining columns, you can use the function convert_cleaned_data to convert your csv into a format that can be read by the data_prep function. Replace "your_data" in the code below with whatever your dataframe is called.

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

### annual-degree days ----------------
yr_mhw_dd <- aggregate(cbind(intensity_cumulative) ~ year(index_start),
                       data = mhw_cube,
                       mean, na.rm=T) |>
  setNames(c('year','intensity_cumulative')) |>
  merge(expand.grid(year=1982:2025),all=T)
yr_mhw_dd$intensity_cumulative[is.na(yr_mhw_dd$intensity_cumulative)] <- 0

plot(yr_mhw_dd$year, yr_mhw_dd$intensity_cumulative, typ = 'l')

#Define header components for the data rows (ignore year). Fill in the blanks here.
indicator_names = c("Percentage of US Gulf EEZ", 'Cummulative Intensity (Degree-Days)')
unit_names = c("Percentage", 'Degree-Days')
extent_names = rep("Marine Heatwaves",2)

formatted_data = IEAnalyzeR::convert_cleaned_data(cbind(yr_mhw$year, yr_mhw$percent, yr_mhw_dd$intensity_cumulative),
                                                  indicator_names, unit_names, extent_names)


#----------------------------------------------------
#### 3. Save Formatted data as csv ####

# This will save your data to the appropriate folder.

write.csv(formatted_data, file = csv_filename, row.names = F)

#----------------------------------------------------
#### 4. Create Data_Prep object ####

#Please use your formatted csv to create a "data_prep" object.
#For more info on the data_prep function see the vignette linked above.

data_obj <- IEAnalyzeR::data_prep(csv_filename)


#----------------------------------------------------
#### 5. Save Formatted data_prep object ####

#This will save your data to the appropriate folder.

saveRDS(data_obj, file = object_filename)


#----------------------------------------------------
#### 6. Preview Plot ####
# Use the IEAnalyzeR plotting function to preview the data. This will not necessarily be the final figure used in reports.
# For more info on the plot_fn_obj function go HERE

IEAnalyzeR::plot_fn_obj(df_obj = data_obj, trend = TRUE, pts = T,
                        sep_ylabs = T, manual_title = 'Marine Heatwaves')

#----------------------------------------------------
#### 7. Save plot ####
# This will save the plot to the correct folder.
# Adjust height & width using (height=, width=, unit="in") if needed.

ggsave(filename = plot_filename, width = 7, height = 6, unit = 'in')




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
mains <- list('Winter - DJF',
              'Spring - MAM',
              'Summer - JJA',
              'Fall - SON')

png(here('figures/plots/mhw-surface-area-seasonal-plot.png'), width = 9, height = 6, units = 'in', res = 300)
par(mfrow = c(2,2), mar = c(3,5,2,1),
    oma = c(0,0,3,0))
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
  plot(yagg$year, yagg$percent, typ = 'l', lwd = 2,
       las = 1, xlab = '', ylab = 'EEZ Proportion', main = mains[[i]],
       panel.first = list(abline(h = .5, lty = 5)),
       ylim = c(0,1))
  if(mod$coefficients[8]<=.05){
    abline(mod, col = 'orange', lwd = 2)
  }
  print(mod)
}

mtext('Proportion of US Gulf EEZ with Marine Heatwaves', side = 3, outer = TRUE, cex = 5/4, font = 2, line = 5/4)
dev.off()


png(here('figures/plots/mhw-surface-dd-seasonal-plot.png'), width = 9, height = 6, units = 'in', res = 300)
par(mfrow = c(2,2), mar = c(3,5,2,1),
    oma = c(0,0,3,0))

for(i in 1:4){
  tmp <- subset(mhw_cube, month %in% seasons[[i]])
  yagg <- aggregate(intensity_cumulative ~ year(index_start),
                    data = tmp,
                    mean, na.rm = T) |>
    setNames(c('year','intensity_cumulative')) |>
    merge(expand.grid(year=1982:2025),all=T)
  yagg$cell[is.na(yagg$intensity_cumulative)] <- 0
  
  mod <- summary(lm(intensity_cumulative ~ year, data = yagg))
  plot(yagg$year, yagg$intensity_cumulative, typ = 'l', lwd = 2,
       las = 1, xlab = '', ylab = 'Degree-Days', main = mains[[i]])
  if(mod$coefficients[8]<=.05){
    abline(mod, col = 'orange', lwd = 2)
  }
  print(mod)
}

mtext('US Gulf EEZ Marine Heatwave Cummulative Intensity', side = 3, outer = TRUE, cex = 5/4, font = 2, line = 5/4)
dev.off()


### spatial plots

### shapefile for plotting
world <- ne_download(scale = 10, type = "countries", 
                     returnclass = 'sv') |>
  crop(ext(min_lon,max_lon,min_lat,max_lat))



### where are the MHWs?
cell_ll <- unique(mhw_cube[, c("cell", "x", 'y')]) |>
  setNames(c('cell','lon','lat'))

# event_no_mean <- aggregate(event_no ~ cell, data = mhw_cube, length) |>
#   merge(cell_ll, all = T) |>
#   merge(lon_lat, all = T)
# mean_event_no <- matrix(event_no_mean$event_no/length(styear:enyear), 29, 69)

event_no_yr <- aggregate(event_no ~ cell + year(index_start), data = mhw_cube, length) |>
  setNames(c('cell','year','event_no'))
event_no_mean <- aggregate(event_no ~ cell, data = event_no_yr, mean, na.rm = T) |>
  merge(cell_ll, all = T) |>
  merge(lon_lat, all = T)
mean_event_no <- matrix(event_no_mean$event_no, 29, 69)

### put into raster
event_rast <- rast(mean_event_no[nrow(mean_event_no):1,])
ext(event_rast) <- c(range(lon_lat$lon),range(lon_lat$lat))
crs(event_rast) <- "EPSG:4326"
plot(event_rast)

### colors and breaks for plotting
e_brks <- seq(2.4,4.2,.05)
e_cols <- (cmocean('thermal')(length(e_brks)-1))


### spatial trend
setDT(event_no_yr)
slopes_dt <- event_no_yr[, 
                         .(slope = coef(lm(event_no ~ year, na.action = na.exclude))[2]), 
                         by = cell]
gridcell_lm <- unique(mhw_cube[, c("cell", "x", 'y')]) |>
  setNames(c('cell','lon','lat')) |>
  merge(slopes_dt) |>
  merge(lon_lat, all = T)
hist(gridcell_lm$slope)

mhw_slope <- matrix(gridcell_lm$slope*10, 29, 69)
### put into raster
slope_rast <- rast(mhw_slope[nrow(mhw_slope):1,])
ext(slope_rast) <- c(range(lon_lat$lon),range(lon_lat$lat))
crs(slope_rast) <- "EPSG:4326"
plot(slope_rast)

### colors and breaks for plotting
s_brks <- seq(.24,1.88,.05)
s_cols <- (cmocean('amp')(length(s_brks)-1))



png(here('figures/plots/mhw-surface-spatial-plot.png'), 
    width = 6, height = 6, units = 'in', res = 300)
par(mfrow=c(2,1))

plot(event_rast,
     col = e_cols, range = c(2.4,4.2),
     plg = list(tick = 'out'),
     main = 'Marine Heatwaves (mean number per year)',
     mar = c(1, 2, 1, 4))
plot(world, add= T, col = 'gray')
contour(ln[ln_i],lt[lt_i],bathy, levels = -100, add=T, lwd = 2)
# plot(gulf_eez, add = T)

plot(slope_rast,
     col = s_cols, range = c(.24,1.88),
     plg = list(tick = 'out'),
     main = 'Marine Heatwave Trend (events per decade)',
     mar = c(1, 2, 1, 4))
plot(world, add= T, col = 'gray')
contour(ln[ln_i],lt[lt_i],bathy, levels = -100, add=T, lwd = 2)
# plot(gulf_eez, add = T)

dev.off()



### detrended

#----------------------------------------------------
#### 2. Clean data and create time series csv ####

#Transform the data to fit the IEA data format.
#For more info on IEA data format go to the IEAnalyzeR vignette (https://gulf-iea.github.io/IEAnalyzeR/articles/How_to_use_IEAnalyzeR.html).
#Once data are formatted with time (annual or monthly) as column 1 and metric values in the remaining columns, you can use the function convert_cleaned_data to convert your csv into a format that can be read by the data_prep function. Replace "your_data" in the code below with whatever your dataframe is called.

setwd(here('data/intermediate'))
load('mhw_dt_results.RData')

### annual ----------------
yr_mhw <- aggregate(cell ~ year(index_start),
                    data = mhw_dt_cube,
                    function(x) length(unique(x))) |>
  setNames(c('year','cell')) |>
  merge(expand.grid(year=1982:2025),all=T)
yr_mhw$cell[is.na(yr_mhw$cell)] <- 0
yr_mhw$percent <- yr_mhw$cell / ngrid
yr_mhw$kmsq <- yr_mhw$cell * cellsize_km

plot(yr_mhw$year, yr_mhw$percent, typ = 'l')

### annual-degree days ----------------
yr_mhw_dd <- aggregate(cbind(intensity_cumulative) ~ year(index_start),
                       data = mhw_dt_cube,
                       mean, na.rm=T) |>
  setNames(c('year','intensity_cumulative')) |>
  merge(expand.grid(year=1982:2025),all=T)
yr_mhw_dd$intensity_cumulative[is.na(yr_mhw_dd$intensity_cumulative)] <- 0

plot(yr_mhw_dd$year, yr_mhw_dd$intensity_cumulative, typ = 'l')

#Define header components for the data rows (ignore year). Fill in the blanks here.
indicator_names = c("Percentage of US Gulf EEZ", 'Cummulative Intensity (Degree-Days)')
unit_names = c("Percentage", 'Degree-Days')
extent_names = rep("Marine Heatwaves",2)

formatted_data = IEAnalyzeR::convert_cleaned_data(cbind(yr_mhw$year, yr_mhw$percent, yr_mhw_dd$intensity_cumulative),
                                                  indicator_names, unit_names, extent_names)


#----------------------------------------------------
#### 3. Save Formatted data as csv ####

# This will save your data to the appropriate folder.

# write.csv(formatted_data, file = csv_filename, row.names = F)

#----------------------------------------------------
#### 4. Create Data_Prep object ####

#Please use your formatted csv to create a "data_prep" object.
#For more info on the data_prep function see the vignette linked above.

data_obj <- IEAnalyzeR::data_prep(formatted_data)


#----------------------------------------------------
#### 5. Save Formatted data_prep object ####

#This will save your data to the appropriate folder.

# saveRDS(data_obj, file = object_filename)


#----------------------------------------------------
#### 6. Preview Plot ####
# Use the IEAnalyzeR plotting function to preview the data. This will not necessarily be the final figure used in reports.
# For more info on the plot_fn_obj function go HERE

IEAnalyzeR::plot_fn_obj(df_obj = data_obj, trend = TRUE, pts = T,
                        sep_ylabs = T, manual_title = 'Marine Heatwaves')

#----------------------------------------------------
#### 7. Save plot ####
# This will save the plot to the correct folder.
# Adjust height & width using (height=, width=, unit="in") if needed.

# ggsave(filename = plot_filename, width = 7, height = 6, unit = 'in')




### seasonal -------------------
mhw_cube <- mhw_dt_cube |> 
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
mains <- list('Winter - DJF',
              'Spring - MAM',
              'Summer - JJA',
              'Fall - SON')

# png(here('figures/plots/mhw-surface-area-seasonal-plot.png'), width = 9, height = 6, units = 'in', res = 300)
par(mfrow = c(2,2), mar = c(3,5,2,1),
    oma = c(0,0,3,0))
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
  plot(yagg$year, yagg$percent, typ = 'l', lwd = 2,
       las = 1, xlab = '', ylab = 'EEZ Proportion', main = mains[[i]],
       panel.first = list(abline(h = .5, lty = 5)),
       ylim = c(0,1))
  if(mod$coefficients[8]<=.05){
    abline(mod, col = 'orange', lwd = 2)
  }
  print(mod)
}

mtext('Proportion of US Gulf EEZ with Marine Heatwaves', side = 3, outer = TRUE, cex = 5/4, font = 2, line = 5/4)
dev.off()


# png(here('figures/plots/mhw-surface-dd-seasonal-plot.png'), width = 9, height = 6, units = 'in', res = 300)
par(mfrow = c(2,2), mar = c(3,5,2,1),
    oma = c(0,0,3,0))

for(i in 1:4){
  tmp <- subset(mhw_cube, month %in% seasons[[i]])
  yagg <- aggregate(intensity_cumulative ~ year(index_start),
                    data = tmp,
                    mean, na.rm = T) |>
    setNames(c('year','intensity_cumulative')) |>
    merge(expand.grid(year=1982:2025),all=T)
  yagg$cell[is.na(yagg$intensity_cumulative)] <- 0
  
  mod <- summary(lm(intensity_cumulative ~ year, data = yagg))
  plot(yagg$year, yagg$intensity_cumulative, typ = 'l', lwd = 2,
       las = 1, xlab = '', ylab = 'Degree-Days', main = mains[[i]])
  if(mod$coefficients[8]<=.05){
    abline(mod, col = 'orange', lwd = 2)
  }
  print(mod)
}

mtext('US Gulf EEZ Marine Heatwave Cummulative Intensity', side = 3, outer = TRUE, cex = 5/4, font = 2, line = 5/4)
# dev.off()


### spatial plots

### shapefile for plotting
world <- ne_download(scale = 10, type = "countries", 
                     returnclass = 'sv') |>
  crop(ext(min_lon,max_lon,min_lat,max_lat))



### where are the MHWs?
cell_ll <- unique(mhw_cube[, c("cell", "x", 'y')]) |>
  setNames(c('cell','lon','lat'))

# event_no_mean <- aggregate(event_no ~ cell, data = mhw_cube, length) |>
#   merge(cell_ll, all = T) |>
#   merge(lon_lat, all = T)
# mean_event_no <- matrix(event_no_mean$event_no/length(styear:enyear), 29, 69)

event_no_yr <- aggregate(event_no ~ cell + year(index_start), data = mhw_cube, length) |>
  setNames(c('cell','year','event_no'))
event_no_mean <- aggregate(event_no ~ cell, data = event_no_yr, mean, na.rm = T) |>
  merge(cell_ll, all = T) |>
  merge(lon_lat, all = T)
mean_event_no <- matrix(event_no_mean$event_no, 29, 69)

### put into raster
event_rast <- rast(mean_event_no[nrow(mean_event_no):1,])
ext(event_rast) <- c(range(lon_lat$lon),range(lon_lat$lat))
crs(event_rast) <- "EPSG:4326"
plot(event_rast)

### colors and breaks for plotting
e_brks <- seq(1.85,3.9,.05)
e_cols <- (cmocean('thermal')(length(e_brks)-1))


### spatial trend
setDT(event_no_yr)
slopes_dt <- event_no_yr[, 
                         .(slope = coef(lm(event_no ~ year, na.action = na.exclude))[2]), 
                         by = cell]
gridcell_lm <- unique(mhw_cube[, c("cell", "x", 'y')]) |>
  setNames(c('cell','lon','lat')) |>
  merge(slopes_dt) |>
  merge(lon_lat, all = T)
hist(gridcell_lm$slope)

mhw_slope <- matrix(gridcell_lm$slope*10, 29, 69)
### put into raster
slope_rast <- rast(mhw_slope[nrow(mhw_slope):1,])
ext(slope_rast) <- c(range(lon_lat$lon),range(lon_lat$lat))
crs(slope_rast) <- "EPSG:4326"
plot(slope_rast)

### colors and breaks for plotting
s_brks <- seq(-1.05,1.05,.05)
s_cols <- (cmocean('balance')(length(s_brks)-1))



# png(here('figures/plots/mhw-surface-spatial-plot.png'), 
    # width = 6, height = 6, units = 'in', res = 300)
par(mfrow=c(2,1))

plot(event_rast,
     col = e_cols, range = c(1.85,3.9),
     plg = list(tick = 'out'),
     main = 'Marine Heatwaves (mean number per year)',
     mar = c(1, 2, 1, 4))
plot(world, add= T, col = 'gray')
contour(ln[ln_i],lt[lt_i],bathy, levels = -100, add=T, lwd = 2)
# plot(gulf_eez, add = T)

plot(slope_rast,
     col = s_cols, range = c(-1.05,1.05),
     plg = list(tick = 'out'),
     main = 'Marine Heatwave Trend (events per decade)',
     mar = c(1, 2, 1, 4))
plot(world, add= T, col = 'gray')
contour(ln[ln_i],lt[lt_i],bathy, levels = -100, add=T, lwd = 2)
# plot(gulf_eez, add = T)

dev.off()

