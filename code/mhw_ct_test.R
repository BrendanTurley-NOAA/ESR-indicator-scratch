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
styear <- 1985
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
  
  ### pull coraltemp sst
  
  url <- 'https://www.ncei.noaa.gov/thredds-ocean/dodsC/crw/5km/v3.1/nc/v1.0/daily/sst/1985/coraltemp_v3.1_19850101.nc'
  
  dat <- nc_open(url)
  
  lon <- ncvar_get(dat, 'lon')
  lat <- ncvar_get(dat, 'lat')
  
  i_lon <- which(lon >= (min_lon) & lon <= (max_lon))
  i_lat <- which(lat <= (max_lat) & lat >= (min_lat))
  lons <- lon[i_lon]
  lats <- lat[i_lat]
  
  # define time domain  --------------------------------
  years <- 1985:2025
  
  
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
      
      sst <- array(NA, dim = c(length(i_lon), length(i_lat), nyr))
      time <- rep(NA,nyr) |> as.Date()
      
      pb <- txtProgressBar(min = 0, max = length(dates), style = 3)
      for(i in 1:length(dates)){
        url <- paste0('https://www.ncei.noaa.gov/thredds-ocean/dodsC/crw/5km/v3.1/nc/v1.0/daily/sst/',
                      substr(dates[i],1,4),
                      '/coraltemp_v3.1_',
                      dates[i],
                      '.nc')
        dat <- nc_open(url)
        time_grab <- ncvar_get(dat, 'time') / 86400
        time_grab <- as.Date(time_grab, origin = '1981-01-01')
        sst_grab <- ncvar_get(dat, 'analysed_sst', 
                              start = c(i_lon[1], i_lat[1], 1), 
                              count = c(length(i_lon), length(i_lat), -1))
        sst[,,i] <- sst_grab
        time[i] <- time_grab
        
        nc_close(dat)
        rm(dat, time_grab, sst_grab)
        gc()
        setTxtProgressBar(pb, i)
      }
      
      out <- list(sst = sst, time = time)
      # assign(paste0('sst_',years[h]), out)
      setwd("C:/Users/brendan.turley/Documents/R_projects/ESR-indicator-scratch/data/intermediate_files")
      saveRDS(out, paste0('ct_sst_',years[h]))
      rm(out, sst, time)
      gc()
    }
  )
  # crs <- 'EPSG:32663'
  
  ### load sst data and combine
  setwd("~/R_projects/ESR-indicator-scratch/data/intermediate_files")
  
  raster_list <- list()
  for(i in styear:enyear){
    cat(i, '\n')
    tmp <- paste0('ct_sst_',i) |> readRDS()

    sst_r <- rast(tmp$sst, crs="EPSG:32663")
    ext(sst_r) <- c(range(lons), range(lats))
    sst_r <- project(sst_r, "EPSG:4326")
    
    time(sst_r) <- as.Date(tmp$time)
    
    # Store in list
    raster_list[[which(i==(styear:enyear))]] <- sst_r
  }
  sst_r <- rast(raster_list)

### save intermediate file
setwd("~/R_projects/ESR-indicator-scratch/data/intermediate_files")
# writeRaster(sst_r, filename = "my_raster.tif", overwrite = TRUE)
writeCDF(sst_r, 'ct_sst_gom.nc', overwrite = TRUE, compression = 4)

ann_gwide <- crop(sst_r, gulf_eez) |> mask(gulf_eez)
cellsize_km <- cellSize(ann_gwide,unit='km') |> values() |> mean()

### save intermediate file
setwd("~/R_projects/ESR-indicator-scratch/data/intermediate_files")
writeCDF(ann_gwide, 'ct_sst_gulf.nc',overwrite=TRUE)
dat <- nc_open('ct_sst_gulf.nc')
data <- ncvar_get(dat, 'ct_sst_gulf')
lon <- ncvar_get(dat, 'longitude')
lat <- ncvar_get(dat, 'latitude')
lon_lat <- expand.grid(lon = lon,lat = lat)

dat_m <- apply(data,c(1,2),mean,na.rm=T)
ngrid <- length(which(!is.na(dat_m)))

### this is the MHW detection function
setwd("~/R_projects/ESR-indicator-scratch/data/intermediate_files")
mhw_cube <- detect3(file_in = 'ct_sst_gulf.nc',
                    return_type = "df", 
                    clim_period = c("1985-01-01", "2015-12-31"))
### save intermediate file
save(mhw_cube, ngrid, cellsize_km, lon_lat,
     file = 'mhw_results_ct_sst.RData')
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
