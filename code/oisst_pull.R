
### oisst
# https://www.ncei.noaa.gov/products/climate-data-records/sea-surface-temperature-optimum-interpolation
# https://upwell.pfeg.noaa.gov/erddap/info/ncdcOisst21Agg_LonPM180/index.html

library(fields)
library(lubridate)
library(ncdf4)
library(sf)

# define spatial domain  --------------------------------
min_lon <- -80
max_lon <- -98
min_lat <- 18
max_lat <- 31

url <- 'https://www.ncei.noaa.gov/thredds/dodsC/OisstBase/NetCDF/V2.1/AVHRR/198109/oisst-avhrr-v02r01.19810901.nc'

dat <- nc_open(url)
# anom/sst[lon,lat,zlev,time]  

lon <- ncvar_get(dat, 'lon')
lat <- ncvar_get(dat, 'lat')

i_lon <- which(lon >= (360+max_lon) & lon <= (360+min_lon))
i_lat <- which(lat <= (max_lat) & lat >= (min_lat))
lons <- lon[i_lon]
lats <- lat[i_lat]

# define time domain  --------------------------------
years <- 1982:2024


system.time(
  setwd("C:/Users/brendan.turley/Documents/R_projects/ESR-indicator-scratch/data/intermediate_files")
  for(h in 1:length(years)){
    cat('\n', years[h], '\n')
    
    dates <- seq(ymd(paste(years[h],'-01-01')),
                 ymd(paste(years[h],'-12-31')))
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

### problem with 2007
years <- 2007:2024
system.time(
  setwd("C:/Users/brendan.turley/Documents/R_projects/ESR-indicator-scratch/data/intermediate_files"),
  for(h in 1:length(years)){
    cat('\n', years[h], '\n')
    
    dates <- seq(ymd(paste(years[h],'-01-01')),
                 ymd(paste(years[h],'-12-31')))
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
      sst_grab <- ncvar_get(dat, 'anom', 
                            start = c(i_lon[1], i_lat[1], 1, 1), 
                            count = c(length(i_lon), length(i_lat), -1, -1))
      sst_a[,,i] <- sst_grab
      time_a[i] <- time
      
      nc_close(dat)
      setTxtProgressBar(pb, i)
    }
    
    out <- list(anom = sst_a, time = time_a)
    # assign(paste0('sst_',years[h]), out)
    saveRDS(out, paste0('anom_',years[h]))
    
  }
)

setwd("C:/Users/brendan.turley/Documents/R_projects/ESR-indicator-scratch/data/intermediate_files")
# saveRDS(paste0('sst_',years[i]), paste0('sst_',years[i]))

files <- ls()
for(i in 1:19){
  iy <- grep(paste0('sst_',years[i]), files)
  saveRDS(get(files[iy]), paste0('sst_',years[i]))
}


url <- 'https://www.ncei.noaa.gov/thredds/dodsC/OisstBase/NetCDF/V2.1/AVHRR/198109/oisst-avhrr-v02r01.19810901.nc'

dat <- nc_open(url)
# anom/sst[lon,lat,zlev,time]  

time <- ncvar_get(dat, 'time')
lon <- ncvar_get(dat, 'lon')
lat <- ncvar_get(dat, 'lat')

time <- as.Date(time, origin = '1978-01-01')
i_lon <- which(lon >= (360+max_lon) & lon <= (360+min_lon))
i_lat <- which(lat <= (max_lat) & lat >= (min_lat))
lons <- lon[i_lon]
lats <- lat[i_lat]

sst_grab <- ncvar_get(dat, 'sst', 
                      start = c(i_lon[1], i_lat[1], 1, 1), 
                      count = c(length(i_lon), length(i_lat), -1, -1))
imagePlot(lons,lats,sst_grab,asp = 1)


### is it easier to store as long dataframe (lon,lat,sst) and subset using sf
lon_lat <- expand.grid(lons,lats)

sst_vec <- data.frame(lon = lon_lat$Var1,
                      lat = lon_lat$Var2,
                      date = time,
                      sst = as.vector(sst_grab))

sst_sf <- st_as_sf(sst_vec, 
                   coords = c("lon", "lat"), 
                   crs = 4326, # Assign a Coordinate Reference System (CRS) - 4326 is WGS84
                   remove = TRUE)

eez_sf <- st_as_sf(eez_4326)
est_sf <- st_as_sf(est)
nearshore_sf <- st_as_sf(nearshore)
offshore_sf <- st_as_sf(offshore)

sst_eez <- st_filter(sst_sf, eez_sf)
sst_est <- st_filter(sst_sf, est_sf)
sst_near <- st_filter(sst_sf, nearshore_sf)
sst_off <- st_filter(sst_sf, offshore_sf)

length(which(!is.na(sst_eez$sst)))/nrow(sst_eez)
length(which(!is.na(sst_est$sst)))/nrow(sst_est)
length(which(!is.na(sst_near$sst)))/nrow(sst_near)
length(which(!is.na(sst_off$sst)))/nrow(sst_off)

mean(sst_eez$sst, na.rm = T)
mean(sst_est$sst, na.rm = T)
mean(sst_near$sst, na.rm = T)
mean(sst_off$sst, na.rm = T)


er1_sf <- project(er1, 'EPSG:4326') |> st_as_sf()
er2_sf <- project(er2, 'EPSG:4326') |> st_as_sf()
er3_sf <- project(er3, 'EPSG:4326') |> st_as_sf()
er4_sf <- project(er4, 'EPSG:4326') |> st_as_sf()
er5_sf <- project(er5, 'EPSG:4326') |> st_as_sf()

sst_er1 <- st_filter(sst_sf, er1_sf)
sst_er2 <- st_filter(sst_sf, er2_sf)
sst_er3 <- st_filter(sst_sf, er3_sf)
sst_er4 <- st_filter(sst_sf, er4_sf)
sst_er5 <- st_filter(sst_sf, er5_sf)

length(which(!is.na(sst_er1$sst)))/nrow(sst_er1)
length(which(!is.na(sst_er2$sst)))/nrow(sst_er2)
length(which(!is.na(sst_er3$sst)))/nrow(sst_er3)
length(which(!is.na(sst_er4$sst)))/nrow(sst_er4)
length(which(!is.na(sst_er5$sst)))/nrow(sst_er5)

mean(sst_er1$sst, na.rm = T)
mean(sst_er2$sst, na.rm = T)
mean(sst_er3$sst, na.rm = T)
mean(sst_er4$sst, na.rm = T)
mean(sst_er5$sst, na.rm = T)


### or store as a data cube and subset by turning into a raster?




rm(list = ls())

plot.new()
dev.off()

library(lubridate)
library(rerddap)

# load("indicator_processing/spec_file.RData")

# define spatial domain  --------------------------------
min_lon <- -81
max_lon <- -98
min_lat <- 18
max_lat <- 31

# define years  --------------------------------
styear <- 1982
enyear <- 2024

# get ERDDAP info  --------------------------------
sst <- info('ncdcOisst21Agg_LonPM180') # this may work better

# empty data  -------------------------------------------------
dat <- c()

n <- 1
nlon <- 73
nlat <- 53
# download by year to avoid timeout errors --------------------
for (yr in styear:enyear) { 
  
  ### BDT rERDDAP fix
  sst_grab <- griddap(sst, fields = 'sst', 
                      time = c(paste0(yr,'-01-01'), paste0(yr,'-12-31')), 
                      longitude = c(min_lon, max_lon), 
                      latitude = c(min_lat, max_lat))
  
  sst_agg <- aggregate(sst_grab$data$sst, 
                       by = list(sst_grab$data$time), 
                       function(x) c(mean(x, na.rm = T), sd(x, na.rm = T)))
  
  matrix(sst_grab$data$sst[1:length(which(sst_grab$data$time==unique(sst_grab$data$time)[1]))],
         nlon,
         nlat) |> image()
  
  sst_cube[,,n] <- array()
  
  if (yr == styear) { dat <- sst_agg  }  else {
    dat <- rbind(dat, sst_agg)  }
}

dat <- data.frame(cbind(dat$Group.1, dat$x))
names(dat) <- c("time", "sst", "sd")

dat$sst <- as.numeric(dat$sst)
dat$sd <- as.numeric(dat$sd)

head(dat)
