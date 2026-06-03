
library(lubridate)
library(ncdf4)

# define spatial domain  --------------------------------
min_lon <- -98
max_lon <- -80
min_lat <- 18
max_lat <- 31

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

crs <- 'EPSG:32663'