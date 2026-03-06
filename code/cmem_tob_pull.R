### Copernicus Marine Toolbox downloads
# https://help.marine.copernicus.eu/en/articles/8638253-how-to-download-data-via-the-copernicus-marine-toolbox-in-r
# https://help.marine.copernicus.eu/en/articles/8228284-how-to-automate-a-series-of-download-via-the-copernicus-marine-toolbox-in-r

library(ncdf4)
library(fields)
library(lubridate)


path_copernicusmarine <- "C:/Users/brendan.turley/Documents/data/copernicusmarine/copernicusmarine.exe"


# GLORYS Analysis/Forecast --------------------------------------------------------------
output_directory = "C:/Users/brendan.turley/Documents/data/copernicusmarine/tob/"

dataset_id <- 'cmems_mod_glo_phy_anfc_0.083deg_P1M-m'
variable <- 'tob'
# start_dt <- '2022-06-01T00:00:00'
# end_dt <- '2026-01-01T00:00:00'
min_lon <- -98
max_lon <- -80
min_lat <- 18
max_lat <- 31
years <- 2022:2025

for(i in years){

  end_dt <- paste0(i, '-12-31','T00:00:00')
  if(i==2022){
    start_dt <- paste0(i, '-06-01','T00:00:00')
    date_i <- paste0(i,'6-12')
  } else {
    start_dt <- paste0(i, '-01-01','T00:00:00')
    date_i <- i
  }
  
  output_filename <- paste('gomx', date_i, variable, dataset_id, sep='_') |>
    paste0(".nc")
  
  command <- paste(
    shQuote(path_copernicusmarine),
    "subset",
    "--dataset-id", dataset_id,
    "--variable", variable,
    "--start-datetime", start_dt,
    "--end-datetime", end_dt,
    "--minimum-longitude", min_lon,
    "--maximum-longitude", max_lon,
    "--minimum-latitude", min_lat,
    "--maximum-latitude", max_lat,
    "--minimum-depth 0.49402499198913574",
    "--maximum-depth 0.49402499198913574",
    "-o", output_directory,
    '-f', output_filename,
    sep = " "
  )
  
  print(paste("======== Download starting on",start_dt,"========"))
  system(command, intern = TRUE)
  
}



# GLORYS Reanalysis --------------------------------------------------------------
output_directory = "C:/Users/brendan.turley/Documents/data/copernicusmarine/bottomT/"

dataset_id <- 'cmems_mod_glo_phy_my_0.083deg_P1M-m'
variable <- 'bottomT'
# start_dt <- '1993-01-01T00:00:00'
# end_dt <- '2025-11-01T00:00:00'
min_lon <- -98
max_lon <- -80
min_lat <- 18
max_lat <- 31
years <- 1993:2025

for(i in years){
  
  start_dt <- paste0(i, '-01-01','T00:00:00')
  if(i==2025){
    end_dt <- paste0(i, '-11-30','T00:00:00')
    date_i <- paste0(i,'-1-11')
  } else {
    end_dt <- paste0(i, '-12-31','T00:00:00')
    date_i <- i
  }
  
  output_filename <- paste('gomx', date_i, variable, dataset_id, sep='_') |>
    paste0(".nc")
  
  command <- paste(
    shQuote(path_copernicusmarine),
    "subset",
    "--dataset-id", dataset_id,
    "--variable", variable,
    "--start-datetime", start_dt,
    "--end-datetime", end_dt,
    "--minimum-longitude", min_lon,
    "--maximum-longitude", max_lon,
    "--minimum-latitude", min_lat,
    "--maximum-latitude", max_lat,
    "--minimum-depth 0.49402499198913574",
    "--maximum-depth 0.49402499198913574",
    "-o", output_directory,
    '-f', output_filename,
    sep = " "
  )
  
  print(paste("======== Download starting on",start_dt,"========"))
  system(command, intern = TRUE)
  
}





# Check output ------------------------------------------------------------

setwd(output_directory)

flist <- list.files()
dat <- nc_open(flist[4])
tob <- ncvar_get(dat, 'tob')
lon <- ncvar_get(dat, 'longitude')
lat <- ncvar_get(dat, 'latitude')
time <- ncvar_get(dat, 'time')
time <- as.Date(time/24, origin = '1950-01-01')
nc_close(dat)

par(mfrow=c(3,4))
for(i in 1:12){
  imagePlot(lon, lat, tob[,,i])  
  mtext(i)
}


