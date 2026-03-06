
# File created on 2026-02-19, adapted from file created 2026-02-06 by Carissa Gervasi

#### 0. Setup ####
# Add any packages that are needed for analysis here.
library(abind)
library(dplyr)
library(lubridate)
library(IEAnalyzeR)
library(here)
library(ggplot2)
library(rerddap)
library(sf)
library(terra)
library(rnaturalearth)
library(rnaturalearthdata)
library(scales)
library(ncdf4)
library(cmocean)

# File Naming Setup.
# !! Auto generated-Do Not Change !!
root_name <- "Chlorophyll"

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

# ERDDAP file: pmlEsaCCI60OceanColorMonthly
# https://coastwatch.pfeg.noaa.gov/erddap/info/pmlEsaCCI60OceanColorMonthly/index.html

#----------------------------------------------------
#### 1. Read Data ####

# define years  --------------------------------
styear <- 1998 # 1997 is partial year
enyear <- 2025

# define spatial domain  --------------------------------
min_lon <- -98
max_lon <- -80
min_lat <- 18
max_lat <- 31

# load shapefile to subset  --------------------------------
### shapefiles downloaded from marineregions.org (future goal implement mregions2 R package for shapefile)
setwd(here("data/intermediate/gulf_eez"))
setwd("C:/Users/brendan.turley/Documents/data/shapefiles/gulf_eez")
eez <- vect('eez.shp') |> makeValid()

setwd(here("data/intermediate/gulf_iho"))
setwd("C:/Users/brendan.turley/Documents/data/shapefiles/gulf_iho")
iho <- vect('iho.shp') |> makeValid()

gulf_eez <- terra::intersect(eez, iho) |>
  st_as_sf() |> 
  st_transform(crs = st_crs(4326))
gulf_iho <- iho |> 
  st_as_sf() |> 
  st_transform(crs = st_crs(4326))

rm(eez, iho); gc()

# plot_regions <- F ### set to F to skip plots
# 
# if(plot_regions==T){
#   
#   ocean <- ne_download(type = 'ocean', 
#                        category = 'physical',
#                        scale = 10, 
#                        returnclass = 'sv')
#   
#   # png(here('figures/plots/chl-spatial.png'), width = 4, height = 6, units = 'in', res = 300)
#   par(mfrow=c(2,1))
#   # par(mfrow=c(1,2))
#   plot(ocean, 
#        ylim = c(min_lat, max_lat), 
#        xlim = c(min_lon, max_lon), 
#        col = 'gray', 
#        main = 'Gulf-wide')
#   plot(st_geometry(gulf_iho), 
#        add = T, 
#        col = alpha(2,.5))
#   plot(ocean, 
#        ylim = c(min_lat, max_lat), 
#        xlim = c(min_lon, max_lon), 
#        col = 'gray',
#        main = 'US Gulf EEZ')
#   plot(st_geometry(gulf_eez), 
#        add = T, 
#        col = alpha(4,.5))
#   # dev.off()
# }

# download by year to avoid timeout errors --------------------

######################################################
#### don't run while reviewing code; takes awhile ####
#### load saved intermediate files below loop ########
######################################################

review_code <- T ### set to F to rerun download loop

# if(review_code == F){

# get ERDDAP info  --------------------------------
# pmlEsaCCI60OceanColorMonthly returns error code 404
# pmlEsaCCI50OceanColorMonthly
chl <- info('pmlEsaCCI50OceanColorMonthly') # this may work better

# empty data  -------------------------------------------------
# dat_gulf <- c()
dat_eez <- c()

setwd(here("data/intermediate"))
system.time(
  for (yr in styear:enyear) { 
    
    cat('\n', yr, '\n')
    
    chl_grab <- griddap(chl, fields = 'chlor_a', 
                        time = c(paste0(yr,'-01-01'), paste0(yr,'-12-31')),
                        longitude = c(min_lon, max_lon), 
                        latitude = c(min_lat, max_lat), 
                        fmt = 'csv')
    
    ### US EEZ
    chl_eez_sf <- st_as_sf(chl_grab, coords = c("longitude", "latitude"), crs = 4326) |>
      st_intersection(gulf_eez)
    
    chl_eez <- chl_eez_sf |>
      st_drop_geometry() |>
      group_by(time) |>
      summarize(chl_mgm3 = mean(chl, na.rm = T))
    
    if (yr == styear) { 
      dat_eez <- chl_eez
    } 
    else {
      dat_eez <- rbind(dat_eez, chl_eez)
    }
  }
)

setwd(here("data/intermediate"))
# save(dat_eez, dat_gulf, file = 'chl_comb_temp2.RData')

# } else {

# setwd(here("data/intermediate"))
# load('chl_comb_temp2.RData')
### load dat_gulf & dat_eez downloaded chl data

# }


### alternative download from Plymouth Marine Lab (from the horse's mouth)
# https://www.oceancolour.org/thredds/dodsC/CCI_ALL-v6.0-1km-MONTHLY
# https://www.oceancolour.org/thredds/dodsC/CCI_ALL-v6.0-MONTHLY
# 'https://www.oceancolour.org/thredds/dodsC/cci/v6.0-release/geographic/monthly/chlor_a/1997/ESACCI-OC-L3S-CHLOR_A-MERGED-1M_MONTHLY_4km_GEO_PML_OCx-199709-fv6.0.nc'
dat <- nc_open('https://www.oceancolour.org/thredds/dodsC/cci/v6.0-release/geographic/monthly/chlor_a/1997/ESACCI-OC-L3S-CHLOR_A-MERGED-1M_MONTHLY_4km_GEO_PML_OCx-199709-fv6.0.nc',
               readunlim = F, return_on_error = T, suppress_dimvals = T)
lon <- ncvar_get(dat, 'lon')
lat <- ncvar_get(dat, 'lat')
chl_fill <- ncatt_get(dat, 'chlor_a', "_FillValue")
nc_close(dat)
rm(dat)

ilon <- which(lon>min_lon & lon<max_lon)
ilat <- which(lat>min_lat & lat<max_lat)

yr_mon <- rbind(data.frame(year = rep(1997,4),
                           month = sprintf('%02d', 9:12)),
                expand.grid(year = 1998:2025, 
                            month = sprintf('%02d', 1:12))) |>
  arrange(year, month)


### chunk downloads for sanity
dat_eez <- array(NA, c(length(ilon), length(ilat), dim(yr_mon)[1]))
time_dat <- rep(NA, dim(yr_mon)[1])

pb <- txtProgressBar(min = 0, max = length(time_dat), style = 3)
for(i in 1:dim(yr_mon)[1]){
  
  nc_url <- paste0('https://www.oceancolour.org/thredds/dodsC/cci/v6.0-release/geographic/monthly/chlor_a/',
                   yr_mon$year[i],
                   '/ESACCI-OC-L3S-CHLOR_A-MERGED-1M_MONTHLY_4km_GEO_PML_OCx-',
                   yr_mon$year[i], yr_mon$month[i],
                   '-fv6.0.nc')
  
  dat <- nc_open(nc_url, readunlim = F, return_on_error = T, suppress_dimvals = T)
  
  while(dat$error==T){
    Sys.sleep(5) 
    dat <- nc_open(nc_url, readunlim = F, return_on_error = T, suppress_dimvals = T)
  }
  
  chl_pull <- try(ncvar_get(dat, 'chlor_a', start = c(ilon[1], ilat[1], 1),
                            count = c(length(ilon), length(ilat), 1)))
  time_pull <- try(ncvar_get(dat, 'time'))
  
  while(any(class(chl_pull) == 'try-error')){
    Sys.sleep(5) 
    chl_pull <- try(ncvar_get(dat, 'chlor_a', start = c(ilon[1], ilat[1], 1),
                              count = c(length(ilon), length(ilat), 1)))
    time_pull <- try(ncvar_get(dat, 'time'))
  }
  
  dat_eez[,,i] <- chl_pull
  time_dat[i] <- time_pull
  
  setwd(here("data/intermediate"))
  save(dat_eez, time_dat, file = 'chl_comb_temp.RData')
  
  nc_close(dat)
  rm(chl_pull, time_pull, dat)
  setTxtProgressBar(pb, i)
}

dat_eez[dat_eez==chl_fill$value] <- NA
dat_eez2 <- dat_eez

time_dat
as.Date(time_dat, origin = '1970-01-01')

data.frame(yr_mon, date = as.Date(time_dat, origin = '1970-01-01'))

apply(dat_eez, 3, function(x)all(is.na(x)))

image(log10(apply(dat_eez, c(1,2), mean, na.rm = T)))

hist(log10(apply(dat_eez, c(1,2), mean, na.rm = T)))

#----------------------------------------------------
#### 2. Clean data and create time series csv ####

#Transform the data to fit the IEA data format.
#For more info on IEA data format go to the IEAnalyzeR vignette (https://gulf-iea.github.io/IEAnalyzeR/articles/How_to_use_IEAnalyzeR.html).
#Once data are formatted with time (annual or monthly) as column 1 and metric values in the remaining columns, you can use the function convert_cleaned_data to convert your csv into a format that can be read by the data_prep function. Replace "your_data" in the code below with whatever your dataframe is called.

#Define header components for the data rows (ignore year). Fill in the blanks here.
indicator_names = c("")
unit_names = c("")
extent_names = c("")

formatted_data = IEAnalyzeR::convert_cleaned_data(your_data, indicator_names, unit_names, extent_names)


#----------------------------------------------------
#### 3. Save Formatted data as csv ####

# This will save your data to the appropriate folder.

write.csv(formatted_data, file = csv_filename, row.names = F)

#----------------------------------------------------
#### 4. Create Data_Prep object ####

#Please use your formatted csv to create a "data_prep" object.
#For more info on the data_prep function see the vignette linked above.

data_obj<-IEAnalyzeR::data_prep(csv_filename)


#----------------------------------------------------
#### 5. Save Formatted data_prep object ####

#This will save your data to the appropriate folder.

saveRDS(data_obj, file = object_filename)


#----------------------------------------------------
#### 6. Preview Plot ####
# Use the IEAnalyzeR plotting function to preview the data. This will not necessarily be the final figure used in reports.
# For more info on the plot_fn_obj function go HERE

IEAnalyzeR::plot_fn_obj(df_obj = data_obj, trends = TRUE)

#----------------------------------------------------
#### 7. Save plot ####
# This will save the plot to the correct folder.
# Adjust height & width using (height=, width=, unit="in") if needed.

ggsave(filename = plot_filename)
