
### simple SST for Gulf ESR draft

library(lubridate)
library(maps)
library(rerddap)
library(sf)
library(terra)

### clean up R envrionmenta
rm(list = ls())
gc(); gc()
plot.new()
dev.off()
cache_delete_all(force = FALSE)
cache_list()


# define years  --------------------------------
styear <- 1982
enyear <- 2024
### 2025 is not completely online yet

### error stopped after completing 2015
styear <- 2016
enyear <- 2024

# define spatial domain  --------------------------------
min_lon <- -80
max_lon <- -98
min_lat <- 18
max_lat <- 31

# load shapefile to subset  --------------------------------
setwd("C:/Users/brendan.turley/Documents/data/shapefiles/Habitat_Zone")
eez <- vect('gulf_eez.shp') |> makeValid() |> st_as_sf() |> st_transform(crs = st_crs(4326))

# get ERDDAP info  --------------------------------
sst <- info('ncdcOisst21Agg_LonPM180') # this may work better

# empty data  -------------------------------------------------
dat_gulf <- c()
dat_eez <- c()

# download by year to avoid timeout errors --------------------
for (yr in styear:enyear) { 
  
  sst_grab <- griddap(sst, fields = 'sst', 
                      time = c(paste0(yr,'-01-01'), paste0(yr,'-12-31')),
                      # time = c(paste0(yr,'-01-01'), paste0(yr,'-01-02')), ### test
                      longitude = c(min_lon, max_lon), 
                      latitude = c(min_lat, max_lat), 
                      fmt = 'csv')
  
  # ### whole Gulf
  # sst_gulf <- aggregate(sst_grab$sst, 
  #                       by = list(sst_grab$time), 
  #                       function(x) c(mean(x, na.rm = T), sd(x, na.rm = T),
  #                                     min(x, na.rm = T), max(x, na.rm = T)))
  
  
  ### US EEZ
  sst_sf <- st_as_sf(sst_grab, coords = c("longitude", "latitude"), crs = 4326) |>
    st_intersection(eez)
  
  sst_eez <- aggregate(sst_sf$sst, 
                       by = list(sst_sf$time), 
                       function(x) c(mean(x, na.rm = T), sd(x, na.rm = T),
                                     min(x, na.rm = T), max(x, na.rm = T)))
  
  if (yr == styear) { 
    # dat_gulf <- sst_gulf
    dat_eez <- sst_eez
    } 
  else {
    # dat_gulf <- rbind(dat_gulf, sst_gulf)
    dat_eez <- rbind(dat_eez, sst_eez)
    }
}

### temp file save
setwd("~/R_projects/ESR-indicator-scratch/data/intermediate_files")
# save(dat_gulf, file = 'dat_gulf_temp.RData')
save(dat_eez, file = 'dat_eez_temp2.RData')
# save(dat_eez, dat_gulf, file = 'sst_temp.RData')
### check
# rm(dat_eez, dat_gulf)
# load('sst_temp.RData')
# rm(dat_eez)
load('dat_gulf_temp.RData')
load('dat_eez_temp.RData')
dat_eez1 <- dat_eez
load('dat_eez_temp2.RData')

dat_eez_com <- rbind(dat_eez1, dat_eez)

dat_gulf <- data.frame(cbind(dat_gulf$Group.1, dat_gulf$x))
names(dat_gulf) <- c("time", "sst_degC", "sd", 'min_degC', 'max_degC')
dat_gulf <- type.convert(dat_gulf)

dat_eez <- data.frame(cbind(dat_eez_com$Group.1, dat_eez_com$x))
names(dat_eez) <- c("time", "sst_degC", "sd", 'min_degC', 'max_degC')
dat_eez <- type.convert(dat_eez)

rm(dat_eez_com, dat_eez)

save(dat_gulf,dat_eez, file = 'sst_comb_temp.RData')



# add yearmonth column --------------------------
dat$year <- year(dat$time)
dat$mon <- month(dat$time)
dat$yrmon <- paste0(dat$year, sprintf("%02.f", dat$mon))
dat
head(dat)
tail(dat)
barplot(table(dat$year))
barplot(table(dat$mon))
table(dat$year)
table(dat$mon)

# calculate min, mean, max and look at correlations ---------------------

me <- tapply(dat$sst, dat$yrmon, mean, na.rm = T)
mi <- tapply(dat$sst, dat$yrmon, min, na.rm = T)
ma <- tapply(dat$sst, dat$yrmon, max, na.rm = T)

me_sd <- tapply(dat$sd, dat$yrmon, mean, na.rm = T)

inddata <- data.frame(cbind(me, mi, ma))
names(inddata) <- c("mean", "min", "max")
datdata <- names(me)
ulidata <- data.frame(me + me_sd)
llidata <- data.frame(me - me_sd)
