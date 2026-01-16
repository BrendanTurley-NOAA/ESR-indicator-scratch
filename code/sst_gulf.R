# References to methods
# https://noaa-edab.github.io/tech-doc/seasonal_oisst_anom_gridded.html
# https://noaa-edab.github.io/catalog/seasonal_oisst_anom.html
# long term mean reference period for anomaly 1982-2010
# winter = jfm; spring = amj; summer = jas; fall = ond

### simple SST for Gulf ESR draft

library(dplyr)
library(lubridate)
library(maps)
library(rerddap)
library(sf)
library(terra)

### clean up R envrionment
rm(list = ls())
gc(); gc()
plot.new()
dev.off()
cache_delete_all(force = FALSE)
cache_list()


# define years  --------------------------------
styear <- 1982
enyear <- 2025

# define spatial domain  --------------------------------
min_lon <- -98
max_lon <- -80
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
                      longitude = c(min_lon, max_lon), 
                      latitude = c(min_lat, max_lat), 
                      fmt = 'csv')
  
  ### whole Gulf
  sst_gulf <- aggregate(sst_grab$sst,
                        by = list(sst_grab$time),
                        function(x) c(mean(x, na.rm = T), sd(x, na.rm = T),
                                      min(x, na.rm = T), max(x, na.rm = T)))
  
  
  ### US EEZ
  sst_sf <- st_as_sf(sst_grab, coords = c("longitude", "latitude"), crs = 4326) |>
    st_intersection(eez)
  
  sst_eez <- aggregate(sst_sf$sst, 
                       by = list(sst_sf$time), 
                       function(x) c(mean(x, na.rm = T), sd(x, na.rm = T),
                                     min(x, na.rm = T), max(x, na.rm = T)))
  
  if (yr == styear) { 
    dat_gulf <- sst_gulf
    dat_eez <- sst_eez
  } 
  else {
    dat_gulf <- rbind(dat_gulf, sst_gulf)
    dat_eez <- rbind(dat_eez, sst_eez)
  }
}
# # dat_gulf25 <- dat_gulf
# dat_eez25 <- dat_eez

### temp file save
setwd("~/R_projects/ESR-indicator-scratch/data/intermediate_files")
# save(dat_gulf, file = 'dat_gulf_temp.RData')
# save(dat_eez, file = 'dat_eez_temp2.RData')
# save(dat_eez, dat_gulf, file = 'sst_temp.RData')
### check
# rm(dat_eez, dat_gulf)
# load('sst_temp.RData')
# rm(dat_eez)
# load('dat_gulf_temp.RData')
# load('dat_eez_temp.RData')
# dat_eez1 <- dat_eez
# load('dat_eez_temp2.RData')

# dat_gulf_com <- rbind(dat_gulf, dat_eez25)
# dat_eez_com <- rbind(dat_eez1, dat_eez, dat_eez25)

# dat_gulf <- data.frame(cbind(dat_gulf_com$Group.1, dat_gulf_com$x))
# names(dat_gulf) <- c("time", "sst_degC", "sd", 'min_degC', 'max_degC')
# dat_gulf <- type.convert(dat_gulf)

# dat_eez <- data.frame(cbind(dat_eez_com$Group.1, dat_eez_com$x))
# names(dat_eez) <- c("time", "sst_degC", "sd", 'min_degC', 'max_degC')
# dat_eez <- type.convert(dat_eez)

# rm(dat_eez_com, dat_eez)

setwd("~/R_projects/ESR-indicator-scratch/data/intermediate_files")
# save(dat_gulf,dat_eez, file = 'sst_comb_temp.RData')
load('sst_comb_temp.RData')

### convert to dates
dat_gulf$time <- as.Date(dat_gulf$time)
dat_eez$time <- as.Date(dat_eez$time)

# add yearmonth column --------------------------
dat_gulf$yrmon <- paste(dat_gulf$time |> year(),
                        sprintf("%02.f", dat_gulf$time |> month()),
                        sep = '-')
dat_eez$yrmon <- paste(dat_eez$time |> year(),
                       sprintf("%02.f", dat_eez$time |> month()),
                       sep = '-')

# table(dat_gulf$time |> year(),
#       dat_gulf$time |> month())
# table(dat_eez$time |> year(),
#       dat_eez$time |> month())

# subset(dat_gulf, year(time)==2023 & month(time)==2)
### data from 2023-02-01 is missing

# sst_grab <- griddap(sst, fields = c('anom','sst'), 
#                     time = c(paste0(2023,'-01-31'), paste0(2023,'-02-02')),
#                     # time = c(paste0(yr,'-01-01'), paste0(yr,'-01-02')), ### test
#                     longitude = c(min_lon, max_lon), 
#                     latitude = c(min_lat, max_lat), 
#                     fmt = 'csv')



### find ltm and create anom
dat_gulf$jday <- yday(dat_gulf$time)
dat_eez$jday <- yday(dat_eez$time)

dat_gulf <- subset(dat_gulf, year(time)<2011) |>
  group_by(jday) |>
  summarize(jday_m = mean(sst_degC, na.rm = T)) |>
  full_join(dat_gulf) |>
  mutate(anom_degC = sst_degC - jday_m,
         season = case_when(
           month(time)==12 | month(time)<3 ~ 'win',
           month(time)>2 & month(time)<6 ~ 'spr',
           month(time)>5 & month(time)<9 ~ 'sum',
           month(time)>8 & month(time)<12 ~ 'aut'
         ))

dat_eez <- subset(dat_eez, year(time)<2011) |>
  group_by(jday) |>
  summarize(jday_m = mean(sst_degC, na.rm = T)) |>
  full_join(dat_eez) |>
  mutate(anom_degC = sst_degC - jday_m,
         season = case_when(
           month(time)==12 | month(time)<3 ~ 'win',
           month(time)>2 & month(time)<6 ~ 'spr',
           month(time)>5 & month(time)<9 ~ 'sum',
           month(time)>8 & month(time)<12 ~ 'aut'
         ))



# simple monthly plots --------------------------

gulf_yrmon <- aggregate(anom_degC ~ yrmon, data = dat_gulf,
                        mean, na.rm = T)
eez_yrmon <- aggregate(anom_degC ~ yrmon, data = dat_eez,
                       mean, na.rm = T)

plot(seq(styear, enyear+.999, 1/12), eez_yrmon$anom_degC, 
     typ = 'l', lwd = 2, panel.first = list(grid(), abline(h = 0, lty = 5)))
points(seq(styear, enyear+.999, 1/12), gulf_yrmon$anom_degC, typ = 'l', lwd = 2, col = 2)

plot(seq(styear, enyear+.999, 1/12), eez_yrmon$anom_degC, typ = 'h', lwd = 2, col = 1)
points(seq(styear, enyear+.999, 1/12), gulf_yrmon$anom_degC, typ = 'h', lwd = 2, col = 2)

plot(seq(styear, enyear+.999, 1/12), eez_yrmon$anom_degC - gulf_yrmon$anom_degC, typ = 'h', lwd = 2)
plot(eez_yrmon$anom_degC, gulf_yrmon$anom_degC, asp = 1,
     panel.first = abline(0,1,lty=5))
hist(eez_yrmon$anom_degC - gulf_yrmon$anom_degC)
     
# annual plots: mean, min, max --------------------------

gulf_ann <- aggregate(anom_degC ~ year(time), data = dat_gulf,
                      mean, na.rm = T)
eez_ann <- aggregate(anom_degC ~ year(time), data = dat_eez,
                     mean, na.rm = T)

plot(styear:enyear, eez_ann$anom_degC, typ = 'o', lwd = 2,
     panel.first = list(grid(), abline(h = 0, lty = 5)))
points(styear:enyear, gulf_ann$anom_degC, typ = 'o', lwd = 2, col = 2)



### seasonal means
dat_gulf$season_yr <- ifelse(month(dat_gulf$time)==12, year(dat_gulf$time)+1, year(dat_gulf$time))
dat_eez$season_yr <- ifelse(month(dat_eez$time)==12, year(dat_eez$time)+1, year(dat_eez$time))


### anomaly
gulf_win <- aggregate(anom_degC ~ year(time), data = subset(dat_gulf, season=='win'),
                     mean, na.rm = T)
gulf_spr <- aggregate(anom_degC ~ year(time), data = subset(dat_gulf, season=='spr'),
                     mean, na.rm = T)
gulf_sum <- aggregate(anom_degC ~ year(time), data = subset(dat_gulf, season=='sum'),
                     mean, na.rm = T)
gulf_aut <- aggregate(anom_degC ~ year(time), data = subset(dat_gulf, season=='aut'),
                     mean, na.rm = T)

par(mfrow = c(2,2))
plot(gulf_win$`year(time)`, gulf_win$anom_degC, 
     typ = 'o', pch = 16,
     panel.first = list(grid(),abline(h = 0, lty = 5)))
plot(gulf_spr$`year(time)`, gulf_spr$anom_degC, 
     typ = 'o', pch = 16,
     panel.first = list(grid(),abline(h = 0, lty = 5)))
plot(gulf_sum$`year(time)`, gulf_sum$anom_degC, 
     typ = 'o', pch = 16,
     panel.first = list(grid(),abline(h = 0, lty = 5)))
plot(gulf_aut$`year(time)`, gulf_aut$anom_degC,
     typ = 'o', pch = 16,
     panel.first = list(grid(),abline(h = 0, lty = 5)))

eez_win <- aggregate(anom_degC ~ year(time), data = subset(dat_eez, season=='win'),
                     mean, na.rm = T)
eez_spr <- aggregate(anom_degC ~ year(time), data = subset(dat_eez, season=='spr'),
                     mean, na.rm = T)
eez_sum <- aggregate(anom_degC ~ year(time), data = subset(dat_eez, season=='sum'),
                     mean, na.rm = T)
eez_aut <- aggregate(anom_degC ~ year(time), data = subset(dat_eez, season=='aut'),
                     mean, na.rm = T)

par(mfrow = c(2,2))
plot(eez_win$`year(time)`, eez_win$anom_degC, 
     typ = 'o', pch = 16,
     panel.first = list(grid(),abline(h = 0, lty = 5)))
plot(eez_spr$`year(time)`, eez_spr$anom_degC, 
     typ = 'o', pch = 16,
     panel.first = list(grid(),abline(h = 0, lty = 5)))
plot(eez_sum$`year(time)`, eez_sum$anom_degC, 
     typ = 'o', pch = 16,
     panel.first = list(grid(),abline(h = 0, lty = 5)))
plot(eez_aut$`year(time)`, eez_aut$anom_degC,
     typ = 'o', pch = 16,
     panel.first = list(grid(),abline(h = 0, lty = 5)))


### degC
gulf_win <- aggregate(sst_degC ~ year(time), data = subset(dat_gulf, season=='win'),
                      mean, na.rm = T)
gulf_spr <- aggregate(sst_degC ~ year(time), data = subset(dat_gulf, season=='spr'),
                      mean, na.rm = T)
gulf_sum <- aggregate(sst_degC ~ year(time), data = subset(dat_gulf, season=='sum'),
                      mean, na.rm = T)
gulf_aut <- aggregate(sst_degC ~ year(time), data = subset(dat_gulf, season=='aut'),
                      mean, na.rm = T)

par(mfrow = c(2,2))
plot(gulf_win$`year(time)`, gulf_win$sst_degC, 
     typ = 'o', pch = 16,
     panel.first = list(grid(),abline(h = 0, lty = 5)))
plot(gulf_spr$`year(time)`, gulf_spr$sst_degC, 
     typ = 'o', pch = 16,
     panel.first = list(grid(),abline(h = 0, lty = 5)))
plot(gulf_sum$`year(time)`, gulf_sum$sst_degC, 
     typ = 'o', pch = 16,
     panel.first = list(grid(),abline(h = 0, lty = 5)))
plot(gulf_aut$`year(time)`, gulf_aut$sst_degC,
     typ = 'o', pch = 16,
     panel.first = list(grid(),abline(h = 0, lty = 5)))

eez_win <- aggregate(sst_degC ~ year(time), data = subset(dat_eez, season=='win'),
                     mean, na.rm = T)
eez_spr <- aggregate(sst_degC ~ year(time), data = subset(dat_eez, season=='spr'),
                     mean, na.rm = T)
eez_sum <- aggregate(sst_degC ~ year(time), data = subset(dat_eez, season=='sum'),
                     mean, na.rm = T)
eez_aut <- aggregate(sst_degC ~ year(time), data = subset(dat_eez, season=='aut'),
                     mean, na.rm = T)

par(mfrow = c(2,2))
plot(eez_win$`year(time)`, eez_win$sst_degC, 
     typ = 'o', pch = 16,
     panel.first = list(grid(),abline(h = 0, lty = 5)))
plot(eez_spr$`year(time)`, eez_spr$sst_degC, 
     typ = 'o', pch = 16,
     panel.first = list(grid(),abline(h = 0, lty = 5)))
plot(eez_sum$`year(time)`, eez_sum$sst_degC, 
     typ = 'o', pch = 16,
     panel.first = list(grid(),abline(h = 0, lty = 5)))
plot(eez_aut$`year(time)`, eez_aut$sst_degC,
     typ = 'o', pch = 16,
     panel.first = list(grid(),abline(h = 0, lty = 5)))


### to do
# spatial plots
# last 5 years
# seasonal plot mean past 5 years
# seasonal plot trend past 5 years

# define years  --------------------------------
styear <- 1982
enyear <- 2024
### 2025 is not completely online yet

# define spatial domain  --------------------------------
min_lon <- -98
max_lon <- -80
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
                      longitude = c(min_lon, max_lon), 
                      latitude = c(min_lat, max_lat), 
                      fmt = 'csv')
  
  ### whole Gulf
  sst_gulf <- aggregate(sst_grab$sst,
                        by = list(sst_grab$time),
                        function(x) c(mean(x, na.rm = T), sd(x, na.rm = T),
                                      min(x, na.rm = T), max(x, na.rm = T)))
  
  
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
