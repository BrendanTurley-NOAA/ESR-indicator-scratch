
library(terra)
library(lubridate)
library(purrr)

setwd("C:/Users/brendan.turley/Documents/data/PEM_artifical_structures/ARP FY26")

struc <- read.csv('ARP_update_Nov25.csv')


struc_shp <- vect('ARPDPP_112525_final.shp')


struc$Year
struc$year_built <- NA
struc$year_built[which(nchar(struc$Year)==4)] <- as.numeric(struc$Year[which(nchar(struc$Year)==4)])
struc$year_built[which(nchar(struc$Year)>4)] <- struc$Year[which(nchar(struc$Year)>4)] |> mdy() |> year()

table(struc$year_built)


struc_rast <- rast(ext(struc_shp), resolution = 0.1, crs=crs(struc_shp))


### test sensitivity to oil platforms

ong <- c('OilCaisson','OilPlatformMaterial','OilRig')
# ong <- c('OilCaisson','OilRig')
struc_no <- struc[which(!is.element(struc$GEOFORM, ong)), ] |>
  vect(geom = c('Longitude','Latitude'), crs = 'EPSG:4326') |>
  rasterize(struc_rast, fun = 'count')

plot(struc_no, col = rev(map.pal('plasma',100)))
### end sensitivity


# str_ll <- data.frame(x = struc$Longitude,
#                      y = struc$Latitude,
#                      year_built = struc$year_built)
str_ll <- vect(struc, geom = c('Longitude','Latitude'), crs = 'EPSG:4326')

struc_rast <- rast(ext(struc_shp), resolution = 0.05, crs=crs(struc_shp))

test <- rasterize(str_ll, struc_rast, 
                  fun = 'count')
plot(test, col = rev(map.pal('plasma',100)))

test2 <- rasterize(str_ll, struc_rast, 
                  field = 'year_built', fun = 'median', na.rm = T)
test2 <- rasterize(str_ll, struc_rast, 
                   field = 'year_built', function(x) round(mean(x,na.rm=T),-1))
plot(test2, col = map.pal('plasma',8))


polys <- as.polygons(test, values = T, dissolve = F)

plot(polys, add=T, border = 'gray')


### BOEM structures
### download data from BOEM.gov
temp_file <- tempfile()
temp_dir <- tempdir()
download.file("https://www.data.boem.gov/Platform/Files/PlatStrucRawData.zip", temp_file, mode = "wb")
unzip(temp_file, exdir = temp_dir)

extracted_files <- list.files(temp_dir, full.names = TRUE)
plat <- read.csv(file.path(temp_dir,'PlatStrucRawData', "mv_platstruc_structures.txt"))

platforms <- plat[which(!is.na(plat$LONGITUDE)), ]

table(platforms$STRUC_TYPE_CODE)
# "CAIS"  "CT"    "FIXED" "FPSO"  "MOPU"  "MTLP"  "SEMI"  "SPAR"  "TLP"   "WP"
plt_typ <- c("CAIS","CT","FIXED")
platforms <- subset(platforms, STRUC_TYPE_CODE %in% plt_typ)
# platforms <- st_as_sf(platforms, 
#                       coords = c('LONGITUDE','LATITUDE'),
#                       crs = st_crs(gulf_shp))
# st_crs(platforms) <- st_crs(gulf_shp)
# platforms <- st_filter(platforms, gulf_shp)

# plot(platforms$LONGITUDE, platforms$LATITUDE)
# plot(platforms$geometry)

platforms$INSTALL_DATE <- mdy(platforms$INSTALL_DATE)
platforms$REMOVAL_DATE <- mdy(platforms$REMOVAL_DATE)

# yrs <- sort(unique(year(platforms$INSTALL_DATE)))
# full_yrs <- data.frame(year = seq(min(yrs),max(yrs)))
yrs <- seq(min(year(platforms$INSTALL_DATE)),2025)

plt_yr <- list()
n <- 1
for(i in yrs){
  plt_i <- subset(platforms, year(INSTALL_DATE)==i | 
                    year(INSTALL_DATE)<i) |>
    subset(year(REMOVAL_DATE)>i |
             is.na(REMOVAL_DATE)) |> 
    nrow()
  plt_yr[[n]] <- data.frame(year = i, 
                            nplt = plt_i)
  n <- n + 1
}
plt_yr <- list_rbind(plt_yr)
# platforms_year <- merge(full_yrs, plt_yr, by = 'year', all = T)

plot(plt_yr$year, plt_yr$nplt, typ = 'l')
# plot(platforms_year$year, platforms_year$nplt, typ = 'l')

plt_2025 <- subset(platforms, year(INSTALL_DATE)==2025 | 
                  year(INSTALL_DATE)<2025) |>
  subset(year(REMOVAL_DATE)>2025 |
           is.na(REMOVAL_DATE))
plot(plt_2025$LONGITUDE, plt_2025$LATITUDE, asp = 1)

### removals
plt_rm <- subset(platforms, year(REMOVAL_DATE)>=2015)
# plot(plt_rm$LONGITUDE, plt_rm$LATITUDE, asp = 1)

plt_rmv <- vect(plt_rm, geom = c('LONGITUDE','LATITUDE'), crs = 'EPSG:4326')
plt_rmv$yr_rm <- year(plt_rmv$REMOVAL_DATE)
plt_rmr <- rasterize(plt_rmv, 
                     rast(ext(struc_shp), resolution = 0.15, crs=crs(struc_shp)), 
                     field = 'yr_rm', function(x) round(median(x,na.rm=T),0))
unique(plt_rmr)
plot(plt_rmr, col = map.pal('plasma',11),
     type='classes')

### combine 2025 data with PEM data for plotting

ong <- c('OilCaisson','OilPlatformMaterial','OilRig')
# ong <- c('OilCaisson','OilRig')
struc_no <- struc[which(!is.element(struc$GEOFORM, ong)), ]

struc_no <- subset(struc_no, select = c('Longitude', 'Latitude', 'year_built'))

plat_2025 <- subset(plt_2025, select = c('LONGITUDE', 'LATITUDE', 'INSTALL_DATE')) |>
  setNames(c('Longitude', 'Latitude', 'year_built'))
plat_2025$year_built <- year(plat_2025$year_built)

struc_comb <- rbind(struc_no, plat_2025) |>
  vect(geom = c('Longitude','Latitude'), crs = 'EPSG:4326')

numb <- rasterize(struc_comb, struc_rast, fun = 'count')
yr_blt <- rasterize(struc_comb, struc_rast, 
                   field = 'year_built', function(x) round(median(x,na.rm=T),-1))

plot(numb, col = rev(map.pal('plasma',100)))
plot(yr_blt, col = map.pal('plasma',8))

str_yr <- aggregate(Longitude ~ year_built, data = struc_no, function(x) length(unique(x))) |>
  setNames(c('year','num_struc')) |>
  merge(data.frame(year = min(plt_yr$year):2025), all=T)
str_yr$num_struc[which(is.na(str_yr$num_struc))] <- 0
str_yr$cummul <- cumsum(str_yr$num_struc)

all_struc <- merge(str_yr, plt_yr, all = T)
all_struc$tot <- all_struc$cummul + all_struc$nplt

plot(all_struc$year, all_struc$tot)
points(all_struc$year, all_struc$cummul,col = 2)
points(all_struc$year, all_struc$nplt, col = 3)
### plot the mean year of removals only




### pipelines
### download data from BOEM.gov
temp_file <- tempfile()
temp_dir <- tempdir()
download.file("https://www.data.boem.gov/Mapping/Files/ppl_arcs.zip", temp_file, mode = "wb")
unzip(temp_file, exdir = temp_dir)

extracted_files <- list.files(temp_dir, full.names = TRUE)
pipe <- vect(file.path(temp_dir,'ppl_arcs.shp'))

pipe <- subset(pipe, pipe$STATUS_COD != 'REM')
pipe <- subset(pipe, pipe$STATUS_COD != 'CNCL')

sum(pipe$SEG_LENGTH)/5280
plot(pipe)
