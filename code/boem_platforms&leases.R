

library(de)
library(purrr)
library(lubridate)
library(sf)
library(terra)

# setwd("C:/Users/brendan.turley/Documents/data/shapefiles/GOM_2500ft")
# gulf_shp <- vect('GOM_2500ft.shp')
# gulf_shp <- gulf_shp[gulf_shp$Jurisdict=='Federal',] |>
#   aggregate() |> st_as_sf()
### filtering by EEZ does not change the results

### boem data
# 1. number of structures
# 2. number of leases active

# https://www.data.boem.gov/Main/RawData.aspx

### what about artifical reefs:
# https://www.bsee.gov/what-we-do/environmental-compliance/environmental-programs/rigs-to-reefs


# structures --------------------------------------------------------------

temp_file <- tempfile()
temp_dir <- tempdir()
download.file("https://www.data.boem.gov/Platform/Files/PlatStrucRawData.zip", temp_file, mode = "wb")
unzip(temp_file, exdir = temp_dir)

extracted_files <- list.files(temp_dir, full.names = TRUE)
platforms <- read.csv(file.path(temp_dir,'PlatStrucRawData', "mv_platstruc_structures.txt"))
platforms <- platforms[which(!is.na(platforms$LONGITUDE)), ]

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

setwd("~/R_projects/ESR-indicator-scratch/data/processed")
write.csv(plt_yr, 'platforms_yr.csv', row.names = F)

# unlink(temp_file)
# unlink(temp_dir, recursive = TRUE) # Use recursive = TRUE to remove directory and its contents



# leases ------------------------------------------------------------------

temp_file <- tempfile()
temp_dir <- tempdir()
download.file("https://www.data.boem.gov/Leasing/Files/LABRawData.zip", temp_file, mode = "wb")
unzip(temp_file, exdir = temp_dir)

extracted_files <- list.files(temp_dir, full.names = TRUE)
leases <- read.csv(file.path(temp_dir,'LABRawData', "mv_lease_area_block.txt"))

leases$LEASE_EFF_DATE <- mdy(leases$LEASE_EFF_DATE)
leases$LEASE_EXPIR_DATE <- mdy(leases$LEASE_EXPIR_DATE)

# yrs <- sort(unique(year(leases$LEASE_EFF_DATE)))
# full_yrs <- data.frame(year = seq(min(yrs),max(yrs)))
yrs <- seq(min(year(leases$LEASE_EFF_DATE)),2025)

lease_yr <- list()
n <- 1
for(i in yrs){
  lease_i <- subset(leases, year(LEASE_EFF_DATE)==i | 
                    year(LEASE_EFF_DATE)<i) |>
    subset(year(LEASE_EXPIR_DATE)>i |
             is.na(LEASE_EXPIR_DATE)) |> 
    nrow()
  lease_yr[[n]] <- data.frame(year = i, 
                            nlease = lease_i)
  n <- n + 1
}
lease_yr <- list_rbind(lease_yr)
# leases_year <- merge(full_yrs, lease_yr, by = 'year', all = T)

plot(lease_yr$year, lease_yr$nlease, typ = 'l')
# plot(leases_year$year, leases_year$nlease, typ = 'l')

setwd("~/R_projects/ESR-indicator-scratch/data/processed")
write.csv(lease_yr, 'leases_yr.csv', row.names = F)

