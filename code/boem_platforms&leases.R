
library(lubridate)

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

plot(platforms$LONGITUDE, platforms$LATITUDE)

platforms$INSTALL_DATE <- mdy(platforms$INSTALL_DATE)
platforms$REMOVAL_DATE <- mdy(platforms$REMOVAL_DATE)

yrs <- sort(unique(year(platforms$INSTALL_DATE)))
full_yrs <- data.frame(year = seq(min(yrs),max(yrs)))

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
platforms_year <- merge(full_yrs, plt_yr, by = 'year', all = T)

plot(platforms_year$year, platforms_year$nplt, typ = 'l')

setwd("~/R_projects/ESR-indicator-scratch/data")
write.csv(platforms_year, 'platforms_yr.csv', row.names = F)

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

yrs <- sort(unique(year(leases$LEASE_EFF_DATE)))
full_yrs <- data.frame(year = seq(min(yrs),max(yrs)))

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
leases_year <- merge(full_yrs, lease_yr, by = 'year', all = T)

plot(leases_year$year, leases_year$nlease, typ = 'l')

setwd("~/R_projects/ESR-indicator-scratch/data")
write.csv(leases_year, 'leases_yr.csv', row.names = F)

