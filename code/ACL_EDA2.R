##### Catch Limits ####
# Code to filter to all Catch Limit regulations for a species of interest

##### Load packages ####
librarian::shelf(here, tidyverse, gt, flextable, officer, dplyr, gt, officer, lubridate, htmltools, rmarkdown, tidyr)

#### Load data ####
# Load the MH Data Log
mh <- readRDS(here("ODM-MH-Data_log", "data", "results", "MH_DL_2025Sep10.RDS"))

#### Define species, region, and sector of interest ####
species = c('SNAPPER, CUBERA', 'AMBERJACK, GREATER', 'AMBERJACK, LESSER', 'JACK, ALMACO', 'SNAPPER, SILK', 'SNAPPER, QUEEN', 'SNAPPER, BLACKFIN', 'WENCHMAN',
            'DRUM, RED', 'COBIA', 'GROUPER, GAG', 'SNAPPER, GRAY', 'MACKEREL, KING', 'GROUPER, YELLOWEDGE', 'SNAPPER, LANE', 'GROUPER, RED', 'SNAPPER, RED', 
            'SCAMP', 'MACKEREL, SPANISH','HOGFISH', 'SNAPPER, MUTTON', 'SNAPPER, YELLOWTAIL', 'SNAPPER, VERMILION', 'TRIGGERFISH, GRAY', 'TRIGGERFISH, QUEEN')
region = 'GULF OF MEXICO'
sector = 'RECREATIONAL'
# sector = 'COMMERCIAL'


for(i in species){
  tmp <- mh |>
    filter(COMMON_NAME_USE %in% i,
           REGION %in% region,
           SECTOR == 'RECREATIONAL')#,
           # DETAILED == "YES",
           # NEVER_IMPLEMENTED %in% c(0, NA),
           # REG_REMOVED == 0,
           # MANAGEMENT_TYPE_USE == "ACL")
  print(i)
  print(nrow(tmp))
}


### this works

acls <- mh %>%
  filter(COMMON_NAME_USE %in% species,
         REGION %in% region,
         DETAILED == "YES",
         NEVER_IMPLEMENTED %in% c(0, NA),
         REG_REMOVED == 0,
         MANAGEMENT_TYPE_USE == "ACL",
         # MANAGEMENT_TYPE == "ACL",
         SPP_TYPE == 'COMMON_NAME', ### removing this gets yellowedge groupers and scamp
         VALUE_UNITS == 'POUNDS') %>%
  arrange(CLUSTER, START_DATE2) |> type.convert() |>
  filter(SUBSECTOR=='ALL' & SECTOR!='ALL')

yllw_scmp <- mh %>%
  filter(COMMON_NAME_USE %in% c('GROUPER, YELLOWEDGE', 'SCAMP'),
         REGION %in% region,
         DETAILED == "YES",
         NEVER_IMPLEMENTED %in% c(0, NA),
         REG_REMOVED == 0,
         MANAGEMENT_TYPE_USE == "ACL",
         # MANAGEMENT_TYPE == "ACL",
         VALUE_UNITS == 'POUNDS') |>
  arrange(CLUSTER, START_DATE2) |> type.convert() #|>
  # filter(SUBSECTOR=='ALL' & SECTOR!='ALL')

acls <- rbind(yllw_scmp, acls)

acl_slice <- acls |>
  arrange(COMMON_NAME_USE, START_YEAR, SECTOR, EFFECTIVE_DATE) |>
  group_by(COMMON_NAME_USE, START_YEAR, SECTOR) |>
  slice_max(order_by = EFFECTIVE_DATE, n = 1, with_ties = F) |>
  select(COMMON_NAME_USE, START_YEAR, SECTOR, VALUE)

spp <- unique(acl_slice$COMMON_NAME_USE)

acl_out <- list()
for(i in spp){ 
  ## this loop imputes missing acls using a previous year value; first year is skipped and propagate
  tmp <- subset(acl_slice, COMMON_NAME_USE==i)
  
  yr_sec <- expand.grid(START_YEAR = min(tmp$START_YEAR):max(tmp$START_YEAR),
                        SECTOR = c('COMMERCIAL','RECREATIONAL')) |>
    merge(tmp, by = c('START_YEAR','SECTOR'), all = T) |>
    arrange(START_YEAR, SECTOR)
  
  if(any(is.na(yr_sec$COMMON_NAME_USE))){
    yr_sec$COMMON_NAME_USE[which(is.na(yr_sec$COMMON_NAME_USE))] <- i
  }
  
  for(j in which(is.na(yr_sec$VALUE))){
    itmp <- yr_sec[j, ]
    if(itmp$START_YEAR!=min(yr_sec$START_YEAR)){
      yr_sec$VALUE[j] <- subset(yr_sec, START_YEAR==(itmp$START_YEAR-1) &
                                  SECTOR==itmp$SECTOR,
                                select = VALUE) |> unlist()
    }
  }
  acl_out[[which(i==spp)]] <- yr_sec
}
acl_sums <- bind_rows(acl_out)

par(mfrow = c(3,3), mar = c(4,4,1.5,1))
for(i in spp){
  tmp <- subset(acl_sums, COMMON_NAME_USE==i)
  with(subset(tmp, SECTOR=='COMMERCIAL'),
       plot(START_YEAR, VALUE,
            ylim = range(tmp$VALUE, na.rm = T),
            typ = 'o', col = 1, lwd = 2,
            xlab = '',
            ylab = 'ACL (pounds)', 
            panel.first = grid()))
  mtext(i, cex = .7)
  points(VALUE ~ START_YEAR, 
         data = subset(tmp, SECTOR=='RECREATIONAL'),
         typ = 'o', col = 2, lwd = 2)
  legend('topright',c('COM','REC'), pch = 1, col = c(1,2), bty = 'n', pt.lwd = 2)
}



### ACTs
acts <- mh %>%
  filter(COMMON_NAME_USE %in% species,
         REGION %in% region,
         DETAILED == "YES",
         NEVER_IMPLEMENTED %in% c(0, NA),
         REG_REMOVED == 0,
         MANAGEMENT_TYPE_USE == "ACT",
         # MANAGEMENT_TYPE == "ACL",
         # SPP_TYPE == 'COMMON_NAME', ### removing this gets yellowedge groupers and scamp
         VALUE_UNITS == 'POUNDS') %>%
  arrange(CLUSTER, START_DATE2) |> type.convert() |>
  filter(SUBSECTOR=='ALL' & SECTOR!='ALL')

table(acts$SPP_NAME)
table(acts$COMMON_NAME_USE)


### Quotas
quotas <- mh %>%
  filter(COMMON_NAME_USE %in% species,
         REGION %in% region,
         SECTOR == 'RECREATIONAL',
         DETAILED == "YES",
         NEVER_IMPLEMENTED %in% c(0, NA),
         REG_REMOVED == 0,
         MANAGEMENT_TYPE_USE == "QUOTA",
         # MANAGEMENT_TYPE == "ACL",
         # SPP_TYPE == 'COMMON_NAME', ### removing this gets yellowedge groupers and scamp
         VALUE_UNITS == 'POUNDS') %>%
  arrange(CLUSTER, START_DATE2) |> type.convert() |>
  filter(SUBSECTOR=='ALL' & SECTOR!='ALL')

table(quotas$SPP_NAME)
table(quotas$COMMON_NAME_USE)


### TACs
tacs <- mh %>%
  filter(COMMON_NAME_USE %in% species,
         REGION %in% region,
         SECTOR == 'RECREATIONAL',
         DETAILED == "YES",
         NEVER_IMPLEMENTED %in% c(0, NA),
         REG_REMOVED == 0,
         MANAGEMENT_TYPE_USE == "TAC",
         # MANAGEMENT_TYPE == "ACL",
         # SPP_TYPE == 'COMMON_NAME', ### removing this gets yellowedge groupers and scamp
         VALUE_UNITS == 'POUNDS') %>%
  arrange(CLUSTER, START_DATE2) |> type.convert() |>
  filter(SUBSECTOR=='ALL' & SECTOR!='ALL')

table(tacs$SPP_NAME)
table(tacs$COMMON_NAME_USE)



#### ACLs ####
# Filter to species, region, and sector of interest
# Remove non-detailed regulations, those that were never implemented, and those that removed a regulation
# Adjust variable titles and format
rec_acls <- mh %>%
  filter(COMMON_NAME_USE %in% species,
         REGION %in% region,
         SECTOR == 'RECREATIONAL',
         DETAILED == "YES",
         NEVER_IMPLEMENTED %in% c(0, NA),
         REG_REMOVED == 0,
         MANAGEMENT_TYPE_USE == "ACL") %>%
  arrange(CLUSTER, START_DATE2) %>%
  mutate(Species = case_when(str_detect(COMMON_NAME_USE, ",") ~ {parts <- str_split_fixed(COMMON_NAME_USE, ",", 2)
  str_to_title(str_trim(parts[,2])) %>% paste(str_to_title(str_trim(parts[,1])))},
  TRUE ~ str_to_title(COMMON_NAME_USE)),
  Fishery = str_to_title(paste0(SECTOR_USE, " ", SUBSECTOR_USE)),
  `Region Affected` = str_to_title(paste0(REGION, " ", ZONE_USE)),
  `First Year in Effect` = year(START_DATE2),
  `Fishing Year Effective Date` = format(START_DATE2, "%m/%d/%Y"),
  `FR Notice Effetive Date` = format(EFFECTIVE_DATE, "%m/%d/%Y"),
  `FR Notice Ineffective Date` = format(INEFFECTIVE_DATE, "%m/%d/%Y"),
  ACL = case_when(MANAGEMENT_TYPE_USE == "ACL" ~ VALUE,
                  TRUE ~ NA_character_),
  ACL_units = case_when(MANAGEMENT_TYPE_USE == "ACL" ~ paste0(tolower(VALUE_UNITS),ifelse(!is.na(VALUE_TYPE), paste0(" (", tolower(VALUE_TYPE), ")"), "")),
                  TRUE ~ NA_character_),
  `FR Reference` = case_when(MANAGEMENT_TYPE_USE == "ACL" ~ FR_CITATION,
                             TRUE ~ NA_character_),
  `FR URL` = case_when(MANAGEMENT_TYPE_USE == "ACL" ~ FR_URL,
                       TRUE ~ NA_character_),
  `Amendment Number or Rule Type` = case_when(!is.na(AMENDMENT_NUMBER) & !is.na(ACTION_TYPE) ~ str_to_title(paste0(ACTION, " ", ACTION_TYPE, " ", AMENDMENT_NUMBER)),
                                              !is.na(AMENDMENT_NUMBER) &  is.na(ACTION_TYPE) ~ str_to_title(paste0(ACTION, " ", AMENDMENT_NUMBER)),
                                              is.na(AMENDMENT_NUMBER) & !is.na(ACTION_TYPE) ~ str_to_title(paste0(ACTION, " ", ACTION_TYPE)),
                                              is.na(AMENDMENT_NUMBER) &  is.na(ACTION_TYPE) ~ str_to_title(ACTION)),
  `Cluster` = CLUSTER) %>%
  select(CLUSTER, Species, Fishery, `Region Affected`,
         `First Year in Effect`, `Fishing Year Effective Date`, `FR Notice Effetive Date`, `FR Notice Ineffective Date`, `ACL`,
         ACL_units, `FR Reference`, `FR URL`, `Amendment Number or Rule Type`) %>%
  arrange(Species, CLUSTER, `First Year in Effect`)

com_acls <- mh %>%
  filter(COMMON_NAME_USE %in% species,
         REGION %in% region,
         SECTOR == 'COMMERCIAL',
         DETAILED == "YES",
         NEVER_IMPLEMENTED %in% c(0, NA),
         REG_REMOVED == 0,
         MANAGEMENT_TYPE_USE == "ACL") %>%
  arrange(CLUSTER, START_DATE2) %>%
  mutate(Species = case_when(str_detect(COMMON_NAME_USE, ",") ~ {parts <- str_split_fixed(COMMON_NAME_USE, ",", 2)
  str_to_title(str_trim(parts[,2])) %>% paste(str_to_title(str_trim(parts[,1])))},
  TRUE ~ str_to_title(COMMON_NAME_USE)),
  Fishery = str_to_title(paste0(SECTOR_USE, " ", SUBSECTOR_USE)),
  `Region Affected` = str_to_title(paste0(REGION, " ", ZONE_USE)),
  `First Year in Effect` = year(START_DATE2),
  `Fishing Year Effective Date` = format(START_DATE2, "%m/%d/%Y"),
  `FR Notice Effetive Date` = format(EFFECTIVE_DATE, "%m/%d/%Y"),
  `FR Notice Ineffective Date` = format(INEFFECTIVE_DATE, "%m/%d/%Y"),
  ACL = case_when(MANAGEMENT_TYPE_USE == "ACL" ~ VALUE,
                  TRUE ~ NA_character_),
  ACL_units = case_when(MANAGEMENT_TYPE_USE == "ACL" ~ paste0(tolower(VALUE_UNITS),ifelse(!is.na(VALUE_TYPE), paste0(" (", tolower(VALUE_TYPE), ")"), "")),
                        TRUE ~ NA_character_),
  `FR Reference` = case_when(MANAGEMENT_TYPE_USE == "ACL" ~ FR_CITATION,
                             TRUE ~ NA_character_),
  `FR URL` = case_when(MANAGEMENT_TYPE_USE == "ACL" ~ FR_URL,
                       TRUE ~ NA_character_),
  `Amendment Number or Rule Type` = case_when(!is.na(AMENDMENT_NUMBER) & !is.na(ACTION_TYPE) ~ str_to_title(paste0(ACTION, " ", ACTION_TYPE, " ", AMENDMENT_NUMBER)),
                                              !is.na(AMENDMENT_NUMBER) &  is.na(ACTION_TYPE) ~ str_to_title(paste0(ACTION, " ", AMENDMENT_NUMBER)),
                                              is.na(AMENDMENT_NUMBER) & !is.na(ACTION_TYPE) ~ str_to_title(paste0(ACTION, " ", ACTION_TYPE)),
                                              is.na(AMENDMENT_NUMBER) &  is.na(ACTION_TYPE) ~ str_to_title(ACTION)),
  `Cluster` = CLUSTER) %>%
  select(CLUSTER, Species, Fishery, `Region Affected`,
         `First Year in Effect`, `Fishing Year Effective Date`, `FR Notice Effetive Date`, `FR Notice Ineffective Date`, `ACL`,
         ACL_units, `FR Reference`, `FR URL`, `Amendment Number or Rule Type`) %>%
  arrange(Species, CLUSTER, `First Year in Effect`)

### modified to get the number and units separate.
### a few entries are percentages; which going through the FR they seem to be the commerical/recreational allocation percentages

# rec_acls <- rec_acls[grep('pounds',rec_acls$ACL_units), ]
# r_spp <- sort(unique(rec_acls$Species))
# fishery <- 'Recreational All'
# # View(subset(acl, Species=='King Mackerel' & Fishery==fishery))
# 
# for(i in spp){
#   with(subset(rec_acls, Species==i & Fishery==fishery),
#        plot(`First Year in Effect`,ACL))
#   mtext(i)
# }
# 
# com_acls <- com_acls[grep('pounds',com_acls$ACL_units), ]
# c_spp <- sort(unique(com_acls$Species))
# 
# spp <- union(c_spp, r_spp)
# 
# fishery <- 'Commercial All'
# View(subset(com_acls, Species=='Gag Grouper' & Fishery==fishery))
# 
# for(i in spp){
#   tcom <- subset(com_acls, Species==i & Fishery=='Commercial All')
#   trec <- subset(rec_acls, Species==i & Fishery=='Recreational All')
#   
#   plot(tcom$`First Year in Effect`, tcom$ACL)
#   mtext(i)
#   plot(trec$`First Year in Effect`, trec$ACL)
#   mtext(i)
# }
# 
# ### test kmk
# kmk_acl <- subset(mh, SPP_NAME=='MACKEREL, KING' & MANAGEMENT_TYPE_USE=='ACL' &
#                     REGION=='GULF OF MEXICO')  |> type.convert()
# # plot(kmk_acl$START_YEAR,kmk_acl$VALUE)
# 
# kmk_sum <- kmk_acl |> 
#   filter(SUBSECTOR=='ALL' & SECTOR!='ALL') |>
#   group_by(START_YEAR, SECTOR, SUBSECTOR)|>
#   summarise(sum = sum(VALUE)) |>
#   select(START_YEAR, SECTOR, sum)
# yr_sec <- expand.grid(START_YEAR = 2012:2023,
#             SECTOR = c('COMMERCIAL','RECREATIONAL')) |>
#   merge(kmk_sum, by = c('START_YEAR','SECTOR'), all = T) |>
#   arrange(START_YEAR, SECTOR)
# 
# for(i in which(is.na(yr_sec$sum))){
#   tmp <- yr_sec[i, ]
#   yr_sec$sum[i] <- subset(yr_sec, START_YEAR==(tmp$START_YEAR-1) &
#            SECTOR==tmp$SECTOR,
#          select = sum) |> unlist()
# }
# 
# plot(yr_sec$START_YEAR, yr_sec$sum, typ = 'p')



### to do
# make dataframe that summarizes changes per species, per sector (rec, com, and total)
# also consider doing this for the quotas

# acls <- mh %>%
#   filter(COMMON_NAME_USE %in% species,
#          REGION %in% region,
#          DETAILED == "YES",
#          NEVER_IMPLEMENTED %in% c(0, NA),
#          REG_REMOVED == 0,
#          MANAGEMENT_TYPE_USE == "ACL",
#          # MANAGEMENT_TYPE == "ACL",
#          VALUE_UNITS == 'POUNDS') %>%
#   arrange(CLUSTER, START_DATE2) |> type.convert() |>
#   filter(SUBSECTOR=='ALL' & SECTOR!='ALL') |>
#   group_by(COMMON_NAME_USE, START_YEAR, SECTOR, SUBSECTOR)|>
#   summarise(sum = sum(VALUE)) |>
#   select(COMMON_NAME_USE, START_YEAR, SECTOR, sum)

### now, how to account for ACL adjustments? Also, there are duplicates???
acls <- mh %>%
  filter(COMMON_NAME_USE %in% species,
         REGION %in% region,
         DETAILED == "YES",
         NEVER_IMPLEMENTED %in% c(0, NA),
         REG_REMOVED == 0,
         MANAGEMENT_TYPE_USE == "ACL",
         # MANAGEMENT_TYPE == "ACL",
         VALUE_UNITS == 'POUNDS',
         SPP_TYPE == 'COMMON_NAME') %>%
  arrange(CLUSTER, START_DATE2) |> type.convert() |>
  filter(SUBSECTOR=='ALL' & SECTOR!='ALL') #|>
  # select(COMMON_NAME_USE, START_YEAR, START_DATE2, END_DATE2, SECTOR, VALUE, MANAGEMENT_TYPE)


acl_slice <- acls |>
  arrange(COMMON_NAME_USE, START_YEAR, SECTOR, EFFECTIVE_DATE) |>
  group_by(COMMON_NAME_USE, START_YEAR, SECTOR) |>
  slice_max(order_by = EFFECTIVE_DATE, n = 1, with_ties = F) |>
  select(COMMON_NAME_USE, START_YEAR, SECTOR, VALUE)


# spp <- unique(acls$COMMON_NAME_USE)
# out <- list()
# for(i in spp){
#   i=spp[1]
#   tmp <- subset(acls, COMMON_NAME_USE==i) |>
#     arrange(START_YEAR, SECTOR, EFFECTIVE_DATE) |>
#     group_by(START_YEAR, SECTOR) |>
#     slice_max(order_by = EFFECTIVE_DATE, n = 1)
#   
#   adj <- grep('adjustment', tmp$MANAGEMENT_TYPE, ignore.case = T)
#   for(j in adj){
#     j=adj[1]
#     
#   }
# }
# 
# adj <- grep('adjustment', acls$MANAGEMENT_TYPE, ignore.case = T)
# acls[adj,] |> View()
# subset(acls, COMMON_NAME_USE=='GROUPER, GAG' & START_YEAR==2023 & SECTOR=='COMMERCIAL')
# subset(acls, COMMON_NAME_USE=='GROUPER, GAG' & START_YEAR==2023)|>View()

acl_units <- mh %>%
  filter(COMMON_NAME_USE %in% species,
         REGION %in% region,
         DETAILED == "YES",
         NEVER_IMPLEMENTED %in% c(0, NA),
         REG_REMOVED == 0,
         MANAGEMENT_TYPE_USE == "ACL") |>
  arrange(CLUSTER, START_DATE2) |> type.convert() |>
  filter(SUBSECTOR=='ALL' & SECTOR!='ALL') |>
  group_by(COMMON_NAME_USE, START_YEAR, SECTOR, SUBSECTOR)|>
  select(COMMON_NAME_USE, MANAGEMENT_TYPE, START_YEAR, SECTOR, VALUE_TYPE, VALUE_UNITS, VALUE_RATE)


spp <- unique(acl_slice$COMMON_NAME_USE)

acl_out <- list()
for(i in spp){ ## this loop imputes missing acls using a previous year value; first year is skipped and propagate
  tmp <- subset(acl_slice, COMMON_NAME_USE==i)
  
  yr_sec <- expand.grid(START_YEAR = min(tmp$START_YEAR):max(tmp$START_YEAR),
                        SECTOR = c('COMMERCIAL','RECREATIONAL')) |>
    merge(tmp, by = c('START_YEAR','SECTOR'), all = T) |>
    arrange(START_YEAR, SECTOR)
  
  if(any(is.na(yr_sec$COMMON_NAME_USE))){
    yr_sec$COMMON_NAME_USE[which(is.na(yr_sec$COMMON_NAME_USE))] <- i
  }
  
  for(j in which(is.na(yr_sec$VALUE))){
    itmp <- yr_sec[j, ]
    if(itmp$START_YEAR!=min(yr_sec$START_YEAR)){
      yr_sec$VALUE[j] <- subset(yr_sec, START_YEAR==(itmp$START_YEAR-1) &
                                SECTOR==itmp$SECTOR,
                              select = VALUE) |> unlist()
    }
    
  }
  
  acl_out[[which(i==spp)]] <- yr_sec
}
acl_sums <- bind_rows(acl_out)

par(mfrow = c(3,3), mar = c(4,4,1.5,1))
for(i in spp){
  tmp <- subset(acl_sums, COMMON_NAME_USE==i)
  with(subset(tmp, SECTOR=='COMMERCIAL'),
       plot(START_YEAR, VALUE,
            ylim = range(tmp$VALUE, na.rm = T),
            typ = 'o', col = 1, lwd = 2,
            xlab = '',
            ylab = 'ACL (pounds)', 
            panel.first = grid()))
  mtext(i, cex = .7)
  points(VALUE~START_YEAR, 
       data = subset(tmp, SECTOR=='RECREATIONAL'),
       typ = 'o', col = 2, lwd = 2)
  legend('topright',c('COM','REC'), pch = 1, col = c(1,2), bty = 'n', pt.lwd = 2)
}

### now how to account for ACL adjustments?



#### Quotas ####
# Filter to species, region, and sector of interest
# Remove non-detailed regulations, those that were never implemented, and those that removed a regulation
# Adjust variable titles and format
Quotas <- mh %>%
  filter(COMMON_NAME_USE %in% species,
         REGION %in% region,
         SECTOR == sector,
         DETAILED == "YES",
         NEVER_IMPLEMENTED %in% c(0, NA),
         REG_REMOVED == 0,
         MANAGEMENT_TYPE_USE == "QUOTA") %>%
  arrange(CLUSTER, START_DATE2) %>%
  mutate(Species = case_when(str_detect(COMMON_NAME_USE, ",") ~ {parts <- str_split_fixed(COMMON_NAME_USE, ",", 2)
  str_to_title(str_trim(parts[,2])) %>% paste(str_to_title(str_trim(parts[,1])))},
  TRUE ~ str_to_title(COMMON_NAME_USE)),
  Fishery = str_to_title(paste0(SECTOR_USE, " ", SUBSECTOR_USE)),
  `Region Affected` = str_to_title(paste0(REGION, " ", ZONE_USE)),
  `First Year in Effect` = year(START_DATE2),
  `Fishing Year Effective Date` = format(START_DATE2, "%m/%d/%Y"),
  `FR Notice Effetive Date` = format(EFFECTIVE_DATE, "%m/%d/%Y"),
  `FR Notice Ineffective Date` = format(INEFFECTIVE_DATE, "%m/%d/%Y"),
  Quota = case_when(MANAGEMENT_TYPE_USE == "QUOTA" ~ paste0(VALUE, " ", tolower(VALUE_UNITS),ifelse(!is.na(VALUE_TYPE), paste0(" (", tolower(VALUE_TYPE), ")"), "")),
                    TRUE ~ NA_character_),
  `FR Reference` = case_when(MANAGEMENT_TYPE_USE == "QUOTA" ~ FR_CITATION,
                             TRUE ~ NA_character_),
  `FR URL` = case_when(MANAGEMENT_TYPE_USE == "QUOTA" ~ FR_URL,
                       TRUE ~ NA_character_),
  `Amendment Number or Rule Type` = case_when(!is.na(AMENDMENT_NUMBER) & !is.na(ACTION_TYPE) ~ str_to_title(paste0(ACTION, " ", ACTION_TYPE, " ", AMENDMENT_NUMBER)),
                                              !is.na(AMENDMENT_NUMBER) &  is.na(ACTION_TYPE) ~ str_to_title(paste0(ACTION, " ", AMENDMENT_NUMBER)),
                                              is.na(AMENDMENT_NUMBER) & !is.na(ACTION_TYPE) ~ str_to_title(paste0(ACTION, " ", ACTION_TYPE)),
                                              is.na(AMENDMENT_NUMBER) &  is.na(ACTION_TYPE) ~ str_to_title(ACTION)),
  `Cluster` = CLUSTER) %>%
  select(CLUSTER, Species, Fishery, `Region Affected`,
         `First Year in Effect`, `Fishing Year Effective Date`, `FR Notice Effetive Date`, `FR Notice Ineffective Date`, `Quota`,
         `FR Reference`, `FR URL`, `Amendment Number or Rule Type`) %>%
  arrange(Species, CLUSTER, `First Year in Effect`)

#### ACTs ####
# Filter to species, region, and sector of interest
# Remove non-detailed regulations, those that were never implemented, and those that removed a regulation
# Adjust variable titles and format
ACTs <- mh %>%
  filter(COMMON_NAME_USE %in% species,
         REGION %in% region,
         SECTOR == sector,
         DETAILED == "YES",
         NEVER_IMPLEMENTED %in% c(0, NA),
         MANAGEMENT_TYPE_USE == "ACT") %>%
  arrange(CLUSTER, START_DATE2) %>%
  mutate(Species = case_when(str_detect(COMMON_NAME_USE, ",") ~ {parts <- str_split_fixed(COMMON_NAME_USE, ",", 2)
  str_to_title(str_trim(parts[,2])) %>% paste(str_to_title(str_trim(parts[,1])))},
  TRUE ~ str_to_title(COMMON_NAME_USE)),
  Fishery = str_to_title(paste0(SECTOR_USE, " ", SUBSECTOR_USE)),
  `Region Affected` = str_to_title(paste0(REGION, " ", ZONE_USE)),
  `First Year in Effect` = year(START_DATE2),
  `Fishing Year Effective Date` = format(START_DATE2, "%m/%d/%Y"),
  `FR Notice Effetive Date` = format(EFFECTIVE_DATE, "%m/%d/%Y"),
  `FR Notice Ineffective Date` = format(INEFFECTIVE_DATE, "%m/%d/%Y"),
  ACT = case_when(MANAGEMENT_TYPE_USE == "ACT" ~ paste0(VALUE, " ", tolower(VALUE_UNITS),ifelse(!is.na(VALUE_TYPE), paste0(" (", tolower(VALUE_TYPE), ")"), "")),
                  TRUE ~ NA_character_),
  `FR Reference` = case_when(MANAGEMENT_TYPE_USE == "ACT" ~ FR_CITATION,
                             TRUE ~ NA_character_),
  `FR URL` = case_when(MANAGEMENT_TYPE_USE == "ACT" ~ FR_URL,
                       TRUE ~ NA_character_),
  `Amendment Number or Rule Type` = case_when(!is.na(AMENDMENT_NUMBER) & !is.na(ACTION_TYPE) ~ str_to_title(paste0(ACTION, " ", ACTION_TYPE, " ", AMENDMENT_NUMBER)),
                                              !is.na(AMENDMENT_NUMBER) &  is.na(ACTION_TYPE) ~ str_to_title(paste0(ACTION, " ", AMENDMENT_NUMBER)),
                                              is.na(AMENDMENT_NUMBER) & !is.na(ACTION_TYPE) ~ str_to_title(paste0(ACTION, " ", ACTION_TYPE)),
                                              is.na(AMENDMENT_NUMBER) &  is.na(ACTION_TYPE) ~ str_to_title(ACTION)),
  `Cluster` = CLUSTER) %>%
  select(CLUSTER, Species, Fishery, `Region Affected`,
         `First Year in Effect`, `Fishing Year Effective Date`, `FR Notice Effetive Date`, `FR Notice Ineffective Date`, `ACT`,
         `FR Reference`, `FR URL`, `Amendment Number or Rule Type`) %>%
  arrange(Species, CLUSTER, `First Year in Effect`)

#### TACs ####
# Filter to species, region, and sector of interest
# Remove non-detailed regulations, those that were never implemented, and those that removed a regulation
# Adjust variable titles and format
TACs <- mh %>%
  filter(COMMON_NAME_USE %in% species,
         REGION %in% region,
         SECTOR == sector,
         DETAILED == "YES",
         NEVER_IMPLEMENTED %in% c(0, NA),
         REG_REMOVED == 0,
         MANAGEMENT_TYPE_USE == "TAC") %>%
  arrange(CLUSTER, START_DATE2) %>%
  mutate(Species = case_when(str_detect(COMMON_NAME_USE, ",") ~ {parts <- str_split_fixed(COMMON_NAME_USE, ",", 2)
  str_to_title(str_trim(parts[,2])) %>% paste(str_to_title(str_trim(parts[,1])))},
  TRUE ~ str_to_title(COMMON_NAME_USE)),
  Fishery = str_to_title(paste0(SECTOR_USE, " ", SUBSECTOR_USE)),
  `Region Affected` = str_to_title(paste0(REGION, " ", ZONE_USE)),
  `First Year in Effect` = year(START_DATE2),
  `Fishing Year Effective Date` = format(START_DATE2, "%m/%d/%Y"),
  `FR Notice Effetive Date` = format(EFFECTIVE_DATE, "%m/%d/%Y"),
  `FR Notice Ineffective Date` = format(INEFFECTIVE_DATE, "%m/%d/%Y"),
  TAC = case_when(MANAGEMENT_TYPE_USE == "TAC" ~ paste0(VALUE, " ", tolower(VALUE_UNITS),ifelse(!is.na(VALUE_TYPE), paste0(" (", tolower(VALUE_TYPE), ")"), "")),
                  TRUE ~ NA_character_),
  `FR Reference` = case_when(MANAGEMENT_TYPE_USE == "TAC" ~ FR_CITATION,
                             TRUE ~ NA_character_),
  `FR URL` = case_when(MANAGEMENT_TYPE_USE == "TAC" ~ FR_URL,
                       TRUE ~ NA_character_),
  `Amendment Number or Rule Type` = case_when(!is.na(AMENDMENT_NUMBER) & !is.na(ACTION_TYPE) ~ str_to_title(paste0(ACTION, " ", ACTION_TYPE, " ", AMENDMENT_NUMBER)),
                                              !is.na(AMENDMENT_NUMBER) &  is.na(ACTION_TYPE) ~ str_to_title(paste0(ACTION, " ", AMENDMENT_NUMBER)),
                                              is.na(AMENDMENT_NUMBER) & !is.na(ACTION_TYPE) ~ str_to_title(paste0(ACTION, " ", ACTION_TYPE)),
                                              is.na(AMENDMENT_NUMBER) &  is.na(ACTION_TYPE) ~ str_to_title(ACTION)),
  `Cluster` = CLUSTER) %>%
  select(CLUSTER, Species, Fishery, `Region Affected`,
         `First Year in Effect`, `Fishing Year Effective Date`, `FR Notice Effetive Date`, `FR Notice Ineffective Date`, `TAC`,
         `FR Reference`, `FR URL`, `Amendment Number or Rule Type`) %>%
  arrange(Species, CLUSTER, `First Year in Effect`)


spp <- c(unique(ACLs$Species),
         unique(ACTs$Species),
         unique(TACs$Species),
         unique(Quotas$Species))
unique(spp)

sort(table(mh$MANAGEMENT_TYPE_USE))
