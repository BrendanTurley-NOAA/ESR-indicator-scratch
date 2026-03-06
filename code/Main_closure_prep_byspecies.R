# Process all regulations related to closures

# Load packages ####
#install.packages("librarian")
librarian::shelf(here, tidyverse, lubridate, dplyr, tidyr, neatRanges, splitstackshape)

# Read in MH Data Log
mh_data_log <- readRDS(here("Data", "MH_DL_2025May05.RDS"))

# Function to expand dates based on management status
source(here("func_expand_status.R"))

################################################################################

# LANE SNAPPER (COMMERCIAL AND RECREATIONAL)

################################################################################

# Select species and region
spp <- 'SNAPPER, LANE'
region <- 'GULF OF MEXICO'

# filter for species and region (all closure related records)
# Recode management types to have a value
mh_spp_closure <- mh_data_log %>%
  filter(COMMON_NAME_USE == spp, REGION == region,
         MANAGEMENT_TYPE_USE %in% c('CLOSURE', 'FISHING SEASON', 'FISHING YEAR',
                                    'PROHIBITED SALE AND PURCHASE',
                                    'PROHIBITED SPECIES')) %>%
  mutate(VALUE = case_when(MANAGEMENT_TYPE_USE %in% c('FISHING SEASON', 'FISHING YEAR') ~ 'OPEN',
                           MANAGEMENT_TYPE_USE == 'PROHIBITED SALE AND PURCHASE' & FLAG == 'YES' ~ 'OPEN',
                           MANAGEMENT_TYPE_USE == 'PROHIBITED SALE AND PURCHASE' & FLAG == 'NO' ~ 'CLOSE',
                           MANAGEMENT_TYPE_USE == 'PROHIBITED SPECIES' ~ 'CLOSE',
                           TRUE ~ VALUE)) %>%
  arrange(SECTOR_USE, START_DATE2)

unique(mh_spp_closure$MANAGEMENT_TYPE_USE)

# Fishing Year
chk <- filter(mh_spp_closure, MANAGEMENT_TYPE_USE == "FISHING YEAR")
spp_year <- expand_status(mh_spp_closure, "FISHING YEAR")

# Closure
unique(select(filter(mh_spp_closure, MANAGEMENT_TYPE_USE == "CLOSURE"), MANAGEMENT_STATUS_USE))
spp_closures <- expand_status(mh_spp_closure, "CLOSURE")

# Combine all management types that refer to closures
# For now just comment out mtypes that do not apply
spp_closure_story <- spp_year %>%
  rename(FR_CITATION_year = "FR_CITATION",
         VALUE_year = "VALUE") %>%
  select(FMP, COMMON_NAME_USE, REGION, ZONE_USE, SECTOR_USE, SUBSECTOR_USE, date_sequence, FR_CITATION_year, VALUE_year) %>%
  # full_join(spp_season %>%
  #             rename(FR_CITATION_season = "FR_CITATION",
  #                    VALUE_season = "VALUE") %>%
  #             select(FMP, COMMON_NAME_USE, REGION, ZONE_USE, SECTOR_USE, SUBSECTOR_USE, date_sequence, FR_CITATION_season, VALUE_season),
  #           by = join_by(FMP, COMMON_NAME_USE, REGION, ZONE_USE, SECTOR_USE, SUBSECTOR_USE,  date_sequence)) %>%
  full_join(spp_closures %>%
              rename(FR_CITATION_close = "FR_CITATION",
                     VALUE_close = "VALUE") %>%
              select(FMP, COMMON_NAME_USE, REGION, ZONE_USE, SECTOR_USE, SUBSECTOR_USE, date_sequence, FR_CITATION_close, VALUE_close),
            by = join_by(FMP, COMMON_NAME_USE, REGION, ZONE_USE, SECTOR_USE, SUBSECTOR_USE,  date_sequence)) %>%
  # full_join(spp_prohibited_sale %>%
  #            rename(FR_CITATION_sale = "FR_CITATION",
  #                    VALUE_sale = "VALUE") %>%
  #             select(FMP, COMMON_NAME_USE, REGION, ZONE_USE, SECTOR_USE, SUBSECTOR_USE, date_sequence, FR_CITATION_sale, VALUE_sale),
  #           by = join_by(FMP, COMMON_NAME_USE, REGION, ZONE_USE, SECTOR_USE, SUBSECTOR_USE,  date_sequence)) %>%
  # full_join(spp_prohibited_spp %>%
  #             rename(FR_CITATION_spp = "FR_CITATION",
  #                    VALUE_spp = "VALUE") %>%
  #             select(FMP, COMMON_NAME_USE, REGION, ZONE_USE, SECTOR_USE, SUBSECTOR_USE, date_sequence, FR_CITATION_spp, VALUE_spp),
  #             by = join_by(FMP, COMMON_NAME_USE, REGION, ZONE_USE, SECTOR_USE, SUBSECTOR_USE, date_sequence)) %>%
  arrange(SECTOR_USE, SUBSECTOR_USE, ZONE_USE, date_sequence) %>%
  # Select the most recent FR CITATION
  mutate(FR_CITATION = pmax(FR_CITATION_year, 
                            #FR_CITATION_season, 
                            FR_CITATION_close, 
                            #                            FR_CITATION_sale, 
                            #                            FR_CITATION_spp,
                            na.rm = T)) %>%
  # Select the fishery status (open/closed) that applies to the most recent FR
  mutate(VALUE = case_when(FR_CITATION == FR_CITATION_close ~ VALUE_close,
                           #                           FR_CITATION == FR_CITATION_sale ~ VALUE_sale,
                           #                          FR_CITATION == FR_CITATION_spp ~ VALUE_spp,
                           #FR_CITATION == FR_CITATION_season ~ VALUE_season,
                           FR_CITATION == FR_CITATION_year ~ VALUE_year)) %>%
  select(-FR_CITATION_year, -VALUE_year, -FR_CITATION_close, -VALUE_close) #, -FR_CITATION_sale, -VALUE_sale, -FR_CITATION_season, -VALUE_season)

# Summarize the open and closed periods for each year
summ_spp_closures <- spp_closure_story %>%
  mutate(YEAR = year(date_sequence)) %>%
  group_by(FMP, COMMON_NAME_USE, REGION, ZONE_USE, SECTOR_USE, SUBSECTOR_USE, YEAR) %>% 
  arrange(FMP, COMMON_NAME_USE, REGION, ZONE_USE, SECTOR_USE, SUBSECTOR_USE, date_sequence) %>%
  mutate(change = VALUE != lag(VALUE, default = first(VALUE)) |
           YEAR != lag(YEAR, default = first(YEAR))) %>%
  mutate(group = cumsum(change)) %>%
  group_by(FMP, COMMON_NAME_USE, REGION, ZONE_USE, SECTOR_USE, SUBSECTOR_USE, YEAR, VALUE, group) %>%
  summarize(ndays = n(),
            start = min(date_sequence),
            end = max(date_sequence),
            .groups = 'drop') %>%
  mutate(VALUE = case_when(is.na(VALUE) ~ "OPEN",
                           TRUE ~ VALUE)) %>%
  arrange(FMP, COMMON_NAME_USE, REGION, ZONE_USE, SECTOR_USE, SUBSECTOR_USE, YEAR, start)

# Summarize the number of days open per year
LSN_summ_days_open <- summ_spp_closures %>%
  filter(VALUE == "OPEN") %>%
  group_by(FMP, COMMON_NAME_USE, REGION, ZONE_USE, SECTOR_USE, SUBSECTOR_USE, YEAR) %>%
  summarize(Days_Open = sum(ndays), .groups = "drop") %>%
  arrange(FMP, COMMON_NAME_USE, REGION, ZONE_USE, SECTOR_USE, SUBSECTOR_USE, YEAR)

LSN_com<-LSN_summ_days_open %>% filter(SECTOR_USE=="COMMERCIAL" & ZONE_USE=="ALL")
Com_Open<-as.data.frame(cbind(LSN_com$YEAR,LSN_com$Days_Open))
colnames(Com_Open)<-c("Year","LSN")

LSN_rec<-LSN_summ_days_open %>% filter(SECTOR_USE=="RECREATIONAL" & ZONE_USE=="ALL")
Rec_Open<-as.data.frame(cbind(LSN_rec$YEAR,LSN_rec$Days_Open))
colnames(Rec_Open)<-c("Year","LSN")



################################################################################

# HOGFISH (COMMERCIAL AND RECREATIONAL)

################################################################################

# Select species and region
spp <- 'HOGFISH'
region <- 'GULF OF MEXICO'

# filter for species and region (all closure related records)
# Recode management types to have a value
mh_spp_closure <- mh_data_log %>%
  filter(COMMON_NAME_USE == spp, REGION == region,
         MANAGEMENT_TYPE_USE %in% c('CLOSURE', 'FISHING SEASON', 'FISHING YEAR',
                                    'PROHIBITED SALE AND PURCHASE',
                                    'PROHIBITED SPECIES')) %>%
  mutate(VALUE = case_when(MANAGEMENT_TYPE_USE %in% c('FISHING SEASON', 'FISHING YEAR') ~ 'OPEN',
                           MANAGEMENT_TYPE_USE == 'PROHIBITED SALE AND PURCHASE' & FLAG == 'YES' ~ 'OPEN',
                           MANAGEMENT_TYPE_USE == 'PROHIBITED SALE AND PURCHASE' & FLAG == 'NO' ~ 'CLOSE',
                           MANAGEMENT_TYPE_USE == 'PROHIBITED SPECIES' ~ 'CLOSE',
                           TRUE ~ VALUE)) %>%
  arrange(SECTOR_USE, START_DATE2)

unique(mh_spp_closure$MANAGEMENT_TYPE_USE)

# Fishing Year
chk <- filter(mh_spp_closure, MANAGEMENT_TYPE_USE == "FISHING YEAR")
spp_year <- expand_status(mh_spp_closure, "FISHING YEAR")

# Closure
unique(select(filter(mh_spp_closure, MANAGEMENT_TYPE_USE == "CLOSURE"), MANAGEMENT_STATUS_USE))
spp_closures <- expand_status(mh_spp_closure, "CLOSURE")

# Combine all management types that refer to closures
# For now just comment out mtypes that do not apply
spp_closure_story <- spp_year %>%
  rename(FR_CITATION_year = "FR_CITATION",
         VALUE_year = "VALUE") %>%
  select(FMP, COMMON_NAME_USE, REGION, ZONE_USE, SECTOR_USE, SUBSECTOR_USE, date_sequence, FR_CITATION_year, VALUE_year) %>%
  # full_join(spp_season %>%
  #             rename(FR_CITATION_season = "FR_CITATION",
  #                    VALUE_season = "VALUE") %>%
  #             select(FMP, COMMON_NAME_USE, REGION, ZONE_USE, SECTOR_USE, SUBSECTOR_USE, date_sequence, FR_CITATION_season, VALUE_season),
  #           by = join_by(FMP, COMMON_NAME_USE, REGION, ZONE_USE, SECTOR_USE, SUBSECTOR_USE,  date_sequence)) %>%
  full_join(spp_closures %>%
              rename(FR_CITATION_close = "FR_CITATION",
                     VALUE_close = "VALUE") %>%
              select(FMP, COMMON_NAME_USE, REGION, ZONE_USE, SECTOR_USE, SUBSECTOR_USE, date_sequence, FR_CITATION_close, VALUE_close),
            by = join_by(FMP, COMMON_NAME_USE, REGION, ZONE_USE, SECTOR_USE, SUBSECTOR_USE,  date_sequence)) %>%
  # full_join(spp_prohibited_sale %>%
  #            rename(FR_CITATION_sale = "FR_CITATION",
  #                    VALUE_sale = "VALUE") %>%
  #             select(FMP, COMMON_NAME_USE, REGION, ZONE_USE, SECTOR_USE, SUBSECTOR_USE, date_sequence, FR_CITATION_sale, VALUE_sale),
  #           by = join_by(FMP, COMMON_NAME_USE, REGION, ZONE_USE, SECTOR_USE, SUBSECTOR_USE,  date_sequence)) %>%
  # full_join(spp_prohibited_spp %>%
  #             rename(FR_CITATION_spp = "FR_CITATION",
  #                    VALUE_spp = "VALUE") %>%
  #             select(FMP, COMMON_NAME_USE, REGION, ZONE_USE, SECTOR_USE, SUBSECTOR_USE, date_sequence, FR_CITATION_spp, VALUE_spp),
  #             by = join_by(FMP, COMMON_NAME_USE, REGION, ZONE_USE, SECTOR_USE, SUBSECTOR_USE, date_sequence)) %>%
  arrange(SECTOR_USE, SUBSECTOR_USE, ZONE_USE, date_sequence) %>%
  # Select the most recent FR CITATION
  mutate(FR_CITATION = pmax(FR_CITATION_year, 
                            #FR_CITATION_season, 
                            FR_CITATION_close, 
                            #                            FR_CITATION_sale, 
                            #                            FR_CITATION_spp,
                            na.rm = T)) %>%
  # Select the fishery status (open/closed) that applies to the most recent FR
  mutate(VALUE = case_when(FR_CITATION == FR_CITATION_close ~ VALUE_close,
                           #                           FR_CITATION == FR_CITATION_sale ~ VALUE_sale,
                           #                          FR_CITATION == FR_CITATION_spp ~ VALUE_spp,
                           #FR_CITATION == FR_CITATION_season ~ VALUE_season,
                           FR_CITATION == FR_CITATION_year ~ VALUE_year)) %>%
  select(-FR_CITATION_year, -VALUE_year, -FR_CITATION_close, -VALUE_close) #, -FR_CITATION_sale, -VALUE_sale, -FR_CITATION_season, -VALUE_season)

# Summarize the open and closed periods for each year
summ_spp_closures <- spp_closure_story %>%
  mutate(YEAR = year(date_sequence)) %>%
  group_by(FMP, COMMON_NAME_USE, REGION, ZONE_USE, SECTOR_USE, SUBSECTOR_USE, YEAR) %>% 
  arrange(FMP, COMMON_NAME_USE, REGION, ZONE_USE, SECTOR_USE, SUBSECTOR_USE, date_sequence) %>%
  mutate(change = VALUE != lag(VALUE, default = first(VALUE)) |
           YEAR != lag(YEAR, default = first(YEAR))) %>%
  mutate(group = cumsum(change)) %>%
  group_by(FMP, COMMON_NAME_USE, REGION, ZONE_USE, SECTOR_USE, SUBSECTOR_USE, YEAR, VALUE, group) %>%
  summarize(ndays = n(),
            start = min(date_sequence),
            end = max(date_sequence),
            .groups = 'drop') %>%
  mutate(VALUE = case_when(is.na(VALUE) ~ "OPEN",
                           TRUE ~ VALUE)) %>%
  arrange(FMP, COMMON_NAME_USE, REGION, ZONE_USE, SECTOR_USE, SUBSECTOR_USE, YEAR, start)

# Summarize the number of days open per year
HOG_summ_days_open <- summ_spp_closures %>%
  filter(VALUE == "OPEN") %>%
  group_by(FMP, COMMON_NAME_USE, REGION, ZONE_USE, SECTOR_USE, SUBSECTOR_USE, YEAR) %>%
  summarize(Days_Open = sum(ndays), .groups = "drop") %>%
  arrange(FMP, COMMON_NAME_USE, REGION, ZONE_USE, SECTOR_USE, SUBSECTOR_USE, YEAR)

HOG_com<-HOG_summ_days_open %>% filter(SECTOR_USE=="COMMERCIAL" & ZONE_USE=="ALL")
Com_Open<-cbind(Com_Open,HOG_com$Days_Open)
colnames(Com_Open)<-c("Year","LSN","HOG")

HOG_rec<-HOG_summ_days_open %>% filter(SECTOR_USE=="RECREATIONAL" & ZONE_USE=="ALL")
Rec_Open<-cbind(Rec_Open,HOG_rec$Days_Open)
colnames(Rec_Open)<-c("Year","LSN","HOG")



################################################################################

# YELLOWEDGE GROUPER (DEEP-WATER GROUPER) - COMMERCIAL AND RECREATIONAL

################################################################################

# Select species and region
spp <- 'GROUPER, YELLOWEDGE'
region <- 'GULF OF MEXICO'

# filter for species and region (all closure related records)
# Recode management types to have a value
mh_spp_closure <- mh_data_log %>%
  filter(COMMON_NAME_USE == spp, REGION == region,
         MANAGEMENT_TYPE_USE %in% c('CLOSURE', 'FISHING SEASON', 'FISHING YEAR',
                                    'PROHIBITED SALE AND PURCHASE',
                                    'PROHIBITED SPECIES')) %>%
  mutate(VALUE = case_when(MANAGEMENT_TYPE_USE %in% c('FISHING SEASON', 'FISHING YEAR') ~ 'OPEN',
                           MANAGEMENT_TYPE_USE == 'PROHIBITED SALE AND PURCHASE' & FLAG == 'YES' ~ 'OPEN',
                           MANAGEMENT_TYPE_USE == 'PROHIBITED SALE AND PURCHASE' & FLAG == 'NO' ~ 'CLOSE',
                           MANAGEMENT_TYPE_USE == 'PROHIBITED SPECIES' ~ 'CLOSE',
                           TRUE ~ VALUE)) %>%
  arrange(SECTOR_USE, START_DATE2)

unique(mh_spp_closure$MANAGEMENT_TYPE_USE)

# Fishing Year
chk <- filter(mh_spp_closure, MANAGEMENT_TYPE_USE == "FISHING YEAR")
spp_year <- expand_status(mh_spp_closure, "FISHING YEAR")

# Closure
unique(select(filter(mh_spp_closure, MANAGEMENT_TYPE_USE == "CLOSURE"), MANAGEMENT_STATUS_USE))
spp_closures <- expand_status(mh_spp_closure, "CLOSURE")

# Combine all management types that refer to closures
# For now just comment out mtypes that do not apply
spp_closure_story <- spp_year %>%
  rename(FR_CITATION_year = "FR_CITATION",
         VALUE_year = "VALUE") %>%
  select(FMP, COMMON_NAME_USE, REGION, ZONE_USE, SECTOR_USE, SUBSECTOR_USE, date_sequence, FR_CITATION_year, VALUE_year) %>%
  # full_join(spp_season %>%
  #             rename(FR_CITATION_season = "FR_CITATION",
  #                    VALUE_season = "VALUE") %>%
  #             select(FMP, COMMON_NAME_USE, REGION, ZONE_USE, SECTOR_USE, SUBSECTOR_USE, date_sequence, FR_CITATION_season, VALUE_season),
  #           by = join_by(FMP, COMMON_NAME_USE, REGION, ZONE_USE, SECTOR_USE, SUBSECTOR_USE,  date_sequence)) %>%
  full_join(spp_closures %>%
              rename(FR_CITATION_close = "FR_CITATION",
                     VALUE_close = "VALUE") %>%
              select(FMP, COMMON_NAME_USE, REGION, ZONE_USE, SECTOR_USE, SUBSECTOR_USE, date_sequence, FR_CITATION_close, VALUE_close),
            by = join_by(FMP, COMMON_NAME_USE, REGION, ZONE_USE, SECTOR_USE, SUBSECTOR_USE,  date_sequence)) %>%
  # full_join(spp_prohibited_sale %>%
  #            rename(FR_CITATION_sale = "FR_CITATION",
  #                    VALUE_sale = "VALUE") %>%
  #             select(FMP, COMMON_NAME_USE, REGION, ZONE_USE, SECTOR_USE, SUBSECTOR_USE, date_sequence, FR_CITATION_sale, VALUE_sale),
  #           by = join_by(FMP, COMMON_NAME_USE, REGION, ZONE_USE, SECTOR_USE, SUBSECTOR_USE,  date_sequence)) %>%
  # full_join(spp_prohibited_spp %>%
  #             rename(FR_CITATION_spp = "FR_CITATION",
  #                    VALUE_spp = "VALUE") %>%
  #             select(FMP, COMMON_NAME_USE, REGION, ZONE_USE, SECTOR_USE, SUBSECTOR_USE, date_sequence, FR_CITATION_spp, VALUE_spp),
  #             by = join_by(FMP, COMMON_NAME_USE, REGION, ZONE_USE, SECTOR_USE, SUBSECTOR_USE, date_sequence)) %>%
  arrange(SECTOR_USE, SUBSECTOR_USE, ZONE_USE, date_sequence) %>%
  # Select the most recent FR CITATION
  mutate(FR_CITATION = pmax(FR_CITATION_year, 
                            #FR_CITATION_season, 
                            FR_CITATION_close, 
                            #                            FR_CITATION_sale, 
                            #                            FR_CITATION_spp,
                            na.rm = T)) %>%
  # Select the fishery status (open/closed) that applies to the most recent FR
  mutate(VALUE = case_when(FR_CITATION == FR_CITATION_close ~ VALUE_close,
                           #                           FR_CITATION == FR_CITATION_sale ~ VALUE_sale,
                           #                          FR_CITATION == FR_CITATION_spp ~ VALUE_spp,
                           #FR_CITATION == FR_CITATION_season ~ VALUE_season,
                           FR_CITATION == FR_CITATION_year ~ VALUE_year)) %>%
  select(-FR_CITATION_year, -VALUE_year, -FR_CITATION_close, -VALUE_close) #, -FR_CITATION_sale, -VALUE_sale, -FR_CITATION_season, -VALUE_season)

# Summarize the open and closed periods for each year
summ_spp_closures <- spp_closure_story %>%
  mutate(YEAR = year(date_sequence)) %>%
  group_by(FMP, COMMON_NAME_USE, REGION, ZONE_USE, SECTOR_USE, SUBSECTOR_USE, YEAR) %>% 
  arrange(FMP, COMMON_NAME_USE, REGION, ZONE_USE, SECTOR_USE, SUBSECTOR_USE, date_sequence) %>%
  mutate(change = VALUE != lag(VALUE, default = first(VALUE)) |
           YEAR != lag(YEAR, default = first(YEAR))) %>%
  mutate(group = cumsum(change)) %>%
  group_by(FMP, COMMON_NAME_USE, REGION, ZONE_USE, SECTOR_USE, SUBSECTOR_USE, YEAR, VALUE, group) %>%
  summarize(ndays = n(),
            start = min(date_sequence),
            end = max(date_sequence),
            .groups = 'drop') %>%
  mutate(VALUE = case_when(is.na(VALUE) ~ "OPEN",
                           TRUE ~ VALUE)) %>%
  arrange(FMP, COMMON_NAME_USE, REGION, ZONE_USE, SECTOR_USE, SUBSECTOR_USE, YEAR, start)

# Summarize the number of days open per year
DWG_summ_days_open <- summ_spp_closures %>%
  filter(VALUE == "OPEN") %>%
  group_by(FMP, COMMON_NAME_USE, REGION, ZONE_USE, SECTOR_USE, SUBSECTOR_USE, YEAR) %>%
  summarize(Days_Open = sum(ndays), .groups = "drop") %>%
  arrange(FMP, COMMON_NAME_USE, REGION, ZONE_USE, SECTOR_USE, SUBSECTOR_USE, YEAR)

DWG_com<-DWG_summ_days_open %>% filter(SECTOR_USE=="COMMERCIAL" & ZONE_USE=="ALL")
Com_Open<-cbind(Com_Open,DWG_com$Days_Open)
colnames(Com_Open)<-c("Year","LSN","HOG","DWG")

DWG_rec<-DWG_summ_days_open %>% filter(SECTOR_USE=="RECREATIONAL" & ZONE_USE=="ALL")
Rec_Open<-cbind(Rec_Open,DWG_rec$Days_Open)
colnames(Rec_Open)<-c("Year","LSN","HOG","DWG")



################################################################################

# YELLOWFIN GROUPER (OTHER SHALLOW-WATER GROUPER COMMERCIAL, 
#                      SHALLOW-WATER GROUPER RECREATIONAL)

################################################################################

# Select species and region
spp <- 'GROUPER, YELLOWFIN'
region <- 'GULF OF MEXICO'

# filter for species and region (all closure related records)
# Recode management types to have a value
mh_spp_closure <- mh_data_log %>%
  filter(COMMON_NAME_USE == spp, REGION == region,
         MANAGEMENT_TYPE_USE %in% c('CLOSURE', 'FISHING SEASON', 'FISHING YEAR',
                                    'PROHIBITED SALE AND PURCHASE',
                                    'PROHIBITED SPECIES')) %>%
  mutate(VALUE = case_when(MANAGEMENT_TYPE_USE %in% c('FISHING SEASON', 'FISHING YEAR') ~ 'OPEN',
                           MANAGEMENT_TYPE_USE == 'PROHIBITED SALE AND PURCHASE' & FLAG == 'YES' ~ 'OPEN',
                           MANAGEMENT_TYPE_USE == 'PROHIBITED SALE AND PURCHASE' & FLAG == 'NO' ~ 'CLOSE',
                           MANAGEMENT_TYPE_USE == 'PROHIBITED SPECIES' ~ 'CLOSE',
                           TRUE ~ VALUE)) %>%
  arrange(SECTOR_USE, START_DATE2)

unique(mh_spp_closure$MANAGEMENT_TYPE_USE)

# Fishing Year
chk <- filter(mh_spp_closure, MANAGEMENT_TYPE_USE == "FISHING YEAR")
spp_year <- expand_status(mh_spp_closure, "FISHING YEAR")

# Closure
unique(select(filter(mh_spp_closure, MANAGEMENT_TYPE_USE == "CLOSURE"), MANAGEMENT_STATUS_USE))
spp_closures <- expand_status(mh_spp_closure, "CLOSURE")

# Combine all management types that refer to closures
# For now just comment out mtypes that do not apply
spp_closure_story <- spp_year %>%
  rename(FR_CITATION_year = "FR_CITATION",
         VALUE_year = "VALUE") %>%
  select(FMP, COMMON_NAME_USE, REGION, ZONE_USE, SECTOR_USE, SUBSECTOR_USE, date_sequence, FR_CITATION_year, VALUE_year) %>%
  # full_join(spp_season %>%
  #             rename(FR_CITATION_season = "FR_CITATION",
  #                    VALUE_season = "VALUE") %>%
  #             select(FMP, COMMON_NAME_USE, REGION, ZONE_USE, SECTOR_USE, SUBSECTOR_USE, date_sequence, FR_CITATION_season, VALUE_season),
  #           by = join_by(FMP, COMMON_NAME_USE, REGION, ZONE_USE, SECTOR_USE, SUBSECTOR_USE,  date_sequence)) %>%
  full_join(spp_closures %>%
              rename(FR_CITATION_close = "FR_CITATION",
                     VALUE_close = "VALUE") %>%
              select(FMP, COMMON_NAME_USE, REGION, ZONE_USE, SECTOR_USE, SUBSECTOR_USE, date_sequence, FR_CITATION_close, VALUE_close),
            by = join_by(FMP, COMMON_NAME_USE, REGION, ZONE_USE, SECTOR_USE, SUBSECTOR_USE,  date_sequence)) %>%
  # full_join(spp_prohibited_sale %>%
  #            rename(FR_CITATION_sale = "FR_CITATION",
  #                    VALUE_sale = "VALUE") %>%
  #             select(FMP, COMMON_NAME_USE, REGION, ZONE_USE, SECTOR_USE, SUBSECTOR_USE, date_sequence, FR_CITATION_sale, VALUE_sale),
  #           by = join_by(FMP, COMMON_NAME_USE, REGION, ZONE_USE, SECTOR_USE, SUBSECTOR_USE,  date_sequence)) %>%
  # full_join(spp_prohibited_spp %>%
  #             rename(FR_CITATION_spp = "FR_CITATION",
  #                    VALUE_spp = "VALUE") %>%
  #             select(FMP, COMMON_NAME_USE, REGION, ZONE_USE, SECTOR_USE, SUBSECTOR_USE, date_sequence, FR_CITATION_spp, VALUE_spp),
  #             by = join_by(FMP, COMMON_NAME_USE, REGION, ZONE_USE, SECTOR_USE, SUBSECTOR_USE, date_sequence)) %>%
  arrange(SECTOR_USE, SUBSECTOR_USE, ZONE_USE, date_sequence) %>%
  # Select the most recent FR CITATION
  mutate(FR_CITATION = pmax(FR_CITATION_year, 
                            #FR_CITATION_season, 
                            FR_CITATION_close, 
                            #                            FR_CITATION_sale, 
                            #                            FR_CITATION_spp,
                            na.rm = T)) %>%
  # Select the fishery status (open/closed) that applies to the most recent FR
  mutate(VALUE = case_when(FR_CITATION == FR_CITATION_close ~ VALUE_close,
                           #                           FR_CITATION == FR_CITATION_sale ~ VALUE_sale,
                           #                          FR_CITATION == FR_CITATION_spp ~ VALUE_spp,
                           #FR_CITATION == FR_CITATION_season ~ VALUE_season,
                           FR_CITATION == FR_CITATION_year ~ VALUE_year)) %>%
  select(-FR_CITATION_year, -VALUE_year, -FR_CITATION_close, -VALUE_close) #, -FR_CITATION_sale, -VALUE_sale, -FR_CITATION_season, -VALUE_season)

# Summarize the open and closed periods for each year
summ_spp_closures <- spp_closure_story %>%
  mutate(YEAR = year(date_sequence)) %>%
  group_by(FMP, COMMON_NAME_USE, REGION, ZONE_USE, SECTOR_USE, SUBSECTOR_USE, YEAR) %>% 
  arrange(FMP, COMMON_NAME_USE, REGION, ZONE_USE, SECTOR_USE, SUBSECTOR_USE, date_sequence) %>%
  mutate(change = VALUE != lag(VALUE, default = first(VALUE)) |
           YEAR != lag(YEAR, default = first(YEAR))) %>%
  mutate(group = cumsum(change)) %>%
  group_by(FMP, COMMON_NAME_USE, REGION, ZONE_USE, SECTOR_USE, SUBSECTOR_USE, YEAR, VALUE, group) %>%
  summarize(ndays = n(),
            start = min(date_sequence),
            end = max(date_sequence),
            .groups = 'drop') %>%
  mutate(VALUE = case_when(is.na(VALUE) ~ "OPEN",
                           TRUE ~ VALUE)) %>%
  arrange(FMP, COMMON_NAME_USE, REGION, ZONE_USE, SECTOR_USE, SUBSECTOR_USE, YEAR, start)

# Summarize the number of days open per year
SWG_summ_days_open <- summ_spp_closures %>%
  filter(VALUE == "OPEN") %>%
  group_by(FMP, COMMON_NAME_USE, REGION, ZONE_USE, SECTOR_USE, SUBSECTOR_USE, YEAR) %>%
  summarize(Days_Open = sum(ndays), .groups = "drop") %>%
  arrange(FMP, COMMON_NAME_USE, REGION, ZONE_USE, SECTOR_USE, SUBSECTOR_USE, YEAR)

SWG_com<-SWG_summ_days_open %>% filter(SECTOR_USE=="COMMERCIAL" & ZONE_USE=="ALL")
Com_Open<-cbind(Com_Open,SWG_com$Days_Open)
colnames(Com_Open)<-c("Year","LSN","HOG","DWG","OSWG")

SWG_rec<-SWG_summ_days_open %>% filter(SECTOR_USE=="RECREATIONAL" & ZONE_USE=="ALL")
Rec_Open<-cbind(Rec_Open,SWG_rec$Days_Open)
colnames(Rec_Open)<-c("Year","LSN","HOG","DWG","SWG")



################################################################################

# BLACK GROUPER (SHALLOW-WATER GROUPER COMMERCIAL,  
#                  BLACK GROUPER RECREATIONAL)

################################################################################

# Select species and region
spp <- 'GROUPER, BLACK'
region <- 'GULF OF MEXICO'

# filter for species and region (all closure related records)
# Recode management types to have a value
mh_spp_closure <- mh_data_log %>%
  filter(COMMON_NAME_USE == spp, REGION == region,
         MANAGEMENT_TYPE_USE %in% c('CLOSURE', 'FISHING SEASON', 'FISHING YEAR',
                                    'PROHIBITED SALE AND PURCHASE',
                                    'PROHIBITED SPECIES')) %>%
  mutate(VALUE = case_when(MANAGEMENT_TYPE_USE %in% c('FISHING SEASON', 'FISHING YEAR') ~ 'OPEN',
                           MANAGEMENT_TYPE_USE == 'PROHIBITED SALE AND PURCHASE' & FLAG == 'YES' ~ 'OPEN',
                           MANAGEMENT_TYPE_USE == 'PROHIBITED SALE AND PURCHASE' & FLAG == 'NO' ~ 'CLOSE',
                           MANAGEMENT_TYPE_USE == 'PROHIBITED SPECIES' ~ 'CLOSE',
                           TRUE ~ VALUE)) %>%
  arrange(SECTOR_USE, START_DATE2)

unique(mh_spp_closure$MANAGEMENT_TYPE_USE)

# Fishing Year
chk <- filter(mh_spp_closure, MANAGEMENT_TYPE_USE == "FISHING YEAR")
spp_year <- expand_status(mh_spp_closure, "FISHING YEAR")

# Closure
unique(select(filter(mh_spp_closure, MANAGEMENT_TYPE_USE == "CLOSURE"), MANAGEMENT_STATUS_USE))
spp_closures <- expand_status(mh_spp_closure, "CLOSURE")

# Combine all management types that refer to closures
# For now just comment out mtypes that do not apply
spp_closure_story <- spp_year %>%
  rename(FR_CITATION_year = "FR_CITATION",
         VALUE_year = "VALUE") %>%
  select(FMP, COMMON_NAME_USE, REGION, ZONE_USE, SECTOR_USE, SUBSECTOR_USE, date_sequence, FR_CITATION_year, VALUE_year) %>%
  # full_join(spp_season %>%
  #             rename(FR_CITATION_season = "FR_CITATION",
  #                    VALUE_season = "VALUE") %>%
  #             select(FMP, COMMON_NAME_USE, REGION, ZONE_USE, SECTOR_USE, SUBSECTOR_USE, date_sequence, FR_CITATION_season, VALUE_season),
  #           by = join_by(FMP, COMMON_NAME_USE, REGION, ZONE_USE, SECTOR_USE, SUBSECTOR_USE,  date_sequence)) %>%
  full_join(spp_closures %>%
              rename(FR_CITATION_close = "FR_CITATION",
                     VALUE_close = "VALUE") %>%
              select(FMP, COMMON_NAME_USE, REGION, ZONE_USE, SECTOR_USE, SUBSECTOR_USE, date_sequence, FR_CITATION_close, VALUE_close),
            by = join_by(FMP, COMMON_NAME_USE, REGION, ZONE_USE, SECTOR_USE, SUBSECTOR_USE,  date_sequence)) %>%
  # full_join(spp_prohibited_sale %>%
  #            rename(FR_CITATION_sale = "FR_CITATION",
  #                    VALUE_sale = "VALUE") %>%
  #             select(FMP, COMMON_NAME_USE, REGION, ZONE_USE, SECTOR_USE, SUBSECTOR_USE, date_sequence, FR_CITATION_sale, VALUE_sale),
  #           by = join_by(FMP, COMMON_NAME_USE, REGION, ZONE_USE, SECTOR_USE, SUBSECTOR_USE,  date_sequence)) %>%
  # full_join(spp_prohibited_spp %>%
  #             rename(FR_CITATION_spp = "FR_CITATION",
  #                    VALUE_spp = "VALUE") %>%
  #             select(FMP, COMMON_NAME_USE, REGION, ZONE_USE, SECTOR_USE, SUBSECTOR_USE, date_sequence, FR_CITATION_spp, VALUE_spp),
  #             by = join_by(FMP, COMMON_NAME_USE, REGION, ZONE_USE, SECTOR_USE, SUBSECTOR_USE, date_sequence)) %>%
  arrange(SECTOR_USE, SUBSECTOR_USE, ZONE_USE, date_sequence) %>%
  # Select the most recent FR CITATION
  mutate(FR_CITATION = pmax(FR_CITATION_year, 
                            #FR_CITATION_season, 
                            FR_CITATION_close, 
                            #                            FR_CITATION_sale, 
                            #                            FR_CITATION_spp,
                            na.rm = T)) %>%
  # Select the fishery status (open/closed) that applies to the most recent FR
  mutate(VALUE = case_when(FR_CITATION == FR_CITATION_close ~ VALUE_close,
                           #                           FR_CITATION == FR_CITATION_sale ~ VALUE_sale,
                           #                          FR_CITATION == FR_CITATION_spp ~ VALUE_spp,
                           #FR_CITATION == FR_CITATION_season ~ VALUE_season,
                           FR_CITATION == FR_CITATION_year ~ VALUE_year)) %>%
  select(-FR_CITATION_year, -VALUE_year, -FR_CITATION_close, -VALUE_close) #, -FR_CITATION_sale, -VALUE_sale, -FR_CITATION_season, -VALUE_season)

# Summarize the open and closed periods for each year
summ_spp_closures <- spp_closure_story %>%
  mutate(YEAR = year(date_sequence)) %>%
  group_by(FMP, COMMON_NAME_USE, REGION, ZONE_USE, SECTOR_USE, SUBSECTOR_USE, YEAR) %>% 
  arrange(FMP, COMMON_NAME_USE, REGION, ZONE_USE, SECTOR_USE, SUBSECTOR_USE, date_sequence) %>%
  mutate(change = VALUE != lag(VALUE, default = first(VALUE)) |
           YEAR != lag(YEAR, default = first(YEAR))) %>%
  mutate(group = cumsum(change)) %>%
  group_by(FMP, COMMON_NAME_USE, REGION, ZONE_USE, SECTOR_USE, SUBSECTOR_USE, YEAR, VALUE, group) %>%
  summarize(ndays = n(),
            start = min(date_sequence),
            end = max(date_sequence),
            .groups = 'drop') %>%
  mutate(VALUE = case_when(is.na(VALUE) ~ "OPEN",
                           TRUE ~ VALUE)) %>%
  arrange(FMP, COMMON_NAME_USE, REGION, ZONE_USE, SECTOR_USE, SUBSECTOR_USE, YEAR, start)

# Summarize the number of days open per year
BGR_summ_days_open <- summ_spp_closures %>%
  filter(VALUE == "OPEN") %>%
  group_by(FMP, COMMON_NAME_USE, REGION, ZONE_USE, SECTOR_USE, SUBSECTOR_USE, YEAR) %>%
  summarize(Days_Open = sum(ndays), .groups = "drop") %>%
  arrange(FMP, COMMON_NAME_USE, REGION, ZONE_USE, SECTOR_USE, SUBSECTOR_USE, YEAR)

BGR_com<-BGR_summ_days_open %>% filter(SECTOR_USE=="COMMERCIAL" & ZONE_USE=="ALL")
Com_Open<-cbind(Com_Open,BGR_com$Days_Open)
colnames(Com_Open)<-c("Year","LSN","HOG","DWG","OSWG","SWG")

BGR_rec<-BGR_summ_days_open %>% filter(SECTOR_USE=="RECREATIONAL" & ZONE_USE=="ALL")
Rec_Open<-cbind(Rec_Open,BGR_rec$Days_Open)
colnames(Rec_Open)<-c("Year","LSN","HOG","DWG","SWG","BGR")



################################################################################

# SPECKLED HIND (COMMERCIAL)

################################################################################

# Select species and region
spp <- 'HIND, SPECKLED'
region <- 'GULF OF MEXICO'

# filter for species and region (all closure related records)
# Recode management types to have a value
mh_spp_closure <- mh_data_log %>%
  filter(COMMON_NAME_USE == spp, REGION == region,
         MANAGEMENT_TYPE_USE %in% c('CLOSURE', 'FISHING SEASON', 'FISHING YEAR',
                                    'PROHIBITED SALE AND PURCHASE',
                                    'PROHIBITED SPECIES')) %>%
  mutate(VALUE = case_when(MANAGEMENT_TYPE_USE %in% c('FISHING SEASON', 'FISHING YEAR') ~ 'OPEN',
                           MANAGEMENT_TYPE_USE == 'PROHIBITED SALE AND PURCHASE' & FLAG == 'YES' ~ 'OPEN',
                           MANAGEMENT_TYPE_USE == 'PROHIBITED SALE AND PURCHASE' & FLAG == 'NO' ~ 'CLOSE',
                           MANAGEMENT_TYPE_USE == 'PROHIBITED SPECIES' ~ 'CLOSE',
                           TRUE ~ VALUE)) %>%
  arrange(SECTOR_USE, START_DATE2)

unique(mh_spp_closure$MANAGEMENT_TYPE_USE)

# Fishing Year
chk <- filter(mh_spp_closure, MANAGEMENT_TYPE_USE == "FISHING YEAR")
spp_year <- expand_status(mh_spp_closure, "FISHING YEAR")

# Closure
unique(select(filter(mh_spp_closure, MANAGEMENT_TYPE_USE == "CLOSURE"), MANAGEMENT_STATUS_USE))
spp_closures <- expand_status(mh_spp_closure, "CLOSURE")

# Combine all management types that refer to closures
# For now just comment out mtypes that do not apply
spp_closure_story <- spp_year %>%
  rename(FR_CITATION_year = "FR_CITATION",
         VALUE_year = "VALUE") %>%
  select(FMP, COMMON_NAME_USE, REGION, ZONE_USE, SECTOR_USE, SUBSECTOR_USE, date_sequence, FR_CITATION_year, VALUE_year) %>%
  # full_join(spp_season %>%
  #             rename(FR_CITATION_season = "FR_CITATION",
  #                    VALUE_season = "VALUE") %>%
  #             select(FMP, COMMON_NAME_USE, REGION, ZONE_USE, SECTOR_USE, SUBSECTOR_USE, date_sequence, FR_CITATION_season, VALUE_season),
  #           by = join_by(FMP, COMMON_NAME_USE, REGION, ZONE_USE, SECTOR_USE, SUBSECTOR_USE,  date_sequence)) %>%
  full_join(spp_closures %>%
              rename(FR_CITATION_close = "FR_CITATION",
                     VALUE_close = "VALUE") %>%
              select(FMP, COMMON_NAME_USE, REGION, ZONE_USE, SECTOR_USE, SUBSECTOR_USE, date_sequence, FR_CITATION_close, VALUE_close),
            by = join_by(FMP, COMMON_NAME_USE, REGION, ZONE_USE, SECTOR_USE, SUBSECTOR_USE,  date_sequence)) %>%
  # full_join(spp_prohibited_sale %>%
  #            rename(FR_CITATION_sale = "FR_CITATION",
  #                    VALUE_sale = "VALUE") %>%
  #             select(FMP, COMMON_NAME_USE, REGION, ZONE_USE, SECTOR_USE, SUBSECTOR_USE, date_sequence, FR_CITATION_sale, VALUE_sale),
  #           by = join_by(FMP, COMMON_NAME_USE, REGION, ZONE_USE, SECTOR_USE, SUBSECTOR_USE,  date_sequence)) %>%
  # full_join(spp_prohibited_spp %>%
  #             rename(FR_CITATION_spp = "FR_CITATION",
  #                    VALUE_spp = "VALUE") %>%
  #             select(FMP, COMMON_NAME_USE, REGION, ZONE_USE, SECTOR_USE, SUBSECTOR_USE, date_sequence, FR_CITATION_spp, VALUE_spp),
  #             by = join_by(FMP, COMMON_NAME_USE, REGION, ZONE_USE, SECTOR_USE, SUBSECTOR_USE, date_sequence)) %>%
  arrange(SECTOR_USE, SUBSECTOR_USE, ZONE_USE, date_sequence) %>%
  # Select the most recent FR CITATION
  mutate(FR_CITATION = pmax(FR_CITATION_year, 
                            #FR_CITATION_season, 
                            FR_CITATION_close, 
                            #                            FR_CITATION_sale, 
                            #                            FR_CITATION_spp,
                            na.rm = T)) %>%
  # Select the fishery status (open/closed) that applies to the most recent FR
  mutate(VALUE = case_when(FR_CITATION == FR_CITATION_close ~ VALUE_close,
                           #                           FR_CITATION == FR_CITATION_sale ~ VALUE_sale,
                           #                          FR_CITATION == FR_CITATION_spp ~ VALUE_spp,
                           #FR_CITATION == FR_CITATION_season ~ VALUE_season,
                           FR_CITATION == FR_CITATION_year ~ VALUE_year)) %>%
  select(-FR_CITATION_year, -VALUE_year, -FR_CITATION_close, -VALUE_close) #, -FR_CITATION_sale, -VALUE_sale, -FR_CITATION_season, -VALUE_season)

# Summarize the open and closed periods for each year
summ_spp_closures <- spp_closure_story %>%
  mutate(YEAR = year(date_sequence)) %>%
  group_by(FMP, COMMON_NAME_USE, REGION, ZONE_USE, SECTOR_USE, SUBSECTOR_USE, YEAR) %>% 
  arrange(FMP, COMMON_NAME_USE, REGION, ZONE_USE, SECTOR_USE, SUBSECTOR_USE, date_sequence) %>%
  mutate(change = VALUE != lag(VALUE, default = first(VALUE)) |
           YEAR != lag(YEAR, default = first(YEAR))) %>%
  mutate(group = cumsum(change)) %>%
  group_by(FMP, COMMON_NAME_USE, REGION, ZONE_USE, SECTOR_USE, SUBSECTOR_USE, YEAR, VALUE, group) %>%
  summarize(ndays = n(),
            start = min(date_sequence),
            end = max(date_sequence),
            .groups = 'drop') %>%
  mutate(VALUE = case_when(is.na(VALUE) ~ "OPEN",
                           TRUE ~ VALUE)) %>%
  arrange(FMP, COMMON_NAME_USE, REGION, ZONE_USE, SECTOR_USE, SUBSECTOR_USE, YEAR, start)

# Summarize the number of days open per year
SH_summ_days_open <- summ_spp_closures %>%
  filter(VALUE == "OPEN") %>%
  group_by(FMP, COMMON_NAME_USE, REGION, ZONE_USE, SECTOR_USE, SUBSECTOR_USE, YEAR) %>%
  summarize(Days_Open = sum(ndays), .groups = "drop") %>%
  arrange(FMP, COMMON_NAME_USE, REGION, ZONE_USE, SECTOR_USE, SUBSECTOR_USE, YEAR)

SH_com<-SH_summ_days_open %>% filter(SECTOR_USE=="COMMERCIAL" & ZONE_USE=="ALL")
Com_Open<-cbind(Com_Open,SH_com$Days_Open)
colnames(Com_Open)<-c("Year","LSN","HOG","DWG","OSWG","SWG","SH")



################################################################################

# TILEFISH (GOLDEN TILEFISH) COMMERCIAL 

################################################################################

# Select species and region
spp <- 'TILEFISH, GOLDEN'
region <- 'GULF OF MEXICO'

# filter for species and region (all closure related records)
# Recode management types to have a value
mh_spp_closure <- mh_data_log %>%
  filter(COMMON_NAME_USE == spp, REGION == region,
         MANAGEMENT_TYPE_USE %in% c('CLOSURE', 'FISHING SEASON', 'FISHING YEAR',
                                    'PROHIBITED SALE AND PURCHASE',
                                    'PROHIBITED SPECIES')) %>%
  mutate(VALUE = case_when(MANAGEMENT_TYPE_USE %in% c('FISHING SEASON', 'FISHING YEAR') ~ 'OPEN',
                           MANAGEMENT_TYPE_USE == 'PROHIBITED SALE AND PURCHASE' & FLAG == 'YES' ~ 'OPEN',
                           MANAGEMENT_TYPE_USE == 'PROHIBITED SALE AND PURCHASE' & FLAG == 'NO' ~ 'CLOSE',
                           MANAGEMENT_TYPE_USE == 'PROHIBITED SPECIES' ~ 'CLOSE',
                           TRUE ~ VALUE)) %>%
  arrange(SECTOR_USE, START_DATE2)

unique(mh_spp_closure$MANAGEMENT_TYPE_USE)

# Fishing Year
chk <- filter(mh_spp_closure, MANAGEMENT_TYPE_USE == "FISHING YEAR")
spp_year <- expand_status(mh_spp_closure, "FISHING YEAR")

# Closure
unique(select(filter(mh_spp_closure, MANAGEMENT_TYPE_USE == "CLOSURE"), MANAGEMENT_STATUS_USE))
spp_closures <- expand_status(mh_spp_closure, "CLOSURE")

# Combine all management types that refer to closures
# For now just comment out mtypes that do not apply
spp_closure_story <- spp_year %>%
  rename(FR_CITATION_year = "FR_CITATION",
         VALUE_year = "VALUE") %>%
  select(FMP, COMMON_NAME_USE, REGION, ZONE_USE, SECTOR_USE, SUBSECTOR_USE, date_sequence, FR_CITATION_year, VALUE_year) %>%
  # full_join(spp_season %>%
  #             rename(FR_CITATION_season = "FR_CITATION",
  #                    VALUE_season = "VALUE") %>%
  #             select(FMP, COMMON_NAME_USE, REGION, ZONE_USE, SECTOR_USE, SUBSECTOR_USE, date_sequence, FR_CITATION_season, VALUE_season),
  #           by = join_by(FMP, COMMON_NAME_USE, REGION, ZONE_USE, SECTOR_USE, SUBSECTOR_USE,  date_sequence)) %>%
  full_join(spp_closures %>%
              rename(FR_CITATION_close = "FR_CITATION",
                     VALUE_close = "VALUE") %>%
              select(FMP, COMMON_NAME_USE, REGION, ZONE_USE, SECTOR_USE, SUBSECTOR_USE, date_sequence, FR_CITATION_close, VALUE_close),
            by = join_by(FMP, COMMON_NAME_USE, REGION, ZONE_USE, SECTOR_USE, SUBSECTOR_USE,  date_sequence)) %>%
  # full_join(spp_prohibited_sale %>%
  #            rename(FR_CITATION_sale = "FR_CITATION",
  #                    VALUE_sale = "VALUE") %>%
  #             select(FMP, COMMON_NAME_USE, REGION, ZONE_USE, SECTOR_USE, SUBSECTOR_USE, date_sequence, FR_CITATION_sale, VALUE_sale),
  #           by = join_by(FMP, COMMON_NAME_USE, REGION, ZONE_USE, SECTOR_USE, SUBSECTOR_USE,  date_sequence)) %>%
  # full_join(spp_prohibited_spp %>%
  #             rename(FR_CITATION_spp = "FR_CITATION",
  #                    VALUE_spp = "VALUE") %>%
  #             select(FMP, COMMON_NAME_USE, REGION, ZONE_USE, SECTOR_USE, SUBSECTOR_USE, date_sequence, FR_CITATION_spp, VALUE_spp),
  #             by = join_by(FMP, COMMON_NAME_USE, REGION, ZONE_USE, SECTOR_USE, SUBSECTOR_USE, date_sequence)) %>%
  arrange(SECTOR_USE, SUBSECTOR_USE, ZONE_USE, date_sequence) %>%
  # Select the most recent FR CITATION
  mutate(FR_CITATION = pmax(FR_CITATION_year, 
                            #FR_CITATION_season, 
                            FR_CITATION_close, 
                            #                            FR_CITATION_sale, 
                            #                            FR_CITATION_spp,
                            na.rm = T)) %>%
  # Select the fishery status (open/closed) that applies to the most recent FR
  mutate(VALUE = case_when(FR_CITATION == FR_CITATION_close ~ VALUE_close,
                           #                           FR_CITATION == FR_CITATION_sale ~ VALUE_sale,
                           #                          FR_CITATION == FR_CITATION_spp ~ VALUE_spp,
                           #FR_CITATION == FR_CITATION_season ~ VALUE_season,
                           FR_CITATION == FR_CITATION_year ~ VALUE_year)) %>%
  select(-FR_CITATION_year, -VALUE_year, -FR_CITATION_close, -VALUE_close) #, -FR_CITATION_sale, -VALUE_sale, -FR_CITATION_season, -VALUE_season)

# Summarize the open and closed periods for each year
summ_spp_closures <- spp_closure_story %>%
  mutate(YEAR = year(date_sequence)) %>%
  group_by(FMP, COMMON_NAME_USE, REGION, ZONE_USE, SECTOR_USE, SUBSECTOR_USE, YEAR) %>% 
  arrange(FMP, COMMON_NAME_USE, REGION, ZONE_USE, SECTOR_USE, SUBSECTOR_USE, date_sequence) %>%
  mutate(change = VALUE != lag(VALUE, default = first(VALUE)) |
           YEAR != lag(YEAR, default = first(YEAR))) %>%
  mutate(group = cumsum(change)) %>%
  group_by(FMP, COMMON_NAME_USE, REGION, ZONE_USE, SECTOR_USE, SUBSECTOR_USE, YEAR, VALUE, group) %>%
  summarize(ndays = n(),
            start = min(date_sequence),
            end = max(date_sequence),
            .groups = 'drop') %>%
  mutate(VALUE = case_when(is.na(VALUE) ~ "OPEN",
                           TRUE ~ VALUE)) %>%
  arrange(FMP, COMMON_NAME_USE, REGION, ZONE_USE, SECTOR_USE, SUBSECTOR_USE, YEAR, start)

# Summarize the number of days open per year
TLF_summ_days_open <- summ_spp_closures %>%
  filter(VALUE == "OPEN") %>%
  group_by(FMP, COMMON_NAME_USE, REGION, ZONE_USE, SECTOR_USE, SUBSECTOR_USE, YEAR) %>%
  summarize(Days_Open = sum(ndays), .groups = "drop") %>%
  arrange(FMP, COMMON_NAME_USE, REGION, ZONE_USE, SECTOR_USE, SUBSECTOR_USE, YEAR)

TLF_com<-TLF_summ_days_open %>% filter(SECTOR_USE=="COMMERCIAL" & ZONE_USE=="ALL")
Com_Open<-cbind(Com_Open,TLF_com$Days_Open)
colnames(Com_Open)<-c("Year","LSN","HOG","DWG","OSWG","SWG","SH","TLF")



################################################################################

# GAG GROUPER (RECREATIONAL)

################################################################################

# Select species and region
spp <- 'GROUPER, GAG'
region <- 'GULF OF MEXICO'

# filter for species and region (all closure related records)
# Recode management types to have a value
mh_spp_closure <- mh_data_log %>%
  filter(COMMON_NAME_USE == spp, REGION == region,
         MANAGEMENT_TYPE_USE %in% c('CLOSURE', 'FISHING SEASON', 'FISHING YEAR',
                                    'PROHIBITED SALE AND PURCHASE',
                                    'PROHIBITED SPECIES')) %>%
  mutate(VALUE = case_when(MANAGEMENT_TYPE_USE %in% c('FISHING SEASON', 'FISHING YEAR') ~ 'OPEN',
                           MANAGEMENT_TYPE_USE == 'PROHIBITED SALE AND PURCHASE' & FLAG == 'YES' ~ 'OPEN',
                           MANAGEMENT_TYPE_USE == 'PROHIBITED SALE AND PURCHASE' & FLAG == 'NO' ~ 'CLOSE',
                           MANAGEMENT_TYPE_USE == 'PROHIBITED SPECIES' ~ 'CLOSE',
                           TRUE ~ VALUE)) %>%
  arrange(SECTOR_USE, START_DATE2)

unique(mh_spp_closure$MANAGEMENT_TYPE_USE)

# Fishing Year
chk <- filter(mh_spp_closure, MANAGEMENT_TYPE_USE == "FISHING YEAR")
spp_year <- expand_status(mh_spp_closure, "FISHING YEAR")

# Closure
unique(select(filter(mh_spp_closure, MANAGEMENT_TYPE_USE == "CLOSURE"), MANAGEMENT_STATUS_USE))
spp_closures <- expand_status(mh_spp_closure, "CLOSURE")

# Combine all management types that refer to closures
# For now just comment out mtypes that do not apply
spp_closure_story <- spp_year %>%
  rename(FR_CITATION_year = "FR_CITATION",
         VALUE_year = "VALUE") %>%
  select(FMP, COMMON_NAME_USE, REGION, ZONE_USE, SECTOR_USE, SUBSECTOR_USE, date_sequence, FR_CITATION_year, VALUE_year) %>%
  # full_join(spp_season %>%
  #             rename(FR_CITATION_season = "FR_CITATION",
  #                    VALUE_season = "VALUE") %>%
  #             select(FMP, COMMON_NAME_USE, REGION, ZONE_USE, SECTOR_USE, SUBSECTOR_USE, date_sequence, FR_CITATION_season, VALUE_season),
  #           by = join_by(FMP, COMMON_NAME_USE, REGION, ZONE_USE, SECTOR_USE, SUBSECTOR_USE,  date_sequence)) %>%
  full_join(spp_closures %>%
              rename(FR_CITATION_close = "FR_CITATION",
                     VALUE_close = "VALUE") %>%
              select(FMP, COMMON_NAME_USE, REGION, ZONE_USE, SECTOR_USE, SUBSECTOR_USE, date_sequence, FR_CITATION_close, VALUE_close),
            by = join_by(FMP, COMMON_NAME_USE, REGION, ZONE_USE, SECTOR_USE, SUBSECTOR_USE,  date_sequence)) %>%
  # full_join(spp_prohibited_sale %>%
  #            rename(FR_CITATION_sale = "FR_CITATION",
  #                    VALUE_sale = "VALUE") %>%
  #             select(FMP, COMMON_NAME_USE, REGION, ZONE_USE, SECTOR_USE, SUBSECTOR_USE, date_sequence, FR_CITATION_sale, VALUE_sale),
  #           by = join_by(FMP, COMMON_NAME_USE, REGION, ZONE_USE, SECTOR_USE, SUBSECTOR_USE,  date_sequence)) %>%
  # full_join(spp_prohibited_spp %>%
  #             rename(FR_CITATION_spp = "FR_CITATION",
  #                    VALUE_spp = "VALUE") %>%
  #             select(FMP, COMMON_NAME_USE, REGION, ZONE_USE, SECTOR_USE, SUBSECTOR_USE, date_sequence, FR_CITATION_spp, VALUE_spp),
  #             by = join_by(FMP, COMMON_NAME_USE, REGION, ZONE_USE, SECTOR_USE, SUBSECTOR_USE, date_sequence)) %>%
  arrange(SECTOR_USE, SUBSECTOR_USE, ZONE_USE, date_sequence) %>%
  # Select the most recent FR CITATION
  mutate(FR_CITATION = pmax(FR_CITATION_year, 
                            #FR_CITATION_season, 
                            FR_CITATION_close, 
                            #                            FR_CITATION_sale, 
                            #                            FR_CITATION_spp,
                            na.rm = T)) %>%
  # Select the fishery status (open/closed) that applies to the most recent FR
  mutate(VALUE = case_when(FR_CITATION == FR_CITATION_close ~ VALUE_close,
                           #                           FR_CITATION == FR_CITATION_sale ~ VALUE_sale,
                           #                          FR_CITATION == FR_CITATION_spp ~ VALUE_spp,
                           #FR_CITATION == FR_CITATION_season ~ VALUE_season,
                           FR_CITATION == FR_CITATION_year ~ VALUE_year)) %>%
  select(-FR_CITATION_year, -VALUE_year, -FR_CITATION_close, -VALUE_close) %>% #, -FR_CITATION_sale, -VALUE_sale, -FR_CITATION_season, -VALUE_season)
 # Changing the time period from 5/2/2024 to 5/31/2024 to CLOSE. Previously, period was defined as OPEN due to time between ineffective date of 88 FR 69553 and effective date of 89 FR 40419
  # Period remained closed to fishing even though effective dates did not overlap
  mutate(VALUE = case_when(SECTOR_USE == "RECREATIONAL" & SUBSECTOR_USE == "ALL" & ZONE_USE == "ALL" &
                             date_sequence >= as.Date("2024-05-02") & date_sequence <= as.Date("2024-05-31") ~ "CLOSE",
                           TRUE ~ VALUE))

# Summarize the open and closed periods for each year
summ_spp_closures <- spp_closure_story %>%
  mutate(YEAR = year(date_sequence)) %>%
  group_by(FMP, COMMON_NAME_USE, REGION, ZONE_USE, SECTOR_USE, SUBSECTOR_USE, YEAR) %>% 
  arrange(FMP, COMMON_NAME_USE, REGION, ZONE_USE, SECTOR_USE, SUBSECTOR_USE, date_sequence) %>%
  mutate(change = VALUE != lag(VALUE, default = first(VALUE)) |
           YEAR != lag(YEAR, default = first(YEAR))) %>%
  mutate(group = cumsum(change)) %>%
  group_by(FMP, COMMON_NAME_USE, REGION, ZONE_USE, SECTOR_USE, SUBSECTOR_USE, YEAR, VALUE, group) %>%
  summarize(ndays = n(),
            start = min(date_sequence),
            end = max(date_sequence),
            .groups = 'drop') %>%
  mutate(VALUE = case_when(is.na(VALUE) ~ "OPEN",
                           TRUE ~ VALUE)) %>%
  arrange(FMP, COMMON_NAME_USE, REGION, ZONE_USE, SECTOR_USE, SUBSECTOR_USE, YEAR, start)

# Summarize the number of days open per year
GAG_summ_days_open <- summ_spp_closures %>%
  filter(VALUE == "OPEN") %>%
  group_by(FMP, COMMON_NAME_USE, REGION, ZONE_USE, SECTOR_USE, SUBSECTOR_USE, YEAR) %>%
  summarize(Days_Open = sum(ndays), .groups = "drop") %>%
  arrange(FMP, COMMON_NAME_USE, REGION, ZONE_USE, SECTOR_USE, SUBSECTOR_USE, YEAR)

GAG_rec<-GAG_summ_days_open %>% filter(SECTOR_USE=="RECREATIONAL" & ZONE_USE=="ALL")
Rec_Open<-cbind(Rec_Open,GAG_rec$Days_Open)
colnames(Rec_Open)<-c("Year","LSN","HOG","DWG","SWG","BGR","GAG")



################################################################################

# RED GROUPER (RECREATIONAL)

################################################################################

# Select species and region
spp <- 'GROUPER, RED'
region <- 'GULF OF MEXICO'

# filter for species and region (all closure related records)
# Recode management types to have a value
mh_spp_closure <- mh_data_log %>%
  filter(COMMON_NAME_USE == spp, REGION == region,
         MANAGEMENT_TYPE_USE %in% c('CLOSURE', 'FISHING SEASON', 'FISHING YEAR',
                                    'PROHIBITED SALE AND PURCHASE',
                                    'PROHIBITED SPECIES')) %>%
  mutate(VALUE = case_when(MANAGEMENT_TYPE_USE %in% c('FISHING SEASON', 'FISHING YEAR') ~ 'OPEN',
                           MANAGEMENT_TYPE_USE == 'PROHIBITED SALE AND PURCHASE' & FLAG == 'YES' ~ 'OPEN',
                           MANAGEMENT_TYPE_USE == 'PROHIBITED SALE AND PURCHASE' & FLAG == 'NO' ~ 'CLOSE',
                           MANAGEMENT_TYPE_USE == 'PROHIBITED SPECIES' ~ 'CLOSE',
                           TRUE ~ VALUE)) %>%
  arrange(SECTOR_USE, START_DATE2)

unique(mh_spp_closure$MANAGEMENT_TYPE_USE)

# Fishing Year
chk <- filter(mh_spp_closure, MANAGEMENT_TYPE_USE == "FISHING YEAR")
spp_year <- expand_status(mh_spp_closure, "FISHING YEAR")

# Closure
unique(select(filter(mh_spp_closure, MANAGEMENT_TYPE_USE == "CLOSURE"), MANAGEMENT_STATUS_USE))
spp_closures <- expand_status(mh_spp_closure, "CLOSURE")

# Combine all management types that refer to closures
# For now just comment out mtypes that do not apply
spp_closure_story <- spp_year %>%
  rename(FR_CITATION_year = "FR_CITATION",
         VALUE_year = "VALUE") %>%
  select(FMP, COMMON_NAME_USE, REGION, ZONE_USE, SECTOR_USE, SUBSECTOR_USE, date_sequence, FR_CITATION_year, VALUE_year) %>%
  # full_join(spp_season %>%
  #             rename(FR_CITATION_season = "FR_CITATION",
  #                    VALUE_season = "VALUE") %>%
  #             select(FMP, COMMON_NAME_USE, REGION, ZONE_USE, SECTOR_USE, SUBSECTOR_USE, date_sequence, FR_CITATION_season, VALUE_season),
  #           by = join_by(FMP, COMMON_NAME_USE, REGION, ZONE_USE, SECTOR_USE, SUBSECTOR_USE,  date_sequence)) %>%
  full_join(spp_closures %>%
              rename(FR_CITATION_close = "FR_CITATION",
                     VALUE_close = "VALUE") %>%
              select(FMP, COMMON_NAME_USE, REGION, ZONE_USE, SECTOR_USE, SUBSECTOR_USE, date_sequence, FR_CITATION_close, VALUE_close),
            by = join_by(FMP, COMMON_NAME_USE, REGION, ZONE_USE, SECTOR_USE, SUBSECTOR_USE,  date_sequence)) %>%
  # full_join(spp_prohibited_sale %>%
  #            rename(FR_CITATION_sale = "FR_CITATION",
  #                    VALUE_sale = "VALUE") %>%
  #             select(FMP, COMMON_NAME_USE, REGION, ZONE_USE, SECTOR_USE, SUBSECTOR_USE, date_sequence, FR_CITATION_sale, VALUE_sale),
  #           by = join_by(FMP, COMMON_NAME_USE, REGION, ZONE_USE, SECTOR_USE, SUBSECTOR_USE,  date_sequence)) %>%
  # full_join(spp_prohibited_spp %>%
  #             rename(FR_CITATION_spp = "FR_CITATION",
  #                    VALUE_spp = "VALUE") %>%
  #             select(FMP, COMMON_NAME_USE, REGION, ZONE_USE, SECTOR_USE, SUBSECTOR_USE, date_sequence, FR_CITATION_spp, VALUE_spp),
  #             by = join_by(FMP, COMMON_NAME_USE, REGION, ZONE_USE, SECTOR_USE, SUBSECTOR_USE, date_sequence)) %>%
  arrange(SECTOR_USE, SUBSECTOR_USE, ZONE_USE, date_sequence) %>%
  # Select the most recent FR CITATION
  mutate(FR_CITATION = pmax(FR_CITATION_year, 
                            #FR_CITATION_season, 
                            FR_CITATION_close, 
                            #                            FR_CITATION_sale, 
                            #                            FR_CITATION_spp,
                            na.rm = T)) %>%
  # Select the fishery status (open/closed) that applies to the most recent FR
  mutate(VALUE = case_when(FR_CITATION == FR_CITATION_close ~ VALUE_close,
                           #                           FR_CITATION == FR_CITATION_sale ~ VALUE_sale,
                           #                          FR_CITATION == FR_CITATION_spp ~ VALUE_spp,
                           #FR_CITATION == FR_CITATION_season ~ VALUE_season,
                           FR_CITATION == FR_CITATION_year ~ VALUE_year)) %>%
  select(-FR_CITATION_year, -VALUE_year, -FR_CITATION_close, -VALUE_close) #, -FR_CITATION_sale, -VALUE_sale, -FR_CITATION_season, -VALUE_season)

# Summarize the open and closed periods for each year
summ_spp_closures <- spp_closure_story %>%
  mutate(YEAR = year(date_sequence)) %>%
  group_by(FMP, COMMON_NAME_USE, REGION, ZONE_USE, SECTOR_USE, SUBSECTOR_USE, YEAR) %>% 
  arrange(FMP, COMMON_NAME_USE, REGION, ZONE_USE, SECTOR_USE, SUBSECTOR_USE, date_sequence) %>%
  mutate(change = VALUE != lag(VALUE, default = first(VALUE)) |
           YEAR != lag(YEAR, default = first(YEAR))) %>%
  mutate(group = cumsum(change)) %>%
  group_by(FMP, COMMON_NAME_USE, REGION, ZONE_USE, SECTOR_USE, SUBSECTOR_USE, YEAR, VALUE, group) %>%
  summarize(ndays = n(),
            start = min(date_sequence),
            end = max(date_sequence),
            .groups = 'drop') %>%
  mutate(VALUE = case_when(is.na(VALUE) ~ "OPEN",
                           TRUE ~ VALUE)) %>%
  arrange(FMP, COMMON_NAME_USE, REGION, ZONE_USE, SECTOR_USE, SUBSECTOR_USE, YEAR, start)

# Summarize the number of days open per year
RGR_summ_days_open <- summ_spp_closures %>%
  filter(VALUE == "OPEN") %>%
  group_by(FMP, COMMON_NAME_USE, REGION, ZONE_USE, SECTOR_USE, SUBSECTOR_USE, YEAR) %>%
  summarize(Days_Open = sum(ndays), .groups = "drop") %>%
  arrange(FMP, COMMON_NAME_USE, REGION, ZONE_USE, SECTOR_USE, SUBSECTOR_USE, YEAR)

RGR_rec<-RGR_summ_days_open %>% filter(SECTOR_USE=="RECREATIONAL" & ZONE_USE=="ALL")
Rec_Open<-cbind(Rec_Open,RGR_rec$Days_Open)
colnames(Rec_Open)<-c("Year","LSN","HOG","DWG","SWG","BGR","GAG","RGR")



################################################################################

# GRAY TRIGGERFISH (COMMERCIAL AND RECREATIONAL)

################################################################################

# Select species and region
spp <- 'TRIGGERFISH, GRAY'
region <- 'GULF OF MEXICO'

# filter for species and region (all closure related records)
# Recode management types to have a value
mh_spp_closure <- mh_data_log %>%
  filter(COMMON_NAME_USE == spp, REGION == region,
         MANAGEMENT_TYPE_USE %in% c('CLOSURE', 'FISHING SEASON', 'FISHING YEAR',
                                    'PROHIBITED SALE AND PURCHASE',
                                    'PROHIBITED SPECIES')) %>%
  mutate(VALUE = case_when(MANAGEMENT_TYPE_USE %in% c('FISHING SEASON', 'FISHING YEAR') ~ 'OPEN',
                           MANAGEMENT_TYPE_USE == 'PROHIBITED SALE AND PURCHASE' & FLAG == 'YES' ~ 'OPEN',
                           MANAGEMENT_TYPE_USE == 'PROHIBITED SALE AND PURCHASE' & FLAG == 'NO' ~ 'CLOSE',
                           MANAGEMENT_TYPE_USE == 'PROHIBITED SPECIES' ~ 'CLOSE',
                           TRUE ~ VALUE)) %>%
  arrange(SECTOR_USE, START_DATE2)

unique(mh_spp_closure$MANAGEMENT_TYPE_USE)

# Fishing Year
chk <- filter(mh_spp_closure, MANAGEMENT_TYPE_USE == "FISHING YEAR")
spp_year <- expand_status(mh_spp_closure, "FISHING YEAR")

# Closure
unique(select(filter(mh_spp_closure, MANAGEMENT_TYPE_USE == "CLOSURE"), MANAGEMENT_STATUS_USE))
spp_closures <- expand_status(mh_spp_closure, "CLOSURE")

# Combine all management types that refer to closures
# For now just comment out mtypes that do not apply
spp_closure_story <- spp_year %>%
  rename(FR_CITATION_year = "FR_CITATION",
         VALUE_year = "VALUE") %>%
  select(FMP, COMMON_NAME_USE, REGION, ZONE_USE, SECTOR_USE, SUBSECTOR_USE, date_sequence, FR_CITATION_year, VALUE_year) %>%
  # full_join(spp_season %>%
  #             rename(FR_CITATION_season = "FR_CITATION",
  #                    VALUE_season = "VALUE") %>%
  #             select(FMP, COMMON_NAME_USE, REGION, ZONE_USE, SECTOR_USE, SUBSECTOR_USE, date_sequence, FR_CITATION_season, VALUE_season),
  #           by = join_by(FMP, COMMON_NAME_USE, REGION, ZONE_USE, SECTOR_USE, SUBSECTOR_USE,  date_sequence)) %>%
  full_join(spp_closures %>%
              rename(FR_CITATION_close = "FR_CITATION",
                     VALUE_close = "VALUE") %>%
              select(FMP, COMMON_NAME_USE, REGION, ZONE_USE, SECTOR_USE, SUBSECTOR_USE, date_sequence, FR_CITATION_close, VALUE_close),
            by = join_by(FMP, COMMON_NAME_USE, REGION, ZONE_USE, SECTOR_USE, SUBSECTOR_USE,  date_sequence)) %>%
  # full_join(spp_prohibited_sale %>%
  #            rename(FR_CITATION_sale = "FR_CITATION",
  #                    VALUE_sale = "VALUE") %>%
  #             select(FMP, COMMON_NAME_USE, REGION, ZONE_USE, SECTOR_USE, SUBSECTOR_USE, date_sequence, FR_CITATION_sale, VALUE_sale),
  #           by = join_by(FMP, COMMON_NAME_USE, REGION, ZONE_USE, SECTOR_USE, SUBSECTOR_USE,  date_sequence)) %>%
  # full_join(spp_prohibited_spp %>%
  #             rename(FR_CITATION_spp = "FR_CITATION",
  #                    VALUE_spp = "VALUE") %>%
  #             select(FMP, COMMON_NAME_USE, REGION, ZONE_USE, SECTOR_USE, SUBSECTOR_USE, date_sequence, FR_CITATION_spp, VALUE_spp),
  #             by = join_by(FMP, COMMON_NAME_USE, REGION, ZONE_USE, SECTOR_USE, SUBSECTOR_USE, date_sequence)) %>%
  arrange(SECTOR_USE, SUBSECTOR_USE, ZONE_USE, date_sequence) %>%
  # Select the most recent FR CITATION
  mutate(FR_CITATION = pmax(FR_CITATION_year, 
                            #FR_CITATION_season, 
                            FR_CITATION_close, 
                            #                            FR_CITATION_sale, 
                            #                            FR_CITATION_spp,
                            na.rm = T)) %>%
  # Select the fishery status (open/closed) that applies to the most recent FR
  mutate(VALUE = case_when(FR_CITATION == FR_CITATION_close ~ VALUE_close,
                           #                           FR_CITATION == FR_CITATION_sale ~ VALUE_sale,
                           #                          FR_CITATION == FR_CITATION_spp ~ VALUE_spp,
                           #FR_CITATION == FR_CITATION_season ~ VALUE_season,
                           FR_CITATION == FR_CITATION_year ~ VALUE_year)) %>%
  select(-FR_CITATION_year, -VALUE_year, -FR_CITATION_close, -VALUE_close) #, -FR_CITATION_sale, -VALUE_sale, -FR_CITATION_season, -VALUE_season)

# Summarize the open and closed periods for each year
summ_spp_closures <- spp_closure_story %>%
  mutate(YEAR = year(date_sequence)) %>%
  group_by(FMP, COMMON_NAME_USE, REGION, ZONE_USE, SECTOR_USE, SUBSECTOR_USE, YEAR) %>% 
  arrange(FMP, COMMON_NAME_USE, REGION, ZONE_USE, SECTOR_USE, SUBSECTOR_USE, date_sequence) %>%
  mutate(change = VALUE != lag(VALUE, default = first(VALUE)) |
           YEAR != lag(YEAR, default = first(YEAR))) %>%
  mutate(group = cumsum(change)) %>%
  group_by(FMP, COMMON_NAME_USE, REGION, ZONE_USE, SECTOR_USE, SUBSECTOR_USE, YEAR, VALUE, group) %>%
  summarize(ndays = n(),
            start = min(date_sequence),
            end = max(date_sequence),
            .groups = 'drop') %>%
  mutate(VALUE = case_when(is.na(VALUE) ~ "OPEN",
                           TRUE ~ VALUE)) %>%
  arrange(FMP, COMMON_NAME_USE, REGION, ZONE_USE, SECTOR_USE, SUBSECTOR_USE, YEAR, start)

# Identify full range of years per group
GTR_all_years_by_group <- summ_spp_closures %>%
  distinct(FMP, COMMON_NAME_USE, REGION, ZONE_USE, SECTOR_USE, SUBSECTOR_USE, YEAR, VALUE) %>%
  group_by(FMP, COMMON_NAME_USE, REGION, ZONE_USE, SECTOR_USE, SUBSECTOR_USE) %>%
  summarize(min_year = min(YEAR), max_year = max(YEAR), .groups = "drop") %>%
  rowwise() %>%
  mutate(YEAR = list(seq(min_year, max_year))) %>%
  unnest(YEAR) %>%
  select(-min_year, -max_year)

# Summarize the number of days open per year
GTR_summ_days_open <- summ_spp_closures %>%
  filter(VALUE == "OPEN") %>%
  group_by(FMP, COMMON_NAME_USE, REGION, ZONE_USE, SECTOR_USE, SUBSECTOR_USE, YEAR) %>%
  summarize(Days_Open = sum(ndays), .groups = "drop") %>%
  arrange(FMP, COMMON_NAME_USE, REGION, ZONE_USE, SECTOR_USE, SUBSECTOR_USE, YEAR)

GTR_open_by_year <- GTR_all_years_by_group %>%
  left_join(GTR_summ_days_open, by = c("FMP", "COMMON_NAME_USE", "REGION", "ZONE_USE", "SECTOR_USE", "SUBSECTOR_USE", "YEAR")) %>%
  mutate(Days_Open = replace_na(Days_Open, 0)) %>%
  arrange(FMP, COMMON_NAME_USE, REGION, ZONE_USE, SECTOR_USE, SUBSECTOR_USE, YEAR)

GTR_com<-GTR_summ_days_open %>% filter(SECTOR_USE=="COMMERCIAL" & ZONE_USE=="ALL")
Com_Open<-cbind(Com_Open,GTR_com$Days_Open)
colnames(Com_Open)<-c("Year","LSN","HOG","DWG","OSWG","SWG","SH","TLF","GTR")

GTR_rec<-GTR_open_by_year %>% filter(SECTOR_USE=="RECREATIONAL" & ZONE_USE=="ALL")
Rec_Open<-cbind(Rec_Open,GTR_rec$Days_Open)
colnames(Rec_Open)<-c("Year","LSN","HOG","DWG","SWG","BGR","GAG","RGR","GTR")



################################################################################

# VERMILION SNAPPER (COMMERCIAL)

################################################################################

# Select species and region
spp <- 'SNAPPER, VERMILION'
region <- 'GULF OF MEXICO'

# filter for species and region (all closure related records)
# Recode management types to have a value
mh_spp_closure <- mh_data_log %>%
  filter(COMMON_NAME_USE == spp, REGION == region,
         MANAGEMENT_TYPE_USE %in% c('CLOSURE', 'FISHING SEASON', 'FISHING YEAR',
                                    'PROHIBITED SALE AND PURCHASE',
                                    'PROHIBITED SPECIES')) %>%
  mutate(VALUE = case_when(MANAGEMENT_TYPE_USE %in% c('FISHING SEASON', 'FISHING YEAR') ~ 'OPEN',
                           MANAGEMENT_TYPE_USE == 'PROHIBITED SALE AND PURCHASE' & FLAG == 'YES' ~ 'OPEN',
                           MANAGEMENT_TYPE_USE == 'PROHIBITED SALE AND PURCHASE' & FLAG == 'NO' ~ 'CLOSE',
                           MANAGEMENT_TYPE_USE == 'PROHIBITED SPECIES' ~ 'CLOSE',
                           TRUE ~ VALUE)) %>%
  arrange(SECTOR_USE, START_DATE2)

unique(mh_spp_closure$MANAGEMENT_TYPE_USE)

# Fishing Year
chk <- filter(mh_spp_closure, MANAGEMENT_TYPE_USE == "FISHING YEAR")
spp_year <- expand_status(mh_spp_closure, "FISHING YEAR")

# Closure
unique(select(filter(mh_spp_closure, MANAGEMENT_TYPE_USE == "CLOSURE"), MANAGEMENT_STATUS_USE))
spp_closures <- expand_status(mh_spp_closure, "CLOSURE")

# Combine all management types that refer to closures
# For now just comment out mtypes that do not apply
spp_closure_story <- spp_year %>%
  rename(FR_CITATION_year = "FR_CITATION",
         VALUE_year = "VALUE") %>%
  select(FMP, COMMON_NAME_USE, REGION, ZONE_USE, SECTOR_USE, SUBSECTOR_USE, date_sequence, FR_CITATION_year, VALUE_year) %>%
  # full_join(spp_season %>%
  #             rename(FR_CITATION_season = "FR_CITATION",
  #                    VALUE_season = "VALUE") %>%
  #             select(FMP, COMMON_NAME_USE, REGION, ZONE_USE, SECTOR_USE, SUBSECTOR_USE, date_sequence, FR_CITATION_season, VALUE_season),
  #           by = join_by(FMP, COMMON_NAME_USE, REGION, ZONE_USE, SECTOR_USE, SUBSECTOR_USE,  date_sequence)) %>%
  full_join(spp_closures %>%
              rename(FR_CITATION_close = "FR_CITATION",
                     VALUE_close = "VALUE") %>%
              select(FMP, COMMON_NAME_USE, REGION, ZONE_USE, SECTOR_USE, SUBSECTOR_USE, date_sequence, FR_CITATION_close, VALUE_close),
            by = join_by(FMP, COMMON_NAME_USE, REGION, ZONE_USE, SECTOR_USE, SUBSECTOR_USE,  date_sequence)) %>%
  # full_join(spp_prohibited_sale %>%
  #            rename(FR_CITATION_sale = "FR_CITATION",
  #                    VALUE_sale = "VALUE") %>%
  #             select(FMP, COMMON_NAME_USE, REGION, ZONE_USE, SECTOR_USE, SUBSECTOR_USE, date_sequence, FR_CITATION_sale, VALUE_sale),
  #           by = join_by(FMP, COMMON_NAME_USE, REGION, ZONE_USE, SECTOR_USE, SUBSECTOR_USE,  date_sequence)) %>%
  # full_join(spp_prohibited_spp %>%
  #             rename(FR_CITATION_spp = "FR_CITATION",
  #                    VALUE_spp = "VALUE") %>%
  #             select(FMP, COMMON_NAME_USE, REGION, ZONE_USE, SECTOR_USE, SUBSECTOR_USE, date_sequence, FR_CITATION_spp, VALUE_spp),
  #             by = join_by(FMP, COMMON_NAME_USE, REGION, ZONE_USE, SECTOR_USE, SUBSECTOR_USE, date_sequence)) %>%
  arrange(SECTOR_USE, SUBSECTOR_USE, ZONE_USE, date_sequence) %>%
  # Select the most recent FR CITATION
  mutate(FR_CITATION = pmax(FR_CITATION_year, 
                            #FR_CITATION_season, 
                            FR_CITATION_close, 
                            #                            FR_CITATION_sale, 
                            #                            FR_CITATION_spp,
                            na.rm = T)) %>%
  # Select the fishery status (open/closed) that applies to the most recent FR
  mutate(VALUE = case_when(FR_CITATION == FR_CITATION_close ~ VALUE_close,
                           #                           FR_CITATION == FR_CITATION_sale ~ VALUE_sale,
                           #                          FR_CITATION == FR_CITATION_spp ~ VALUE_spp,
                           #FR_CITATION == FR_CITATION_season ~ VALUE_season,
                           FR_CITATION == FR_CITATION_year ~ VALUE_year)) %>%
  select(-FR_CITATION_year, -VALUE_year, -FR_CITATION_close, -VALUE_close) #, -FR_CITATION_sale, -VALUE_sale, -FR_CITATION_season, -VALUE_season)

# Summarize the open and closed periods for each year
summ_spp_closures <- spp_closure_story %>%
  mutate(YEAR = year(date_sequence)) %>%
  group_by(FMP, COMMON_NAME_USE, REGION, ZONE_USE, SECTOR_USE, SUBSECTOR_USE, YEAR) %>% 
  arrange(FMP, COMMON_NAME_USE, REGION, ZONE_USE, SECTOR_USE, SUBSECTOR_USE, date_sequence) %>%
  mutate(change = VALUE != lag(VALUE, default = first(VALUE)) |
           YEAR != lag(YEAR, default = first(YEAR))) %>%
  mutate(group = cumsum(change)) %>%
  group_by(FMP, COMMON_NAME_USE, REGION, ZONE_USE, SECTOR_USE, SUBSECTOR_USE, YEAR, VALUE, group) %>%
  summarize(ndays = n(),
            start = min(date_sequence),
            end = max(date_sequence),
            .groups = 'drop') %>%
  mutate(VALUE = case_when(is.na(VALUE) ~ "OPEN",
                           TRUE ~ VALUE)) %>%
  arrange(FMP, COMMON_NAME_USE, REGION, ZONE_USE, SECTOR_USE, SUBSECTOR_USE, YEAR, start)

# Summarize the number of days open per year
VSN_summ_days_open <- summ_spp_closures %>%
  filter(VALUE == "OPEN") %>%
  group_by(FMP, COMMON_NAME_USE, REGION, ZONE_USE, SECTOR_USE, SUBSECTOR_USE, YEAR) %>%
  summarize(Days_Open = sum(ndays), .groups = "drop") %>%
  arrange(FMP, COMMON_NAME_USE, REGION, ZONE_USE, SECTOR_USE, SUBSECTOR_USE, YEAR)

VSN_com<-VSN_summ_days_open %>% filter(SECTOR_USE=="COMMERCIAL" & ZONE_USE=="ALL")
Com_Open<-cbind(Com_Open,VSN_com$Days_Open)
colnames(Com_Open)<-c("Year","LSN","HOG","DWG","OSWG","SWG","SH","TLF","GTR","VSN")



################################################################################

# RED SNAPPER

################################################################################

# Select species and region
spp <- 'SNAPPER, RED'
region <- 'GULF OF MEXICO'

# filter for species and region (all closure related records)
# Recode management types to have a value
mh_spp_closure <- mh_data_log %>%
  filter(COMMON_NAME_USE == spp, REGION == region,
         MANAGEMENT_TYPE_USE %in% c('CLOSURE', 'FISHING SEASON', 'FISHING YEAR',
                                    'PROHIBITED SALE AND PURCHASE',
                                    'PROHIBITED SPECIES')) %>%
  mutate(VALUE = case_when(MANAGEMENT_TYPE_USE %in% c('FISHING SEASON', 'FISHING YEAR') ~ 'OPEN',
                           MANAGEMENT_TYPE_USE == 'PROHIBITED SALE AND PURCHASE' & FLAG == 'YES' ~ 'OPEN',
                           MANAGEMENT_TYPE_USE == 'PROHIBITED SALE AND PURCHASE' & FLAG == 'NO' ~ 'CLOSE',
                           MANAGEMENT_TYPE_USE == 'PROHIBITED SPECIES' ~ 'CLOSE',
                           TRUE ~ VALUE)) 

recreational_fishing_year_forhire <- mh_spp_closure %>%
  filter(MANAGEMENT_TYPE_USE == "FISHING YEAR", SECTOR_USE == "RECREATIONAL") %>%
  mutate(SUBSECTOR_USE = "FOR-HIRE")

recreational_fishing_year_private <- mh_spp_closure %>%
  filter(MANAGEMENT_TYPE_USE == "FISHING YEAR", SECTOR_USE == "RECREATIONAL") %>%
  mutate(SUBSECTOR_USE = "PRIVATE")

recreational_fishing_season_forhire <- mh_spp_closure %>%
  filter(MANAGEMENT_TYPE_USE == "FISHING SEASON", SECTOR_USE == "RECREATIONAL") %>%
  mutate(SUBSECTOR_USE = "FOR-HIRE")

recreational_fishing_season_private <- mh_spp_closure %>%
  filter(MANAGEMENT_TYPE_USE == "FISHING SEASON", SECTOR_USE == "RECREATIONAL") %>%
  mutate(SUBSECTOR_USE = "PRIVATE")

mh_spp_closure_cleaned <- mh_spp_closure %>%
  filter(!(MANAGEMENT_TYPE_USE == "FISHING YEAR" & SECTOR_USE == "RECREATIONAL" & SUBSECTOR_USE == "ALL")) %>%
  filter(!(MANAGEMENT_TYPE_USE == "FISHING SEASON" & SECTOR_USE == "RECREATIONAL" & SUBSECTOR_USE == "ALL"))

mh_spp_closure_final <- bind_rows(mh_spp_closure_cleaned, recreational_fishing_year_forhire, recreational_fishing_year_private,
                                  recreational_fishing_season_forhire, recreational_fishing_season_private) %>%
  arrange(SECTOR_USE, START_DATE2)

unique(mh_spp_closure$MANAGEMENT_TYPE_USE)

# Fishing Year
chk <- filter(mh_spp_closure_final, MANAGEMENT_TYPE_USE == "FISHING YEAR")
spp_year <- expand_status(mh_spp_closure_final, "FISHING YEAR")

# Fishing Season - still needs some work if not entered consistently
chk <- filter(mh_spp_closure_final, MANAGEMENT_TYPE_USE == "FISHING SEASON")
spp_season <- expand_status(mh_spp_closure_final, "FISHING SEASON")

# Closure
unique(select(filter(mh_spp_closure_final, MANAGEMENT_TYPE_USE == "CLOSURE"), MANAGEMENT_STATUS_USE))
spp_closures <- expand_status(mh_spp_closure_final, "CLOSURE")

# # Prohibited sale and purchase
# unique(select(filter(mh_spp_closure, MANAGEMENT_TYPE_USE == "PROHIBITED SALE AND PURCHASE"), MANAGEMENT_STATUS_USE))
# spp_prohibited_sale <- expand_status(mh_spp_closure, "PROHIBITED SALE AND PURCHASE")
# 
# # Prohibited species
# unique(select(filter(mh_spp_closure, MANAGEMENT_TYPE_USE == "PROHIBITED SPECIES"), MANAGEMENT_STATUS_USE))
# spp_prohibited_spp <- expand_status(mh_spp_closure, "PROHIBITED SPECIES")

# Combine all management types that refer to closures
# For now just comment out mtypes that do not apply
spp_closure_story <- spp_year %>%
  rename(FR_CITATION_year = "FR_CITATION",
         TIME_year = "TIME",
         VALUE_year = "VALUE") %>%
  select(FMP, COMMON_NAME_USE, REGION, ZONE_USE, SECTOR_USE, SUBSECTOR_USE, date_sequence, FR_CITATION_year, TIME_year, VALUE_year) %>%
  full_join(spp_season %>%
              rename(FR_CITATION_season = "FR_CITATION",
                     TIME_season = "TIME",
                     VALUE_season = "VALUE") %>%
              select(FMP, COMMON_NAME_USE, REGION, ZONE_USE, SECTOR_USE, SUBSECTOR_USE, date_sequence, FR_CITATION_season, TIME_season, VALUE_season),
            by = join_by(FMP, COMMON_NAME_USE, REGION, ZONE_USE, SECTOR_USE, SUBSECTOR_USE,  date_sequence)) %>%
  full_join(spp_closures %>%
              rename(FR_CITATION_close = "FR_CITATION",
                     TIME_close = "TIME",
                     VALUE_close = "VALUE") %>%
              select(FMP, COMMON_NAME_USE, REGION, ZONE_USE, SECTOR_USE, SUBSECTOR_USE, date_sequence, FR_CITATION_close, TIME_close, VALUE_close),
            by = join_by(FMP, COMMON_NAME_USE, REGION, ZONE_USE, SECTOR_USE, SUBSECTOR_USE,  date_sequence)) %>%
  # full_join(spp_prohibited_sale %>%
  #          rename(FR_CITATION_sale = "FR_CITATION",
  #                TIME_sale = "TIME", 
  #                 VALUE_sale = "VALUE") %>%
  #         select(FMP, COMMON_NAME_USE, REGION, ZONE_USE, SECTOR_USE, SUBSECTOR_USE, date_sequence, FR_CITATION_sale, VALUE_sale),
  #      by = join_by(FMP, COMMON_NAME_USE, REGION, ZONE_USE, SECTOR_USE, SUBSECTOR_USE,  date_sequence)) %>%
  # full_join(spp_prohibited_spp %>%
  #            rename(FR_CITATION_spp = "FR_CITATION",
  #                   TIME_spp = "TIME",
  #                  VALUE_spp = "VALUE") %>%
  #          select(FMP, COMMON_NAME_USE, REGION, ZONE_USE, SECTOR_USE, SUBSECTOR_USE, date_sequence, FR_CITATION_spp, VALUE_spp),
#         by = join_by(FMP, COMMON_NAME_USE, REGION, ZONE_USE, SECTOR_USE, SUBSECTOR_USE, date_sequence)) %>%
arrange(SECTOR_USE, SUBSECTOR_USE, ZONE_USE, date_sequence) %>%
  # Select the most recent FR CITATION
  mutate(FR_CITATION = pmax(FR_CITATION_year, 
                            FR_CITATION_season, 
                            FR_CITATION_close, 
                            # FR_CITATION_sale, 
                            #FR_CITATION_spp,
                            na.rm = T),
         TIME = case_when(FR_CITATION == FR_CITATION_close ~ TIME_close,
                          #FR_CITATION == FR_CITATION_sale ~ TIME_sale,
                          #FR_CITATION == FR_CITATION_spp ~ TIME_spp,
                          FR_CITATION == FR_CITATION_season ~ TIME_season,
                          FR_CITATION == FR_CITATION_year ~ TIME_year)) %>%
  # Select the fishery status (open/closed) that applies to the most recent FR
  mutate(VALUE = case_when(FR_CITATION == FR_CITATION_close ~ VALUE_close,
                           #FR_CITATION == FR_CITATION_sale ~ VALUE_sale,
                           #FR_CITATION == FR_CITATION_spp ~ VALUE_spp,
                           FR_CITATION == FR_CITATION_season ~ VALUE_season,
                           FR_CITATION == FR_CITATION_year ~ VALUE_year)) %>%
  select(-FR_CITATION_year, -TIME_year, -VALUE_year, -FR_CITATION_close, -TIME_close, -VALUE_close, -FR_CITATION_season, -TIME_season, -VALUE_season) %>%
  # Create day_calc to indicate cases where the start/end time is midday 
  # midday start/end times should be counted toward both open and closed periods
  mutate(day_calc = case_when(TIME == "06:00:00 PM" ~ 0.5,
                              TIME == "12:00:00 PM" ~ 0.5,
                              TRUE ~ 1))

# After expanding dates and filling missing values
spp_closure_story <- spp_closure_story %>%
  group_by(FMP, COMMON_NAME_USE, REGION, ZONE_USE, SECTOR_USE, SUBSECTOR_USE) %>%
  complete(date_sequence = seq(min(date_sequence, na.rm = TRUE), max(date_sequence, na.rm = TRUE), by = "1 day")) %>%
  fill(FMP, COMMON_NAME_USE, REGION, ZONE_USE, SECTOR_USE, SUBSECTOR_USE, .direction = "downup") %>%
  mutate(VALUE = if_else(is.na(VALUE), "OPEN", VALUE),
         TIME = if_else(is.na(TIME), NA_character_, TIME),
         day_calc = if_else(is.na(day_calc), 1, day_calc)) %>%
  ungroup()

# Summarize the open and closed periods for each year
summ_spp_closures <- spp_closure_story %>%
  mutate(YEAR = year(date_sequence),
         VALUE = as.character(VALUE),
         VALUE = if_else(is.na(VALUE), "OPEN", VALUE)) %>%
  filter(VALUE %in% c("OPEN", "CLOSE")) %>%
  filter(!(FR_CITATION == "77 FR 31734" & date_sequence == as.Date("2012-07-11"))) %>%
  filter(!(FR_CITATION == "77 FR 31734" & date_sequence == as.Date("2012-07-12"))) %>%
  filter(!(FR_CITATION == "77 FR 31734" & date_sequence == as.Date("2012-07-13"))) %>%
  filter(!(FR_CITATION == "77 FR 31734" & date_sequence == as.Date("2012-07-14"))) %>%
  filter(!(FR_CITATION == "77 FR 31734" & date_sequence == as.Date("2012-07-15"))) %>%
  filter(!(FR_CITATION == "77 FR 31734" & date_sequence == as.Date("2012-07-16")))

summ_spp_closures2 <- summ_spp_closures %>%
  filter(day_calc == 0.5) %>%
  mutate(VALUE = if_else(VALUE == "OPEN", "CLOSE", "OPEN"))

spp_closure_combined <- bind_rows(summ_spp_closures, summ_spp_closures2)

spp_close_sum <- spp_closure_combined %>%
  group_by(FMP, COMMON_NAME_USE, REGION, ZONE_USE, SECTOR_USE, SUBSECTOR_USE, YEAR) %>% 
  arrange(FMP, COMMON_NAME_USE, REGION, ZONE_USE, SECTOR_USE, SUBSECTOR_USE, date_sequence, TIME) %>%
  mutate(change = VALUE != lag(VALUE, default = first(VALUE)) |
           YEAR != lag(YEAR, default = first(YEAR))) %>%
  mutate(group = cumsum(change)) %>%
  group_by(FMP, COMMON_NAME_USE, REGION, ZONE_USE, SECTOR_USE, SUBSECTOR_USE, YEAR, VALUE, group) %>%
  summarize(ndays = sum(day_calc, na.rm = TRUE),
            start = min(date_sequence),
            end = max(date_sequence),
            .groups = 'drop') %>%
  arrange(FMP, COMMON_NAME_USE, REGION, ZONE_USE, SECTOR_USE, SUBSECTOR_USE, YEAR, start)

### old
# Summarize the open and closed periods for each year - old
 # mutate(YEAR = year(date_sequence)) %>%
#  group_by(FMP, COMMON_NAME_USE, REGION, ZONE_USE, SECTOR_USE, SUBSECTOR_USE, YEAR) %>% 
#  arrange(FMP, COMMON_NAME_USE, REGION, ZONE_USE, SECTOR_USE, SUBSECTOR_USE, date_sequence) %>%
#  mutate(change = VALUE != lag(VALUE, default = first(VALUE)) |
#           YEAR != lag(YEAR, default = first(YEAR))) %>%
#  mutate(group = cumsum(change)) %>%
#  group_by(FMP, COMMON_NAME_USE, REGION, ZONE_USE, SECTOR_USE, SUBSECTOR_USE, YEAR, VALUE, group) %>%
#  summarize(ndays = n(),
#            start = min(date_sequence),
#            end = max(date_sequence),
#            .groups = 'drop') %>%
#  mutate(VALUE = case_when(is.na(VALUE) ~ "OPEN",
#                           TRUE ~ VALUE)) %>%
#  arrange(FMP, COMMON_NAME_USE, REGION, ZONE_USE, SECTOR_USE, SUBSECTOR_USE, YEAR, start)

all_years_by_group <- spp_close_sum %>%
  distinct(FMP, COMMON_NAME_USE, REGION, ZONE_USE, SECTOR_USE, SUBSECTOR_USE, YEAR, VALUE) %>%
  group_by(FMP, COMMON_NAME_USE, REGION, ZONE_USE, SECTOR_USE, SUBSECTOR_USE) %>%
  summarize(min_year = min(YEAR), max_year = max(YEAR), .groups = "drop") %>%
  rowwise() %>%
  mutate(YEAR = list(seq(min_year, max_year))) %>%
  unnest(YEAR) %>%
  select(-min_year, -max_year)

# Summarize the number of days open per year
summ_days_open <- spp_close_sum %>%
  filter(VALUE == "OPEN") %>%
  group_by(FMP, COMMON_NAME_USE, REGION, ZONE_USE, SECTOR_USE, SUBSECTOR_USE, YEAR) %>%
  summarize(Days_Open = sum(ndays), .groups = "drop") %>%
  arrange(FMP, COMMON_NAME_USE, REGION, ZONE_USE, SECTOR_USE, SUBSECTOR_USE, YEAR)

RSN_summ_days_open <- all_years_by_group %>%
  left_join(summ_days_open, by = c("FMP", "COMMON_NAME_USE", "REGION", "ZONE_USE", "SECTOR_USE", "SUBSECTOR_USE", "YEAR")) %>%
  mutate(Days_Open = replace_na(Days_Open, 0)) %>%
  arrange(FMP, COMMON_NAME_USE, REGION, ZONE_USE, SECTOR_USE, SUBSECTOR_USE, YEAR)

RSN_com<-RSN_summ_days_open %>% filter(SECTOR_USE=="COMMERCIAL" & ZONE_USE=="ALL")
Com_Open<-cbind(Com_Open,RSN_com$Days_Open)
colnames(Com_Open)<-c("Year","LSN","HOG","DWG","OSWG","SWG","SH","TLF","GTR","VSN","RSN")

RSN_rec<-RSN_summ_days_open %>% filter(SECTOR_USE=="RECREATIONAL" & SUBSECTOR_USE=="FOR-HIRE" & ZONE_USE=="ALL")
Rec_Open<-cbind(Rec_Open,RSN_rec$Days_Open)
colnames(Rec_Open)<-c("Year","LSN","HOG","DWG","SWG","BGR","GAG","RGR","GTR","RSN")



################################################################################

# GREATER AMBERJACK

################################################################################

# Select species and region
spp <- 'AMBERJACK, GREATER'
region <- 'GULF OF MEXICO'

# filter for species and region (all closure related records)
# Recode management types to have a value
mh_spp_closure <- mh_data_log %>%
  filter(COMMON_NAME_USE == spp, REGION == region,
         MANAGEMENT_TYPE_USE %in% c('CLOSURE', 'FISHING SEASON', 'FISHING YEAR',
                                    'PROHIBITED SALE AND PURCHASE',
                                    'PROHIBITED SPECIES')) %>%
  mutate(VALUE = case_when(MANAGEMENT_TYPE_USE %in% c('FISHING SEASON', 'FISHING YEAR') ~ 'OPEN',
                           MANAGEMENT_TYPE_USE == 'PROHIBITED SALE AND PURCHASE' & FLAG == 'YES' ~ 'OPEN',
                           MANAGEMENT_TYPE_USE == 'PROHIBITED SALE AND PURCHASE' & FLAG == 'NO' ~ 'CLOSE',
                           MANAGEMENT_TYPE_USE == 'PROHIBITED SPECIES' ~ 'CLOSE',
                           TRUE ~ VALUE)) %>%
  arrange(SECTOR_USE, START_DATE2)

unique(mh_spp_closure$MANAGEMENT_TYPE_USE)

# Fishing Year
chk <- filter(mh_spp_closure, MANAGEMENT_TYPE_USE == "FISHING YEAR")
spp_year <- expand_status(mh_spp_closure, "FISHING YEAR")

# Closure
unique(select(filter(mh_spp_closure, MANAGEMENT_TYPE_USE == "CLOSURE"), MANAGEMENT_STATUS_USE))
spp_closures <- expand_status(mh_spp_closure, "CLOSURE")

# Combine all management types that refer to closures
# For now just comment out mtypes that do not apply
spp_closure_story <- spp_year %>%
  rename(FR_CITATION_year = "FR_CITATION",
         VALUE_year = "VALUE") %>%
  select(FMP, COMMON_NAME_USE, REGION, ZONE_USE, SECTOR_USE, SUBSECTOR_USE, date_sequence, FR_CITATION_year, VALUE_year) %>%
  # full_join(spp_season %>%
  #             rename(FR_CITATION_season = "FR_CITATION",
  #                    VALUE_season = "VALUE") %>%
  #             select(FMP, COMMON_NAME_USE, REGION, ZONE_USE, SECTOR_USE, SUBSECTOR_USE, date_sequence, FR_CITATION_season, VALUE_season),
  #           by = join_by(FMP, COMMON_NAME_USE, REGION, ZONE_USE, SECTOR_USE, SUBSECTOR_USE,  date_sequence)) %>%
  full_join(spp_closures %>%
              rename(FR_CITATION_close = "FR_CITATION",
                     VALUE_close = "VALUE") %>%
              select(FMP, COMMON_NAME_USE, REGION, ZONE_USE, SECTOR_USE, SUBSECTOR_USE, date_sequence, FR_CITATION_close, VALUE_close),
            by = join_by(FMP, COMMON_NAME_USE, REGION, ZONE_USE, SECTOR_USE, SUBSECTOR_USE,  date_sequence)) %>%
  # full_join(spp_prohibited_sale %>%
  #            rename(FR_CITATION_sale = "FR_CITATION",
  #                    VALUE_sale = "VALUE") %>%
  #             select(FMP, COMMON_NAME_USE, REGION, ZONE_USE, SECTOR_USE, SUBSECTOR_USE, date_sequence, FR_CITATION_sale, VALUE_sale),
  #           by = join_by(FMP, COMMON_NAME_USE, REGION, ZONE_USE, SECTOR_USE, SUBSECTOR_USE,  date_sequence)) %>%
  # full_join(spp_prohibited_spp %>%
  #             rename(FR_CITATION_spp = "FR_CITATION",
  #                    VALUE_spp = "VALUE") %>%
  #             select(FMP, COMMON_NAME_USE, REGION, ZONE_USE, SECTOR_USE, SUBSECTOR_USE, date_sequence, FR_CITATION_spp, VALUE_spp),
  #             by = join_by(FMP, COMMON_NAME_USE, REGION, ZONE_USE, SECTOR_USE, SUBSECTOR_USE, date_sequence)) %>%
  arrange(SECTOR_USE, SUBSECTOR_USE, ZONE_USE, date_sequence) %>%
  # Select the most recent FR CITATION
  mutate(FR_CITATION = pmax(FR_CITATION_year, 
                            #FR_CITATION_season, 
                            FR_CITATION_close, 
                            #                            FR_CITATION_sale, 
                            #                            FR_CITATION_spp,
                            na.rm = T)) %>%
  # Select the fishery status (open/closed) that applies to the most recent FR
  mutate(VALUE = case_when(FR_CITATION == FR_CITATION_close ~ VALUE_close,
                           #                           FR_CITATION == FR_CITATION_sale ~ VALUE_sale,
                           #                          FR_CITATION == FR_CITATION_spp ~ VALUE_spp,
                           #FR_CITATION == FR_CITATION_season ~ VALUE_season,
                           FR_CITATION == FR_CITATION_year ~ VALUE_year)) %>%
  select(-FR_CITATION_year, -VALUE_year, -FR_CITATION_close, -VALUE_close) %>% #, -FR_CITATION_sale, -VALUE_sale, -FR_CITATION_season, -VALUE_season)
  # Changing the time period from 7/29/2023 to 7/31/2023 to CLOSE. Previously, period was defined as OPEN due to end of 87 FR 77526 effective period. After end of effective period,
  # rule reverts to closures outlined in 83 FR 13426 where closure is from June 1 through July 31
  mutate(VALUE = case_when(SECTOR_USE == "RECREATIONAL" & SUBSECTOR_USE == "ALL" & ZONE_USE == "ALL" &
                             date_sequence >= as.Date("2023-07-29") & date_sequence <= as.Date("2023-07-31") ~ "CLOSE",
                           TRUE ~ VALUE))


# Summarize the open and closed periods for each year
summ_spp_closures <- spp_closure_story %>%
  mutate(YEAR = year(date_sequence)) %>%
  group_by(FMP, COMMON_NAME_USE, REGION, ZONE_USE, SECTOR_USE, SUBSECTOR_USE, YEAR) %>% 
  arrange(FMP, COMMON_NAME_USE, REGION, ZONE_USE, SECTOR_USE, SUBSECTOR_USE, date_sequence) %>%
  mutate(change = VALUE != lag(VALUE, default = first(VALUE)) |
           YEAR != lag(YEAR, default = first(YEAR))) %>%
  mutate(group = cumsum(change)) %>%
  group_by(FMP, COMMON_NAME_USE, REGION, ZONE_USE, SECTOR_USE, SUBSECTOR_USE, YEAR, VALUE, group) %>%
  summarize(ndays = n(),
            start = min(date_sequence),
            end = max(date_sequence),
            .groups = 'drop') %>%
  mutate(VALUE = case_when(is.na(VALUE) ~ "OPEN",
                           TRUE ~ VALUE)) %>%
  arrange(FMP, COMMON_NAME_USE, REGION, ZONE_USE, SECTOR_USE, SUBSECTOR_USE, YEAR, start)

# Summarize the number of days open per year
GAJ_summ_days_open <- summ_spp_closures %>%
  filter(VALUE == "OPEN") %>%
  group_by(FMP, COMMON_NAME_USE, REGION, ZONE_USE, SECTOR_USE, SUBSECTOR_USE, YEAR) %>%
  summarize(Days_Open = sum(ndays), .groups = "drop") %>%
  arrange(FMP, COMMON_NAME_USE, REGION, ZONE_USE, SECTOR_USE, SUBSECTOR_USE, YEAR)

GAJ_com<-GAJ_summ_days_open %>% filter(SECTOR_USE=="COMMERCIAL" & ZONE_USE=="ALL")
GAJ_com$YEAR
#add in missing years prior to 1990 for GAJ
Com_Open<-cbind(Com_Open,c(rep(NA,6),GAJ_com$Days_Open)) 
colnames(Com_Open)<-c("Year","LSN","HOG","DWG","OSWG","SWG","SH","TLF","GTR","VSN","RSN","GAJ")

GAJ_rec<-GAJ_summ_days_open %>% filter(SECTOR_USE=="RECREATIONAL" & ZONE_USE=="ALL")
Rec_Open<-cbind(Rec_Open,c(rep(NA,6),GAJ_rec$Days_Open))
colnames(Rec_Open)<-c("Year","LSN","HOG","DWG","SWG","BGR","GAG","RGR","GTR","RSN","GAJ")

#Reorder so similar species are together
Com_Open2<-Com_Open[c("Year","RSN","VSN","LSN","GTR","GAJ","HOG","SWG","OSWG","SH","DWG","TLF")]
write.csv(Com_Open2,paste0(getwd(),"/Open season/Commercial/com_openseason.csv"),row.names = FALSE)

Rec_Open2<-Rec_Open[c("Year","RSN","GTR","RGR","GAJ","GAG","HOG","LSN","DWG","SWG","BGR")]
write.csv(Rec_Open2,paste0(getwd(),"/Open season/Recreational/rec_openseason.csv"),row.names = FALSE)
