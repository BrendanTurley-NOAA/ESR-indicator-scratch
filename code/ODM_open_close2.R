### to do:
# 1. find open and close dates
# 2. find end dates of assessment (or is this terminal year); then pair with implementation of catch limits

### use as reference: 'Synthesis of Management Histories for Gulf of America Reef Fishes through 2024"'### DOI: 10.25923/h0jt-xe51
# https://github.com/SEFSC/SEFSC-ODM-MH-GulfReefFish/tree/main
# https://github.com/SEFSC/SEFSC-ODM-MH-GulfReefFish/blob/main/Main_closure_prep_byspecies.R

library(dplyr)
library(here)
library(lubridate)
library(tidyr)
library(purrr)

#### Load data ####
# Load the MH Data Log
# mh <- readRDS(here("ODM-MH-Data_log", "data", "results", "MH_DL_2025Sep10.RDS"))
mh_data_log <- readRDS('C:/Users/brendan.turley/Documents/R_projects/SEFSC-ODM-Management-History/ODM-MH-Data_log/data/results/MH_DL_2025Sep10.RDS')

# Function to expand dates based on management status
source(here("code/func_expand_status.R"))


#### Define species, region, and sector of interest ####
species = c('SNAPPER, CUBERA', 'AMBERJACK, GREATER', 'AMBERJACK, LESSER', 'JACK, ALMACO', 'SNAPPER, SILK', 'SNAPPER, QUEEN', 'SNAPPER, BLACKFIN', 'WENCHMAN',
            'DRUM, RED', 'COBIA', 'GROUPER, GAG', 'SNAPPER, GRAY', 'MACKEREL, KING', 'GROUPER, YELLOWEDGE', 'SNAPPER, LANE', 'GROUPER, RED', 'SNAPPER, RED', 
            'SCAMP', 'MACKEREL, SPANISH','HOGFISH', 'SNAPPER, MUTTON', 'SNAPPER, YELLOWTAIL', 'SNAPPER, VERMILION', 'TRIGGERFISH, GRAY', 'TRIGGERFISH, QUEEN')
region = 'GULF OF MEXICO'

management_type_use <- 'CLOSURE'

output <- c()

for(i in species){
  
  print(i)
  
  # Filter and reclassify closure-related records ####
  # Retain only species and region of interest and management types that impact the status of the fishery
  # Provide management types that impact fishery status with a VALUE of OPEN or CLOSE
  # FISHING SEASON/FISHING YEAR implies that the fishery is OPEN during the defined window
  # PROHIBITED SALE AND PURCHASE 
  #    - If FLAG = YES -> OPEN (sale is allowed)
  #    - If FLAG = NO -> CLOSE (sale is not allowed)
  # PROHIBITED SPECIES implies the fishery is closed as species cannot be retained
  # CLOSURE VALUE already indicates fishery status
  mh_spp_closure <- mh_data_log %>%
    filter(COMMON_NAME_USE == i,
           REGION == region,
           MANAGEMENT_TYPE_USE %in% c('CLOSURE', 'FISHING SEASON', 'FISHING YEAR',
                                      'PROHIBITED SALE AND PURCHASE',
                                      'PROHIBITED SPECIES')) %>%
    mutate(VALUE = case_when(MANAGEMENT_TYPE_USE %in% c('FISHING SEASON', 'FISHING YEAR') ~ 'OPEN',
                             MANAGEMENT_TYPE_USE == 'PROHIBITED SALE AND PURCHASE' & FLAG == 'YES' ~ 'OPEN',
                             MANAGEMENT_TYPE_USE == 'PROHIBITED SALE AND PURCHASE' & FLAG == 'NO' ~ 'CLOSE',
                             MANAGEMENT_TYPE_USE == 'PROHIBITED SPECIES' ~ 'CLOSE',
                             TRUE ~ VALUE)) %>%
    arrange(SECTOR_USE, START_DATE2)
  
  # Check to see which MANAGEMENT_TYPE_USE are present after filtering
  mtu <- unique(mh_spp_closure$MANAGEMENT_TYPE_USE)
  
  cat('\n', i, '\n' ,mtu)

  spp_year <- expand_status(mh_spp_closure, "FISHING YEAR")
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
  
  results <- subset(summ_spp_closures, SUBSECTOR_USE=='ALL' & ZONE_USE=='ALL', 
         select = c('COMMON_NAME_USE','SECTOR_USE','YEAR','VALUE','start','end')) |>
    arrange(YEAR, start, end, SECTOR_USE, VALUE)
  
  if(i == species[1]){
    output <- results
  } else {
    output <- bind_rows(output, results)
  }
  
  setwd(here("data/intermediate_files"))
  save(output, file='spp_open_close_output.RData')
  
  #  
  # 
  # spp_closure_story <- spp_year %>%
  #   rename(FR_CITATION_year = "FR_CITATION",
  #          TIME_year = "TIME",
  #          VALUE_year = "VALUE") %>%
  #   select(FMP, COMMON_NAME_USE, REGION, ZONE_USE, SECTOR_USE, SUBSECTOR_USE, date_sequence, FR_CITATION_year, TIME_year, VALUE_year) %>%
  #   # Comment out MANAGEMENT_TYPE_USE that do not apply to your filter criteria
  #   # full_join(spp_season %>%
  #   #             rename(FR_CITATION_season = "FR_CITATION",
  #   #                    TIME_season = "TIME",
  #   #                    VALUE_season = "VALUE") %>%
  #   #             select(FMP, COMMON_NAME_USE, REGION, ZONE_USE, SECTOR_USE, SUBSECTOR_USE, date_sequence, FR_CITATION_season, TIME_season, VALUE_season),
  #   #           by = join_by(FMP, COMMON_NAME_USE, REGION, ZONE_USE, SECTOR_USE, SUBSECTOR_USE,  date_sequence)) %>%
  #   full_join(spp_closures %>%
  #               rename(FR_CITATION_close = "FR_CITATION",
  #                      TIME_close = "TIME",
  #                      VALUE_close = "VALUE") %>%
  #               select(FMP, COMMON_NAME_USE, REGION, ZONE_USE, SECTOR_USE, SUBSECTOR_USE, date_sequence, FR_CITATION_close, TIME_close, VALUE_close),
  #             by = join_by(FMP, COMMON_NAME_USE, REGION, ZONE_USE, SECTOR_USE, SUBSECTOR_USE,  date_sequence)) %>%
  #   # full_join(spp_prohibited_sale %>%
  #   #             rename(FR_CITATION_sale = "FR_CITATION",
  #   #                    TIME_sale = "TIME", 
  #   #                    VALUE_sale = "VALUE") %>%
  #   #             select(FMP, COMMON_NAME_USE, REGION, ZONE_USE, SECTOR_USE, SUBSECTOR_USE, date_sequence, FR_CITATION_sale, VALUE_sale),
  #   #           by = join_by(FMP, COMMON_NAME_USE, REGION, ZONE_USE, SECTOR_USE, SUBSECTOR_USE,  date_sequence)) %>%
  #   # full_join(spp_prohibited_spp %>%
  #   #             rename(FR_CITATION_spp = "FR_CITATION",
  #   #                    TIME_spp = "TIME",
  #   #                    VALUE_spp = "VALUE") %>%
  #   #             select(FMP, COMMON_NAME_USE, REGION, ZONE_USE, SECTOR_USE, SUBSECTOR_USE, date_sequence, FR_CITATION_spp, VALUE_spp),
  #   #           by = join_by(FMP, COMMON_NAME_USE, REGION, ZONE_USE, SECTOR_USE, SUBSECTOR_USE, date_sequence)) %>%
  #   arrange(SECTOR_USE, SUBSECTOR_USE, ZONE_USE, date_sequence) %>%
  #   # Select the most recent FR CITATION and retain its TIME/VALUE
  #   mutate(FR_CITATION = pmax(FR_CITATION_year, 
  #                             # FR_CITATION_season, 
  #                             FR_CITATION_close, 
  #                             # FR_CITATION_sale, 
  #                             # FR_CITATION_spp,
  #                             na.rm = T),
  #          TIME = case_when(FR_CITATION == FR_CITATION_close ~ TIME_close,
  #                           # FR_CITATION == FR_CITATION_sale ~ TIME_sale,
  #                           # FR_CITATION == FR_CITATION_spp ~ TIME_spp,
  #                           # FR_CITATION == FR_CITATION_season ~ TIME_season,
  #                           FR_CITATION == FR_CITATION_year ~ TIME_year)) %>%
  #   mutate(VALUE = case_when(FR_CITATION == FR_CITATION_close ~ VALUE_close,
  #                            # FR_CITATION == FR_CITATION_sale ~ VALUE_sale,
  #                            # FR_CITATION == FR_CITATION_spp ~ VALUE_spp,
  #                            # FR_CITATION == FR_CITATION_season ~ VALUE_season,
  #                            FR_CITATION == FR_CITATION_year ~ VALUE_year)) %>%
  #   # Remove management type-specific columns now that a single FR/TIME/VALUE has been selected for each day
  #   select(-FR_CITATION_year, 
  #          -TIME_year, 
  #          -VALUE_year, 
  #          -FR_CITATION_close, 
  #          -TIME_close, 
  #          -VALUE_close ) %>%
  #          # -FR_CITATION_sale, 
  #          # -VALUE_sale, 
  #          # -TIME_sale, 
  #          # -FR_CITATION_season, 
  #          # -VALUE_season, 
  #          # -TIME_season
  #         
  #   # How to handle half days
  #   # Some FR notices take effect midday or later. To correctly count days for annual tallies, treat noon/6pm
  #   # TIMEs as half-days) (0.5)
  #   # This ensures that a midday start/end time contributes to both OPEN and CLOSED period calculations
  #   mutate(day_calc = case_when(TIME == "06:00:00 PM" ~ 0.5,
  #                               TIME == "12:00:00 PM" ~ 0.5,
  #                               TRUE ~ 1))
  # 
  # # Summarize the open and closed periods for each year ####
  # # Fill in any mission VALUE as OPEN (assumes that any window without an applicable rule is open)
  # summ_spp_closures <- spp_closure_story %>%
  #   mutate(YEAR = year(date_sequence),
  #          VALUE = as.character(VALUE),
  #          VALUE = if_else(is.na(VALUE), "OPEN", VALUE)) %>%
  #   filter(VALUE %in% c("OPEN", "CLOSE"))
  # 
  # # Create half-day complements ####
  # # When a day is counted as 0.5 under one status (OPEN/CLOSED), add a complementary 0.5 day to 
  # # the opposite status so that the calendar day sums to 1 acorss OPEN/CLOSE
  # summ_spp_closures2 <- summ_spp_closures %>%
  #   filter(day_calc == 0.5) %>%
  #   mutate(VALUE = if_else(VALUE == "OPEN", "CLOSE", "OPEN"))
  # 
  # # Combine whole-day and half-day rows for final daily time series
  # spp_closure_combined <- bind_rows(summ_spp_closures, summ_spp_closures2)
  # 
  # # Collapse individual days into continuous periods within each year ####
  # # Group consecutive days with the same VALUE into periods and compute total days within the period (ndays)
  # # and list the start/end as the first/last days within the period
  # spp_close_sum <- spp_closure_combined %>%
  #   group_by(FMP, COMMON_NAME_USE, REGION, ZONE_USE, SECTOR_USE, SUBSECTOR_USE, YEAR) %>% 
  #   arrange(FMP, COMMON_NAME_USE, REGION, ZONE_USE, SECTOR_USE, SUBSECTOR_USE, date_sequence, TIME) %>%
  #   mutate(change = VALUE != lag(VALUE, default = first(VALUE)) |
  #            YEAR != lag(YEAR, default = first(YEAR))) %>%
  #   mutate(group = cumsum(change)) %>%
  #   group_by(FMP, COMMON_NAME_USE, REGION, ZONE_USE, SECTOR_USE, SUBSECTOR_USE, YEAR, VALUE, group) %>%
  #   summarize(ndays = sum(day_calc, na.rm = TRUE),
  #             start = min(date_sequence),
  #             end = max(date_sequence),
  #             .groups = 'drop') %>%
  #   arrange(FMP, COMMON_NAME_USE, REGION, ZONE_USE, SECTOR_USE, SUBSECTOR_USE, YEAR, start)
  # 
  # 
  # unique(spp_close_sum$SUBSECTOR_USE)
  # 
  # all_open_close <- subset(spp_close_sum, SUBSECTOR_USE=='ALL')
  
    
}

