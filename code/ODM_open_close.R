### to do:
# 1. find open and close dates
# 2. find end dates of assessment (or is this terminal year); then pair with implementation of catch limits

### use as reference: 'Synthesis of Management Histories for Gulf of America Reef Fishes through 2024"'### DOI: 10.25923/h0jt-xe51
# https://github.com/SEFSC/SEFSC-ODM-MH-GulfReefFish/tree/main
# https://github.com/SEFSC/SEFSC-ODM-MH-GulfReefFish/blob/main/Main_closure_prep_byspecies.R

library(dplyr)
library(here)
library(lubridate)

#### Load data ####
# Load the MH Data Log
# mh <- readRDS(here("ODM-MH-Data_log", "data", "results", "MH_DL_2025Sep10.RDS"))
mh <- readRDS('C:/Users/brendan.turley/Documents/R_projects/SEFSC-ODM-Management-History/ODM-MH-Data_log/data/results/MH_DL_2025Sep10.RDS')
region <- 'GULF OF MEXICO'

mh %>%
  filter(COMMON_NAME_USE %in% 'SNAPPER, RED',
         REGION %in% region,
         # MANAGEMENT_TYPE_USE %in% 'CLOSURE',
         ZONE == 'ALL') |>
  View()

#### Define species, region, and sector of interest ####
species = c('SNAPPER, CUBERA', 'AMBERJACK, GREATER', 'AMBERJACK, LESSER', 'JACK, ALMACO', 'SNAPPER, SILK', 'SNAPPER, QUEEN', 'SNAPPER, BLACKFIN', 'WENCHMAN',
            'DRUM, RED', 'COBIA', 'GROUPER, GAG', 'SNAPPER, GRAY', 'MACKEREL, KING', 'GROUPER, YELLOWEDGE', 'SNAPPER, LANE', 'GROUPER, RED', 'SNAPPER, RED', 
            'SCAMP', 'MACKEREL, SPANISH','HOGFISH', 'SNAPPER, MUTTON', 'SNAPPER, YELLOWTAIL', 'SNAPPER, VERMILION', 'TRIGGERFISH, GRAY', 'TRIGGERFISH, QUEEN')
region = 'GULF OF MEXICO'

management_type_use <- 'CLOSURE'


close_date <- mh %>%
  filter(COMMON_NAME_USE %in% species,
         REGION %in% region,
         DETAILED == "YES",
         NEVER_IMPLEMENTED %in% c(0, NA),
         REG_REMOVED == 0,
         # MANAGEMENT_TYPE_USE == "ACL",
         (MANAGEMENT_TYPE_USE %in% management_type_use),
         ZONE == 'ALL') |>
         # MANAGEMENT_TYPE == "ACL",
         # SPP_TYPE == 'COMMON_NAME', ### removing this gets yellowedge groupers and scamp
         # VALUE_UNITS == 'POUNDS') %>%
  arrange(CLUSTER, START_DATE2) |> type.convert(as.is = T) |>
  filter(SUBSECTOR=='ALL')
