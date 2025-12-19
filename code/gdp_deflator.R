###############################
### this is done and ready to import into Gulf ESR 2025 as a metric
###############################

###############################
######## GDP deflator #########
###############################

library(dplyr)
library(here)
library(purrr)
library(tidyr)
library(httr2)
library(jsonlite)


here::i_am("README.md")

keys <- readLines('keys.api')
bea_api <- keys[grep('bea',keys)+1]

### GDP deflator; only available for annual or quarterly
### have requested an API kep, but for now downloaded the data
### https://apps.bea.gov/iTable/?reqid=19&step=3&isuri=1&1921=survey&1903=13
# Suggested citation: U.S. Bureau of Economic Analysis, "Table 1.1.9. Implicit Price Deflators for Gross Domestic Product" (accessed Monday, August 14, 2023).
# Calendar years and quarters. Unless noted otherwise, annual and quarterly data are presented on a calendar basis.

url <- paste0('https://apps.bea.gov/api/data/?&UserID=',
              bea_api,
              '&method=GetData&DataSetName=NIPA&TableName=T10109&Frequency=A,M,Q&Year=ALL&ResultFormat=json')

results <- url |>
  request() |>
  req_perform() |>
  resp_body_string() |>
  fromJSON()

gdp_df <- results$BEAAPI$Results$Data |>
  subset(LineDescription == 'Gross domestic product') |>
  type.convert(as.is = T)

gdp_dfq <- gdp_df[which(nchar(gdp_df$TimePeriod)>4), ] 
yr_qr <- strsplit(gdp_dfq$TimePeriod, 'Q') |> 
  unlist() |> as.numeric() |> 
  matrix(314,2,byrow=T) |> as.data.frame() |>
  setNames(c('year','quarter'))
gdp_dfq <- cbind(gdp_dfq, yr_qr)

### for interpolation, I will assume that the quarterly value is the mid-point of the quarter (or should it be the last month of the quarter)
gdp_dfq$month <- (gdp_dfq$quarter - 1)/4*12+2
gdp_dfq$yr_mth <- gdp_dfq$year + (gdp_dfq$month - 1)/12

value_out <- seq(gdp_dfq$year[1], gdp_dfq$year[nrow(gdp_dfq)]+(5/12), 1/12)

gdp_int <- approx(gdp_dfq$yr_mth, gdp_dfq$DataValue, xout = value_out) |> 
  as.data.frame() |> 
  setNames(c('year_decimal', 'gdp_deflator_dollar'))
gdp_int$gdp_deflator_dollar <- round(gdp_int$gdp_deflator_dollar, digits = 2)
gdp_int$date <- paste(as.integer(gdp_int$year_decimal),
                      round((gdp_int$year_decimal%%1)*12+1),
                      1, sep = '-') |> ymd()
gdp_int <- gdp_int[,c(3,1:2)]

plot(gdp_dfq$yr_mth, gdp_dfq$DataValue)
points(gdp_int$year_decimal, gdp_int$gdp_deflator_dollar, col = 2)

setwd('data/intermediate_files')
write.csv(gdp_dfq, 'bea_gdp_deflator_table119.csv', row.names = F)
write.csv(gdp_int, 'gdp_deflator_interpolated.csv', row.names = F)

### end of code




### alternative 
bls_api <- keys[grep('bls',keys)+1]

### CPI; both seasonally adjusted and non
### not much difference between seasonal and non-seasonal
# not_seasonal <- 'CUUR0000SA0'
seasonal <- 'CUSR0000SA0'
bls_url <- "https://api.bls.gov/publicAPI/v2/timeseries/data/"
bls_payload <- list(
  seriesid = seasonal,
  registrationkey = bls_api, 
  catalog = TRUE,
  startyear = min_yr,
  endyear = 2025,
  calculations = F,
  annualaverage = TRUE,
  aspects = F
)

bls_response <- request(bls_url) |>
  req_method("POST") |>
  req_body_form(!!!bls_payload) |>
  req_perform() |>
  resp_body_string() |>
  jsonlite::fromJSON()

names_keep <- c('year','period','periodName','value')
cpi_nsa_1 <- bls_response$Results$series$data[[1]]
cpi_nsa_1 <- cpi_nsa_1 |> 
  mutate(year = as.numeric(year),
         value = as.numeric(value))
cpi_nsa_1 <- cpi_nsa_1[ ,which(names(cpi_nsa_1) %in% names_keep)]

max_yr <- max(cpi_nsa_1$year)+1

# bls_payload <- list(
#   seriesid = seasonal,
#   registrationkey = bls_api, 
#   catalog = TRUE,
#   startyear = max_yr,
#   endyear = 2025,
#   calculations = F,
#   annualaverage = TRUE,
#   aspects = F
# )
bls_payload$startyear <- max_yr

bls_response <- request(bls_url) |>
  req_method("POST") |>
  req_body_form(!!!bls_payload) |>
  req_perform() |>
  resp_body_string() |>
  jsonlite::fromJSON()

cpi_nsa_2 <- bls_response$Results$series$data[[1]]
cpi_nsa_2 <- cpi_nsa_2 |> 
  mutate(year = as.numeric(year),
         value = as.numeric(value))
cpi_nsa_2 <- cpi_nsa_2[ ,which(names(cpi_nsa_2) %in% names(cpi_nsa_1))]

cpi_nsa <- rbind(cpi_nsa_1, cpi_nsa_2) |>
  subset(periodName!='Annual')

cpi_nsa$date <- as.Date(paste(cpi_nsa$year,substr(cpi_nsa$period,2,3),01,sep = '-'))
names(cpi_nsa)[which(names(cpi_nsa)=='value')] <- 'cpi_value'

# deflat_2024 <- rbind(cpi_nsa_1, cpi_nsa_2) |>
#   subset(periodName=='Annual' & year==2024,
# select = value) |> unlist()
deflat_2024 <- rbind(cpi_nsa_1, cpi_nsa_2) |>
  subset(periodName!='Annual' & year==2024) |> 
  mutate(mean = mean(value)) |>
  select(mean) |> distinct() |> unlist()
