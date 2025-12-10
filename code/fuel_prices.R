
library(dplyr)
library(here)
library(lubridate)
library(purrr)
library(tidyr)
library(httr2)
library(jsonlite)

here::i_am("README.md")

keys <- readLines('keys.api')
eia_api <- keys[grep('eia',keys)+1]


### fuel prices
url <- paste0('https://api.eia.gov/v2/petroleum/pri/gnd/data/?api_key=',
              eia_api,
              '&frequency=monthly&data[0]=value&facets[series][]=EMD_EPD2DXL0_PTE_R30_DPG&facets[series][]=EMD_EPD2D_PTE_R30_DPG&facets[series][]=EMM_EPM0_PTE_R30_DPG&sort[0][column]=period&sort[0][direction]=desc&offset=0&length=5000')


results <- url |>
  request() |>
  req_perform() |>
  resp_body_string() |>
  jsonlite::fromJSON()

fuel_prices <- results$response$data
fuel_prices$value <- as.numeric(fuel_prices$value)
fuel_prices$date <- paste0(fuel_prices$period,'-01') |> as.Date()
names(fuel_prices)[which(names(fuel_prices)=='value')] <- 'fuel_value'

### No 2 Diesel Low Sulfur (0-15 ppm) is fuel used for marine or offroad
### No 2 Diesel and low sulfur ar the same price; but No 2 is longer time series
# table(fuel_prices$`product-name`)

diesel_m <- subset(fuel_prices, `product-name`=='No 2 Diesel Low Sulfur (0-15 ppm)')
diesel <- subset(fuel_prices, `product-name`=='No 2 Diesel')
gas <- subset(fuel_prices, `product-name`=='Total Gasoline')

plot(diesel$date,diesel$fuel_value, typ = 'l', lwd = 2)
points(diesel_m$date,diesel_m$fuel_value, typ = 'l', lwd = 3, col = 2, lty = 3)
points(gas$date,gas$fuel_value, typ = 'l', lwd = 2, col = 3, lty = 2)

min(gas$date)
min(diesel$date)
min_yr <- 1995


### gdp deflation

setwd('data/intermediate_files')
gdp_df <- read.csv('gdp_deflator_interpolated.csv')
gdp_df$date <- ymd(gdp_df$date)
gdp_df <- subset(gdp_df, year(date)>=min_yr)

deflat_2024 <- subset(gdp_df, year(date)==2024, select = gdp_deflator_dollar) |>
  unlist() |> mean(na.rm=T) |> round(digits = 3)


g_merge <- merge(gas, gdp_df, by = 'date')
d_merge <- merge(diesel, gdp_df, by = 'date')

g_merge$value_2024 <- g_merge$fuel_value*(deflat_2024/g_merge$gdp_deflator_dollar)
d_merge$value_2024 <- d_merge$fuel_value*(deflat_2024/d_merge$gdp_deflator_dollar)

plot(d_merge$date,d_merge$fuel_value, typ = 'l', lwd = 3,panel.first = grid())
points(g_merge$date,g_merge$fuel_value, typ = 'l', lwd = 3, col = 3)

gas_out <- g_merge |> 
  select(date, fuel_value, value_2024) |>
  rename(gas_usd_gal = fuel_value,
         gas_2024_usd_gal = value_2024) |>
  mutate(gas_2024_usd_gal = round(gas_2024_usd_gal, digits = 3))
diesel_out <- d_merge |> 
  select(date, fuel_value, value_2024) |>
  rename(diesel_usd_gal = fuel_value,
         diesel_2024_usd_gal = value_2024) |>
  mutate(diesel_2024_usd_gal = round(diesel_2024_usd_gal, digits = 3))

fuel_merged <- merge(gas_out, diesel_out, by = c('date'))

setwd("~/R_projects/ESR-indicator-scratch/data/processed")
write.csv(fuel_merged, 'gulf_fuel_prices.csv', row.names = F)


