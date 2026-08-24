
library(rerddap)
library(terra)
library(dplyr)
library(lubridate)
library(purrr)
library(ggplot2)

# 1. Configuration & Server Setup
aoml_erddap <- "https://cwcgom.aoml.noaa.gov/erddap/" # CoastWatch AOML Node
dataset_id <- "noaa_aoml_atlantic_oceanwatch_AFAI_7D" # AFAI 7D Dataset
var_name   <- "AFAI"

lon_bounds <- c(-98, -81)
lat_bounds <- c(24, 31)

# 2. Monthly Processing Function
process_monthly_afai <- function(yr, mo) {
  start_dt <- sprintf("%04d-%02d-01", yr, mo)
  end_dt   <- as.character(ceiling_date(as.Date(start_dt), "month") - days(1))
  
  message(sprintf("Processing: %04d-%02d...", yr, mo))
  
  tryCatch({
    # Query ERDDAP for monthly slice
    res <- griddap(
      dataset_id,
      url = aoml_erddap,
      time = c(start_dt, end_dt),
      longitude = lon_bounds,
      latitude = lat_bounds,
      fields = var_name,
      fmt = "nc"
    )
    
    # Read NetCDF into Spatial Raster Stack
    r <- rast(res$summary$filename)
    
    # Step 1: Pixel-wise Maximum for the month
    r_monthly_max <- max(r, na.rm = TRUE)
    
    # Step 2: Spatial Mean and Max over the GoM Extent
    val_mean <- global(r_monthly_max, "mean", na.rm = TRUE)$mean
    val_max  <- global(r_monthly_max, "max", na.rm = TRUE)$max
    
    data.frame(
      year = yr,
      month = mo,
      date = as.Date(start_dt),
      mean_afai = val_mean,
      max_afai = val_max
    )
  }, error = function(e) {
    warning(sprintf("Failed to retrieve or process %04d-%02d: %s", yr, mo, e$message))
    data.frame(
      year = yr,
      month = mo,
      date = as.Date(start_dt),
      mean_afai = NA_real_,
      max_afai = NA_real_
    )
  })
}

# 3. Generate Time Series Array (2016 - 2025)
time_grid <- expand.grid(yr = 2016:2025, mo = 1:12) |> 
  arrange(yr, mo)

# 4. Execute Iterative Extraction
monthly_sargassum_index <- pmap_dfr(time_grid, process_monthly_afai)

# View summary table head
print(head(monthly_sargassum_index))

# 5. Visualizing the Index: Spatial Mean vs Spatial Max
ggplot(monthly_sargassum_index, aes(x = date)) +
  geom_line(aes(y = max_afai, color = "Monthly Max Pixel"), linewidth = 0.9) +
  geom_line(aes(y = mean_afai, color = "Monthly Regional Mean"), linewidth = 0.9) +
  scale_color_manual(values = c("Monthly Max Pixel" = "#d95f02", "Monthly Regional Mean" = "#1b9e77")) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  labs(
    title = "Gulf of Mexico Sargassum Monthly Index (2016–2025)",
    subtitle = "Aggregated 7-Day Cumulative USF AFAI (NOAA/AOML ERDDAP)",
    x = "Year",
    y = "AFAI Value",
    color = "Metric",
    caption = "Extent: Lat 24°N-31°N, Lon -99°W to -81°W"
  ) +
  theme_minimal(base_size = 12) +
  theme(legend.position = "bottom")
