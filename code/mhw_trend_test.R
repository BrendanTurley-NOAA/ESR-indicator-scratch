
t_vals <- 1:nlyr(sst_r)

trend_model <- regress(sst_r, t_vals, na.rm = T)

# 3. Calculate the linear trend for each layer
# trend = intercept + slope * time
intercept <- trend_model[[1]]
slope <- trend_model[[2]]

# Generate the trend raster stack
trend_stack <- intercept + slope * t_vals

# 4. Subtract the trend from the original data (Detrend)
sst_rdt <- sst_r - trend_stack


trend_model2 <- regress(sst_r, t_vals, formula = y ~ x + I(x^2), na.rm = T)
trend_stack2 <- trend_model2[[1]] + 
  trend_model2[[2]] * t_vals + 
  trend_model2[[3]] * t_vals^2

# 4. Subtract the trend from the original data (Detrend)
sst_rdt2 <- sst_r - trend_stack2


trend_model3 <- regress(sst_r, t_vals, formula = y ~ x + I(x^2) + I(x^3), na.rm = T)
trend_stack3 <- trend_model3[[1]] + 
  trend_model3[[2]] * t_vals + 
  trend_model3[[3]] * t_vals^2 +
  trend_model3[[4]] * t_vals^3

# 4. Subtract the trend from the original data (Detrend)
sst_rdt3 <- sst_r - trend_stack3


plot(time(sst_r) ,global(sst_rdt3, fun = "mean", na.rm = TRUE) |> unlist(),
     typ='l')

plot(time(sst_r) ,global(sst_rdt2, fun = "mean", na.rm = TRUE) |> unlist(),
     typ='l')
points(time(sst_r) ,global(sst_rdt, fun = "mean", na.rm = TRUE) |> unlist(),
     typ='l', col = 2)


plot(time(sst_r) ,global(sst_r, fun = "mean", na.rm = TRUE) |> unlist(),
     typ='l')

plot(time(sst_r) ,global(sst_rdt, fun = "mean", na.rm = TRUE) |> unlist(),
     typ='l')
points(time(sst_r) ,global(sst_rdt2, fun = "mean", na.rm = TRUE) |> unlist(),
     typ='l', col = 2)
points(time(sst_r) ,global(sst_rdt3, fun = "mean", na.rm = TRUE) |> unlist(),
     typ='l', col = 3)

plot(density(global(sst_rdt, fun = "mean", na.rm = TRUE) |> unlist(), na.rm = T), lwd = 2)
lines(density(global(sst_rdt2, fun = "mean", na.rm = TRUE) |> unlist(), na.rm = T), col = 2, lwd = 2)
lines(density(global(sst_rdt3, fun = "mean", na.rm = TRUE) |> unlist(), na.rm = T), col = 3, lwd = 2)

plot(global(sst_rdt, fun = "mean", na.rm = TRUE) |> unlist(),
     global(sst_rdt3, fun = "mean", na.rm = TRUE) |> unlist(),
     asp = 1)
abline(0,1,col=2)

plot(global(sst_rdt, fun = "mean", na.rm = TRUE) |> unlist(),
     global(sst_rdt2, fun = "mean", na.rm = TRUE) |> unlist(),
     asp = 1)
abline(0,1,col=2)

plot(global(sst_rdt2, fun = "mean", na.rm = TRUE) |> unlist(),
     global(sst_rdt3, fun = "mean", na.rm = TRUE) |> unlist(),
     asp = 1)
abline(0,1,col=2)

global(sst_rdt3, fun = "mean", na.rm = TRUE)^2 |> sum()
global(sst_rdt2, fun = "mean", na.rm = TRUE)^2 |> sum()
global(sst_rdt, fun = "mean", na.rm = TRUE)^2 |> sum()


# 2. Define the pixel-wise linear detrending function
linear_detrend <- function(x) {
  # Handle missing data cleanly
  if (all(is.na(x))) return(rep(NA, length(x))) 
  
  # Create index steps representing your timeline
  time_steps <- 1:length(x)
  
  # Fit the linear model
  mod <- lm(x ~ time_steps)
  
  # Return the residuals (Original data minus the calculated linear trend)
  return(residuals(mod))
}

# 3. Apply the function across all layers of the SpatRaster
detrended_linear <- app(sst_r, fun = linear_detrend)

plot(time(sst_r) ,global(detrended_linear, fun = "mean", na.rm = TRUE) |> unlist(),
     typ='l')


stl_detrend <- function(x) {
  if (all(is.na(x)) || length(na.omit(x)) < 24) {
    return(rep(NA, length(x))) 
  }
  
  # Convert the vector to an R time series object (e.g., monthly data = frequency 12)
  ts_data <- ts(x, frequency = 12)
  
  # Perform STL decomposition
  decomp <- stl(ts_data, s.window = "periodic")
  
  # Subtract ONLY the trend component from the original data
  # This leaves you with (Seasonal + Remainder)
  detrended_val <- ts_data - decomp$time.series[, "trend"]
  
  return(as.numeric(detrended_val))
}
detrended_stl <- app(sst_r, fun = stl_detrend)

plot(time(sst_r) ,global(detrended_stl, fun = "mean", na.rm = TRUE) |> unlist(),
     typ='l')



yr_mhw <- aggregate(duration ~ year(index_start),
                    data = mhw_dt_cube,
                   median) |>
  setNames(c('year','cell')) |>
  merge(expand.grid(year=1982:2025),all=T)


aggregate(event_no ~ cell + year(index_start), data = mhw_dt_cube, max)
aggregate(event_no ~ cell + year(index_start), data = mhw_dt_cube, length)

plot(yr_mhw$year, yr_mhw$cell, typ='l', col=2)

