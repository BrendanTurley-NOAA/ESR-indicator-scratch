
### remove linear trend in SST

sst_r

# 1. Create a time vector (e.g., layer indices 1 to n)
t_vals <- 1:nlyr(sst_r)
# t_vals <- time(sst_r)

# 2. Run cell-level regression to get intercept and slope
# regress returns a SpatRaster with 2 layers: (Intercept) and x (slope)
trend_model <- regress(sst_r, t_vals)

# 3. Calculate the linear trend for each layer
# trend = intercept + slope * time
intercept <- trend_model[[1]]
slope <- trend_model[[2]]

# Generate the trend raster stack
trend_stack <- intercept + slope * t_vals

# 4. Subtract the trend from the original data (Detrend)
detrended_raster <- sst_r - trend_stack



### test polynominal detrend
library(terra)

# Load multi-layer time series stack
sst_r

# Define temporal variable (e.g., years, or sequence of layers)
t_vals <- 1:nlyr(sst_r)

# Create a function to fit polynomial and extract residuals for a single pixel time series
poly_detrend_func <- function(x) {
  if (all(is.na(x))) return(rep(NA, length(x))) # Handle masked cells
  
  # Fit a polynomial (degree = 2)
  model <- lm(x ~ poly(t_vals, degree = 3, raw = TRUE))
  
  # Return the residuals
  return(residuals(model))
}

# Apply the function across all pixels using app()
detrended_s <- app(sst_r, poly_detrend_func)

plot(detrended_s)


#----------------------------------------------------
#### 2. Clean data and create time series csv ####

#Transform the data to fit the IEA data format.
#For more info on IEA data format go to the IEAnalyzeR vignette (https://gulf-iea.github.io/IEAnalyzeR/articles/How_to_use_IEAnalyzeR.html).
#Once data are formatted with time (annual or monthly) as column 1 and metric values in the remaining columns, you can use the function convert_cleaned_data to convert your csv into a format that can be read by the data_prep function. Replace "your_data" in the code below with whatever your dataframe is called.

### annual ----------------
yr_mhw <- aggregate(cell ~ year(index_start),
                    data = mhw_dt_cube,
                    function(x) length(unique(x))) |>
  setNames(c('year','cell')) |>
  merge(expand.grid(year=1982:2025),all=T)
yr_mhw$cell[is.na(yr_mhw$cell)] <- 0
yr_mhw$percent <- yr_mhw$cell / ngrid
yr_mhw$kmsq <- yr_mhw$cell * cellsize_km

plot(yr_mhw$year, yr_mhw$percent, typ = 'l')

### annual-degree days ----------------
yr_mhw_dd <- aggregate(cbind(intensity_cumulative) ~ year(index_start),
                       data = mhw_dt_cube,
                       mean, na.rm=T) |>
  setNames(c('year','intensity_cumulative')) |>
  merge(expand.grid(year=1982:2025),all=T)
yr_mhw_dd$intensity_cumulative[is.na(yr_mhw_dd$intensity_cumulative)] <- 0

plot(yr_mhw_dd$year, yr_mhw_dd$intensity_cumulative, typ = 'l')

#Define header components for the data rows (ignore year). Fill in the blanks here.
indicator_names = c("Percentage of US Gulf EEZ", 'Cummulative Intensity (Degree-Days)')
unit_names = c("Percentage", 'Degree-Days')
extent_names = rep("Marine Heatwaves",2)

formatted_data = IEAnalyzeR::convert_cleaned_data(cbind(yr_mhw$year, yr_mhw$percent, yr_mhw_dd$intensity_cumulative),
                                                  indicator_names, unit_names, extent_names)


#----------------------------------------------------
#### 3. Save Formatted data as csv ####

# This will save your data to the appropriate folder.

# write.csv(formatted_data, file = csv_filename, row.names = F)

#----------------------------------------------------
#### 4. Create Data_Prep object ####

#Please use your formatted csv to create a "data_prep" object.
#For more info on the data_prep function see the vignette linked above.

data_obj <- IEAnalyzeR::data_prep(formatted_data)


#----------------------------------------------------
#### 5. Save Formatted data_prep object ####

#This will save your data to the appropriate folder.

# saveRDS(data_obj, file = object_filename)


#----------------------------------------------------
#### 6. Preview Plot ####
# Use the IEAnalyzeR plotting function to preview the data. This will not necessarily be the final figure used in reports.
# For more info on the plot_fn_obj function go HERE

IEAnalyzeR::plot_fn_obj(df_obj = data_obj, trend = TRUE, pts = T,
                        sep_ylabs = T, manual_title = 'Marine Heatwaves')

#----------------------------------------------------
#### 7. Save plot ####
# This will save the plot to the correct folder.
# Adjust height & width using (height=, width=, unit="in") if needed.

# ggsave(filename = plot_filename, width = 7, height = 6, unit = 'in')




### seasonal -------------------
mhw_cube <- mhw_dt_cube |> 
  mutate(year = year(index_start),
         month = month(index_start),
         yr_sea = case_when(
           month==12 ~ year+1,
           TRUE ~ year
         ))
seasons <- list(c(12,1,2),
                c(3,4,5),
                c(6,7,8),
                c(9,10,11))
# yr_mon$yr_sea <- ifelse(yr_mon$month==12, yr_mon$year+1, yr_mon$year)
mains <- list('Winter - DJF',
              'Spring - MAM',
              'Summer - JJA',
              'Fall - SON')

# png(here('figures/plots/mhw-surface-area-seasonal-plot.png'), width = 9, height = 6, units = 'in', res = 300)
par(mfrow = c(2,2), mar = c(3,5,2,1),
    oma = c(0,0,3,0))
for(i in 1:4){
  tmp <- subset(mhw_cube, month %in% seasons[[i]])
  yagg <- aggregate(cell ~ year(index_start),
                    data = tmp,
                    function(x) length(unique(x))) |>
    setNames(c('year','cell')) |>
    merge(expand.grid(year=1982:2025),all=T)
  yagg$cell[is.na(yagg$cell)] <- 0
  yagg$percent <- yagg$cell / ngrid
  yagg$kmsq <- yagg$cell * cellsize_km
  
  mod <- summary(lm(percent ~ year, data = yagg))
  plot(yagg$year, yagg$percent, typ = 'l', lwd = 2,
       las = 1, xlab = '', ylab = 'EEZ Proportion', main = mains[[i]],
       panel.first = list(abline(h = .5, lty = 5)),
       ylim = c(0,1))
  if(mod$coefficients[8]<=.05){
    abline(mod, col = 'orange', lwd = 2)
  }
  print(mod)
}

mtext('Proportion of US Gulf EEZ with Marine Heatwaves', side = 3, outer = TRUE, cex = 5/4, font = 2, line = 5/4)
dev.off()


png(here('figures/plots/mhw-surface-dd-seasonal-plot.png'), width = 9, height = 6, units = 'in', res = 300)
par(mfrow = c(2,2), mar = c(3,5,2,1),
    oma = c(0,0,3,0))

for(i in 1:4){
  tmp <- subset(mhw_cube, month %in% seasons[[i]])
  yagg <- aggregate(intensity_cumulative ~ year(index_start),
                    data = tmp,
                    mean, na.rm = T) |>
    setNames(c('year','intensity_cumulative')) |>
    merge(expand.grid(year=1982:2025),all=T)
  yagg$cell[is.na(yagg$intensity_cumulative)] <- 0
  
  mod <- summary(lm(intensity_cumulative ~ year, data = yagg))
  plot(yagg$year, yagg$intensity_cumulative, typ = 'l', lwd = 2,
       las = 1, xlab = '', ylab = 'Degree-Days', main = mains[[i]])
  if(mod$coefficients[8]<=.05){
    abline(mod, col = 'orange', lwd = 2)
  }
  print(mod)
}

mtext('US Gulf EEZ Marine Heatwave Cummulative Intensity', side = 3, outer = TRUE, cex = 5/4, font = 2, line = 5/4)
dev.off()


### spatial plots

### shapefile for plotting
world <- ne_download(scale = 10, type = "countries", 
                     returnclass = 'sv') |>
  crop(ext(min_lon,max_lon,min_lat,max_lat))



### where are the MHWs?
cell_ll <- unique(mhw_cube[, c("cell", "x", 'y')]) |>
  setNames(c('cell','lon','lat'))

# event_no_mean <- aggregate(event_no ~ cell, data = mhw_cube, length) |>
#   merge(cell_ll, all = T) |>
#   merge(lon_lat, all = T)
# mean_event_no <- matrix(event_no_mean$event_no/length(styear:enyear), 29, 69)

event_no_yr <- aggregate(event_no ~ cell + year(index_start), data = mhw_cube, length) |>
  setNames(c('cell','year','event_no'))
event_no_mean <- aggregate(event_no ~ cell, data = event_no_yr, mean, na.rm = T) |>
  merge(cell_ll, all = T) |>
  merge(lon_lat, all = T)
mean_event_no <- matrix(event_no_mean$event_no, 29, 69)

### put into raster
event_rast <- rast(mean_event_no[nrow(mean_event_no):1,])
ext(event_rast) <- c(range(lon_lat$lon),range(lon_lat$lat))
crs(event_rast) <- "EPSG:4326"
plot(event_rast)

### colors and breaks for plotting
e_brks <- seq(1.85,3.9,.05)
e_cols <- (cmocean('thermal')(length(e_brks)-1))


### spatial trend
setDT(event_no_yr)
slopes_dt <- event_no_yr[, 
                         .(slope = coef(lm(event_no ~ year, na.action = na.exclude))[2]), 
                         by = cell]
gridcell_lm <- unique(mhw_cube[, c("cell", "x", 'y')]) |>
  setNames(c('cell','lon','lat')) |>
  merge(slopes_dt) |>
  merge(lon_lat, all = T)
hist(gridcell_lm$slope)

mhw_slope <- matrix(gridcell_lm$slope*10, 29, 69)
### put into raster
slope_rast <- rast(mhw_slope[nrow(mhw_slope):1,])
ext(slope_rast) <- c(range(lon_lat$lon),range(lon_lat$lat))
crs(slope_rast) <- "EPSG:4326"
plot(slope_rast)

### colors and breaks for plotting
s_brks <- seq(-1.05,1.05,.05)
s_cols <- (cmocean('balance')(length(s_brks)-1))



png(here('figures/plots/mhw-surface-spatial-plot.png'), 
    width = 6, height = 6, units = 'in', res = 300)
par(mfrow=c(2,1))

plot(event_rast,
     col = e_cols, range = c(1.85,3.9),
     plg = list(tick = 'out'),
     main = 'Marine Heatwaves (mean number per year)',
     mar = c(1, 2, 1, 4))
plot(world, add= T, col = 'gray')
contour(ln[ln_i],lt[lt_i],bathy, levels = -100, add=T, lwd = 2)
# plot(gulf_eez, add = T)

plot(slope_rast,
     col = s_cols, range = c(-1.05,1.05),
     plg = list(tick = 'out'),
     main = 'Marine Heatwave Trend (events per decade)',
     mar = c(1, 2, 1, 4))
plot(world, add= T, col = 'gray')
contour(ln[ln_i],lt[lt_i],bathy, levels = -100, add=T, lwd = 2)
# plot(gulf_eez, add = T)

dev.off()




# by pixel detection
library(heatwaveR)

xy <- cbind(lon=lon_lat$lon[1], lat=lon_lat$lat[1])
p <- vect(xy, crs=crs(sst_rdt))
x <- extract(sst_rdt, p)

x <- sst_rdt[,1]

x <- app(sst_rdt, fun = \(x) ts2clm(x))
