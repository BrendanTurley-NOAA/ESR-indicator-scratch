library(rerddap)
library(terra)
library(dplyr)
library(ggplot2)

erddap_url <- "https://cwcgom.aoml.noaa.gov/erddap/"
dataset_id <- "noaa_aoml_atlantic_oceanwatch_AFAI_7D"
afai_variable <- "USFAFAI7D"

longitude_bounds <- c(-99, -81)
latitude_bounds <- c(24, 31)
start_date <- as.Date("2016-01-01")
end_date <- as.Date("2025-12-31")

output_dir <- file.path("data", "processed")
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

dataset <- info(dataset_id, url = erddap_url)
month_starts <- seq(start_date, end_date, by = "month")

download_month <- function(month_start, month_end, attempts = 3L) {
  for (attempt in seq_len(attempts)) {
    result <- try(
      griddap(
        dataset,
        fields = afai_variable,
        time = format(c(month_start, month_end), "%Y-%m-%d"),
        longitude = longitude_bounds,
        latitude = latitude_bounds,
        fmt = "nc",
        read = FALSE
      ),
      silent = TRUE
    )

    if (!inherits(result, "try-error")) {
      return(result)
    }

    if (attempt < attempts) {
      Sys.sleep(2^attempt)
    }
  }

  stop(
    "ERDDAP download failed for ",
    format(month_start, "%Y-%m"),
    " after ",
    attempts,
    " attempts."
  )
}

summarize_month <- function(month_start) {
  month_end <- min(seq(month_start, by = "month", length.out = 2L)[2L] - 1, end_date)
  message("Processing ", format(month_start, "%Y-%m"))

  download <- download_month(month_start, month_end)
  nc_path <- attr(download, "path")
  on.exit(unlink(nc_path), add = TRUE)

  afai <- rast(nc_path, subds = afai_variable)
  monthly_max <- app(
    afai,
    function(x) {
      if (all(is.na(x))) {
        NA_real_
      } else {
        max(x, na.rm = TRUE)
      }
    }
  )

  mean_afai <- global(monthly_max, "mean", na.rm = TRUE)[1, 1]
  max_afai <- global(monthly_max, "max", na.rm = TRUE)[1, 1]

  tibble(
    year = as.integer(format(month_start, "%Y")),
    month = as.integer(format(month_start, "%m")),
    date = month_start,
    mean_afai = ifelse(is.finite(mean_afai), mean_afai, NA_real_),
    max_afai = ifelse(is.finite(max_afai), max_afai, NA_real_)
  )
}

monthly_afai <- bind_rows(lapply(month_starts, summarize_month))

write.csv(
  monthly_afai,
  file.path(output_dir, "sargassum_afai_monthly_2016_2025.csv"),
  row.names = FALSE
)

plot_data <- bind_rows(
  monthly_afai |>
    transmute(date, statistic = "Monthly Mean", afai = mean_afai),
  monthly_afai |>
    transmute(date, statistic = "Monthly Max", afai = max_afai)
) |>
  mutate(statistic = factor(statistic, levels = c("Monthly Mean", "Monthly Max")))

afai_plot <- ggplot(plot_data, aes(x = date, y = afai, color = statistic)) +
  geom_line(linewidth = 0.7, na.rm = TRUE) +
  scale_color_manual(values = c("Monthly Mean" = "#0072B2", "Monthly Max" = "#D55E00")) +
  labs(
    title = "Monthly Sargassum Index in the Gulf of Mexico",
    subtitle = "Pixel-wise monthly maxima of 7-day cumulative USF AFAI fields",
    x = NULL,
    y = "USF AFAI",
    color = NULL
  ) +
  theme_minimal() +
  theme(legend.position = "top")

print(afai_plot)

ggsave(
  file.path(output_dir, "sargassum_afai_monthly_2016_2025.png"),
  plot = afai_plot,
  width = 11,
  height = 6,
  units = "in",
  dpi = 300
)
