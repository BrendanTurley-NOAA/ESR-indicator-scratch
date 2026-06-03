
# https://ocmgeodatastor1.blob.core.windows.net/ccap/bulk_download/C-CAP_Regional_30-meter_Data/C-CAP_Regional_Land_Cover_Classification/CONUS/index.html
# https://coast.noaa.gov/digitalcoast/data/ccapregional.html

library(terra)
library(XML) # Required for XML parsing


setwd("C:/Users/brendan.turley/Downloads")

ccap <- rast('conus_2021_ccap_landcover.tif')
# ccap <- as.factor(ccap)
coltab(ccap)
plot(ccap)


xml_data <- xmlToDataFrame("conus_2021_ccap_landcover.tif.aux.xml")

library(xml2)
library(terra)

# 1. Define the NOAA C-CAP XML URL provided
xml_url <- "https://ocmgeodatastor1.blob.core.windows.net/ccap/bulk_download/C-CAP_Regional_30-meter_Data/C-CAP_Regional_Land_Cover_Classification/CONUS/conus_2021_ccap_landcover.tif.aux.xml"

# 2. Read the XML directly from the web
xml_data <- read_xml(xml_url)

# 3. Find the table rows (<Row>)
rows <- xml_find_all(xml_data, ".//Row")

if (length(rows) > 0) {
  cat("Found Raster Attribute Table structure. Parsing...\n")
  
  # Extract Column Headers/Field names
  fields <- xml_find_all(xml_data, ".//FieldDefinition")
  field_names <- xml_attr(fields, "Name")
  
  # Parse all row values safely as text (to preserve text class names)
  row_list <- lapply(rows, function(r) {
    xml_text(xml_find_all(r, "./F"))
  })
  
  # Bind into a data frame
  df <- as.data.frame(do.call(rbind, row_list), stringsAsFactors = FALSE)
  colnames(df) <- field_names
  
  # Use regular expressions to dynamically locate required columns
  val_col   <- grep("Value|ID", field_names, ignore.case = TRUE)[1]
  red_col   <- grep("Red|^R$", field_names, ignore.case = TRUE)[1]
  green_col <- grep("Green|^G$", field_names, ignore.case = TRUE)[1]
  blue_col  <- grep("Blue|^B$", field_names, ignore.case = TRUE)[1]
  name_col  <- grep("Name|Class|Label", field_names, ignore.case = TRUE)[1]
  alpha_col <- grep("Alpha|^A$", field_names, ignore.case = TRUE)[1]
  
  # Construct a clean color table data frame for terra
  color_df <- data.frame(
    value = as.numeric(df[[val_col]]),
    red   = as.numeric(df[[red_col]]),
    green = as.numeric(df[[green_col]]),
    blue  = as.numeric(df[[blue_col]])
  )
  
  # Append Alpha if it exists, otherwise default to solid (255)
  if (!is.na(alpha_col)) {
    color_df$alpha <- as.numeric(df[[alpha_col]])
  } else {
    color_df$alpha <- 255
  }
  
  print("Color Table extracted successfully! Preview:")
  print(head(color_df))
  
} else {
  stop("The XML structure does not contain '<Row>' nodes. Check the URL content.")
}

library(xml2)
library(terra)

# 1. Define the NOAA C-CAP XML URL
xml_url <- "https://ocmgeodatastor1.blob.core.windows.net/ccap/bulk_download/C-CAP_Regional_30-meter_Data/C-CAP_Regional_Land_Cover_Classification/CONUS/conus_2021_ccap_landcover.tif.aux.xml"

# 2. Read the XML directly from the web
xml_data <- read_xml(xml_url)

# 3. Correctly extract Column Headers/Field names using GDAL schema (<FieldDefn><Name>...</Name></FieldDefn>)
fields <- xml_find_all(xml_data, ".//FieldDefn")
field_names <- xml_text(xml_find_all(fields, "./Name"))

cat("Found columns in XML:", paste(field_names, collapse = ", "), "\n")

# 4. Find the table rows (<Row>)
rows <- xml_find_all(xml_data, ".//Row")
cat("Found", length(rows), "rows in the attribute table.\n")

if (length(rows) > 0 && length(field_names) > 0) {
  
  # Parse row values safely
  row_list <- lapply(rows, function(r) {
    xml_text(xml_find_all(r, "./F"))
  })
  
  # Bind into a data frame
  df <- as.data.frame(do.call(rbind, row_list), stringsAsFactors = FALSE)
  colnames(df) <- field_names
  
  # Map columns dynamically by searching for keywords
  val_col   <- grep("Value|ID", field_names, ignore.case = TRUE)[1]
  red_col   <- grep("Red|^R$", field_names, ignore.case = TRUE)[1]
  green_col <- grep("Green|^G$", field_names, ignore.case = TRUE)[1]
  blue_col  <- grep("Blue|^B$", field_names, ignore.case = TRUE)[1]
  name_col  <- grep("Name|Class|Label", field_names, ignore.case = TRUE)[1]
  alpha_col <- grep("Alpha|^A$", field_names, ignore.case = TRUE)[1]
  
  # Fail-safe check to make sure the core attributes exist
  if (is.na(val_col) || is.na(red_col) || is.na(green_col) || is.na(blue_col)) {
    stop("Could not map essential color columns. Columns found: ", paste(field_names, collapse=", "))
  }
  
  # Build color table data frame explicitly matching vector lengths
  color_df <- data.frame(
    value = as.numeric(df[[val_col]]),
    red   = as.numeric(df[[red_col]]),
    green = as.numeric(df[[green_col]]),
    blue  = as.numeric(df[[blue_col]])
  )
  
  # Append Alpha safely based on whether it is present in the XML
  if (!is.na(alpha_col)) {
    color_df$alpha <- as.numeric(df[[alpha_col]])
  } else {
    color_df$alpha <- rep(255, nrow(color_df))  # Safely matches the exact dataframe row count
  }
  
  cat("\nColor Table successfully built! Preview:\n")
  print(head(color_df))
  
  # Construct a clean text categories (Land Cover Names) data frame
  levels_df <- data.frame(
    ID = as.numeric(df[[val_col]]), 
    LandCover = df[[name_col]]
  )
  
} else {
  stop("The XML structure does not match a standard GDAL Raster Attribute Table.")
}

# Load your local C-CAP GeoTIFF
r <- rast("path/to/conus_2021_ccap_landcover.tif")

# 1. Assign the extracted color table
coltab(r) <- color_df

# 2. Assign the categorical land cover labels
levels(r) <- levels_df

# 3. Plot to verify colors and text legends render correctly
plot(r)