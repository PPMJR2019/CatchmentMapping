# Script Name: Generic R Script for Prepping INPUS for Watershed Analysis.R
# Purpose: Set project settings and allows user to manually input threshold level and EPSG code.

# Author: Petronilo P. Munez, Jr.

# Copyright (c) 2025 Petronilo P. Munez, Jr.

# Permission is hereby granted, free of charge, to any person obtaining a copy
# of this software and associated documentation files (the "Software"), to deal
# in the Software without restriction, including without limitation the rights
# to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
# copies of the Software, and to permit persons to whom the Software is
# furnished to do so, subject to the following conditions:

# The above copyright notice and this permission notice shall be included in all
# copies or substantial portions of the Software.

# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
# IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
# FITNESS FOR A PARTICULAR PURPOSE AND NON-INFRINGEMENT. IN NO EVENT SHALL THE
# AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
# LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
# OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
# SOFTWARE.

# Input Files
# - [country]_ws_boundaries.shp: Watershed boundaries with a WSNAME column (EPSG:4326).
# -	[country]_admin_boundaries.shp: Administrative boundaries (e.g., counties, municipalities; EPSG:4326).
# -	[country]_land_use.shp: Land use classification (EPSG:4326).
# -	[country]_dem.tif: Digital elevation model (EPSG:4326).
# -	average_annual_rainfall_[country].tif: Annual rainfall data (EPSG:4326).
# -	[country]_ndvi_dry.tif: Dry season NDVI raster (EPSG:4326).
# -	[country]_ndvi_wet.tif: Wet season NDVI raster (EPSG:4326).

# Output Files
# -	[watershed_name]/[watershed_name]_ws.shp: Watershed boundary in UTM projection.
# -	[watershed_name]/[watershed_name]_admin.shp: Clipped administrative boundaries.
# -	[watershed_name]/[watershed_name]_land_use.shp: Clipped land use.
# -	[watershed_name]/[watershed_name]_dem.tif: Processed DEM.
# -	[watershed_name]/[watershed_name]_rain.tif: Processed rainfall.
# -	[watershed_name]/[watershed_name]_ndvi_dry.tif: Processed dry NDVI.
# -	[watershed_name]/[watershed_name]_ndvi_wet.tif: Processed wet NDVI.

# Notes
# -	Users must specify watershed_name and country in the script.
# -	Non-4326 inputs are forced to EPSG:4326 with a warning; reproject manually if needed.
# -	Requires sufficient memory for large rasters.
# -	NDVI processing mirrors rainfall workflow for consistency.


# Load Libraries
library(terra)
library(sf)

# User-defined variables
watershed_name <- "Your_Watershed_Name"  # Replace with your watershed name
country <- "Your_Country_Code"  # Replace with your country code (e.g., US, BR)

# Set working directory
tryCatch({
  setwd("/path/to/your/working/directory")  # Replace with your working directory
  cat("Working directory set to: ", getwd(), "\n")
}, error = function(e) {
  stop("Error setting working directory: ", e$message)
})

# Check write permissions
if (!file.access(getwd(), 2) == 0) {
  stop("No write permission in directory: ", getwd())
}
cat("Write permission confirmed for directory\n")

# Create watershed directory
watershed_dir <- file.path(getwd(), watershed_name)
tryCatch({
  if (!dir.exists(watershed_dir)) {
    dir.create(watershed_dir, recursive = TRUE)
    cat("Created directory: ", watershed_dir, "\n")
  } else {
    cat("Directory already exists: ", watershed_dir, "\n")
  }
}, error = function(e) {
  stop("Error creating watershed directory: ", e$message)
})

# Check input files
input_files <- c(
  paste0(country, "_ws_boundaries.shp"),
  paste0(country, "_admin_boundaries.shp"),  # Changed from suco to generic admin
  paste0(country, "_land_use.shp"),
  paste0(country, "_dem.tif"),
  paste0("average_annual_rainfall_", tolower(country), ".tif"),
  paste0(country, "_ndvi_dry.tif"),
  paste0(country, "_ndvi_wet.tif")
)
for (file in input_files) {
  if (!file.exists(file.path(getwd(), file))) {
    stop("Input file not found: ", file.path(getwd(), file))
  }
}
cat("All input files found\n")

# Define file paths
dem_file <- file.path(getwd(), paste0(country, "_dem.tif"))
rainfall_file <- file.path(getwd(), paste0("average_annual_rainfall_", tolower(country), ".tif"))
ndvi_dry_file <- file.path(getwd(), paste0(country, "_ndvi_dry.tif"))
ndvi_wet_file <- file.path(getwd(), paste0(country, "_ndvi_wet.tif"))
output_file_ws <- paste0(watershed_name, "_ws.shp")
output_file_admin <- paste0(watershed_name, "_admin.shp")
output_land_use <- file.path(watershed_dir, paste0(watershed_name, "_land_use.shp"))
output_dem <- file.path(watershed_dir, paste0(watershed_name, "_dem.tif"))
output_rain_r <- file.path(watershed_dir, paste0(watershed_name, "_rain_cropped.tif"))
output_rain_resampled <- file.path(watershed_dir, paste0(watershed_name, "_rain_resampled.tif"))
output_rainfall <- file.path(watershed_dir, paste0(watershed_name, "_rain.tif"))
output_ndvi_dry_cropped <- file.path(watershed_dir, paste0(watershed_name, "_ndvi_dry_cropped.tif"))
output_ndvi_wet_cropped <- file.path(watershed_dir, paste0(watershed_name, "_ndvi_wet_cropped.tif"))
output_ndvi_dry_resampled <- file.path(watershed_dir, paste0(watershed_name, "_ndvi_dry_resampled.tif"))
output_ndvi_wet_resampled <- file.path(watershed_dir, paste0(watershed_name, "_ndvi_wet_resampled.tif"))
output_ndvi_dry <- file.path(watershed_dir, paste0(watershed_name, "_ndvi_dry.tif"))
output_ndvi_wet <- file.path(watershed_dir, paste0(watershed_name, "_ndvi_wet.tif"))

# Validate NDVI files
tryCatch({
  ndvi_dry <- rast(ndvi_dry_file)
  ndvi_wet <- rast(ndvi_wet_file)
  cat("Loaded NDVI dry and wet rasters\n")
  
  # Validate and set CRS to EPSG:4326
  if (is.na(crs(ndvi_dry))) {
    crs(ndvi_dry) <- "EPSG:4326"
    cat("Assigned EPSG:4326 to NDVI dry\n")
  } else if (crs(ndvi_dry, proj=TRUE) != "EPSG:4326") {
    warning("NDVI dry has non-4326 CRS: ", crs(ndvi_dry, proj=TRUE), ". Forcing EPSG:4326")
    crs(ndvi_dry) <- "EPSG:4326"
  }
  if (is.na(crs(ndvi_wet))) {
    crs(ndvi_wet) <- "EPSG:4326"
    cat("Assigned EPSG:4326 to NDVI wet\n")
  } else if (crs(ndvi_wet, proj=TRUE) != "EPSG:4326") {
    warning("NDVI wet has non-4326 CRS: ", crs(ndvi_wet, proj=TRUE), ". Forcing EPSG:4326")
    crs(ndvi_wet) <- "EPSG:4326"
  }
  
  # Validate NDVI values
  dry_vals <- values(ndvi_dry)
  wet_vals <- values(ndvi_wet)
  dry_range <- minmax(ndvi_dry)[1:2]
  wet_range <- minmax(ndvi_wet)[1:2]
  dry_na_count <- sum(is.na(dry_vals))
  wet_na_count <- sum(is.na(wet_vals))
  cat("NDVI dry min/max: ", dry_range, " NA count: ", dry_na_count, "\n")
  cat("NDVI wet min/max: ", wet_range, " NA count: ", wet_na_count, "\n")
  
  if (all(is.na(dry_vals))) {
    stop("NDVI dry raster contains only NA values")
  }
  if (all(is.na(wet_vals))) {
    stop("NDVI wet raster contains only NA values")
  }
  if (dry_range[1] < -1 || dry_range[2] > 1) {
    warning("NDVI dry raster has values outside valid range (-1 to 1): ", dry_range)
  }
  if (wet_range[1] < -1 || wet_range[2] > 1) {
    warning("NDVI wet raster has values outside valid range (-1 to 1): ", wet_range)
  }
}, error = function(e) {
  stop("Error validating NDVI files: ", e$message)
}, warning = function(w) {
  cat("Warning in NDVI validation: ", w$message, "\n")
})

# Read and process watershed boundaries
tryCatch({
  ws_data <- st_read(paste0(country, "_ws_boundaries.shp"))
  cat("Successfully read ", paste0(country, "_ws_boundaries.shp"), " with ", nrow(ws_data), " features\n")
  cat("Input CRS: ", ifelse(is.na(st_crs(ws_data)$epsg), "Unknown", st_crs(ws_data)$epsg), "\n")
  
  # Validate and set CRS to EPSG:4326
  if (is.na(st_crs(ws_data)$epsg)) {
    st_crs(ws_data) <- 4326
    cat("Assigned EPSG:4326 to watershed boundaries\n")
  } else if (st_crs(ws_data)$epsg != 4326) {
    warning("Watershed boundaries have non-4326 CRS: ", st_crs(ws_data)$epsg, ". Forcing EPSG:4326")
    ws_data <- st_transform(ws_data, 4326)
  }
  
  # Check WSNAME column
  if (!"WSNAME" %in% names(ws_data)) {
    stop("WSNAME column not found in ", paste0(country, "_ws_boundaries.shp"))
  }
  if (!is.character(ws_data$WSNAME) && !is.factor(ws_data$WSNAME)) {
    stop("WSNAME column must contain character or factor data")
  }
  cat("WSNAME column found with ", length(unique(ws_data$WSNAME)), " unique values\n")
  
  # Verify watershed name
  ws_names <- sort(unique(as.character(ws_data$WSNAME)))
  cat("Available watershed names:\n", paste(ws_names, collapse=", "), "\n")
  if (!(watershed_name %in% ws_names)) {
    stop("Watershed name '", watershed_name, "' not found in WSNAME column")
  }
  cat("Watershed name '", watershed_name, "' confirmed\n")
  
  # Extract watershed
  selected_ws <- ws_data[ws_data$WSNAME == watershed_name, ]
  if (nrow(selected_ws) == 0) {
    stop("No watershed found with name: ", watershed_name)
  }
  cat("Extracted ", nrow(selected_ws), " watershed features\n")
  
  # Transform to EPSG:4326 (if not already)
  selected_ws_wgs84 <- st_transform(selected_ws, 4326)
  cat("Transformed watershed to EPSG:4326\n")
  
  # Buffer watershed
  selected_ws_wgs84 <- st_buffer(selected_ws_wgs84, dist = 0.01)
  cat("Applied 0.01-degree buffer to watershed\n")
}, error = function(e) {
  stop("Error processing watershed boundaries: ", e$message)
})

# Determine UTM projection
tryCatch({
  bbox <- st_bbox(selected_ws_wgs84)
  lon_min <- bbox["xmin"]
  lon_max <- bbox["xmax"]
  cat("Watershed longitude range: ", lon_min, " to ", lon_max, "\n")
  centroid <- st_centroid(st_geometry(selected_ws_wgs84))
  centroid_lon <- st_coordinates(centroid)[1]
  cat("Centroid longitude: ", centroid_lon, "\n")
  
  # Calculate UTM zone
  utm_zone <- floor((centroid_lon + 180) / 6) + 1
  hemisphere <- ifelse(st_coordinates(centroid)[2] >= 0, "N", "S")
  epsg_code <- ifelse(hemisphere == "N", 32600 + utm_zone, 32700 + utm_zone)
  final_crs <- st_crs(epsg_code)
  output_epsg <- as.character(epsg_code)
  final_ws <- st_transform(selected_ws, final_crs)
  cat("Selected EPSG:", output_epsg, " (UTM Zone ", utm_zone, hemisphere, ")\n")
}, error = function(e) {
  stop("Error determining UTM projection: ", e$message)
})

# Save watershed shapefile
tryCatch({
  st_write(final_ws, file.path(watershed_dir, output_file_ws), delete_layer = TRUE)
  cat("Saved watershed shapefile: ", file.path(watershed_dir, output_file_ws), "\n")
}, error = function(e) {
  stop("Error saving watershed shapefile: ", e$message)
})

# Process administrative boundaries
tryCatch({
  admin_data <- st_read(paste0(country, "_admin_boundaries.shp"))
  cat("Read ", paste0(country, "_admin_boundaries.shp"), " with ", nrow(admin_data), " features\n")
  
  # Validate and set CRS
  if (is.na(st_crs(admin_data)$epsg)) {
    st_crs(admin_data) <- 4326
    cat("Assigned EPSG:4326 to admin boundaries\n")
  } else if (st_crs(admin_data)$epsg != 4326) {
    warning("Admin boundaries have non-4326 CRS: ", st_crs(admin_data)$epsg, ". Forcing EPSG:4326")
    admin_data <- st_transform(admin_data, 4326)
  }
  
  admin_data <- st_transform(admin_data, final_crs)
  cat("Transformed admin boundaries to EPSG:", output_epsg, "\n")
  admin_selected <- admin_data[st_intersects(admin_data, final_ws, sparse = FALSE)[, 1], ]
  if (nrow(admin_selected) == 0) {
    stop("No admin boundaries intersect watershed: ", watershed_name)
  }
  cat("Selected ", nrow(admin_selected), " admin boundaries\n")
  admin_selected$admin_id <- seq_len(nrow(admin_selected))
  st_write(admin_selected, file.path(watershed_dir, output_file_admin), delete_layer = TRUE)
  cat("Saved admin shapefile: ", file.path(watershed_dir, output_file_admin), "\n")
}, error = function(e) {
  stop("Error processing admin boundaries: ", e$message)
})

# Process land use
tryCatch({
  land_use_data <- st_read(paste0(country, "_land_use.shp"))
  cat("Read ", paste0(country, "_land_use.shp"), " with ", nrow(land_use_data), " features\n")
  
  # Validate and set CRS
  if (is.na(st_crs(land_use_data)$epsg)) {
    st_crs(land_use_data) <- 4326
    cat("Assigned EPSG:4326 to land use\n")
  } else if (st_crs(land_use_data)$epsg != 4326) {
    warning("Land use has non-4326 CRS: ", st_crs(land_use_data)$epsg, ". Forcing EPSG:4326")
    land_use_data <- st_transform(land_use_data, 4326)
  }
  
  land_use_data <- st_transform(land_use_data, final_crs)
  cat("Transformed land use to EPSG:", output_epsg, "\n")
  land_use_clipped <- st_intersection(land_use_data, final_ws)
  if (nrow(land_use_clipped) == 0) {
    stop("No land use features intersect watershed: ", watershed_name)
  }
  cat("Clipped ", nrow(land_use_clipped), " land use features\n")
  st_write(land_use_clipped, output_land_use, delete_layer = TRUE)
  cat("Saved land use shapefile: ", output_land_use, "\n")
}, error = function(e) {
  stop("Error processing land use: ", e$message)
})

# Process DEM
tryCatch({
  dem_raster <- rast(dem_file)
  cat("Read DEM: ", dem_file, "\n")
  if (is.na(crs(dem_raster))) {
    crs(dem_raster) <- "EPSG:4326"
    cat("Assigned EPSG:4326 to DEM\n")
  } else if (crs(dem_raster, proj=TRUE) != "EPSG:4326") {
    warning("DEM has non-4326 CRS: ", crs(dem_raster, proj=TRUE), ". Forcing EPSG:4326")
    crs(dem_raster) <- "EPSG:4326"
  }
  dem_cropped <- crop(dem_raster, selected_ws_wgs84, snap="near")
  cat("Cropped DEM to watershed extent\n")
  cat("Cropped DEM min/max: ", minmax(dem_cropped)[1:2], "\n")
  dem_reprojected <- project(dem_cropped, paste0("EPSG:", output_epsg), method="bilinear")
  cat("Reprojected DEM to EPSG:", output_epsg, "\n")
  writeRaster(dem_reprojected, output_dem, overwrite=TRUE)
  cat("Saved DEM: ", output_dem, "\n")
}, error = function(e) {
  stop("Error processing DEM: ", e$message)
}, warning = function(w) {
  cat("Warning in DEM processing: ", w$message, "\n")
})

# Process rainfall
tryCatch({
  rainfall_raster <- rast(rainfall_file)
  cat("Read rainfall raster: ", rainfall_file, "\n")
  if (is.na(crs(rainfall_raster))) {
    crs(rainfall_raster) <- "EPSG:4326"
    cat("Assigned EPSG:4326 to rainfall\n")
  } else if (crs(rainfall_raster, proj=TRUE) != "EPSG:4326") {
    warning("Rainfall has non-4326 CRS: ", crs(rainfall_raster, proj=TRUE), ". Forcing EPSG:4326")
    crs(rainfall_raster) <- "EPSG:4326"
  }
  rainfall_values <- values(rainfall_raster)
  na_count <- sum(is.na(rainfall_values))
  cat("Rainfall NA values: ", na_count, "\n")
  if (na_count > 0) {
    rainfall_raster <- ifel(is.na(rainfall_raster), 0, rainfall_raster)
    cat("Replaced NA values with 0 in rainfall\n")
  }
  rainfall_cropped <- crop(rainfall_raster, selected_ws_wgs84, snap="near")
  cat("Cropped rainfall to watershed extent\n")
  cat("Cropped rainfall min/max: ", minmax(rainfall_cropped)[1:2], "\n")
  writeRaster(rainfall_cropped, output_rain_r, overwrite=TRUE)
  cat("Saved intermediate rainfall: ", output_rain_r, "\n")
  rainfall_resampled <- resample(rainfall_cropped, dem_cropped, method="bilinear")
  cat("Resampled rainfall to match DEM\n")
  cat("Resampled rainfall min/max: ", minmax(rainfall_resampled)[1:2], "\n")
  writeRaster(rainfall_resampled, output_rain_resampled, overwrite=TRUE)
  cat("Saved resampled rainfall: ", output_rain_resampled, "\n")
  rainfall_reprojected <- project(rainfall_resampled, paste0("EPSG:", output_epsg), method="bilinear")
  cat("Reprojected rainfall to EPSG:", output_epsg, "\n")
  writeRaster(rainfall_reprojected, output_rainfall, overwrite=TRUE)
  cat("Saved final rainfall: ", output_rainfall, "\n")
  if (file.exists(output_rain_r)) {
    file.remove(output_rain_r)
    cat("Deleted intermediate rainfall: ", output_rain_r, "\n")
  }
  if (file.exists(output_rain_resampled)) {
    file.remove(output_rain_resampled)
    cat("Deleted resampled rainfall: ", output_rain_resampled, "\n")
  }
}, error = function(e) {
  stop("Error processing rainfall: ", e$message)
}, warning = function(w) {
  cat("Warning in rainfall processing: ", w$message, "\n")
})

# Process dry season NDVI
tryCatch({
  ndvi_dry <- rast(ndvi_dry_file)
  cat("Read NDVI dry: ", ndvi_dry_file, "\n")
  
  # Crop to watershed extent
  ndvi_dry_cropped <- crop(ndvi_dry, selected_ws_wgs84, snap="near")
  cat("Cropped NDVI dry to watershed extent\n")
  dry_cropped_range <- minmax(ndvi_dry_cropped)[1:2]
  dry_cropped_na <- sum(is.na(values(ndvi_dry_cropped)))
  cat("Cropped NDVI dry min/max: ", dry_cropped_range, " NA count: ", dry_cropped_na, "\n")
  if (all(is.na(values(ndvi_dry_cropped))) || (dry_cropped_range[1] == 0 && dry_cropped_range[2] == 0)) {
    stop("Cropped NDVI dry contains no valid data or all values are 0")
  }
  
  # Replace invalid values
  ndvi_dry_cropped <- ifel(is.na(ndvi_dry_cropped) | ndvi_dry_cropped < -1 | ndvi_dry_cropped > 1, 0, ndvi_dry_cropped)
  cat("Cropped NDVI dry min/max after NA replacement: ", minmax(ndvi_dry_cropped)[1:2], "\n")
  
  # Save cropped NDVI dry
  output_dir <- dirname(output_ndvi_dry_cropped)
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }
  writeRaster(ndvi_dry_cropped, output_ndvi_dry_cropped, overwrite=TRUE)
  cat("Saved cropped NDVI dry: ", output_ndvi_dry_cropped, "\n")
  
  # Resample to match DEM
  ndvi_dry_resampled <- resample(ndvi_dry_cropped, dem_cropped, method="bilinear")
  ndvi_dry_resampled <- ifel(is.na(ndvi_dry_resampled) | ndvi_dry_resampled < -1 | ndvi_dry_resampled > 1, 0, ndvi_dry_resampled)
  cat("Resampled NDVI dry min/max: ", minmax(ndvi_dry_resampled)[1:2], "\n")
  
  # Save resampled NDVI dry
  output_dir <- dirname(output_ndvi_dry_resampled)
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }
  writeRaster(ndvi_dry_resampled, output_ndvi_dry_resampled, overwrite=TRUE)
  cat("Saved resampled NDVI dry: ", output_ndvi_dry_resampled, "\n")
  
  # Reproject to final EPSG
  ndvi_dry_reprojected <- project(ndvi_dry_resampled, paste0("EPSG:", output_epsg), method="bilinear")
  ndvi_dry_reprojected <- ifel(is.na(ndvi_dry_reprojected) | ndvi_dry_reprojected < -1 | ndvi_dry_reprojected > 1, 0, ndvi_dry_reprojected)
  cat("Reprojected NDVI dry min/max: ", minmax(ndvi_dry_reprojected)[1:2], "\n")
  
  # Save final NDVI dry
  output_dir <- dirname(output_ndvi_dry)
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }
  writeRaster(ndvi_dry_reprojected, output_ndvi_dry, overwrite=TRUE)
  cat("Saved final NDVI dry: ", output_ndvi_dry, "\n")
  
  # Clean up intermediate files
  if (file.exists(output_ndvi_dry_cropped)) {
    file.remove(output_ndvi_dry_cropped)
    cat("Deleted intermediate NDVI dry: ", output_ndvi_dry_cropped, "\n")
  }
  if (file.exists(output_ndvi_dry_resampled)) {
    file.remove(output_ndvi_dry_resampled)
    cat("Deleted resampled NDVI dry: ", output_ndvi_dry_resampled, "\n")
  }
}, error = function(e) {
  stop("Error processing NDVI dry: ", e$message)
}, warning = function(w) {
  cat("Warning in NDVI dry processing: ", w$message, "\n")
})

# Process wet season NDVI
tryCatch({
  ndvi_wet <- rast(ndvi_wet_file)
  cat("Read NDVI wet: ", ndvi_wet_file, "\n")
  
  # Crop to watershed extent
  ndvi_wet_cropped <- crop(ndvi_wet, selected_ws_wgs84, snap="near")
  cat("Cropped NDVI wet to watershed extent\n")
  wet_cropped_range <- minmax(ndvi_wet_cropped)[1:2]
  wet_cropped_na <- sum(is.na(values(ndvi_wet_cropped)))
  cat("Cropped NDVI wet min/max: ", wet_cropped_range, " NA count: ", wet_cropped_na, "\n")
  if (all(is.na(values(ndvi_wet_cropped))) || (wet_cropped_range[1] == 0 && wet_cropped_range[2] == 0)) {
    stop("Cropped NDVI wet contains no valid data or all values are 0")
  }
  
  # Replace invalid values
  ndvi_wet_cropped <- ifel(is.na(ndvi_wet_cropped) | ndvi_wet_cropped < -1 | ndvi_wet_cropped > 1, 0, ndvi_wet_cropped)
  cat("Cropped NDVI wet min/max after NA replacement: ", minmax(ndvi_wet_cropped)[1:2], "\n")
  
  # Save cropped NDVI wet
  output_dir <- dirname(output_ndvi_wet_cropped)
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }
  writeRaster(ndvi_wet_cropped, output_ndvi_wet_cropped, overwrite=TRUE)
  cat("Saved cropped NDVI wet: ", output_ndvi_wet_cropped, "\n")
  
  # Resample to match DEM
  ndvi_wet_resampled <- resample(ndvi_wet_cropped, dem_cropped, method="bilinear")
  ndvi_wet_resampled <- ifel(is.na(ndvi_wet_resampled) | ndvi_wet_resampled < -1 | ndvi_wet_resampled > 1, 0, ndvi_wet_resampled)
  cat("Resampled NDVI wet min/max: ", minmax(ndvi_wet_resampled)[1:2], "\n")
  
  # Save resampled NDVI wet
  output_dir <- dirname(output_ndvi_wet_resampled)
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }
  writeRaster(ndvi_wet_resampled, output_ndvi_wet_resampled, overwrite=TRUE)
  cat("Saved resampled NDVI wet: ", output_ndvi_wet_resampled, "\n")
  
  # Reproject to final EPSG
  ndvi_wet_reprojected <- project(ndvi_wet_resampled, paste0("EPSG:", output_epsg), method="bilinear")
  ndvi_wet_reprojected <- ifel(is.na(ndvi_wet_reprojected) | ndvi_wet_reprojected < -1 | ndvi_wet_reprojected > 1, 0, ndvi_wet_reprojected)
  cat("Reprojected NDVI wet min/max: ", minmax(ndvi_wet_reprojected)[1:2], "\n")
  
  # Save final NDVI wet
  output_dir <- dirname(output_ndvi_wet)
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }
  writeRaster(ndvi_wet_reprojected, output_ndvi_wet, overwrite=TRUE)
  cat("Saved final NDVI wet: ", output_ndvi_wet, "\n")
  
  # Clean up intermediate files
  if (file.exists(output_ndvi_wet_cropped)) {
    file.remove(output_ndvi_wet_cropped)
    cat("Deleted intermediate NDVI wet: ", output_ndvi_wet_cropped, "\n")
  }
  if (file.exists(output_ndvi_wet_resampled)) {
    file.remove(output_ndvi_wet_resampled)
    cat("Deleted resampled NDVI wet: ", output_ndvi_wet_resampled, "\n")
  }
}, error = function(e) {
  stop("Error processing NDVI wet: ", e$message)
}, warning = function(w) {
  cat("Warning in NDVI wet processing: ", w$message, "\n")
})

# Final messages
message("Watershed saved as: ", file.path(watershed_dir, output_file_ws))
message("Admin boundaries saved as: ", file.path(watershed_dir, output_file_admin))
message("Land use saved as: ", output_land_use)
message("DEM saved as: ", output_dem)
message("Final rainfall saved as: ", output_rainfall)
message("Final NDVI dry saved as: ", output_ndvi_dry)
message("Final NDVI wet saved as: ", output_ndvi_wet)
message("All extracted files moved to: ", watershed_dir)