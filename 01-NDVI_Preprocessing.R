# Script Name: 01-NDVI_Preprocessing.R
# Purpose: Preprocess NDVI rasters for a given watershed by cleaning and aligning them with a DEM.
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

# This script uses the following R packages:
# - terra

# References:
# Hijmans, R.J. (2022). terra: Spatial Data Analysis. R package version 1.6-17.
# https://CRAN.R-project.org/package=terra

# Required INPUTS
# Re-sampled NDVI's for wet and dry periods matching the filled-DEM of the project 

# RUN this script before Script 02-Land_Use_Class_Extraction.R


if (!requireNamespace("terra", quietly = TRUE)) {
  install.packages("terra", dependencies = TRUE)
}
library(terra)
# Load terra package
if (!requireNamespace("terra", quietly = TRUE)) {
  install.packages("terra", dependencies = TRUE)
}
library(terra)

# Define a function for the workflow
process_ndvi <- function(watershed_name, path) {
  cat("terra version:", as.character(packageVersion("terra")), "\n")
  flush.console()
  
  # Set working directory
  if (!dir.exists(path)) {
    stop("Working directory does not exist: ", path)
  }
  setwd(path)
  cat("Working directory set to:", getwd(), "\n")
  flush.console()
  
  # Verify input files
  input_files <- c(paste0(watershed_name, "_ndvi_dry.tif"), 
                   paste0(watershed_name, "_ndvi_wet.tif"), 
                   paste0(watershed_name, "_filled_c.tif"))
  for (file in input_files) {
    if (!file.exists(file.path(path, file))) {
      stop(paste("Input file not found:", file))
    }
  }
  
  # Load rasters
  cat("Loading NDVI and DEM rasters...\n")
  ndvi_dry_raw <- rast(file.path(path, paste0(watershed_name, "_ndvi_dry.tif")))
  ndvi_wet_raw <- rast(file.path(path, paste0(watershed_name, "_ndvi_wet.tif")))
  dem <- rast(file.path(path, paste0(watershed_name, "_filled_c.tif")))
  
  # Clean NDVI rasters (remove invalid values)
  cat("Cleaning NDVI rasters...\n")
  ndvi_dry_clean <- classify(ndvi_dry_raw, rcl = matrix(c(-Inf, -1e38, NA, 1e38, Inf, NA), ncol = 3, byrow = TRUE))
  ndvi_wet_clean <- classify(ndvi_wet_raw, rcl = matrix(c(-Inf, -1e38, NA, 1e38, Inf, NA), ncol = 3, byrow = TRUE))
  
  # Align NDVI rasters with DEM
  cat("Aligning NDVI rasters with DEM...\n")
  ndvi_dry_aligned <- resample(ndvi_dry_clean, dem, method = "bilinear")
  ndvi_wet_aligned <- resample(ndvi_wet_clean, dem, method = "bilinear")
  
  # Save cleaned NDVI rasters
  cat("Saving cleaned NDVI rasters...\n")
  writeRaster(ndvi_dry_aligned, file.path(path, paste0(watershed_name, "_ndvi_dry_clean.tif")), overwrite = TRUE)
  writeRaster(ndvi_wet_aligned, file.path(path, paste0(watershed_name, "_ndvi_wet_clean.tif")), overwrite = TRUE)
  
  # Verify output
  cat("Verifying cleaned NDVI rasters...\n")
  ndvi_dry_verified <- rast(file.path(path, paste0(watershed_name, "_ndvi_dry_clean.tif")))
  ndvi_wet_verified <- rast(file.path(path, paste0(watershed_name, "_ndvi_wet_clean.tif")))
  cat("NDVI dry clean range:", paste(range(values(ndvi_dry_verified), na.rm = TRUE), collapse = ", "), "\n")
  cat("NDVI wet clean range:", paste(range(values(ndvi_wet_verified), na.rm = TRUE), collapse = ", "), "\n")
  cat("NDVI dry clean CRS:", crs(ndvi_dry_verified), "\n")
  cat("NDVI wet clean CRS:", crs(ndvi_wet_verified), "\n")
  cat("NDVI preprocessing completed successfully\n")
}


# Call the function
watershed_name <- "watershed"
path <- "path"
process_ndvi(watershed_name, path)
