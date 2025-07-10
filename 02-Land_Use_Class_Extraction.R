# Script Name: 02-Land_Use_Class_Extraction.R
# Purpose: Extract and display unique class names from a land use shapefile for a given watershed.
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
# Clipped land-use shapefile of the project's watershed 

# RUN this script before Script 03a-Project_Settings.R

# Load terra package
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
extract_class_names <- function(watershed_name, path) {
  cat("terra version:", as.character(packageVersion("terra")), "\n")
  flush.console()
  
  # Set working directory
  if (!dir.exists(path)) {
    stop("Working directory does not exist: ", path)
  }
  setwd(path)
  cat("Working directory set to:", getwd(), "\n")
  flush.console()
  
  # Verify input shapefile
  shapefile_name <- paste0(watershed_name, "_LU.shp")
  if (!file.exists(file.path(path, shapefile_name))) {
    stop(paste("Input shapefile", shapefile_name, "not found"))
  }
  
  # Load land use shapefile
  cat("Loading land use shapefile...\n")
  land_use <- vect(file.path(path, shapefile_name))
  
  # Check for class_name field
  if (!"class_name" %in% names(land_use)) {
    stop(paste("Input shapefile", shapefile_name, "missing required field: class_name"))
  }
  
  # Extract and display class_name values
  cat("Unique class_name values in", shapefile_name, ":", paste(unique(as.character(land_use$class_name)), collapse = ", "), "\n")
  cat("Sample class_name values:", paste(head(as.character(land_use$class_name), 10), collapse = ", "), "\n")
  cat("Class name extraction completed successfully\n")
}

# Call the function
watershed_name <- "watershed"
path <- "path"
extract_class_names(watershed_name, path)
