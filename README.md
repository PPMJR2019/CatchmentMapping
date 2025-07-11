# Generic Watershed Processing Script
**Overview
This R script processes geospatial data for a user-specified watershed in any country, extracting and aligning watershed boundaries, administrative boundaries, land use, DEM, rainfall, and NDVI (dry and wet seasons) for catchment mapping. It assumes input data is in EPSG:4326 (WGS84) and outputs data in a UTM projection calculated from the watershed’s centroid longitude. Outputs are saved in a directory named after the watershed ([watershed_name]/). The script includes validation, error handling, and detailed logging.

# Prerequisites
•	R: Version 4.0 or higher.
•	Packages:
o	terra: For raster processing.
o	sf: For vector processing. Install with:

# install.packages(c("terra", "sf"))
•	Storage: Ensure write permissions in your working directory.
•	Input Files: Place the following in your working directory, using your country code (e.g., US, BR):
o	[country]_ws_boundaries.shp (and associated .shx, .dbf, .prj files)
o	[country]_admin_boundaries.shp (and associated files)
o	[country]_land_use.shp (and associated files)
o	[country]_dem.tif
o	average_annual_rainfall_[country].tif
o	[country]_ndvi_dry.tif
o	[country]_ndvi_wet.tifAll inputs should be in EPSG:4326. Non-4326 inputs are forced to 4326 with a warning.

# Installation
1.	Clone or download the script (watershed_processing_generic.R) to your local machine.
2.	Place input files in your working directory, named with your country code (e.g., US_ws_boundaries.shp).
3.	Install required R packages (see Prerequisites).

# Usage
1.	Open R or RStudio.
2.	Edit the script to set:
o	watershed_name: The name of the watershed (must match the WSNAME column in [country]_ws_boundaries.shp).
o	country: Your country code (e.g., US, BR).
o	setwd: Your working directory path. Example:
3.	watershed_name <- "MyWatershed"
4.	country <- "US"
setwd("/path/to/your/working/directory")
5.	Run the script:
source("watershed_processing_generic.R")
6.	Monitor the console for progress, warnings, and errors.
7.	Check outputs in the [working_directory]/[watershed_name]/ directory.

# Input Files
•	[country]_ws_boundaries.shp: Watershed boundaries with a WSNAME column (EPSG:4326).
•	[country]_admin_boundaries.shp: Administrative boundaries (e.g., counties, municipalities; EPSG:4326).
•	[country]_land_use.shp: Land use classification (EPSG:4326).
•	[country]_dem.tif: Digital elevation model (EPSG:4326).
•	average_annual_rainfall_[country].tif: Annual rainfall data (EPSG:4326).
•	[country]_ndvi_dry.tif: Dry season NDVI raster (EPSG:4326).
•	[country]_ndvi_wet.tif: Wet season NDVI raster (EPSG:4326).

# Output Files
All outputs are saved in [working_directory]/[watershed_name]/, in the calculated UTM projection (e.g., EPSG:326XX or 327XX):
•	[watershed_name]_ws.shp: Watershed boundary.
•	[watershed_name]_admin.shp: Clipped administrative boundaries with admin_id.
•	[watershed_name]_land_use.shp: Clipped land use.
•	[watershed_name]_dem.tif: Processed DEM.
•	[watershed_name]_rain.tif: Processed rainfall raster.
•	[watershed_name]_ndvi_dry.tif: Processed dry season NDVI.
•	[watershed_name]_ndvi_wet.tif: Processed wet season NDVI.

# Workflow
1.	Validates input files and forces EPSG:4326 for all inputs.
2.	Extracts the specified watershed, buffers it (0.01 degrees), and calculates the UTM zone.
3.	Clips and reprojects vector data (watershed, admin, land use) to the UTM zone.
4.	Crops, resamples, and reprojects raster data (DEM, rainfall, NDVI) to match the watershed extent and DEM resolution, then to the UTM zone.
5.	Validates NDVI values (-1 to 1), replacing NA or invalid values with 0.
6.	Saves outputs and deletes intermediate files.

# Troubleshooting
•	Missing Files: Ensure all input files are in the working directory with the correct [country] prefix.
•	NDVI Issues: If NDVI outputs have min/max of 0, check input rasters:
•	library(terra)
•	summary(rast("[country]_ndvi_dry.tif"))
summary(rast("[country]_ndvi_wet.tif"))
•	CRS Issues: If inputs are not in EPSG:4326, reproject them manually:
•	library(terra)
•	rast <- rast("[country]_ndvi_dry.tif")
•	rast_4326 <- project(rast, "EPSG:4326")
writeRaster(rast_4326, "[country]_ndvi_dry_4326.tif", overwrite=TRUE)
•	Memory Errors: For large rasters, use a machine with sufficient RAM.
•	Log Analysis: Check console logs for min/max values and NA counts to diagnose issues.

# Notes
•	Configure watershed_name and country at the script’s start.
•	Inputs not in EPSG:4326 are forced to 4326 with a warning; reproject manually for accuracy.
•	The UTM zone is calculated dynamically for global applicability.
•	Intermediate files are deleted to save space.
•	Console logs provide detailed diagnostics


# Watershed Analysis Scripts

These R scripts are for preprocessing NDVI rasters, extracting land use classes, setting project settings, and performing watershed morphometric and NDVI analysis.

# Scripts
- `01-NDVI_Preprocessing.R`: Preprocesses NDVI rasters for a given watershed by cleaning and aligning them with a DEM.
- `02-Land_Use_Class_Extraction.R`: Extracts and displays unique class names from a land use shapefile for a given watershed.
- `03a-Project_Settings.R`: Sets project settings and allows user to manually input threshold level and EPSG code.
- `03-Watershed_Analysis.R`: Performs watershed morphometric and NDVI analysis with narrative output.

# Requirements
- R
- terra package
- whitebox package

# Usage
1. Clone this repository to your local machine.
2. Set the working directory to the repository path.
3. Run the scripts in the following order:
    1. `01-NDVI_Preprocessing.R`
    2. `02-Land_Use_Class_Extraction.R` (optional)
    3. `03a-Project_Settings.R`
    4. `03-Watershed_Analysis.R`
4. Follow the prompts in each script to input required settings and files.

# Inputs
- NDVI rasters for dry and wet seasons
- DEM
- Watershed shapefile
- Land use shapefile (for `02-Land_Use_Class_Extraction.R`)
- Resampled rainfall raster matching the Filled-DEM

# Outputs
- Cleaned and aligned NDVI rasters
- Unique class names extracted from land use shapefile (if run)
- Project settings (threshold level and EPSG code) saved as RDS files
- Morphometric analysis results (CSV file)
- Narrative output (text file)
- Land cover rasters for dry and wet seasons
- NDVI statistics (CSV file)

# Author
Petronilo P. Munez, Jr.

# License
This software is released under the MIT License.
