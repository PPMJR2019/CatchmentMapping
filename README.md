
Here's a README that includes all four scripts:

# Watershed Analysis Scripts

This repository contains R scripts for preprocessing NDVI rasters, extracting land use classes, setting project settings, and performing watershed morphometric and NDVI analysis.

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
