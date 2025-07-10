# Script Name: 03-Watershed_Analysis.R
# Purpose: Perform watershed morphometric and NDVI analysis with narrative output.
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
# - whitebox 

# References for Terra:
# Hijmans, R.J. (2022). terra: Spatial Data Analysis. R package version 1.6-17.
# https://CRAN.R-project.org/package=terra

# References for WhiteBox:
# Lindsay, J.B. (2016). Whitebox GAT: A case study in geomorphometric analysis. Computers & Geosciences, 95, 75-84. doi: 10.1016/j.cageo.2016.07.003 and
# Chung, C. (2022). whitebox: An R package for using the WhiteboxTools geospatial analysis software. R package version 2.2.0. 

# Morphometric Parametersbeing analyze
# 1. Area (m²)
# 2. Perimeter (m)
# 3. Maximum Length (m)
# 4. Mean Slope (degrees and percentage)
# 5. Relief (m)
# 6. Mean Elevation (m)
# 7. Basin Width (m)
# 8. Circularity Ratio
# 9. Elongation Ratio
# 10. Form Factor
# 11. Compactness Coefficient
# 12. Drainage Density (m/m²)
# 13. Stream Frequency (streams/m²)
# 14. Drainage Intensity
# 15. Drainage Texture
# 16. Infiltration Number
# 17. Orographic Coefficient
# 18. Massivity Index (m/m²)
# 19. Length of Overland Flow (m)
# 20. Constant of Channel Maintenance (m²/m)
# 21. Stream Order
# 22. Bifurcation Ratio
# 23. Main Channel Length (m)
# 24. Main Channel Gradient (m/m)
# 25. Main Channel Sinuosity
# 26. Ruggedness Number
# 27. Hypsometric Integral
# 28. Junction Density (junctions/m²)
# 29. Mean Stream Length (m)

# Required INPUTS
# - watershed shapefile
# - Resampled rainfall raster matching the Filled-DEM
# - Filled-DEM with the same extent of the watershed

# Load required libraries
library(terra)
library(whitebox)

# Load project settings
if (file.exists("threshold_ha.RDS") && file.exists("epsg_code.RDS")) {
  threshold_ha <- readRDS("threshold_ha.RDS")
  epsg_code <- readRDS("epsg_code.RDS")
} else {
  stop("Project settings not found. Please run script 3a to set threshold level and EPSG code.")
}

# Enable verbose WhiteboxTools output and set log file
wbt_verbose(TRUE)
wbt_init(log = "/path/whitebox_log.txt")

# Set working directory
path <- "/path"

# Input files
shp_file <- "watershed_ws.shp"
rainfall_file <- "watershed_rain.tif"
dem_file <- "watershed_filled_c.tif"
ndvi_dry_file <- "watershed_ndvi_dry.tif"
ndvi_wet_file <- "watershed_ndvi_wet.tif"

# Output files
flow_dir_file <- "watershed_flow_dir_c.tif"
flow_acc_file <- "watershed_flow_accu_c.tif"
stream_order_file <- "watershed_stream_order_c.tif"
clipped_dem_file <- "watershed_clipped_filled.tif"
clipped_flow_acc_file <- "watershed_clipped_flow_acc.tif"
clipped_flow_dir_file <- "watershed_clipped_flow_dir.tif"
clipped_rainfall_file <- "watershed_clipped_rain.tif"
streams_file <- "watershed_streams.tif"
stream_links_file <- "watershed_stream_links.tif"
pour_points_file <- "watershed_pour_points_ws.shp"
catchments_file <- "watershed_catchments_ws.tif"
pour_points_csv <- "watershed_pour_points_coordinates.csv"
morph_results_csv <- "watershed_morphometric_analysis.csv"
catchments_shp <- "watershed_catchments_ws.shp"
morph_narrative_txt <- "watershed_morphometric_narrative.txt"
land_cover_dry_file <- "watershed_land_cover_dry.tif"
land_cover_wet_file <- "watershed_land_cover_wet.tif"
ndvi_stats_csv <- "watershed_ndvi_stats.csv"

# Function to classify land cover based on NDVI
classify_land_cover <- function(ndvi_raster) {
  # NDVI thresholds for land cover classification
  # < 0.2: Bare soil/sparse vegetation
  # 0.2 - 0.5: Grassland/shrubland
  # > 0.5: Dense vegetation/forest
  land_cover <- app(ndvi_raster, function(x) {
    ifelse(is.na(x), NA,
           ifelse(x < 0.2, 1, # Bare soil/sparse vegetation
                  ifelse(x <= 0.5, 2, 3))) # Grassland/shrubland, Dense vegetation
  })
  return(land_cover)
}

# Function to perform morphometric analysis
morphometric_analysis <- function(catchments_file, dem_file, streams_file, stream_links_file, stream_order_file, rainfall_file, ndvi_dry_file, ndvi_wet_file, output_csv, output_shp, output_narrative, output_ndvi_stats, land_cover_dry_file, land_cover_wet_file) {
  # Load rasters
  catchments <- rast(catchments_file)
  dem <- rast(dem_file)
  streams <- rast(streams_file)
  stream_links <- rast(stream_links_file)
  rainfall <- rast(rainfall_file)
  ndvi_dry <- rast(ndvi_dry_file)
  ndvi_wet <- rast(ndvi_wet_file)
  
  # Check for stream order file
  stream_order_available <- file.exists(stream_order_file)
  if (stream_order_available) {
    stream_order <- rast(stream_order_file)
    if (!compareGeom(catchments, stream_order, stopOnError = FALSE)) {
      cat("Warning: Stream order raster has different extent or resolution; skipping stream order calculations\n")
      stream_order_available <- FALSE
    }
  } else {
    cat("Warning: Stream order file not found; skipping Stream_Order and Bifurcation_Ratio calculations\n")
  }
  
  # Ensure rasters are aligned
  if (!compareGeom(catchments, dem, stopOnError = FALSE) || !compareGeom(catchments, streams, stopOnError = FALSE) || 
      !compareGeom(catchments, stream_links, stopOnError = FALSE) || !compareGeom(catchments, rainfall, stopOnError = FALSE) ||
      !compareGeom(catchments, ndvi_dry, stopOnError = FALSE) || !compareGeom(catchments, ndvi_wet, stopOnError = FALSE)) {
    stop("Rasters have different extents or resolutions")
  }
  
  # Convert catchments raster to vector
  cat("Converting catchments raster to vector polygons...\n")
  catchments_vec <- as.polygons(catchments, dissolve = FALSE, na.rm = TRUE)
  if (is.null(catchments_vec) || nrow(catchments_vec) == 0) {
    stop("Failed to convert catchments to vector polygons")
  }
  
  # Aggregate multi-part polygons by Catchment_ID
  field_name <- names(catchments_vec)[1]
  catchments_vec <- aggregate(catchments_vec, by = field_name)
  
  # Save vectorized catchments as shapefile
  writeVector(catchments_vec, output_shp, overwrite = TRUE)
  cat("Vectorized catchments saved to", output_shp, "\n")
  
  # Get unique catchment IDs
  catchment_ids <- values(catchments_vec)[, field_name]
  catchment_ids <- catchment_ids[!is.na(catchment_ids) & catchment_ids > 0]
  if (length(catchment_ids) == 0) {
    stop("No valid catchment IDs found")
  }
  cat("Number of catchments detected:", length(catchment_ids), "\n")
  if (length(catchment_ids) == 1) {
    cat("Single catchment detected (whole watershed analysis)\n")
  } else if (length(catchment_ids) <= 50) {
    cat("Multiple catchments detected (sub-catchment analysis)\n")
  } else {
    cat("Many catchments detected (micro-catchment analysis)\n")
  }
  
  # Classify land cover for dry and wet seasons
  cat("Classifying land cover based on NDVI...\n")
  land_cover_dry <- classify_land_cover(ndvi_dry)
  land_cover_wet <- classify_land_cover(ndvi_wet)
  writeRaster(land_cover_dry, land_cover_dry_file, overwrite = TRUE)
  writeRaster(land_cover_wet, land_cover_wet_file, overwrite = TRUE)
  cat("Land cover rasters saved to", land_cover_dry_file, "and", land_cover_wet_file, "\n")
  
  # Initialize results data frame
  morph_results <- data.frame(
    Catchment_ID = integer(),
    Area_m2 = numeric(),
    Perimeter_m = numeric(),
    Max_Length_m = numeric(),
    Mean_Slope_deg = numeric(),
    Mean_Slope_percent = numeric(),
    Relief_m = numeric(),
    Mean_Elevation_m = numeric(),
    Min_Elevation_m = numeric(),
    Max_Elevation_m = numeric(),
    Basin_Width_m = numeric(),
    Circularity_Ratio = numeric(),
    Elongation_Ratio = numeric(),
    Form_Factor = numeric(),
    Compactness_Coefficient = numeric(),
    Drainage_Density_m_per_m2 = numeric(),
    Stream_Frequency_streams_per_m2 = numeric(),
    Drainage_Intensity = numeric(),
    Drainage_Texture = numeric(),
    Infiltration_Number = numeric(),
    Orographic_Coefficient = numeric(),
    Massivity_Index_m_per_m2 = numeric(),
    Length_Overland_Flow_m = numeric(),
    Constant_Channel_Maintenance_m2_per_m = numeric(),
    Stream_Order = integer(),
    Bifurcation_Ratio = numeric(),
    Main_Channel_Length_m = numeric(),
    Main_Channel_Gradient_m_per_m = numeric(),
    Main_Channel_Sinuosity = numeric(),
    Main_Channel_Start_Elevation_m = numeric(),
    Main_Channel_End_Elevation_m = numeric(),
    Total_Channel_Length_m = numeric(),
    Number_of_Streams = integer(),
    Ruggedness_Number = numeric(),
    Hypsometric_Integral = numeric(),
    Junction_Density_junctions_per_m2 = numeric(),
    Mean_Stream_Length_m = numeric(),
    Mean_Annual_Rainfall_mm = numeric(),
    Total_Annual_Rainfall_m3 = numeric(),
    Mean_NDVI_Dry = numeric(),
    Mean_NDVI_Wet = numeric(),
    Bare_Soil_Pct_Dry = numeric(),
    Grassland_Pct_Dry = numeric(),
    Dense_Veg_Pct_Dry = numeric(),
    Bare_Soil_Pct_Wet = numeric(),
    Grassland_Pct_Wet = numeric(),
    Dense_Veg_Pct_Wet = numeric()
  )
  
  # Initialize NDVI stats data frame
  ndvi_stats <- data.frame(
    Catchment_ID = integer(),
    Mean_NDVI_Dry = numeric(),
    Mean_NDVI_Wet = numeric(),
    Bare_Soil_Pct_Dry = numeric(),
    Grassland_Pct_Dry = numeric(),
    Dense_Veg_Pct_Dry = numeric(),
    Bare_Soil_Pct_Wet = numeric(),
    Grassland_Pct_Wet = numeric(),
    Dense_Veg_Pct_Wet = numeric()
  )
  
  # Initialize narrative file
  fileConn <- file(output_narrative, "w")
  if (length(catchment_ids) == 1) {
    writeLines("Exploring the watershed Watershed: A Simple Guide for Watershed Communities\n", fileConn)
    writeLines("This analysis looks at the shape, size, water flow, and vegetation of the watershed watershed to understand how it supports farming and communities. Here's what we found, explained simply for everyone to understand!\n", fileConn)
  } else {
    writeLines("Exploring the watershed Watershed: A Simple Guide for Watershed  Communities\n", fileConn)
    writeLines(paste("This analysis looks at", length(catchment_ids), "smaller areas (sub-catchments) within the watershed watershed to understand their shapes, sizes, water flow, and vegetation. This helps us see how water and land support farming and communities in different parts of the watershed. Here's a simple summary!\n"), fileConn)
  }
  
  # Cell area in square meters
  cell_area <- prod(res(catchments))
  
  cat("Performing morphometric and NDVI analysis for", length(catchment_ids), "catchment(s)...\n")
  
  for (i in seq_along(catchment_ids)) {
    id <- catchment_ids[i]
    cat("Processing Catchment ID", id, "(", i, "of", length(catchment_ids), ")...\n")
    
    # Extract polygon
    cat_poly <- catchments_vec[catchments_vec[[field_name]] == id]
    if (!all(is.valid(cat_poly))) {
      cat("Skipping Catchment ID", id, ": Invalid polygon geometry\n")
      writeLines(paste("Catchment", id, ": Skipped due to invalid geometry.\n"), fileConn)
      next
    }
    
    # Calculate area (m² and km²)
    area_m2 <- expanse(cat_poly, unit = "m")
    area_km2 <- area_m2 / 1e6
    if (area_m2 <= 0) {
      cat("Skipping Catchment ID", id, ": Invalid area\n")
      writeLines(paste("Catchment", id, ": Skipped due to invalid area.\n"), fileConn)
      next
    }
    
    # Calculate perimeter (m and km)
    perimeter_m <- perim(cat_poly)
    perimeter_km <- perimeter_m / 1000
    if (is.na(perimeter_m) || perimeter_m <= 0) {
      cat("Skipping Catchment ID", id, ": Invalid perimeter\n")
      writeLines(paste("Catchment", id, ": Skipped due to invalid perimeter.\n"), fileConn)
      next
    }
    
    # Calculate maximum length (m and km)
    ext <- ext(cat_poly)
    max_length_m <- sqrt((ext$xmax - ext$xmin)^2 + (ext$ymax - ext$ymin)^2)
    max_length_km <- max_length_m / 1000
    if (is.na(max_length_m) || max_length_m <= 0) {
      cat("Warning: Invalid maximum length for Catchment ID", id, "\n")
      max_length_m <- NA
      max_length_km <- NA
    }
    
    # Calculate elevation and slope
    dem_masked <- crop(dem, ext(cat_poly))
    dem_masked <- mask(dem_masked, cat_poly)
    if (all(is.na(values(dem_masked)))) {
      cat("Warning: No valid DEM data for Catchment ID", id, "\n")
      mean_slope_deg <- NA
      mean_slope_percent <- NA
      relief_m <- NA
      mean_elevation_m <- NA
      min_elevation_m <- NA
      max_elevation_m <- NA
      hypsometric_integral <- NA
      writeLines(paste("Catchment", id, ": Skipped due to no valid DEM data.\n"), fileConn)
    } else {
      slope <- terrain(dem_masked, v = "slope", unit = "degrees")
      mean_slope_deg <- mean(values(slope), na.rm = TRUE)
      if (is.nan(mean_slope_deg) || is.na(mean_slope_deg)) {
        mean_slope_deg <- NA
        mean_slope_percent <- NA
      } else {
        mean_slope_percent <- tan(mean_slope_deg * pi / 180) * 100
      }
      dem_vals <- values(dem_masked, na.rm = TRUE)
      relief_m <- max(dem_vals) - min(dem_vals)
      mean_elevation_m <- mean(dem_vals)
      min_elevation_m <- min(dem_vals)
      max_elevation_m <- max(dem_vals)
      hypsometric_integral <- if (max_elevation_m > min_elevation_m) {
        (mean_elevation_m - min_elevation_m) / (max_elevation_m - min_elevation_m)
      } else {
        NA
      }
    }
    
    # Calculate rainfall
    rainfall_masked <- crop(rainfall, ext(cat_poly))
    rainfall_masked <- mask(rainfall_masked, cat_poly)
    rainfall_vals <- values(rainfall_masked, na.rm = TRUE)
    if (length(rainfall_vals) == 0 || all(is.na(rainfall_vals))) {
      cat("Warning: No valid rainfall data for Catchment ID", id, "\n")
      mean_annual_rainfall_mm <- NA
      total_annual_rainfall_m3 <- NA
    } else {
      mean_annual_rainfall_mm <- mean(rainfall_vals, na.rm = TRUE)
      total_annual_rainfall_m3 <- (mean_annual_rainfall_mm / 1000) * area_m2
    }
    
    # Calculate NDVI statistics
    ndvi_dry_masked <- crop(ndvi_dry, ext(cat_poly))
    ndvi_dry_masked <- mask(ndvi_dry_masked, cat_poly)
    ndvi_wet_masked <- crop(ndvi_wet, ext(cat_poly))
    ndvi_wet_masked <- mask(ndvi_wet_masked, cat_poly)
    
    ndvi_dry_vals <- values(ndvi_dry_masked, na.rm = TRUE)
    ndvi_wet_vals <- values(ndvi_wet_masked, na.rm = TRUE)
    
    mean_ndvi_dry <- if (length(ndvi_dry_vals) > 0) mean(ndvi_dry_vals) else NA
    mean_ndvi_wet <- if (length(ndvi_wet_vals) > 0) mean(ndvi_wet_vals) else NA
    
    # Calculate land cover percentages
    land_cover_dry_masked <- crop(land_cover_dry, ext(cat_poly))
    land_cover_dry_masked <- mask(land_cover_dry_masked, cat_poly)
    land_cover_wet_masked <- crop(land_cover_wet, ext(cat_poly))
    land_cover_wet_masked <- mask(land_cover_wet_masked, cat_poly)
    
    dry_counts <- table(values(land_cover_dry_masked, na.rm = TRUE))
    wet_counts <- table(values(land_cover_wet_masked, na.rm = TRUE))
    total_cells_dry <- sum(dry_counts)
    total_cells_wet <- sum(wet_counts)
    
    bare_soil_pct_dry <- if ("1" %in% names(dry_counts)) (dry_counts["1"] / total_cells_dry) * 100 else 0
    grassland_pct_dry <- if ("2" %in% names(dry_counts)) (dry_counts["2"] / total_cells_dry) * 100 else 0
    dense_veg_pct_dry <- if ("3" %in% names(dry_counts)) (dry_counts["3"] / total_cells_dry) * 100 else 0
    
    bare_soil_pct_wet <- if ("1" %in% names(wet_counts)) (wet_counts["1"] / total_cells_wet) * 100 else 0
    grassland_pct_wet <- if ("2" %in% names(wet_counts)) (wet_counts["2"] / total_cells_wet) * 100 else 0
    dense_veg_pct_wet <- if ("3" %in% names(wet_counts)) (wet_counts["3"] / total_cells_wet) * 100 else 0
    
    # Append NDVI stats
    ndvi_stats <- rbind(ndvi_stats, data.frame(
      Catchment_ID = id,
      Mean_NDVI_Dry = mean_ndvi_dry,
      Mean_NDVI_Wet = mean_ndvi_wet,
      Bare_Soil_Pct_Dry = bare_soil_pct_dry,
      Grassland_Pct_Dry = grassland_pct_dry,
      Dense_Veg_Pct_Dry = dense_veg_pct_dry,
      Bare_Soil_Pct_Wet = bare_soil_pct_wet,
      Grassland_Pct_Wet = grassland_pct_wet,
      Dense_Veg_Pct_Wet = dense_veg_pct_wet
    ))
    
    # Calculate basin width (m and km)
    basin_width_m <- if (!is.na(max_length_m) && max_length_m > 0) area_m2 / max_length_m else NA
    basin_width_km <- if (!is.na(basin_width_m)) basin_width_m / 1000 else NA
    
    # Calculate circularity ratio
    circularity_ratio <- (4 * pi * area_m2) / (perimeter_m)^2
    if (is.na(circularity_ratio) || is.infinite(circularity_ratio) || circularity_ratio <= 0) {
      circularity_ratio <- NA
    }
    
    # Calculate elongation ratio
    elongation_ratio <- if (!is.na(max_length_m)) {
      (2 * sqrt(area_m2 / pi)) / max_length_m
    } else {
      NA
    }
    if (is.na(elongation_ratio) || is.infinite(elongation_ratio) || elongation_ratio <= 0) {
      elongation_ratio <- NA
    }
    
    # Calculate form factor
    form_factor <- if (!is.na(max_length_m) && max_length_m > 0) {
      area_m2 / (max_length_m^2)
    } else {
      NA
    }
    
    # Calculate compactness coefficient
    compactness_coefficient <- if (!is.na(perimeter_m) && !is.na(area_m2)) {
      perimeter_m / (2 * sqrt(pi * area_m2))
    } else {
      NA
    }
    
    # Calculate drainage density and total channel length
    streams_masked <- crop(streams, ext(cat_poly))
    streams_masked <- mask(streams_masked, cat_poly)
    stream_cells <- sum(values(streams_masked) > 0, na.rm = TRUE)
    total_channel_length_m <- stream_cells * res(streams)[1]
    total_channel_length_km <- total_channel_length_m / 1000
    drainage_density <- if (area_m2 > 0) total_channel_length_m / area_m2 else NA
    drainage_density_km <- if (!is.na(drainage_density)) drainage_density * 1000 else NA
    if (is.na(drainage_density) || is.infinite(drainage_density)) {
      drainage_density <- NA
      drainage_density_km <- NA
    }
    
    # Calculate stream frequency and number of streams
    stream_links_masked <- crop(stream_links, ext(cat_poly))
    stream_links_masked <- mask(stream_links_masked, cat_poly)
    stream_links_vec <- as.polygons(stream_links_masked, dissolve = FALSE, na.rm = TRUE)
    num_streams <- if (nrow(stream_links_vec) > 0) nrow(stream_links_vec) else 0
    stream_frequency <- if (area_m2 > 0) num_streams / area_m2 else NA
    if (is.na(stream_frequency) || is.infinite(stream_frequency)) {
      stream_frequency <- NA
    }
    
    # Calculate drainage intensity
    drainage_intensity <- if (!is.na(stream_frequency) && !is.na(drainage_density) && drainage_density > 0) {
      stream_frequency / drainage_density
    } else {
      NA
    }
    
    # Calculate drainage texture
    drainage_texture <- if (!is.na(num_streams) && !is.na(perimeter_m) && perimeter_m > 0) {
      num_streams / perimeter_m
    } else {
      NA
    }
    
    # Calculate infiltration number
    infiltration_number <- if (!is.na(drainage_density) && !is.na(stream_frequency)) {
      drainage_density * stream_frequency
    } else {
      NA
    }
    
    # Calculate orographic coefficient
    orographic_coefficient <- if (!is.na(relief_m) && !is.na(area_m2)) {
      relief_m * area_m2
    } else {
      NA
    }
    
    # Calculate massivity index
    massivity_index <- if (!is.na(relief_m) && !is.na(area_m2) && area_m2 > 0) {
      relief_m / area_m2
    } else {
      NA
    }
    
    # Calculate length of overland flow
    length_overland_flow_m <- if (!is.na(drainage_density) && drainage_density > 0) {
      0.5 / drainage_density
    } else {
      NA
    }
    length_overland_flow_km <- if (!is.na(length_overland_flow_m)) length_overland_flow_m / 1000 else NA
    
    # Calculate constant of channel maintenance
    constant_channel_maintenance <- if (!is.na(drainage_density) && drainage_density > 0) {
      1 / drainage_density
    } else {
      NA
    }
    
    # Calculate ruggedness number
    ruggedness_number <- if (!is.na(relief_m) && !is.na(drainage_density)) {
      relief_m * drainage_density
    } else {
      NA
    }
    
    # Calculate stream order and bifurcation ratio
    max_stream_order <- NA
    bifurcation_ratio <- NA
    if (stream_order_available) {
      stream_order_masked <- crop(stream_order, ext(cat_poly))
      stream_order_masked <- mask(stream_order_masked, cat_poly)
      stream_order_values <- values(stream_order_masked, na.rm = TRUE)
      max_stream_order <- if (length(stream_order_values) > 0) max(stream_order_values) else NA
      if (!is.na(max_stream_order) && max_stream_order > 1) {
        order_counts <- table(stream_order_values)
        cat("Catchment", id, "stream order counts:", paste(names(order_counts), order_counts, sep = "=", collapse = ", "), "\n")
        if (length(order_counts) > 1) {
          ratios <- numeric()
          for (order in 1:(max_stream_order - 1)) {
            order_str <- as.character(order)
            next_order_str <- as.character(order + 1)
            if (order_str %in% names(order_counts) && next_order_str %in% names(order_counts)) {
              if (order_counts[next_order_str] > 0) {
                ratios <- c(ratios, order_counts[order_str] / order_counts[next_order_str])
              }
            }
          }
          bifurcation_ratio <- if (length(ratios) > 0) mean(ratios, na.rm = TRUE) else NA
        } else {
          cat("Warning: Insufficient stream orders for Catchment", id, "\n")
        }
        if (is.na(bifurcation_ratio)) {
          cat("Warning: Bifurcation ratio is NA for Catchment", id, "; insufficient stream orders or invalid stream order counts\n")
        }
      } else {
        cat("Warning: Max stream order", max_stream_order, "for Catchment", id, "; bifurcation ratio set to NA\n")
      }
    }
    
    # Calculate mean stream length
    mean_stream_length_m <- if (!is.na(total_channel_length_m) && !is.na(num_streams) && num_streams > 0) {
      total_channel_length_m / num_streams
    } else {
      NA
    }
    
    # Calculate junction density
    junction_density <- NA
    if (nrow(stream_links_vec) > 1) {
      cat("Stream links vector nrow:", nrow(stream_links_vec), "\n")
      cat("Stream links vector geometry:", geomtype(stream_links_vec), "\n")
      
      # Check if stream_links_vec is a valid sf object
      if (inherits(stream_links_vec, "sf")) {
        # Try to calculate intersections
        intersections <- tryCatch(
          st_intersection(stream_links_vec),
          error = function(e) {
            cat("Error calculating intersections:", e$message, "\n")
            return(NULL)
          }
        )
        
        if (!is.null(intersections)) {
          cat("Intersections nrow:", nrow(intersections), "\n")
          if (!is.na(nrow(intersections)) && nrow(intersections) > 0) {
            junction_density <- nrow(intersections) / area_m2
          } else {
            cat("No intersections found for Catchment", id, "\n")
            junction_density <- NA
          }
        } else {
          cat("Intersections calculation returned NULL for Catchment", id, "\n")
          junction_density <- NA
        }
      } else {
        cat("Stream links vector is not a valid sf object for Catchment", id, "\n")
        junction_density <- NA
      }
    } else {
      junction_density <- NA
    }
    
    # Calculate main channel metrics
    main_channel_length_m <- NA
    main_channel_length_km <- NA
    main_channel_gradient_m_per_m <- NA
    main_channel_sinuosity <- NA
    main_channel_start_elevation_m <- NA
    main_channel_end_elevation_m <- NA
    
    pour_points <- vect(file.path(path, pour_points_file))
    pour_point <- pour_points[pour_points$stream_link_id == id]
    if (nrow(pour_point) > 0) {
      temp_pour_point_file <- file.path(path, paste0("temp_pour_point_", id, ".shp"))
      writeVector(pour_point, temp_pour_point_file, overwrite = TRUE)
      temp_flowpath_file <- file.path(path, paste0("longest_flowpath_", id, ".shp"))
      tryCatch({
        wbt_run_tool("LongestFlowpath", args = c(
          "--d8_pntr", file.path(path, clipped_flow_dir_file),
          "--pour_pts", temp_pour_point_file,
          "--output", temp_flowpath_file
        ), verbose = TRUE)
        if (file.exists(temp_flowpath_file)) {
          flowpath <- vect(temp_flowpath_file)
          main_channel_length_m <- sum(perim(flowpath))
          main_channel_length_km <- main_channel_length_m / 1000
          coords <- crds(flowpath)
          start_coords <- coords[1, ]
          end_coords <- coords[nrow(coords), ]
          straight_distance_m <- sqrt(sum((end_coords - start_coords)^2))
          main_channel_sinuosity <- if (straight_distance_m > 0) main_channel_length_m / straight_distance_m else NA
          start_elevation <- extract(dem, start_coords)[1, 1]
          end_elevation <- extract(dem, end_coords)[1, 1]
          main_channel_start_elevation_m <- if (!is.na(start_elevation)) start_elevation else NA
          main_channel_end_elevation_m <- if (!is.na(end_elevation)) end_elevation else NA
          main_channel_gradient_m_per_m <- if (!is.na(main_channel_length_m) && main_channel_length_m > 0) {
            (main_channel_start_elevation_m - main_channel_end_elevation_m) / main_channel_length_m
          } else {
            NA
          }
        }
      }, error = function(e) {
        cat("wbt_run_tool LongestFlowpath error for Catchment ID", id, ":", e$message, "\n")
      })
      if (file.exists(temp_pour_point_file)) unlink(temp_pour_point_file)
      if (file.exists(temp_flowpath_file)) unlink(temp_flowpath_file)
    } else {
      cat("Warning: No valid pour point for Catchment ID", id, "; main channel metrics set to NA\n")
    }
    
    # Generate community-focused narrative
    narrative <- paste0(
      "\nCatchment ", id, ":\n",
      "1. Size and Shape\n",
      "   - How Big Is It? This area covers ", round(area_km2, 2), " square kilometers, about the size of ",
      round(area_km2 * 135), " football fields! It stretches ", round(max_length_km, 2), " kilometers long and ",
      round(basin_width_km, 2), " kilometers wide, with a perimeter of ", round(perimeter_km, 2), " kilometers.\n",
      "   - What’s Its Shape? It has a form factor of ", round(form_factor, 2),
      ifelse(!is.na(form_factor) && form_factor < 0.3, ", meaning it’s more like a stretched-out rectangle.",
             ", meaning it’s less elongated."), "\n",
      "   - Why It Matters: The shape affects how water moves. ",
      ifelse(!is.na(form_factor) && form_factor < 0.3,
             "A stretched shape spreads water flow, reducing sudden floods.",
             "A less elongated shape can concentrate water flow."), " Careful planning is needed to ensure water reaches farms and villages.\n",
      "2. The Land: Heights and Slopes\n",
      "   - How High Is It? The land ranges from ", round(min_elevation_m, 1), " meters to ",
      round(max_elevation_m, 1), " meters, with an average of ", round(mean_elevation_m, 1), " meters. ",
      ifelse(!is.na(main_channel_start_elevation_m) && !is.na(main_channel_end_elevation_m),
             paste0("The main river starts at ", round(main_channel_start_elevation_m, 1),
                    " meters and ends at ", round(main_channel_end_elevation_m, 1), " meters."),
             "Main river elevation data is unavailable."), "\n",
      "   - How Steep Is It? The average slope is ", round(mean_slope_deg, 1), " degrees (",
      round(mean_slope_percent, 1), "%), like climbing ", round(mean_slope_percent / 100 * 100, 1),
      " meters for every 100 meters walked. ",
      ifelse(!is.na(main_channel_gradient_m_per_m),
             paste0("The main river has a gentle slope of ", round(main_channel_gradient_m_per_m * 1000, 1), " meters per kilometer."),
             "Main river slope data is unavailable."), "\n",
      "   - Why It Matters: ",
      ifelse(!is.na(mean_slope_deg) && mean_slope_deg > 20, "Steep slopes mean water can rush down quickly, risking erosion or landslides.",
             ifelse(!is.na(mean_slope_deg) && mean_slope_deg < 10, "Gentle slopes help the land soak up water, good for crops.",
                    "Moderate slopes balance water flow and absorption.")), " Communities need to protect steep areas to prevent soil loss.\n",
      "3. How Water Flows\n",
      "   - Rivers and Streams: There are ", num_streams, " streams stretching ",
      round(total_channel_length_km, 2), " kilometers, with the main river at ",
      ifelse(!is.na(main_channel_length_km), round(main_channel_length_km, 2), "unknown"), " kilometers long. ",
      "The drainage density is ", round(drainage_density_km, 2), " kilometers of streams per square kilometer, which is ",
      ifelse(!is.na(drainage_density_km) && drainage_density_km < 0.5, "coarse", "moderate"), ".\n",
      "   - Why It Matters: ",
      ifelse(!is.na(drainage_density_km) && drainage_density_km < 0.5,
             "Fewer streams mean water may pool in some areas during heavy rain, so watch for flooding risks.",
             "A good stream network helps water reach crops evenly."), " The land’s ability to hold water supports farming.\n",
      "4. Safety from Floods\n",
      "   - Shape and Flood Risk: The compactness coefficient is ", round(compactness_coefficient, 2),
      ifelse(!is.na(compactness_coefficient) && compactness_coefficient > 1.5, ", showing a stretched shape that lowers flash flood risk.",
             ", indicating a shape that may concentrate water flow."), " The ruggedness number is ",
      round(ruggedness_number, 2), ", suggesting ",
      ifelse(!is.na(ruggedness_number) && ruggedness_number < 1, "lower", "higher"), " flood risk.\n",
      "   - River Path: The main river has a sinuosity of ",
      ifelse(!is.na(main_channel_sinuosity), round(main_channel_sinuosity, 2), "unknown"),
      ifelse(!is.na(main_channel_sinuosity) && main_channel_sinuosity < 1.2, ", meaning it’s fairly straight.",
             ", meaning it’s winding."), "\n",
      "   - Why It Matters: ",
      ifelse(!is.na(compactness_coefficient) && compactness_coefficient > 1.5,
             "The stretched shape and straight river path reduce sudden floods, giving communities time to prepare.",
             "The shape and river path may increase flood risks in some areas."), " Protection like tree planting can help.\n",
      "5. Water and Soil Health\n",
      "   - Water Absorption: The infiltration number is ", round(infiltration_number, 2),
      ifelse(!is.na(infiltration_number) && infiltration_number < 0.1, ", meaning the land soaks up water well.",
             ", indicating moderate water absorption."), " Water travels ",
      round(length_overland_flow_km, 2), " kilometers over the surface before joining streams.\n",
      "   - Terrain: The massivity index is ", round(massivity_index, 2),
      ifelse(!is.na(massivity_index) && massivity_index < 0.6, ", showing a less hilly terrain.",
             ", indicating hilly terrain."), " Some areas may have quick elevation drops, increasing erosion risk.\n",
      "   - Why It Matters: Good water absorption supports crops like rice or coffee. Steep areas need extra care, like planting trees, to keep soil healthy.\n",
      "6. Vegetation and Farming Potential\n",
      "   - Dry Season: The average NDVI is ", round(mean_ndvi_dry, 2), ", with ",
      round(bare_soil_pct_dry, 1), "% bare soil, ", round(grassland_pct_dry, 1), "% grassland/shrubland, and ",
      round(dense_veg_pct_dry, 1), "% dense vegetation. ",
      ifelse(!is.na(mean_ndvi_dry) && mean_ndvi_dry < 0.2, "Low vegetation cover suggests limited farming potential without irrigation.",
             ifelse(!is.na(mean_ndvi_dry) && mean_ndvi_dry <= 0.5, "Moderate vegetation supports grazing or seasonal crops.",
                    "Dense vegetation indicates potential for perennial crops or forestry.")), "\n",
      "   - Wet Season: The average NDVI is ", round(mean_ndvi_wet, 2), ", with ",
      round(bare_soil_pct_wet, 1), "% bare soil, ", round(grassland_pct_wet, 1), "% grassland/shrubland, and ",
      round(dense_veg_pct_wet, 1), "% dense vegetation. ",
      ifelse(!is.na(mean_ndvi_wet) && mean_ndvi_wet < 0.2, "Low vegetation cover, even in the wet season, suggests challenging conditions for farming.",
             ifelse(!is.na(mean_ndvi_wet) && mean_ndvi_wet <= 0.5, "Improved vegetation supports crops like rice or maize during the wet season.",
                    "High vegetation cover supports diverse crops and reduces erosion.")), "\n",
      "   - Why It Matters: ",
      ifelse(!is.na(mean_ndvi_wet) && !is.na(mean_ndvi_dry) && mean_ndvi_wet - mean_ndvi_dry > 0.2,
             "A big increase in vegetation from dry to wet seasons suggests strong potential for seasonal farming, but irrigation may be needed in the dry season.",
             "Stable vegetation across seasons indicates reliable conditions for farming or grazing, though soil and water management are still key."), " Communities can use this to plan crops and protect land.\n"
    )
    
    # Additional factors note
    if (length(catchment_ids) == 1) {
      narrative <- paste0(narrative,
                          "7. Why We Need More Than Numbers\n",
                          "This analysis is a great start, but to fully understand the watershed watershed, we must also look at:\n",
                          "- Land Use and Cover: Are people farming, grazing livestock, or leaving land bare? This affects water flow and soil health.\n",
                          "- Climate: Rainfall patterns and seasonal changes shape how water and vegetation behave.\n",
                          "- Soil Types: Different soils hold water and support crops differently, impacting farming and erosion.\n",
                          "- Socio-Cultural Factors: Practices like Tara Bandu, a traditional Timorese way of managing land, influence how communities care for the watershed.\n\n"
      )
    } else {
      narrative <- paste0(narrative,
                          "7. Why We Need More Than Numbers\n",
                          "This analysis of ", length(catchment_ids), " sub-catchments is a starting point. To fully understand how these areas work, we must also consider:\n",
                          "- Land Use and Cover: Different farming or grazing practices in each sub-catchment affect water flow, vegetation, and soil health.\n",
                          "- Climate: Rainfall changes across the watershed impact water availability and vegetation growth.\n",
                          "- Soil Types: Soil differences in each area affect water retention and crop growth.\n",
                          "- Socio-Cultural Factors: Traditional practices like Tara Bandu shape how communities manage water and land.\n\n"
      )
    }
    
    writeLines(narrative, fileConn)
    
    # Append results
    morph_results <- rbind(morph_results, data.frame(
      Catchment_ID = id,
      Area_m2 = area_m2,
      Perimeter_m = perimeter_m,
      Max_Length_m = max_length_m,
      Mean_Slope_deg = mean_slope_deg,
      Mean_Slope_percent = mean_slope_percent,
      Relief_m = relief_m,
      Mean_Elevation_m = mean_elevation_m,
      Min_Elevation_m = min_elevation_m,
      Max_Elevation_m = max_elevation_m,
      Basin_Width_m = basin_width_m,
      Circularity_Ratio = circularity_ratio,
      Elongation_Ratio = elongation_ratio,
      Form_Factor = form_factor,
      Compactness_Coefficient = compactness_coefficient,
      Drainage_Density_m_per_m2 = drainage_density,
      Stream_Frequency_streams_per_m2 = stream_frequency,
      Drainage_Intensity = drainage_intensity,
      Drainage_Texture = drainage_texture,
      Infiltration_Number = infiltration_number,
      Orographic_Coefficient = orographic_coefficient,
      Massivity_Index_m_per_m2 = massivity_index,
      Length_Overland_Flow_m = length_overland_flow_m,
      Constant_Channel_Maintenance_m2_per_m = constant_channel_maintenance,
      Stream_Order = max_stream_order,
      Bifurcation_Ratio = bifurcation_ratio,
      Main_Channel_Length_m = main_channel_length_m,
      Main_Channel_Gradient_m_per_m = main_channel_gradient_m_per_m,
      Main_Channel_Sinuosity = main_channel_sinuosity,
      Main_Channel_Start_Elevation_m = main_channel_start_elevation_m,
      Main_Channel_End_Elevation_m = main_channel_end_elevation_m,
      Total_Channel_Length_m = total_channel_length_m,
      Number_of_Streams = num_streams,
      Ruggedness_Number = ruggedness_number,
      Hypsometric_Integral = hypsometric_integral,
      Junction_Density_junctions_per_m2 = junction_density,
      Mean_Stream_Length_m = mean_stream_length_m,
      Mean_Annual_Rainfall_mm = mean_annual_rainfall_mm,
      Total_Annual_Rainfall_m3 = total_annual_rainfall_m3,
      Mean_NDVI_Dry = mean_ndvi_dry,
      Mean_NDVI_Wet = mean_ndvi_wet,
      Bare_Soil_Pct_Dry = bare_soil_pct_dry,
      Grassland_Pct_Dry = grassland_pct_dry,
      Dense_Veg_Pct_Dry = dense_veg_pct_dry,
      Bare_Soil_Pct_Wet = bare_soil_pct_wet,
      Grassland_Pct_Wet = grassland_pct_wet,
      Dense_Veg_Pct_Wet = dense_veg_pct_wet
    ))
  }
  
  close(fileConn)
  cat("Narrative description saved to", output_narrative, "\n")
  write.csv(morph_results, output_csv, row.names = TRUE)
  cat("Morphometric and NDVI analysis results saved to", output_csv, "\n")
  write.csv(ndvi_stats, output_ndvi_stats, row.names = FALSE)
  cat("NDVI statistics saved to", output_ndvi_stats, "\n")
  cat("Number of catchments analyzed:", nrow(morph_results), "\n")
}

# Main function to process inputs and perform analysis
process_watershed <- function(dem_file, rainfall_file, shp_file, ndvi_dry_file, ndvi_wet_file, epsg_code, threshold_ha = 0.1) {
  # File paths
  dem_path <- file.path(path, dem_file)
  rainfall_path <- file.path(path, rainfall_file)
  ws_path <- file.path(path, shp_file)
  ndvi_dry_path <- file.path(path, ndvi_dry_file)
  ndvi_wet_path <- file.path(path, ndvi_wet_file)
  
  # Verify file existence
  files <- c(dem_file, rainfall_file, shp_file, ndvi_dry_file, ndvi_wet_file)
  for (file in files) {
    if (!file.exists(file.path(path, file))) stop(paste("File not found:", file))
  }
  
  # Load and validate DEM
  dem <- rast(dem_path)
  cat("Processing", dem_file, "\n")
  cat("DEM dimensions:", dim(dem), "Resolution:", res(dem), "\n")
  dem_vals <- values(dem, na.rm = TRUE)
  cat("DEM value range:", range(dem_vals), "\n")
  if (global(dem, "notNA")[1] == 0) stop("DEM has no valid data")
  cat("DEM CRS:", substr(crs(dem), 1, 50), "...\n")
  
  # Load and validate rainfall
  rainfall <- rast(rainfall_path)
  cat("Rainfall dimensions:", dim(rainfall), "\n")
  rainfall_vals <- values(rainfall, na.rm = TRUE)
  cat("Rainfall value range:", range(rainfall_vals), "\n")
  cat("Rainfall CRS:", substr(crs(rainfall), 1, 50), "...\n")
  
  # Load and validate NDVI rasters
  ndvi_dry <- rast(ndvi_dry_path)
  cat("NDVI Dry dimensions:", dim(ndvi_dry), "\n")
  ndvi_dry_vals <- values(ndvi_dry, na.rm = TRUE)
  cat("NDVI Dry value range:", range(ndvi_dry_vals), "\n")
  cat("NDVI Dry CRS:", substr(crs(ndvi_dry), 1, 50), "...\n")
  
  ndvi_wet <- rast(ndvi_wet_path)
  cat("NDVI Wet dimensions:", dim(ndvi_wet), "\n")
  ndvi_wet_vals <- values(ndvi_wet, na.rm = TRUE)
  cat("NDVI Wet value range:", range(ndvi_wet_vals), "\n")
  cat("NDVI Wet CRS:", substr(crs(ndvi_wet), 1, 50), "...\n")
  
  # Load watershed shapefile
  ws <- vect(ws_path)
  cat("Watershed shapefile loaded:", ws_path, "\n")
  cat("Watershed area (km²):", round(expanse(ws, unit = "m") / 1e6, 2), "\n")
  cat("Watershed CRS:", substr(crs(ws), 1, 50), "...\n")
  
  # Reproject watershed if necessary
  if (crs(ws) != crs(dem)) {
    cat("Reprojecting watershed to match DEM CRS...\n")
    ws <- project(ws, crs(dem))
    writeVector(ws, ws_path, overwrite = TRUE)
  }
  
  # Generate flow direction and accumulation
  cat("Generating flow direction and accumulation...\n")
  tryCatch({
    wbt_flow_accumulation_full_workflow(
      dem = dem_path,
      out_dem = file.path(path, "filled_dem.tif"),
      out_pntr = file.path(path, flow_dir_file),
      out_accum = file.path(path, flow_acc_file),
      out_type = "cells",
      verbose = TRUE
    )
  }, error = function(e) {
    cat("wbt_flow_accumulation_full_workflow error:", e$message, "\n")
    stop("Flow accumulation and direction generation failed")
  })
  
  # Load and validate flow direction
  flow_dir <- rast(file.path(path, flow_dir_file))
  cat("Flow direction dimensions:", dim(flow_dir), "\n")
  dir_vals <- values(flow_dir, na.rm = TRUE)
  cat("Flow direction value range:", range(dir_vals), "\n")
  if (global(flow_dir, "notNA")[1] == 0) stop("Flow direction has no valid data")
  
  # Load and validate flow accumulation
  flow_acc <- rast(file.path(path, flow_acc_file))
  cat("Flow accumulation dimensions:", dim(flow_acc), "\n")
  acc_vals <- values(flow_acc, na.rm = TRUE)
  cat("Flow accumulation value range:", range(acc_vals), "\n")
  if (global(flow_acc, "max", na.rm = TRUE)[1] <= 0) stop("Flow accumulation has no valid data")
  
  # Clip rasters to watershed
  cat("Clipping rasters to watershed boundary...\n")
  dem_clipped <- mask(dem, ws)
  flow_acc_clipped <- mask(flow_acc, ws)
  flow_dir_clipped <- mask(flow_dir, ws)
  rainfall_clipped <- resample(rainfall, dem, method = "bilinear")
  rainfall_clipped <- mask(rainfall_clipped, ws)
  ndvi_dry_clipped <- resample(ndvi_dry, dem, method = "bilinear")
  ndvi_dry_clipped <- mask(ndvi_dry_clipped, ws)
  ndvi_wet_clipped <- resample(ndvi_wet, dem, method = "bilinear")
  ndvi_wet_clipped <- mask(ndvi_wet_clipped, ws)
  
  writeRaster(dem_clipped, file.path(path, clipped_dem_file), overwrite = TRUE)
  writeRaster(flow_acc_clipped, file.path(path, clipped_flow_acc_file), overwrite = TRUE)
  writeRaster(flow_dir_clipped, file.path(path, clipped_flow_dir_file), overwrite = TRUE)
  writeRaster(rainfall_clipped, file.path(path, clipped_rainfall_file), overwrite = TRUE)
  writeRaster(ndvi_dry_clipped, file.path(path, "watershed_clipped_ndvi_dry.tif"), overwrite = TRUE)
  writeRaster(ndvi_wet_clipped, file.path(path, "watershed_clipped_ndvi_wet.tif"), overwrite = TRUE)
  
  # Validate clipped flow accumulation
  acc_clipped_vals <- values(flow_acc_clipped, na.rm = TRUE)
  cat("Clipped flow accumulation value range:", range(acc_clipped_vals), "\n")
  if (length(acc_clipped_vals) == 0) stop("Clipped flow accumulation has no valid values")
  
  # Calculate and recommend thresholds
  max_acc <- max(acc_clipped_vals)
  cell_area <- res(dem)[1] * res(dem)[2]
  threshold <- (threshold_ha * 10000) / cell_area
  cat("Using threshold (cells):", threshold, "for", threshold_ha, "hectares\n")
  cat("Recommended thresholds (based on max flow accumulation", max_acc, "cells):\n")
  cat("  Whole watershed: threshold_ha ≈", round((max_acc * 0.9 * cell_area) / 10000, 2), "\n")
  cat("  Sub-catchments: threshold_ha ≈", round((max_acc * 0.5 * cell_area) / 10000, 2), "to", round((max_acc * 0.7 * cell_area) / 10000, 2), "\n")
  cat("  Micro-catchments: threshold_ha ≈ 0.01 to", round((max_acc * 0.1 * cell_area) / 10000, 2), "\n")
  cat("Based on previous runs:\n")
  cat("  threshold_ha = 2000 → 1 catchment (whole watershed)\n")
  cat("  threshold_ha = 1500 → 5 catchments (sub-catchments)\n")
  cat("  threshold_ha = 1000 → 11 catchments (sub-catchments)\n")
  cat("  threshold_ha = 0.1 → ~778 catchments (micro-catchments)\n")
  if (threshold > max_acc) {
    cat("Warning: Threshold (", threshold, ") exceeds max flow accumulation (", max_acc, "); adjusting to 90% of max\n")
    threshold <- max_acc * 0.9
    threshold_ha <- (threshold * cell_area) / 10000
    cat("Adjusted threshold (cells):", threshold, "(", threshold_ha, "hectares)\n")
  }
  
  # Extract streams
  cat("Extracting streams...\n")
  tryCatch({
    wbt_extract_streams(
      flow_accum = file.path(path, clipped_flow_acc_file),
      output = file.path(path, streams_file),
      threshold = threshold,
      verbose = TRUE
    )
  }, error = function(e) {
    cat("wbt_extract_streams error for threshold", threshold, ":", e$message, "\n")
    stop("Stream extraction failed")
  })
  
  # Validate streams
  streams <- rast(file.path(path, streams_file))
  cat("Streams dimensions:", dim(streams), "\n")
  stream_vals <- values(streams, na.rm = TRUE)
  cat("Streams value range:", range(stream_vals), "\n")
  if (length(stream_vals) == 0 || all(stream_vals == 0)) {
    stop("No valid streams extracted; try lowering threshold_ha or checking flow accumulation")
  }
  
  # Generate stream order
  cat("Generating stream order...\n")
  tryCatch({
    wbt_strahler_stream_order(
      d8_pntr = file.path(path, clipped_flow_dir_file),
      streams = file.path(path, streams_file),
      output = file.path(path, stream_order_file),
      verbose = TRUE
    )
  }, error = function(e) {
    cat("wbt_strahler_stream_order error:", e$message, "\n")
    stop("Stream order generation failed")
  })
  
  # Validate stream order
  stream_order_available <- file.exists(file.path(path, stream_order_file))
  if (stream_order_available) {
    stream_order <- rast(file.path(path, stream_order_file))
    stream_order_vals <- values(stream_order, na.rm = TRUE)
    cat("Stream order range:", range(stream_order_vals), "\n")
    cat("Stream order counts:", paste(names(table(stream_order_vals)), table(stream_order_vals), sep = "=", collapse = ", "), "\n")
  } else {
    cat("Warning: Stream order file not found; skipping stream order validation\n")
  }
  
  # Compute stream links
  cat("Computing stream links...\n")
  tryCatch({
    wbt_stream_link_identifier(
      d8_pntr = file.path(path, clipped_flow_dir_file),
      streams = file.path(path, streams_file),
      output = file.path(path, stream_links_file),
      verbose = TRUE
    )
  }, error = function(e) {
    cat("wbt_stream_link_identifier error:", e$message, "\n")
    stop("Stream link computation failed")
  })
  
  # Validate stream links
  stream_links <- rast(file.path(path, stream_links_file))
  link_ids <- unique(values(stream_links))
  link_ids <- link_ids[!is.na(link_ids)]
  cat("Number of stream links:", length(link_ids), "\n")
  if (length(link_ids) == 0) stop("No stream links identified")
  
  # Find pour points at watershed outlets
  cat("Identifying pour points...\n")
  ws_raster <- rasterize(ws, stream_links, background = 0)
  boundary_cells <- which(values(ws_raster) == 0 & !is.na(values(stream_links)))
  
  pour_points <- lapply(link_ids, function(id) {
    mask <- stream_links == id
    acc_masked <- mask(flow_acc_clipped, mask, maskvalue = 0)
    max_val <- values(acc_masked, na.rm = TRUE)
    if (length(max_val) == 0 || all(is.na(max_val))) return(NULL)
    max_cells <- which(values(acc_masked) == max(max_val, na.rm = TRUE))
    coords_list <- lapply(max_cells, function(cell) xyFromCell(stream_links, cell))
    for (coords in coords_list) {
      cell_idx <- cellFromXY(stream_links, coords)
      if (cell_idx %in% boundary_cells) {
        return(list(coords = coords, link_id = id))
      }
    }
    list(coords = coords_list[[1]], link_id = id)
  })
  
  pour_points <- pour_points[!sapply(pour_points, is.null)]
  if (length(pour_points) == 0) {
    cat("No pour points at boundary; using all maxima...\n")
    pour_points <- lapply(link_ids, function(id) {
      mask <- stream_links == id
      acc_masked <- mask(flow_acc_clipped, mask, maskvalue = 0)
      max_val <- values(acc_masked, na.rm = TRUE)
      if (length(max_val) == 0 || all(is.na(max_val))) return(NULL)
      max_cell <- which.max(max_val)
      coords <- xyFromCell(stream_links, max_cell)
      list(coords = coords, link_id = id)
    })
    pour_points <- pour_points[!sapply(pour_points, is.null)]
    if (length(pour_points) == 0) stop("No valid pour points found")
  }
  
  # Save pour points
  pour_points_coords <- do.call(rbind, lapply(pour_points, function(pt) {
    data.frame(
      x = pt$coords[1],
      y = pt$coords[2],
      threshold_ha = threshold_ha,
      stream_link_id = pt$link_id
    )
  }))
  
  pour_points_vec <- do.call(rbind, lapply(pour_points, function(pt) pt$coords))
  pour_points_vec <- unique(pour_points_vec)
  pour_points_spat <- vect(pour_points_vec, type = "points", crs = epsg_code)
  writeVector(pour_points_spat, file.path(path, pour_points_file), overwrite = TRUE)
  write.csv(pour_points_coords, file.path(path, pour_points_csv), row.names = FALSE)
  cat("Pour points saved to", pour_points_csv, "\n")
  cat("Number of pour points:", nrow(pour_points_coords), "\n")
  
  # Delineate catchments
  cat("Delineating catchments...\n")
  tryCatch({
    wbt_watershed(
      d8_pntr = file.path(path, clipped_flow_dir_file),
      pour_pts = file.path(path, pour_points_file),
      output = file.path(path, catchments_file),
      verbose = TRUE
    )
  }, error = function(e) {
    cat("wbt_watershed error:", e$message, "\n")
    stop("Catchment delineation failed")
  })
  
  # Validate catchments
  catchments <- rast(file.path(path, catchments_file))
  cat("Catchments raster saved to", catchments_file, "\n")
  num_catchments <- length(unique(values(catchments), na.rm = TRUE)) - 1
  cat("Number of catchments:", num_catchments, "\n")
  
  # Perform morphometric analysis
  cat("Starting morphometric and NDVI analysis...\n")
  morphometric_analysis(
    file.path(path, catchments_file),
    file.path(path, clipped_dem_file),
    file.path(path, streams_file),
    file.path(path, stream_links_file),
    file.path(path, stream_order_file),
    file.path(path, clipped_rainfall_file),
    file.path(path, "watershed_clipped_ndvi_dry.tif"),
    file.path(path, "watershed_clipped_ndvi_wet.tif"),
    file.path(path, morph_results_csv),
    file.path(path, catchments_shp),
    file.path(path, morph_narrative_txt),
    file.path(path, ndvi_stats_csv),
    file.path(path, land_cover_dry_file),
    file.path(path, land_cover_wet_file)
  )
}

# Run analysis with user-specified threshold (in hectares)
cat("Starting watershed analysis for watershed Watershed...\n")
process_watershed(
  dem_file = dem_file,
  rainfall_file = rainfall_file,
  shp_file = shp_file,
  ndvi_dry_file = ndvi_dry_file,
  ndvi_wet_file = ndvi_wet_file,
  epsg_code = epsg_code,
  threshold_ha = threshold_ha
)
cat("Analysis complete!\n")
