# Script Name: 03a-Project_Settings.R
# Purpose: Set project settings and allows user to manually input threshold level and EPSG code.

# Additional info: Threshold levels controls the number of sub-catchments that will be automatically derived using Script 03

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

# RUN this script before Script 03-Watershed_Analysis.R

# Function to get numeric input from user
get_numeric_input <- function(prompt) {
  repeat {
    cat(prompt)
    input <- readline()
    if (!is.na(as.numeric(input))) {
      return(as.numeric(input))
    } else {
      cat("Invalid input. Please enter a number.\n")
    }
  }
}

# Function to get EPSG code from user
get_epsg_code <- function() {
  repeat {
    epsg_code <- readline(prompt = "Enter EPSG code (e.g., EPSG:32751): ")
    if (grepl("^EPSG:[0-9]+$", epsg_code)) {
      return(epsg_code)
    } else {
      cat("Invalid EPSG code format. Please enter in the format EPSG:XXXXX.\n")
    }
  }
}

# Function to set threshold level and EPSG code
set_project_settings <- function() {
  threshold_ha <- get_numeric_input("Enter new threshold level (in hectares): ")
  saveRDS(threshold_ha, "threshold_ha.RDS")
  cat("Threshold level set to", threshold_ha, "hectares.\n")
  epsg_code <- get_epsg_code()
  saveRDS(epsg_code, "epsg_code.RDS")
  cat("EPSG code set to", epsg_code, "\n")
}

# Call the function
set_project_settings()
