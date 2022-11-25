# Details -----------------------------------------------------------------
## Script name: aggregate_raster
##
## Purpose of script: The original raster image produced by the drone is at 3
## cm resolution which means it contains over 1 billion pixels. So many pixels
## are difficult to work with therefore aggregating the raster to 30 cm may
## aid in testing possible methods.
##
## Author: Sam Woodman
##
## Date Created: 2022-11-25
##
## Email: samuel.woodman@gmail.com
##
## Inputs:
##    cropped_raster - GeoTIFF of drane imagery cropped to 200 m radius around
##                     the flux tower
##
## Outputs:
##    cropped_raster_30cm - GeoTIFF of cropped raster aggregated to 30 cm res
##
##
## Notes:
##
##
# Options -----------------------------------------------------------------

options(scipen = 6, digits = 4)

# Packages ----------------------------------------------------------------

library(tidyverse)
library(here)
library(sf)
library(raster)
library(terra)

# Load data ---------------------------------------------------------------

cropped_raster <- rast(here("data/processed/cropped_raster.tif"))

# Aggregate ---------------------------------------------------------------

cropped_raster_30cm <- terra::aggregate(cropped_raster, 10, fun = mean)

# Save output -------------------------------------------------------------

writeRaster(cropped_raster_30cm, here("data/processed/cropped_raster_30cm.tif"))

# Visualize ---------------------------------------------------------------

plot(cropped_raster_30cm)
plotRGB(cropped_raster_30cm)
