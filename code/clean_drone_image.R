# Details -----------------------------------------------------------------
## Script name: clean_drone_image
##
## Purpose of script: Clean the image collected using the DJI drone to reduce
## the file size and select only the bands of interest
##
## Author: Sam Woodman
##
## Date Created: 2022-11-03
##
## Email: samuel.woodman@gmail.com
##
## Inputs:
##    ft_rast - GeoTIFF of the area surrounding the flux tower
##
## Outputs:
##    clean_ft_rast - cleaned raster with values outside
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
library(uavRst)

# Load data ---------------------------------------------------------------

## Raster data
ft_rast <- rast("/Volumes/Transcend/Frank3EFull_Orthomosaic_FriOct14183254418664/Frank3EFull_Orthomosaic_export_FriOct14183254418664.tif") %>%
  subset(., 1:3)

ft_raster <- stack("/Volumes/Transcend/Frank3EFull_Orthomosaic_FriOct14183254418664/Frank3EFull_Orthomosaic_export_FriOct14183254418664.tif")
ft_raster <- dropLayer(ft_raster, 4)

## Vector data
ft_150 <- vect(here("data/processed/150m_circle.gpkg"))



# Rasterize vectors -------------------------------------------------------

ft_150$val <- 1

ft_150_rast <- rasterize(ft_150, ft_rast, "val", background = NA, sum = F) %>%
  crop(., ft_150)

# Mask raster -------------------------------------------------------------

test <- ft_rast %>%
  terra::crop(., ft_150) %>%
  # mask values outside aou
  terra::mask(., ft_150_rast)
