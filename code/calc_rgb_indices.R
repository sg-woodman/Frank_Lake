# Details -----------------------------------------------------------------
## Script name: calc_rgb_indices
##
## Purpose of script: Generate a variety of rgb indices for the Frank Lake
## Flux Tower drone imagery. These indices may provide a better approach to
## classifying vegetation
##
## Author: Sam Woodman
##
## Date Created: 2022-11-15
##
## Email: samuel.woodman@gmail.com
##
## Inputs:
##    raster_cropped_30cm - GeoTIFF of the flux tower site that has been cropped to
##                          a 200 m radius around the flux tower and aggregated
##                          to 30 cm resolution
##
## Outputs:
##    ft_rgb_indices - GeoTIFF of various rgb indices produced by the uavRst
##                     package
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

## Cropped raster
masked_raster_cropped_30cm <- stack(here("data/processed/masked_cropped_raster_30cm.tif"))


# Calculate indices -------------------------------------------------------
tictoc::tic()
ft_rgb_indices <- rgb_indices(red   = masked_raster_cropped_30cm[[1]], # red band
                              green = masked_raster_cropped_30cm[[2]], # blue band
                              blue  = masked_raster_cropped_30cm[[3]], # green band
                              # all indices provided by the uavRst package
                              rgbi = c("VVI", "VARI", "NDTI", "RI", "SCI",
                                       "BI", "SI", "HI", "TGI", "GLI",
                                       "NGRDI", "GLAI", "CI", "SAT", "SHP"))
tictoc::toc()

ft_rgb_indices <- rast(ft_rgb_indices)

# Visualize ---------------------------------------------------------------

plotRGB(masked_raster_cropped_30cm)

plot(ft_rgb_indices[[1]]) #VVI
plot(ft_rgb_indices[[2]]) #VARI
plot(ft_rgb_indices[[3]]) #NDTI
plot(ft_rgb_indices[[4]]) #RI
plot(ft_rgb_indices[[5]]) #SCI
plot(ft_rgb_indices[[6]]) #BI
plot(ft_rgb_indices[[7]]) #SI
plot(ft_rgb_indices[[8]]) #HI
plot(ft_rgb_indices[[9]]) #TGI
plot(ft_rgb_indices[[10]]) #GLI
plot(ft_rgb_indices[[11]]) #NGRDI
plot(ft_rgb_indices[[12]]) #GLAI
plot(ft_rgb_indices[[13]]) #CI
plot(ft_rgb_indices[[14]]) #SAT
plot(ft_rgb_indices[[15]]) #SHP

# Save output -------------------------------------------------------------

terra::writeRaster(ft_rgb_indices,
                    here("data/processed/ft_rgb_indices.tif"),
                    overwrite = T)

