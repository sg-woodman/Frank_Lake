# Details -----------------------------------------------------------------
## Script name: mask_non_veg
##
## Purpose of script: The flux tower site has several non-vegetative features
## that complicate image classification. Notably, these include a boardwalk to
## access the site, a path for ATV vehicle access, and the various instruments
## for measurement. Preliminary attempts at image classification showed that
## these features reduced the model accuaracy. Since they are not of interest
## for calculating vegetation proportion they need to be removed from the
## raster image
##
## Author: Sam Woodman
##
## Date Created: 2022-11-29
##
## Email: samuel.woodman@gmail.com
##
## Inputs:
##    - cropped_raster_30cm: GeoTIFF of cropped raster aggregated to 30 cm res
##    - boardwalk: GPKG of manually digitized boardwalk
##    - intruments: GPKG of manually digitized intruments
##    - path: GPKG of manually digitized vehicle path
##
## Outputs:
##    - masked_cropped_raster_30cm
##
##
## Notes:
##   - Masking is done on the aggregated 30 cm data since this image is what was
##   used to manually digitize the features.
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

## Raster
cropped_raster_30cm <- rast(here("data/processed/cropped_raster_30cm.tif"))

## Vectors
boardwalk <- vect(here("data/raw/boardwalk.gpkg"))
instruments <- vect(here("data/raw/instruments.gpkg"))
path <- vect(here("data/raw/path.gpkg"))


# Rasterize vectors -------------------------------------------------------

## Boardwalk
boardwalk$val <- 0

boardwalk_rast <- rasterize(boardwalk, cropped_raster_30cm,
                            "val", background = 1, sum = F)
boardwalk_rast[boardwalk_rast == 0] <- NA

## Instruments
instruments$val <- 0

instruments_rast <- rasterize(instruments, cropped_raster_30cm, "val",
                         background = 1, sum = F)
instruments_rast[instruments_rast == 0] <- NA

## Path
path$val <- 0

path_rast <- rasterize(path, cropped_raster_30cm, "val",
                         background = 1, sum = F)
path_rast[path_rast == 0] <- NA

# Mask raster -------------------------------------------------------------

masked_cropped_raster_30cm <- cropped_raster_30cm %>%
  # mask values
  terra::mask(., boardwalk_rast) %>%
  terra::mask(., instruments_rast) %>%
  terra::mask(., path_rast)


# Visualize output --------------------------------------------------------

plot(masked_cropped_raster_30cm)

# Save output -------------------------------------------------------------

writeRaster(masked_cropped_raster_30cm, here("data/processed/masked_cropped_raster_30cm.tif"))
