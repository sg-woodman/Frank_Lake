# Details -----------------------------------------------------------------
## Script name: combine_cart_manual_classes
##
## Purpose of script: Due to the compications of the mixed vegetation make-up of
## island the cart model does not accurately classify the species present.
## Manual delineation works better so the two methods are combined to create a
## raster where the island was manually classified and the surrounding
## vegetation is classified by the cart model.
##
## Author: Sam Woodman
##
## Date Created: 2022-12-15
##
## Email: samuel.woodman@gmail.com
##
## Inputs:
##    - ml_class_no_island: GeoTIFF of vegetation classifications using the
##                          rpart package. Island pixels were excluded.
##    - lc_class_manual_raster: GeoTIFF of manually digitized vegetation classes
##    - ft_150: GPKG of 150 metre radius surrounding the flux tower to ensure
##              rasters have some extent.
##
## Outputs:
##    - cart_manual_class:
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
library(terra)

# Load data ---------------------------------------------------------------

## Vector
ft_150 <- vect(here("data/processed/150m_circle.gpkg"))

## Raster
### crop and mask to set identical extents
ml_class_no_island <- rast(here("data/processed/ml_class_no_island.tif")) %>%
  mask(ft_150) %>%
  crop(ft_150)
lc_class_manual_raster <- rast(here("data/processed/lc_class_manual_raster.tif")) %>%
  mask(ft_150) %>%
  crop(ft_150)

## Visualize
plot(ml_class_no_island)
plot(lc_class_manual_raster)

# Process -----------------------------------------------------------------

## Create raster for masking island (island = 3)
### Keep only the island to add to ml_class
lc_island_only <- classify(lc_class_manual_raster, # input raster
                           cbind(3, 4), # set 3 to 4 for merging
                           others = NA) # set all other values to NA



### Remove island for masking ml_class
lc_island_mask <- classify(lc_class_manual_raster, # input raster
                           cbind(3, NA)) # set 3 to NA


## Combine CART and manual
cart_manual_class <- ml_class_no_island %>%
  # set factors to numeric (values 1 to 3)
  as.numeric() %>%
  # mask the island by setting to NA
  mask(., lc_island_mask) %>%
  # add manually delineated island with value of 4
  merge(lc_island_only)

## Set back to named factors
levels(cart_manual_class) <- data.frame(id=1:4,
                                        class = c("barren", "bulrush",
                                                 "hordeum", "island"))

### Visualize
plot(cart_manual_class)

# Save output -------------------------------------------------------------

writeRaster(cart_manual_class, here("data/processed/cart_manual_class.tif"),
            overwrite = TRUE)
