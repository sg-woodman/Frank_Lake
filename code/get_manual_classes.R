# Details -----------------------------------------------------------------
## Script name: get_manual_classes
##
## Purpose of script: Vegetation categories can also be determined manually
## through digitizing. Since Bulrush is the dominant category, it it easier to
## digitize all other veg types and set the inverse as Bulrush
##
## Author: Sam Woodman
##
## Date Created: 2022-12-07
##
## Email: samuel.woodman@gmail.com
##
## Inputs:
##    lc_digitized_snapped - GPKG of manually digitized Hordeum, Island, Barren
##                           and Path pixels. "Snap geometries to layer"
##                           function in QGIS was used to with 1 metre tolerance
##                           fill small gaps between adjacent classifications
##
##    ft_150 - GPKG of circle with 150 metre radius from the flux tower
##
##    cropped_raster_30cm - GeoTIFF of the flux tower site that has been cropped
##                          to a 200 m radius around the flux tower and
##                          aggregated to 30 cm resolution
##
## Outputs:
##    lc_class_manual - GPKG of all features classified by digitizing
##
##    lc_class_manual_raster - rasterized manual classifications at 30 cm
##                             resolution
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

# Load data ---------------------------------------------------------------

## Vectors
lc_digitized_snapped <- st_read(here("data/processed/lc_digitized_snapped.gpkg"))

ft_150 <- st_read(here("data/processed/150m_circle.gpkg"))

## Raster
raster_cropped_30cm <- rast(here("data/processed/cropped_raster_30cm.tif"))


# Process -----------------------------------------------------------------

## Get bulrush polygone
### define bulrush as the opposite of all other classes
### unite all other classes first to ensure complete opposite is generated
bulruch_manual <- st_difference(ft_150, st_union(lc_digitized_snapped)) %>%
  # create name to match lc_digitized_snapped
  mutate(name = "Bulrush") %>%
  # remove unnecessary columns
  select(-radius)

full_lc_class <- bind_rows(lc_digitized_snapped, bulruch_manual)

## Rasterize manual classes

full_lc_class_rast <- rasterize(full_lc_class, raster_cropped_30cm,
                                field = "name", background = "NA") %>%
  crop(ft_150)


# Visualize ---------------------------------------------------------------

ggplot() +
  geom_sf(data = full_lc_class, aes(fill = name))

plot(full_lc_class_rast)


# Save output  ------------------------------------------------------------

st_write(full_lc_class, here("data/processed/lc_class_manual.gpkg"))

writeRaster(full_lc_class_rast, here("data/processed/lc_class_manual_raster.tif"))
