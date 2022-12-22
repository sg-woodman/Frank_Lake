# Details -----------------------------------------------------------------
## Script name: extract_land_cover
##
## Purpose of script: Extract pixels that fall within the radial sections based
## on the various methods of land cover classifications
##
## Author: Sam Woodman
##
## Date Created: 2022-12-07
##
## Email: samuel.woodman@gmail.com
##
## Inputs:
##    wedge_sections - GPKG of 22.5° intervals wedges at 25 m increments from
##                     from the flux tower
##
##    lc_class_manual_raster - GeoTIFF of manually digitized land cover types
##
##    ml_class - GeoTIFF of land cover classified by CART model. The
##               central island, which is a mix of ??Puccinellia?? and Hordeum
##               is included in the classification
##
##    ml_class_manual - GeoTIFF with the island pixels set using the manually
##                      delineated polygons and the surround vegetation
##                      classified according the island excluded CART model.
##
##
##
##
## Outputs:
##
##
## Notes:
##    - Since the island is a combination is a mixture of species the CART
##     model has higher error for this section. The island is visually distinct
##     so combining the two methods produces a potentially more accurate output.
##     Furthermore, the flux tower has a "dead zone" immediately surround the
##     tower. This area completely contains the island so a less precise
##     manually vegetation estimate is a safe assumption.
##
##
# Options -----------------------------------------------------------------

options(scipen = 6, digits = 4)


# Packages ----------------------------------------------------------------

library(tidyverse)
library(here)
library(sf)
library(terra)

# Load data ---------------------------------------------------------------

## Vector

wedge_sections <- vect(here("data/processed/flux_tower_sections.gpkg"))

## Raster

lc_class_manual_raster <- rast(here("data/processed/lc_class_manual_raster.tif"))
ml_class_island <- rast(here("data/processed/ml_class_island.tif"))
cart_manual_class <- rast(here("data/processed/cart_manual_class.tif"))

## Constants
cell_size_m <- 0.2375003

# Process -----------------------------------------------------------------

## calc area of each wedge section
wedge_sections %>%
  st_as_sf() %>%
  mutate(area_m2 = as.numeric(st_area(.)))

## Manual delineation
### extract number of pixels in each wedge section with additional column
### specifying the weight (i.e. proportion of pixel within the section)
lc_manual_extract <- terra::extract(lc_class_manual_raster,
                                    wedge_sections,
                                    ID = T, weights = T) %>%
  # convert to data frame
  as.data.frame() %>%
  # join wedge section areas using the ID column as guide
  left_join(., wedge_sections %>%
              as.data.frame() %>%
              mutate(ID = row_number())) %>%
  # convert classes to named factors
  dplyr::mutate(class = case_when(name == 0 ~ "barren",
                                  name == 1 ~ "bulrush",
                                  name == 2 ~ "hordeum",
                                  name == 3 ~ "island",
                                  name == 4 ~ "boardwalk")) %>%
  # calc area of each class as function of amount of cell in section
  mutate(area_m2 = weight*(cell_size_m*cell_size_m)) %>%
  # sum within class and section id
  group_by(id, class) %>%
  summarise(class_area_m2 = sum(area_m2)) %>%
  # remove boardwalk and NA
  filter(class != "boardwalk") %>%
  filter(!is.na(class)) %>%
  # calc total area and prop of each class
  mutate(total_area_m2 = sum(class_area_m2),
         prop_area = class_area_m2/total_area_m2,
         method = "manual")

## CART model
### extract number of pixels in each wedge section with additional column
### specifying the weight (i.e. proportion of pixel within the section)
ml_island_extract <- terra::extract(ml_class_island,
                                    wedge_sections,
                                    ID = T, weights = T) %>%
  # convert to data frame
  as.data.frame() %>%
  # join wedge section areas using the ID column as guide
  left_join(., wedge_sections %>%
              as.data.frame() %>%
              mutate(ID = row_number())) %>%
  # calc area of each class as function of amount of cell in section
  mutate(area_m2 = weight*(cell_size_m*cell_size_m)) %>%
  # sum within class and section id
  group_by(id, class) %>%
  summarise(class_area_m2 = sum(area_m2)) %>%
  # remove NA
  filter(!is.na(class)) %>%
  # calc total area and prop of each class
  mutate(total_area_m2 = sum(class_area_m2),
         prop_area = class_area_m2/total_area_m2,
         method = "cart")

## CART + manual island
### extract number of pixels in each wedge section with additional column
### specifying the weight (i.e. proportion of pixel within the section)
cart_manual_extract <- terra::extract(cart_manual_class,
                                       wedge_sections,
                                       ID = T, weights = T) %>%
  as.data.frame() %>%
  # join wedge section areas using the ID column as guide
  left_join(., wedge_sections %>%
              as.data.frame() %>%
              mutate(ID = row_number())) %>%
  # calc area of each class as function of amount of cell in section
  mutate(area_m2 = weight*(cell_size_m*cell_size_m)) %>%
  # sum within class and section id
  group_by(id, class) %>%
  summarise(class_area_m2 = sum(area_m2)) %>%
  # remove NA
  filter(!is.na(class)) %>%
  # calc total area and prop of each class
  mutate(total_area_m2 = sum(class_area_m2),
         prop_area = class_area_m2/total_area_m2,
         method = "cart_manual")

## Combine dfs
df <- bind_rows(lc_manual_extract,
          ml_island_extract,
          cart_manual_extract)

# Save output -------------------------------------------------------------


write_csv(df, here("output/ft_radial_veg_composition.csv"))
