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
##    ml_class_island - GeoTIFF of land cover classified by CART model. The
##                      central island, which is a mix of Bulrush and Hordeum
##                      is included in the classification
##
##    ml_class_no_island - GeoTIFF of land cover classified by CART model. The
##                         island is not included in the classification which
##                         allows for island vegetation to be classified as
##                         Bulrush or Hordeum individually.
##
##
##
##
## Outputs:
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
library(terra)
library(raster)
library(exactextractr)

# Load data ---------------------------------------------------------------

## Vector

wedge_sections <- vect(here("data/processed/flux_tower_sections.gpkg"))

## Raster

lc_class_manual_raster <- rast(here("data/processed/lc_class_manual_raster.tif"))
ml_class_island <- rast(here("data/processed/ml_class_island.tif"))
ml_class_no_island <- rast(here("data/processed/ml_class_no_island.tif"))

## Constants
cell_size_m <- 0.2375003

# Process -----------------------------------------------------------------

wedge_sections %>%
  st_as_sf() %>%
  mutate(area_m2 = as.numeric(st_area(.)))

lc_manual_extract <- terra::extract(lc_class_manual_raster,
                                    wedge_sections,
                                    ID = T, weights = T) %>%
  as.data.frame() %>%
  left_join(., wedge_sections %>%
              as.data.frame() %>%
              mutate(ID = row_number())) %>%
  dplyr::mutate(class = case_when(name == 0 ~ "barren",
                                  name == 1 ~ "bulrush",
                                  name == 2 ~ "hordeum",
                                  name == 3 ~ "island",
                                  name == 4 ~ "boardwalk")) %>%
  mutate(area_m2 = weight*(cell_size_m*cell_size_m)) %>%
  group_by(id, class) %>%
  summarise(class_area_m2 = sum(area_m2)) %>%
  filter(class != "boardwalk") %>%
  filter(!is.na(class)) %>%
  mutate(total_area_m2 = sum(class_area_m2),
         prop_area = class_area_m2/total_area_m2,
         method = "manual")

ml_island_extract <- terra::extract(ml_class_island,
                                    wedge_sections,
                                    ID = T, weights = T) %>%
  as.data.frame() %>%
  left_join(., wedge_sections %>%
              as.data.frame() %>%
              mutate(ID = row_number())) %>%
  mutate(area_m2 = weight*(cell_size_m*cell_size_m)) %>%
  group_by(id, class) %>%
  summarise(class_area_m2 = sum(area_m2)) %>%
  filter(!is.na(class)) %>%
  mutate(total_area_m2 = sum(class_area_m2),
         prop_area = class_area_m2/total_area_m2,
         method = "ml_island")

ml_no_island_extract <- terra::extract(ml_class_no_island,
                                       wedge_sections,
                                       ID = T, weights = T) %>%
  as.data.frame() %>%
  left_join(., wedge_sections %>%
              as.data.frame() %>%
              mutate(ID = row_number())) %>%
  mutate(area_m2 = weight*(cell_size_m*cell_size_m)) %>%
  group_by(id, class) %>%
  summarise(class_area_m2 = sum(area_m2)) %>%
  filter(!is.na(class)) %>%
  mutate(total_area_m2 = sum(class_area_m2),
         prop_area = class_area_m2/total_area_m2,
         method = "ml_noi_island")


bind_rows(lc_manual_extract,
          ml_island_extract,
          ml_no_island_extract) %>% view()

