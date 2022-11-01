# Details -----------------------------------------------------------------

## Script name: create_sections
##
## Purpose of script: Using the Frank Lake flux tower as the center point this
## script aims to generate a set of sections that will be used to determine
## vegetation composition. Sections will be determined by the angle and distance
## from the flux tower. Distances are set at 25 m increments up to 150 m while
## angles will be set at 22.5° intervals.
##
## Author: Sam Woodman
##
## Date Created: 2022-10-27
##
## Email: samuel.woodman@gmail.com
##
## Inputs:
##    flux_tower.gpkg - point file with coordinates of flux tower, created
##    in QGIS by visually identifying flux tower from drone imagery
##
## Outputs:
##    sections for each distance and angle.
##
## Notes:
##   Coordiantes of each bisecting line can be calculated using methods
##   described in the links below.
##   https://stackoverflow.com/questions/43641798/how-to-find-x-and-y-coordinates-on-a-flipped-circle-using-javascript-methods
##   https://math.stackexchange.com/questions/676249/calculate-x-y-positions-in-circle-every-n-degrees
##   https://stackoverflow.com/questions/839899/how-do-i-calculate-a-point-on-a-circle-s-circumference/839931#839931

# Options -----------------------------------------------------------------

options(scipen = 6, digits = 4)


# Packages ----------------------------------------------------------------

library(tidyverse)
library(here)
library(sf)
library(measurements)

# Load data ---------------------------------------------------------------

## GPS coordinates from field
### FIXME does not work, coordinates do no match drone imagery
ft_gps <- tibble(latitude = "50 31 20.9", # N
               longitude = "-113 41 04.47") # W

# Convert from DMS to decimal degrees
lat <- conv_unit(ft_gps$latitude, from = "deg_min_sec", to = "dec_deg")
long <- conv_unit(ft_gps$longitude , from = "deg_min_sec", to = "dec_deg")

## Create spatial dataframe for flux tower
ft_coord_gps <- tibble(name = "flux tower",
             latitude = as.numeric(lat),
             longitude = as.numeric(long)) %>%
  st_as_sf(., coords = c("longitude", "latitude"),
           crs = 4326, agr = "constant") %>%
  st_transform(crs = 2956)

## Coordinates manually selected from drone imagery

ft_coord_drone <- st_read(here("data/raw/flux_tower.gpkg"))

ft_pnt <- ft_coord_drone

# Process -----------------------------------------------------------------

## Create circles increasing 25 m increments around the flux tower
### Buffer function create a polygon with the distance argument determining the
### radius of the circle.
ft_25 <- st_buffer(ft_pnt, 25)
ft_50 <- st_buffer(ft_pnt, 50)
ft_75 <- st_buffer(ft_pnt, 75)
ft_100 <- st_buffer(ft_pnt, 100)
ft_125 <- st_buffer(ft_pnt, 125)
ft_150 <- st_buffer(ft_pnt, 150)

## Create a dataframe of coordinates
point_df <-
  # convert sf point object to x,y coordinates
  st_coordinates(ft_pnt) %>%
  # convert to dataframe
  as.data.frame() %>%
  # rename columns
  rename(center_x = X,
         center_y = Y) %>%
  # create dataframe from all combinations of inputs
  expand_grid(.,
              # TODO remove points along inner circles since they are unnecessary
              # degrees at 22.5 degree increments
              degrees = seq(0, 337.5, 22.5),
              # points at 150 m, furthest from flux tower
              radius = 150) %>%
  # calculate new variables
  mutate(
    # convert degrees to radians
    radians = degrees*pi/180,
    # calculate coordinate on a circle of given distance and angle from
    # the center, see notes for methods.
    longitude = center_x + radius * cos(radians),
    latitude = center_y + radius * sin(radians),
    # create ID colum
    id = paste0(degrees, "_", radius)) %>%
  # move ID column first position in DF
  relocate(id)


circle_coords <- st_as_sf(point_df, coords = c("longitude", "latitude"),
                 crs = 2956, agr = "constant")

# TODO create lines from center to outer (150 m) points



# TODO test clipping polygons by lnes

# Visualize ---------------------------------------------------------------

ggplot() +
  geom_sf(data = ft_150) +
  geom_sf(data = ft_125) +
  geom_sf(data = ft_100) +
  geom_sf(data = ft_75) +
  geom_sf(data = ft_50) +
  geom_sf(data = ft_25) +
  geom_sf(data = ft_pnt) +
  geom_sf(data = circle_coords, colour = "red") +
  geom_sf(data = ft_lines, colour = "blue") +
  coord_sf(datum = st_crs(ft_150))

