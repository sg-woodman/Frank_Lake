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
library(buffeRs)

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

create_buffer <- function(pnt, dist) {
  ## create a polygon with a given radius from the center
  ## inputs:
  ##    - pnt = center point spatial object
  ##    - dist = distance from center
  ## outputs:
  ##    - sf object
  st_buffer(pnt, dist) %>%
    mutate(radius = paste0("dist_", dist))
}

ft_25 <- create_buffer(ft_pnt, 25)
ft_50 <- create_buffer(ft_pnt, 50)
ft_75 <- create_buffer(ft_pnt, 75)
ft_100 <- create_buffer(ft_pnt, 100)
ft_125 <- create_buffer(ft_pnt, 125)
ft_150 <- create_buffer(ft_pnt, 150)

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

## Create wedge shaped buffers

### Function assumes 0° is at the 12 o'clock position
### Wedges are created such that the degree width is split evenly on either
### side of the degree argument
###
### Helper functions
#### Name wedges according to angle, 0 at 12 o'clock position
name_wedge <- function(df, x) {
  df$angle = paste0(x)
  return(df)
}
#### Sequence used to create names for wedges based on angle
nam <- sprintf("angle_%g", seq(0, 360-22.5, 22.5)) %>%
  as.list()
#### Rename geometry colimn as geometry
rename_geom <- function(df) {
  df %>%
    rename(geometry = x)
}

wedge_buff <-
  # sequence of degrees of interest
  seq(11.25, 360, 22.5) %>%
  # convert to list
  as.list() %>%
  # apply over the various degrees specified above
  map(~buffer_wedge(ft_pnt,
                    radius  = 150,
                    degree = .x,
                    degree_width = 11.25)) %>%
  # convert to sf data frame class
  map(st_as_sf) %>%
  map(st_cast) %>%
  map2(., nam, name_wedge) %>%
  map(rename_geom)


## Create individual sections

clip_wedge_polygons <- function(wedge) {

  ## clip circular buffers to with each wedge
  ## inputs:
  ##    - wedge = wedge created using buffeRs
  ## outputs:
  ##    - individual sections of each wedge

  # clip circular buffers using wedge
  rad150 <- st_intersection(ft_150, wedge)
  rad125 <- st_intersection(ft_125, wedge)
  rad100 <- st_intersection(ft_100, wedge)
  rad75 <- st_intersection(ft_75, wedge)
  rad50 <- st_intersection(ft_50, wedge)
  rad25 <- st_intersection(ft_25, wedge)

  # progressively remove innermost section of wedge
  ## need to use buffered circles not smaller radii to avoid clipping error
  sec150 <- st_difference(rad150, ft_125)
  sec125 <- st_difference(rad125, ft_100)
  sec100 <- st_difference(rad100, ft_75)
  sec75 <- st_difference(rad75, ft_50)
  sec50 <- st_difference(rad50, ft_25)
  sec25 <- rad25

  out <- bind_rows(sec150, sec125, sec100, sec75, sec50, sec25) %>%
    mutate(id = paste0(angle, "_", radius))

  return(out)
}

wedge_sections <- wedge_buff %>%
  map(clip_wedge_polygons) %>%
  reduce(bind_rows) %>%
  mutate(area_m2 = as.numeric(st_area(.)))


# Save outputs ------------------------------------------------------------

st_write(ft_150, here("data/processed/150m_circle.gpkg"))
st_write(wedge_sections, here("data/processed/flux_tower_sections.gpkg"))


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
  geom_sf(data = test99[[1]], colour = "green") +
  geom_sf(data = test99[[5]], colour = "yellow") +
  coord_sf(datum = st_crs(ft_150))

