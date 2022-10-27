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

# Load data ---------------------------------------------------------------

ft_coord <- st_read(here("data/raw/flux_tower.gpkg"))

# Process -----------------------------------------------------------------

## Create circles increasing 25 m increments around the flux tower
### Buffer function create a polygon with the distance argument determining the
### radius of the circle.
ft_25 <- st_buffer(ft_coord, 25)
ft_50 <- st_buffer(ft_coord, 50)
ft_75 <- st_buffer(ft_coord, 75)
ft_100 <- st_buffer(ft_coord, 100)
ft_125 <- st_buffer(ft_coord, 125)
ft_150 <- st_buffer(ft_coord, 150)

## Convert degrees to radians

### Function to convert degrees to radians
#### Input:
####    degrees - units = degrees, class = numeric
#### Output:
####    radians - units = radians, class = numeric
deg2rad <- function(deg) {
  deg*pi/180
}

### Sequence of degrees at 22.5 increments
#### Creates 16 sections
degrees <- seq(0, 360, 22.5)

### Run deg2rad function on the vector of degrees.
radians <- deg2rad(degrees)


## Calculate circle coordinates
### Using the links provided in the notes section a set of coordinates for each
### radius and angle are calculated
### Method assumes 0° is at the 3 o'clock position on the circle
#### Inputs:
####    r - radius; units = meters; class = numeric
####    a - angle; units = radians, class = numeric


x = 25*(cos(67.5))
y = 25*(sin(67.5))

x
y

x = cx + r * cos(a)
y = cy + r * sin(a)

x = 309667.5 + 25 * cos(0.3926991)
y = 5600237 + 25 * sin(0.3926991)

x
y

x = 25 * sin(pi * 2 * 0 / 360)
y = 25 * cos(pi * 2 * 0 / 360)

x
y
ggplot() +
  geom_sf(data = ft_150) +
  geom_sf(data = ft_125) +
  geom_sf(data = ft_100) +
  geom_sf(data = ft_75) +
  geom_sf(data = ft_50) +
  geom_sf(data = ft_25) +
  geom_sf(data = ft_coord)

