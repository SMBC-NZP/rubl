library(raster)
library(sf)
library(tidyverse)


# Get data ----------------------------------------------------------------

# Make temp file to store ecoregions:

temp <- 
  tempfile()

# Unzip ecoregion data to the temp file:

unzip(
  'data/raw_data/us_eco_l4.zip',
  exdir = temp
)

# Extract the blackbelt region:

blackbelt_shp <-
  read_sf(temp) %>%
  # filter(US_L4CODE %in% c('65a', '65b')) %>%
  filter(US_L4CODE %in% c('65a', '65b', '65p')) %>%
  st_transform(crs = '+init=epsg:4326')

# Floodplain forest cover:

floodplain_cover <-
  raster(
  x = 'data/derived_data/land_cover/flood.asc',
  crs = '+init=epsg:4326')

# Define the extent of the Black Belt region:

blackbelt_extent <-
  extent(
    filter(
      blackbelt_shp,
      US_L4CODE == '65a'))

# Explore data ------------------------------------------------------------


floodplain_cover %>%
  crop(blackbelt_extent) %>%
  as.data.frame(xy = TRUE) %>% 
  as_tibble() %>% 
  ggplot() +
  geom_raster(
    aes(x = x, y = y, fill = flood)
  ) + 
  # scale_fill_gradient2() +
  scale_fill_gradient2(
    low = 'blue',
    mid = 'white',
    high = 'red'
  ) +
  coord_quickmap() +
  geom_sf(
    data = st_crop(blackbelt_shp, blackbelt_extent),
    fill = NA, 
    color = 'white')
  

plot(floodplain_cover)

plot(blackbelt_shp)

# How big is the Black Belt?

st_area(blackbelt_shp) %>%
  # Calculate sum of the features that make up the Black Belt, and
  # convert to square kilometers:
  sum() * 1E-6
  
