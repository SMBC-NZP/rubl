library(sf)
library(tigris)
library(leaflet)

states <- states(cb = TRUE)

leaflet(states) %>%
  addProviderTiles("CartoDB.Positron") %>%
  addPolygons(fillColor = "white",
              color = "black",
              weight = 0.5) %>%
  setView(-98.5795, 39.8282, zoom=3)

median_water <- 
  median(as.numeric(states@data$AWATER))


states_sf <-
  sf::st_as_sf(states) %>%
  filter(
    NAME %in% state.name,
    !NAME %in% c('Alaska', 'Hawaii')) %>% 
  mutate_at(
    vars(AWATER, ALAND),
    function(x) as.numeric(x)
  ) %>%
  mutate(
    prop_water = AWATER/ALAND)

median_prop_water <-
  median(states_sf$prop_water)


states_sf %>% 
  ggplot() +
  geom_sf(
    aes(fill = prop_water), 
    color = 'black') +
  scale_fill_gradient2(
    low = 'blue',
    mid = 'white',
    high = 'red',
    midpoint = median_prop_water)

