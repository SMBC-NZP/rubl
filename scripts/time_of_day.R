lapply(
  c('activity', 'suncalc', 'lutz', 'sf', 'lubridate', 'tidyverse'),
  require,
  character.only = TRUE
)

# Get and format eBird data:

ebd <-
  eBirdLists %>% 
  transmute(
    cellAddress,
    protocol,
    lat,
    lon,
    date_time = paste(date, time, sep = ' ') %>% 
      as_datetime(),
    effortDist,
    count) %>% 
  filter(
    month(date_time) %in% 1:2,
    year(date_time) %in% 2009:2011,
    effortDist <= 8) %>% 
  group_by(cellAddress, protocol) %>% 
  filter(count == max(count)) %>% 
  ungroup() %>% 
  filter(count > 0)

# Get sunrise times for each longitude:

sunrise_times <-
  map_dfr(
  1:nrow(ebd),
  function(i) {
    suncalc::getSunlightTimes(
      date = as_date(ebd[i,]$date_time),
      lat = ebd[i,]$lat,
      lon = ebd[i,]$lon,
      keep = 'sunrise',
      tz = 'UTC') %>% 
      select(-c(lat, lon)) %>% 
      as_tibble()
  }) %>% 
  select(-date)

# Get time zones for each observation:

ebd_sf <-
  sf::st_as_sf(ebd, coords = c('lon', 'lat'), crs = 4326)

ebd$tz <-
  lutz::tz_lookup(ebd_sf, crs = 4326, method = 'accurate')

ebd_tz <-
  bind_cols(
    ebd,
    purrr::map_dfr(
      1:nrow(ebd),
      function(i) {
        tz_offset(
          as_date(ebd[i,]$date_time),
          tz = ebd[i,]$tz) %>% 
          select(utc_offset_h)
      })
  )

# Calculate solar time for each observation:
 
ebd_solar_times <-
  bind_cols(ebd_tz, sunrise_times) %>% 
  # Convert date time to UTC:
  mutate(date_time = date_time + utc_offset_h*60*60) %>% 
  # Convert times to decimal hours:
  mutate_at(
    vars(date_time, sunrise),
    ~ hour(.) + minute(.)/60) %>% 
  # Convert time to radians:
  mutate_at(
    vars(date_time, sunrise),
    ~ ./24*2*pi) %>% 
  # Transform to solar time:
  mutate(
    time_solar =
      activity::transtime(
        date_time, 
        anchor = sunrise, 
        type = 'single'))

# Add flock size classes:

ebd_flock <-
  ebd_solar_times %>% 
  mutate(
    count = 
      case_when(
        count <= 20 ~ 'small',
        count >= 100 ~ 'large',
        TRUE ~ 'medium'
      ))

ebd_flock %>% 
  group_by(count) %>% 
  summarize(m_time = mean(time_solar))

ks.test(
  filter(ebd_flock, count == 'small') %>% 
    pull(time_solar),
  filter(ebd_flock, count == 'medium') %>% 
    pull(time_solar))

ks.test(
  filter(ebd_flock, count == 'medium') %>% 
    pull(time_solar),
  filter(ebd_flock, count == 'large') %>% 
    pull(time_solar))

ks.test(
  filter(ebd_flock, count == 'small') %>% 
    pull(time_solar),
  filter(ebd_flock, count == 'large') %>% 
    pull(time_solar))

# Fit circular density distributions:

fit_list <-
  purrr::map(
    c('small', 'medium', 'large'),
    ~ {
      filter(ebd_flock, count == .) %>% 
        pull(time_solar) %>% 
        activity::fitact(sample = 'none')
    }) %>% 
  set_names(c('small', 'medium', 'large'))


# A list for comparisons between classes:

flock_comparisons <-
  list(
    c('small', 'medium'),
    c('small', 'large'),
    c('medium', 'large')) %>% 
  set_names(c('small-medium', 'small-large', 'medium-large'))
  

# Compare distributions:

distribution_stats_list <-
  purrr::map(
    flock_comparisons,
    ~ activity::compareCkern(
      fit_list[[pluck(., 1)]], 
      fit_list[[pluck(., 2)]], 
      reps = 1000))

# Set color palette for flock size classes:

color_blind_palette <-
  c('Small' = '#0072B2',
    'Medium' = '#E69F00',
    'Large' = 'red')


# Plot distributions:

purrr::map_dfr(
  names(fit_list),
  function(flock_size) {
    as.data.frame(fit_list[[flock_size]]@pdf) %>% 
      mutate(flock_size = flock_size) %>% 
      as_tibble()
  }) %>% 
  mutate(
    time_of_day = x*24/2/pi,
    `Flock size` = 
      factor(flock_size, 
             levels = c('small', 'medium', 'large'),
             labels = c('Small', 'Medium', 'Large'))) %>% 
  ggplot(
    aes(x = time_of_day,
        y = y, 
        color = `Flock size`,
        fill = `Flock size`)) + 
  geom_line(size = 0.75) +
  geom_area(
    position = 'identity',
    alpha = 0.3,
    color = NA) +
  scale_fill_manual(values = color_blind_palette) + 
  scale_color_manual(values = color_blind_palette) +
  scale_x_continuous(
    limits = c(0, 24),
    expand = c(0,0),
    breaks = c(0, 6, 12, 18, 24)) +
  scale_y_continuous(
    limits = c(0, 0.6),
    expand = c(0,0),
    breaks = seq(0, 0.6, by = 0.1)) +
  labs(
    x = 'Time of day (hour - UTC)',
    y = 'Density') +
  theme_bw() +
  plot_theme()

# Save plot:

ggsave(
  'time_of_day.png', 
  width = 7.5, 
  height = 3.8, 
  units = 'in')
 