# Load libraries
library(tidyverse)
library(lubridate)
library(patchwork)

source('src/gethypso.R')

# Get hypsometry and create character elevation
hypo.use = hypo_new |> 
  ungroup() |> 
  mutate(depth.asl.char = as.character(round(Elevation_masl,1))) |> 
  select(-lake, -Elevation_masl)

# Check that days have full depths
max.depths = df.spcH %>% 
  group_by(location_name, date_time) %>% 
  summarise(max.depth = last(depth_m)) %>% 
  filter((location_name == 'East Lake Bonney' & max.depth > 30) |
          (location_name == 'Lake Fryxell' & max.depth > 14) |
          (location_name == 'Lake Hoare' & max.depth > 15) |
          (location_name == 'West Lake Bonney' & max.depth > 30))

# Join hyspometry
hypo.join = df.spcH |> 
  filter(date_time %in% max.depths$date_time) %>% 
  left_join(hypo.use, by = join_by(depth.asl.char, location_name)) %>% 
  mutate(temp_K = ctd_temp_c + 273.15) %>% 
  mutate(spHeat_J_m3K = spHeat_J_kgK * density_kg_m3) %>% 
  mutate(heat_J = spHeat_J_m3K * vol_layer_m3 * temp_K) %>% 
  mutate(heat_J_m2 = heat_J/Area_2D)
# caloric content (Kelvin) of ice or water (avg temp x thickness x sp heat)

heat.day = hypo.join %>% 
  group_by(location_name, date_time) %>% 
  summarise(heat_J = sum(heat_J, na.rm = T))

# Plot timeseries
ggplot(heat.day) +
  geom_point(aes(x = date_time, y = heat_J)) +
  facet_wrap(~location_name)

# profiles
ggplot(hypo.join) +
  geom_point(aes(x = heat_J_m2 , y = depth.asl)) +
  facet_wrap(~location_name, scales = 'free')

# Plot heat maps
h1 = ggplot(hypo.join %>% filter(location_name == 'Lake Fryxell')) + 
  geom_tile(aes(x = date_time, y = depth.asl, fill = heat_J_m2), width = 150) +
  scale_fill_viridis_c(option = 'F')
  
h2 = ggplot(hypo.join %>% filter(location_name == 'Lake Hoare')) + 
  geom_tile(aes(x = date_time, y = depth.asl, fill = heat_J_m2), width = 150) +
  scale_fill_viridis_c(option = 'F')

h3 = ggplot(hypo.join %>% filter(location_name == 'East Lake Bonney')) + 
  geom_tile(aes(x = date_time, y = depth.asl, fill = heat_J_m2), width = 150) +
  scale_fill_viridis_c(option = 'F')

h4 = ggplot(hypo.join %>% filter(location_name == 'West Lake Bonney')) + 
  geom_tile(aes(x = date_time, y = depth.asl, fill = heat_J_m2), width = 150) +
  scale_fill_viridis_c(option = 'F')

h1 + h2 + h3 + h4
