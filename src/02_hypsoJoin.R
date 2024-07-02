# Load libraries
library(tidyverse)
library(lubridate)
library(patchwork)
library(scico)

source('src/00_gethypso.R')



##################### Add lake ice thickness ########################
df.full.ice = df.spcH |> 
  left_join(ice.interp, by = join_by(date_time, location_name)) |> 
  mutate(tempUse = if_else(depth.asl > ice.asl, NA, ctd_temp_c)) |> 
  mutate(iceDensity_kgm3 = if_else(depth.asl > ice.asl, 900, NA))

# specHeatIce = 0.506 cal/degC/g # 2108 J/kgK
# latentHeatIce = 70.8 cal/g (or 333883 J/kg)

# The latent heat of fusion of ice is 33,600 joules per kilogram
# latent heat of ice = density * thickness *  latent heat of ice (333883 J/kg)

# Check plot 
ggplot(df.full.ice) +
  geom_path(aes(x = tempUse, y = depth.asl, group = date_time, color = year(date_time))) +
  ylab('Elevation (m asl)') + xlab('Temperature (°C)') +
  scale_colour_viridis_c(option = 'F', name = 'Year') +
  theme_bw(base_size = 9) +
  facet_wrap(~location_name, scales = 'free')

##################### Add hypsometry ########################

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
hypo.join = df.full.ice |> 
  filter(date_time %in% max.depths$date_time) %>% 
  left_join(hypo.use, by = join_by(depth.asl.char, location_name)) %>% 
  mutate(temp_K = tempUse + 273.15) %>% 
  mutate(spHeat_J_m3K = spHeat_J_kgK * density_kg_m3) %>% 
  # latent heat of ice = density * thickness *  latent heat of ice (333883 J/kg)
  mutate(LHice_J_m3 = iceDensity_kgm3 * 333883) |> 
  mutate(heatIce_J = LHice_J_m3 * vol_layer_m3) |> 
  mutate(heat_J = spHeat_J_m3K * vol_layer_m3 * temp_K) %>% 
  mutate(heat_J_m2 = heat_J/Area_2D) |> 
  ungroup()
# caloric content (Kelvin) of ice or water (avg temp x thickness x sp heat)

heat.day = hypo.join %>% 
  group_by(location_name, date_time) %>% 
  summarise(heat_J = sum(heat_J, na.rm = T), heatIce_J = sum(heatIce_J, na.rm = T))

# Plot timeseries
ggplot(heat.day) +
  geom_point(aes(x = date_time, y = heat_J), fill = 'lightblue4', shape = 21, stroke = 0.2) +
  geom_point(aes(x = date_time, y = heatIce_J), fill = 'white', shape = 21, stroke = 0.2) +
  facet_wrap(~location_name)

# profiles
ggplot(hypo.join) +
  geom_point(aes(x = heat_J_m2 , y = depth.asl)) +
  facet_wrap(~location_name, scales = 'free')

# Plot heat maps
makeHeat <- function(name) {
  ggplot(hypo.join %>% filter(location_name == name)) + 
    geom_tile(aes(x = date_time, y = depth.asl, fill = heat_J_m2/1e6), width = 150,height = 0.1) +
    scale_fill_scico(palette = 'roma', direction = -1, name = 'MJ/m2') +
    labs(title = name) +
    ylab('Depth asl (m)') +
    theme_bw(base_size = 9) +
    theme(axis.title.x = element_blank())
}

h1 = makeHeat('Lake Fryxell')
h2 = makeHeat('Lake Hoare')
h3 = makeHeat('East Lake Bonney')
h4 = makeHeat('West Lake Bonney')

h1 + h2 + h3 + h4

ggsave('figures/MDVlakeHeatContent.png', width = 6, height = 4, dpi = 500)


a = hypo.join %>% filter(location_name == 'West Lake Bonney') |> 
  slice(1:1000) |> 
  select(date_time, depth.asl, heat_J, heat_J_m2) |> 
  filter(!is.na(heat_J))

ggplot(hypo.join %>% filter(location_name == 'West Lake Bonney')) + 
  geom_tile(aes(x = as.factor(date_time), y = depth.asl, fill = heat_J), height = 0.1)
