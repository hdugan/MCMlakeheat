# Load libraries
library(tidyverse)
library(lubridate)
library(patchwork)
library(scico)
library(ggtext)

source('src/00_gethypso.R')

##################### Add lake ice thickness ########################
df.full.ice = df.spcH |> 
  left_join(ice.interp, by = join_by(date_time, location_name)) |> 
  mutate(tempUse = if_else(depth.asl > ice.asl, NA, ctd_temp_c)) |> 
  mutate(condUse = if_else(depth.asl > ice.asl, NA, ctd_conductivity_mscm)) |> 
  mutate(isIce = if_else(depth.asl > ice.asl, TRUE, FALSE)) |> 
  mutate(iceDensity_kgm3 = if_else(depth.asl > ice.asl, 900, NA)) |> 
  mutate(location_name = factor(location_name, levels = c('Lake Fryxell','Lake Hoare', 'East Lake Bonney', 'West Lake Bonney')))

# specHeatIce = 0.506 cal/degC/g # 2108 J/kgK
# latentHeatIce = 70.8 cal/g (or 334000 J/kg)

# The latent heat of fusion of ice is 33,600 joules per kilogram
# latent heat of ice = density * thickness *  latent heat of ice (334000 J/kg)

# for ice plotting
toptemp = df.full.ice |> filter(!isIce) |> group_by(date_time, location_name) |> filter(depth.asl == max(depth.asl, na.rm = T)) |> 
  select(location_name, date_time, depth.asl, ctd_temp_c, ctd_conductivity_mscm)
icebox = df.full.ice |> filter(isIce == TRUE) |> group_by(location_name, date_time) |> 
  summarise(min.depth = min(depth.asl), max.depth = max(depth.asl)) |> 
  left_join(toptemp)

# Check plot - temperature
ct1 = ggplot(df.full.ice) +
  geom_rect(data = icebox,
            aes(xmin = ctd_temp_c, xmax = ctd_temp_c, ymin = max.depth, ymax = min.depth, group = year(date_time)), 
            color = 'grey50',size = 0.3) +
  geom_path(aes(x = tempUse, y = depth.asl, group = date_time, color = year(date_time))) +
  ylab('Elevation (m asl)') + xlab('Temperature (°C)') +
  scale_colour_viridis_c(option = 'F', name = 'Year') +
  theme_bw(base_size = 9) +
  facet_wrap(~location_name, scales = 'free', ncol = 1)

# Check plot - conductivity
ct2 = ggplot(df.full.ice) +
  geom_rect(data = icebox,
            aes(xmin = ctd_conductivity_mscm, xmax = ctd_conductivity_mscm, ymin = max.depth, ymax = min.depth, group = year(date_time)), 
            color = 'grey50',size = 0.3) +
  geom_path(aes(x = condUse, y = depth.asl, group = date_time, color = year(date_time))) +
  ylab('Elevation (m asl)') + xlab('Conductivity (mS cm^-1 )') +
  scale_colour_viridis_c(option = 'F', name = 'Year') +
  theme_bw(base_size = 9) +
  facet_wrap(~location_name, scales = 'free', ncol = 1) +
  theme(axis.title.x = element_markdown())

ct1 + ct2 + plot_layout(guides = 'collect') +
  plot_annotation(tag_levels = 'a', tag_suffix = ')') &
  theme(plot.tag = element_text(size = 8), legend.position = 'bottom', 
        legend.key.width = unit(1, 'cm'), legend.key.height = unit(0.3, 'cm'),
        legend.margin = margin(0, 0, 0, 0))
ggsave('figures/Fig1_CTD.png', width = 6, height = 6, dpi = 500)

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
  # latent heat of ice = density * thickness *  latent heat of ice (334000 J/kg)
  mutate(LHice_J_m3 = iceDensity_kgm3 * 334000) |> 
  mutate(heatIce_J = LHice_J_m3 * vol_layer_m3) |> 
  mutate(heat_J = spHeat_J_m3K * vol_layer_m3 * temp_K) %>% 
  mutate(heat_J_m2 = heat_J/Area_2D) |> 
  mutate(heat_J_m2 = if_else(Area_2D == 0, 0, heat_J_m2)) |> 
  ungroup()
# caloric content (Kelvin) of ice or water (avg temp x thickness x sp heat)

heat.day = hypo.join %>% 
  group_by(location_name, date_time) %>% 
  arrange(location_name, date_time, desc(depth.asl)) |> 
  summarise(heat_J = sum(heat_J, na.rm = T), heatIce_J = sum(heatIce_J, na.rm = T), 
            Area_2D = first(Area_2D)) |> 
  mutate(heatTot_J_m2 = (heat_J - heatIce_J)/Area_2D)

# % of ice heat
heat.day |> mutate(icep = 100*heatIce_J / heat_J) |> 
  group_by(location_name) |> 
  summarise(mean(icep, na.rm = T))

######### Plot timeseries
ggplot(heat.day) +
  geom_smooth(aes(x = date_time, y = heatTot_J_m2/1e9, color = location_name)) +
  # geom_point(aes(x = date_time, y = heat_J, fill = location_name), shape = 21, stroke = 0.2, alpha = 0.2) +
  geom_point(aes(x = date_time, y = heatTot_J_m2/1e9, fill = location_name), shape = 21, stroke = 0.2) +
  scale_color_manual(values = c('#4477c9', '#e3dc10', '#b34f0c', '#4c944a'), name = 'Lake') +
  scale_fill_manual(values = c('#4477c9', '#e3dc10', '#b34f0c', '#4c944a'), name = 'Lake') +
  ylab('Heat storage (GJ m^2 )') +
  theme_bw(base_size = 9) +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_markdown()) +
  # facet_wrap(~location_name) +
  labs(caption = 'Blue points are lake heat content. White points are including latent heat of ice.')

ggsave('figures/MDVlakeHeat_TimeSeries.png', width = 6, height = 4, dpi = 500)


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
