# Load libraries
library(tidyverse)
library(lubridate)
library(patchwork)
library(scico)
library(ggtext)

# Run 01 first 

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


# Check plot - temperature
ggplot(df.full.ice |> dplyr::filter(location_name %in% c('East Lake Bonney', 'West Lake Bonney'))) +
  geom_rect(data = icebox |> dplyr::filter(location_name %in% c('East Lake Bonney', 'West Lake Bonney')),
            aes(xmin = ctd_temp_c, xmax = ctd_temp_c, ymin = max.depth, ymax = min.depth, group = year(date_time)), 
            color = 'grey50',size = 0.3) +
  geom_path(aes(x = tempUse, y = depth.asl, group = date_time, color = year(date_time))) +
  ylab('Elevation (m asl)') + xlab('Temperature (°C)') +
  scale_colour_viridis_c(option = 'F', name = 'Year') +
  theme_bw(base_size = 9) +
  facet_wrap(~location_name, scales = 'free', ncol = 2)
ggsave('figures/SI_LB.png', width = 6, height = 6, dpi = 500)

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
  # mutate(heat_J_m2 = if_else(Area_2D == 0, 0, heat_J_m2)) |> 
  ungroup() |> 
  mutate(volUse = if_else(depth.asl > ice.asl, NA, vol_layer_m3)) 
# caloric content (Kelvin) of ice or water (avg temp x thickness x sp heat)


##################### Plot heat maps #####################
makeHeat <- function(name, filllimits = c(NA,NA)) {
  ggplot(hypo.join %>% 
           filter(month(date_time) %in% c(11,12,1)) |> 
           filter(location_name == name)) + 
    geom_tile(aes(x = date_time, y = depth.asl, fill = heat_J_m2/1e6), width = 150,height = 0.1) +
    geom_tile(data = hypo.join %>% filter(location_name == name & isIce),
              aes(x = date_time, y = depth.asl), fill = 'grey80', width = 150, height = 0.1) +
    scale_fill_scico(palette = 'roma', direction = -1, name = 'MJ/m2', limits = filllimits, na.value = 'black') +
    labs(title = name) +
    ylab('Depth asl (m)') +
    theme_bw(base_size = 9) +
    theme(axis.title.x = element_blank(),
          legend.text = element_text(size = 7),
          legend.key.width = unit(0.2,'cm'))
}

h1 = makeHeat('Lake Fryxell', filllimits = c(114.5,117))
h2 = makeHeat('Lake Hoare', filllimits = c(114.5,117))
h3 = makeHeat('East Lake Bonney', filllimits = c(114.5,117)) + ylim(45, NA)
h4 = makeHeat('West Lake Bonney', filllimits = c(114.5,117)) + ylim(45, NA)

h1 + h2 + h3 + h4 + plot_layout(guides = 'collect')
ggsave('figures/Fig3_HeatContent_epi.png', width = 6, height = 4, dpi = 500)

h1 = makeHeat('Lake Fryxell')
h2 = makeHeat('Lake Hoare')
h3 = makeHeat('East Lake Bonney')
h4 = makeHeat('West Lake Bonney')

h1 + h2 + h3 + h4
ggsave('figures/SI_HeatContent_full.png', width = 6, height = 4, dpi = 500)

makeTemp <- function(name, filllimits = c(NA,NA)) {
  ggplot(hypo.join %>% 
           filter(month(date_time) %in% c(11,12,1)) |> 
           filter(location_name == name)) + 
    geom_tile(aes(x = date_time, y = depth.asl, fill = ctd_temp_c), width = 150,height = 0.1) +
    geom_tile(data = hypo.join %>% filter(location_name == name & isIce),
              aes(x = date_time, y = depth.asl), fill = 'grey80', width = 150, height = 0.1) +
    scale_fill_scico(palette = 'roma', direction = -1, name = 'Temp (°C)', limits = filllimits, na.value = 'black') +
    labs(title = name) +
    ylab('Depth asl (m)') +
    theme_bw(base_size = 9) +
    theme(axis.title.x = element_blank(),
          legend.text = element_text(size = 7),
          legend.key.width = unit(0.2,'cm'))
}
h1 = makeTemp('Lake Fryxell')
h2 = makeTemp('Lake Hoare')
h3 = makeTemp('East Lake Bonney')
h4 = makeTemp('West Lake Bonney')

h1 + h2 + h3 + h4
ggsave('figures/SI_CTD_Temp.png', width = 6, height = 4, dpi = 500)

##################### Create daily totals #####################
hypo.join |> filter(location_name == "Lake Hoare") |> 
  filter(depth.asl <= 55 & depth.asl >= 48) |> 
  select(heat_J, heat_J_m2, tempUse) |> 
  summarise(mean(heat_J_m2), min(heat_J_m2), max(heat_J_m2), 
            mean(tempUse), min(tempUse), max(tempUse))
# For Lake Hoare, Heat J/m2 between 50-55 m ~ 114.9 MJ/m2
# For Lake Hoare, temp °C between 50-55 m ~ 0.223 °C

# Problem is not all casts go deep enough, specifically for Lake Hoare
hoare.dates = hypo.join |> filter(location_name == "Lake Hoare") |> 
  group_by(date_time) |> summarise_all(first) |> pull(date_time)

fill.gaps.LH = expand.grid(location_name = 'Lake Hoare',
                       date_time = hoare.dates,
                       depth.asl.char = as.character(round(seq(48,75, by = 0.1),1))) |> 
  left_join(hypo.use, by = join_by(depth.asl.char, location_name)) |> 
  arrange(location_name, date_time, desc(depth.asl.char)) |> 
  left_join(hypo.join) |> 
  mutate(depth.asl = as.numeric(depth.asl.char)) |> 
  mutate(tempUse = if_else(is.na(tempUse) &
                              depth.asl < 58, 0.223, tempUse)) |> 
  mutate(heat_J_m2 = if_else(is.na(heat_J_m2) &
                               depth.asl < 58, 114905039, heat_J_m2)) |> 
  mutate(heat_J = if_else(is.na(heat_J) &
                            depth.asl < 58, heat_J_m2*Area_2D, heat_J)) 

# Take new extrapolated Lake Hoare dataframe and join to other lakes
# Add cutoff depth to align bottom of most profiles
hypo.fill = hypo.join |> 
  group_by(location_name, date_time) %>% 
  arrange(location_name, date_time, desc(depth.asl)) |> 
  mutate(cutoffDepth = case_when(location_name == 'East Lake Bonney' ~ 25,
                                 location_name == 'West Lake Bonney' ~ 25,
                                 location_name == 'Lake Fryxell' ~ 2.5,
                                 location_name == 'Lake Hoare' ~ 58)) |> 
  filter(depth.asl >= cutoffDepth) 

