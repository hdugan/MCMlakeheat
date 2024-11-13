usecolors =  c('#4477c9', '#e3dc10', '#b34f0c', '#4c944a')
usecolors = c("#BB9F2F", "#94B9AF", "#942911", "#593837")

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
  mutate(heat_J_m3 = heat_J/vol_layer_m3) |> 
  # mutate(heat_J_m2 = if_else(Area_2D == 0, 0, heat_J_m2)) |> 
  ungroup() |> 
  mutate(volUse = if_else(depth.asl > ice.asl, NA, vol_layer_m3)) 
# caloric content (Kelvin) of ice or water (avg temp x thickness x sp heat)

##################### Plot heat maps #####################
makeHeat <- function(name, filllimits = c(NA,NA)) {
  ggplot(hypo.join %>% 
           filter(month(date_time) %in% c(11,12,1)) |> 
           filter(location_name == name)) + 
    geom_tile(aes(x = date_time, y = depth.asl, fill = heat_J_m3/1e6), width = 150,height = 0.1) +
    geom_tile(data = hypo.join %>% filter(location_name == name & isIce),
              aes(x = date_time, y = depth.asl), fill = 'grey80', width = 150, height = 0.1) +
    scale_fill_scico(palette = 'vik', direction = 1, name = 'MJ m^-3', limits = filllimits, na.value = 'black') +
    labs(subtitle = name) +
    ylab('Depth asl (m)') +
    theme_bw(base_size = 9) +
    theme(axis.title.x = element_blank(),
          legend.text = element_text(size = 7),
          legend.key.width = unit(0.2,'cm'),
          legend.title = element_markdown())
}

h1.epi = makeHeat('Lake Fryxell', filllimits = c(1145,1170))
h2.epi = makeHeat('Lake Hoare', filllimits = c(1145,1170))
h3.epi = makeHeat('East Lake Bonney', filllimits = c(1145,1170)) + ylim(45, NA)
h4.epi = makeHeat('West Lake Bonney', filllimits = c(1145,1170)) + ylim(45, NA)

h1.epi + h2.epi + h3.epi + h4.epi + plot_layout(guides = 'collect')
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

# Summarise by day 
heat.day = hypo.fill |> 
  mutate(tempV = tempUse * vol_layer_m3) |> 
  summarise(ice.approx = mean(ice.approx, na.rm = T), heat_J = sum(heat_J, na.rm = T), heatIce_J = sum(heatIce_J, na.rm = T), 
            Area_2D = first(Area_2D), vol = sum(volUse, na.rm = T), LL = first(masl.approx), 
            tempV = sum(tempV, na.rm = T)/vol, tempUse = mean(tempUse, na.rm = T),
  ) |> 
  mutate(heatTot_J_m2 = (heat_J - heatIce_J)/Area_2D) |> 
  ungroup() |> 
  mutate(location_name = factor(location_name, levels = c('Lake Fryxell','Lake Hoare', 'East Lake Bonney', 'West Lake Bonney'))) |> 
  mutate(dec.date = decimal_date(date_time), yday = yday(date_time)) |> 
  mutate(yday = if_else(yday > 200, yday, yday+365)) |> 
  filter(month(date_time) >= 10) |> 
  mutate(tempUse = if_else(location_name == 'West Lake Bonney' & year(date_time) == 2005, NA, tempUse))

######### Plot timeseries ##########
h.ts = ggplot(heat.day) +
  geom_smooth(aes(x = date_time, y = heatTot_J_m2/1e6, color = location_name), method = 'gam') +
  geom_point(aes(x = date_time, y = heatTot_J_m2/1e6, fill = location_name), shape = 21, stroke = 0.2) +
  scale_color_manual(values = usecolors, name = 'Lake') +
  scale_fill_manual(values = usecolors, name = 'Lake') +
  ylab('Heat storage (MJ m^-2 )') +
  theme_bw(base_size = 9) +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_markdown(),
        legend.position = 'bottom',
        legend.title = element_blank(),
        legend.text = element_text(size = 7),
        legend.key.size = unit(0.2,'cm'),
        legend.margin = margin(0, 0, 0, 0))

ggsave('figures/Fig2_Heat_TimeSeries.png', width = 4, height = 2.5, dpi = 500)

(h1.epi + h2.epi + h3.epi + h4.epi + plot_layout(guides = 'collect')) 

# Combine heat plots 
layout <- "
AA
AA
BB
"
(h1.epi + h2.epi + h3.epi + h4.epi + plot_layout(guides = 'collect')) / h.ts +
  plot_layout(design = layout) + plot_annotation(tag_levels = 'a', tag_suffix = ')') &
  theme(plot.tag = element_text(size = 8))

ggsave('figures/Fig3_HeatContent.png', width = 6, height = 5, dpi = 500)

# Output heat data. 
write_csv(heat.day, 'dataout/MDVLakes_dailyHeatStorage.csv')

# results for paper 
heat.day |> group_by(location_name) |> 
  summarise(min(heatTot_J_m2/1e6), max(heatTot_J_m2/1e6))

hypo.join |> group_by(location_name) |> 
  summarise(minHeat = min(heat_J_m3/1e6, na.rm = T), maxHeat = max(heat_J_m3/1e6, na.rm = T))
