usecolors =  c('#4477c9', '#e3dc10', '#b34f0c', '#4c944a')
usecolors = c("#BB9F2F", "#94B9AF", "#942911", "#593837")

##################### Add hypsometry ########################
# Get hypsometry and create character elevation
hypo.use = hypo_new |> 
  ungroup() |> 
  mutate(depth.asl.char = as.character(round(Elevation_masl,1))) |> 
  select(-lake, -Elevation_masl)

# Check that days have full depths (deletes 6 sampling dates)
max.depths = df.spcH %>% 
  group_by(location_name, date_time) %>% 
  summarise(max.depth = last(depth_m)) %>% 
  filter((location_name == 'East Lake Bonney' & max.depth > 30) |
          (location_name == 'Lake Fryxell' & max.depth > 14) |
          (location_name == 'Lake Hoare' & max.depth > 15) |
          (location_name == 'West Lake Bonney' & max.depth > 30))

# Join hyspometry. For bottom sampling depth, set this layer's volume equal to the cumulative volume beneath it. 
hypo.join = df.full.ice |> 
  filter(date_time %in% max.depths$date_time) %>% 
  left_join(hypo.use, by = join_by(depth.asl.char, location_name)) %>% 
  group_by(location_name, date_time) |>
  mutate(vol_layer_m3 = if_else(depth.asl == min(depth.asl), cum_vol_m3, vol_layer_m3)) |> 
  mutate(temp_FPD = tempUse - FPD) %>% # set baseline temperature to -5°C
  # latent heat of ice = density * thickness *  latent heat of ice (334000 J/kg)
  mutate(LHice_J_m3 = iceDensity_kgm3 * 334000) |> 
  mutate(heatIce_J = LHice_J_m3 * vol_layer_m3) |> 
  mutate(heat_J = spHeat_J_kgK * density_kg_m3 * vol_layer_m3 * temp_FPD) %>% 
  mutate(heat_J_m2 = heat_J/Area_2D) |> 
  mutate(heat_J_m3 = heat_J/vol_layer_m3) |> 
  # mutate(heat_J_m2 = if_else(Area_2D == 0, 0, heat_J_m2)) |> 
  ungroup() |> 
  mutate(volUse = if_else(depth.asl > ice.asl, NA, vol_layer_m3))  |> 
  mutate(cumvolUse = if_else(depth.asl > ice.asl, NA, cum_vol_m3)) |> 
  mutate(icevolUse = if_else(depth.asl <= ice.asl, NA, vol_layer_m3)) 

# caloric content (Kelvin) of ice or water (avg temp x thickness x sp heat)

##################### Plot heat maps #####################
makeHeat <- function(name, filllimits = c(NA,NA)) {
  ggplot(hypo.join %>% 
           filter(month(date_time) %in% c(11,12)) |> 
           filter(location_name == name)) + 
    geom_tile(aes(x = date_time, y = depth.asl, fill = heat_J_m3/1e6), width = 150,height = 0.1) +
    geom_tile(data = hypo.join %>% filter(location_name == name & isIce),
              aes(x = date_time, y = depth.asl), fill = 'grey80', width = 150, height = 0.1) +
    scale_fill_scico(palette = 'vik', direction = 1, name = 'MJ m^-3', limits = filllimits, na.value = 'black') +
    labs(subtitle = name) +
    ylab('Elevation (m asl)') +
    theme_bw(base_size = 9) +
    theme(axis.title.x = element_blank(),
          legend.text = element_text(size = 7),
          legend.key.width = unit(0.15,'cm'),
          legend.title = element_markdown())
}

# h1.epi = makeHeat('Lake Fryxell', filllimits = c(0,17))
# h2.epi = makeHeat('Lake Hoare', filllimits = c(0,17))
# h3.epi = makeHeat('East Lake Bonney', filllimits = c(0,17)) + ylim(45, NA)
# h4.epi = makeHeat('West Lake Bonney', filllimits = c(0,17)) + ylim(45, NA)
# 
# h1.epi + h2.epi + h3.epi + h4.epi + plot_layout(guides = 'collect')
# ggsave('figures/Fig3_HeatContent_epi.png', width = 6, height = 4, dpi = 500)

h1 = makeHeat('Lake Fryxell')
h2 = makeHeat('Lake Hoare')
h3 = makeHeat('East Lake Bonney')
h4 = makeHeat('West Lake Bonney')

h1 + h2 + h3 + h4
ggsave('figures/Fig3_HeatMap.png', width = 6, height = 4, dpi = 500)

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
t1 = makeTemp('Lake Fryxell')
t2 = makeTemp('Lake Hoare')
t3 = makeTemp('East Lake Bonney')
t4 = makeTemp('West Lake Bonney')

t1 + t2 + t3 + t4
ggsave('figures/SI_CTD_Temp.png', width = 6, height = 4, dpi = 500)

##################### Create daily totals #####################
hypo.join |> filter(location_name == "Lake Hoare") |> 
  filter(depth.asl <= 55 & depth.asl >= 48) |> 
  select(heat_J, heat_J_m2, tempUse) |> 
  summarise(mean(heat_J_m2), min(heat_J_m2), max(heat_J_m2), 
            mean(tempUse), min(tempUse), max(tempUse))
# For Lake Hoare, Heat J/m2 between 50-55 m ~ 103634 J/m2
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
                              depth.asl < 58, 0.22659, tempUse)) |> 
  mutate(heat_J_m2 = if_else(is.na(heat_J_m2) &
                               depth.asl < 58, 103634, heat_J_m2)) |> 
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
  summarise(ice.approx = mean(ice.approx, na.rm = T), ice.vol = sum(icevolUse, na.rm = T),
            heat_J = sum(heat_J, na.rm = T), heatIce_J = sum(heatIce_J, na.rm = T), 
            Area_2D = first(Area_2D), vol = sum(volUse, na.rm = T), cum_vol = max(cumvolUse,na.rm = T),
            LL = first(masl.approx), 
            tempV = sum(tempV, na.rm = T)/vol, tempUse = mean(tempUse, na.rm = T),
  ) |> 
  mutate(heatTot_J_m2 = (heat_J - heatIce_J)/Area_2D) |> 
  mutate(heatLake_J_m3 = (heat_J)/cum_vol) |> 
  mutate(heatIce_J_m3 = heatIce_J/ice.vol) |> 
  ungroup() |> 
  mutate(location_name = factor(location_name, levels = c('Lake Fryxell','Lake Hoare', 'East Lake Bonney', 'West Lake Bonney'))) |> 
  mutate(dec.date = decimal_date(date_time), yday = yday(date_time)) |> 
  mutate(yday = if_else(yday > 200, yday, yday+365)) |> 
  mutate(ice.approx = - ice.approx) # change ice to positive value

heat.day_DecJan = heat.day |>
  filter(yday(date_time) < 244 | yday(date_time) > 365) # past Dec 31st

heat.day = heat.day |> 
  filter(yday(date_time) >= 244 & yday(date_time) <= 365) ##Between Sep 1 and Dec Dec 31st for all lakes

# heat.day = heat.day |>
#   filter(case_when(location_name == "Lake Hoare" ~ yday(date_time) >= 244 & yday(date_time) <= 350, #Between Sep 1 and Dec 15th for Lake Hoare
#                    location_name == "Lake Fryxell" ~ yday(date_time) < 244 | yday(date_time) <= 340, #Between Sep 1 and Dec 5th for Lake Fryxell
#                    T ~ yday(date_time) >= 244 & yday(date_time) <= 335)) # Between Sep 1 and Dec 1 for other lakes

# yday(as.Date('1997-12-01'))

######### Plots timeseries of heat/m3 #########
make.tsheat <- function(usename, j) {
  ggplot(heat.day |> filter(location_name == usename)) +
  geom_smooth(data = heat.day |> filter(location_name == usename) |> filter(year(date_time) <= 2020),
              aes(x = date_time, y = heatLake_J_m3/1e6, color = location_name), method = 'gam', se = FALSE, 
              formula = y ~ s(x, k = 25, bs = "tp", m = 1)) +
  geom_point(data = heat.day |> filter(location_name == usename) |> filter(year(date_time) <= 2020),
                aes(x = date_time, y = heatLake_J_m3/1e6, color = location_name), size = 0.5) +
  geom_point(data = heat.day |> filter(location_name == usename) |> filter(year(date_time) > 2020),
                aes(x = date_time, y = heatLake_J_m3/1e6, color = location_name), size = 0.5) +
  geom_path(data = heat.day |> filter(location_name == usename) |> filter(year(date_time) > 2020),
               aes(x = date_time, y = heatLake_J_m3/1e6, color = location_name), size = 1) +
  scale_color_manual(values = usecolors[j], name = 'Lake') +
  ylab('Heat (MJ m^<sup>-3</sup>)') +
  theme_bw(base_size = 9) +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_markdown(),
        legend.position = 'none')
}
h5 = make.tsheat(usename = 'Lake Fryxell', j = 1)
h6 = make.tsheat(usename = 'Lake Hoare', j = 2)
h7 = make.tsheat(usename = 'East Lake Bonney', j = 3)
h8 = make.tsheat(usename = 'West Lake Bonney', j = 4)

# Combine heat plots 
layout <- "
AC
AC
BD
EG
EG
FH
"

h1 + h5 + h2 + h6 + h3 + h7 + h4 +  h8 +
  plot_layout(design = layout) + plot_annotation(tag_levels = 'a', tag_suffix = ')') &
  theme(plot.tag = element_text(size = 8))
ggsave('figures/Fig3_HeatMap2.png', width = 6, height = 6, dpi = 500)

######### Plot timeseries ##########
h.ice = ggplot(heat.day) +
  geom_smooth(data = heat.day |> filter(year(date_time) <= 2020),
              aes(x = date_time, y = -heatIce_J/Area_2D/1e6, color = location_name), method = 'gam', se = FALSE, 
              formula = y ~ s(x, k = 25, bs = "tp", m = 1)) +
  geom_smooth(data = heat.day |> filter(year(date_time) > 2020),
              aes(x = date_time, y = -heatIce_J/Area_2D/1e6, color = location_name), method = 'lm', se = FALSE) +
  # geom_point(data = heat.day_DecJan, 
  #            aes(x = date_time, y = -heatIce_J/Area_2D/1e6, fill = location_name), shape = 22, stroke = 0.2, alpha = 0.5) +
  # geom_point(aes(x = date_time, y = -heatIce_J/Area_2D/1e6, fill = location_name), shape = 21, stroke = 0.2, size = 1.2) +
  scale_color_manual(values = usecolors, name = 'Lake') +
  scale_fill_manual(values = usecolors, name = 'Lake') +
  ylab('Latent Heat Ice (MJ m^-2 )') +
  theme_bw(base_size = 9) +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_markdown(),
        legend.position = 'none'); h.ice

h.wc = ggplot(heat.day) +
  geom_smooth(data = heat.day |> filter(year(date_time) <= 2020),
              aes(x = date_time, y = heat_J/Area_2D/1e6, color = location_name), method = 'gam', se = FALSE, 
              formula = y ~ s(x, k = 25, bs = "tp", m = 1)) +
  geom_smooth(data = heat.day |> filter(year(date_time) > 2020),
              aes(x = date_time, y = heat_J/Area_2D/1e6, color = location_name), method = 'lm', se = FALSE) +
  # geom_point(data = heat.day_DecJan, 
  #            aes(x = date_time, y = heat_J/Area_2D/1e6, fill = location_name), shape = 22, stroke = 0.2, alpha = 0.5) +
  # geom_point(aes(x = date_time, y = heat_J/Area_2D/1e6, fill = location_name), shape = 21, stroke = 0.2, size = 1.2) +
  scale_color_manual(values = usecolors, name = 'Lake') +
  scale_fill_manual(values = usecolors, name = 'Lake') +
  ylab('Heat in Water (MJ m^-2 )') +
  theme_bw(base_size = 9) +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_markdown(),
        legend.position = 'none'); h.wc

h.ts = ggplot(heat.day) +
  geom_smooth(data = heat.day |> filter(year(date_time) <= 2020),
              aes(x = date_time, y = (heat_J-heatIce_J)/Area_2D/1e6, color = location_name), method = 'gam', 
              formula = y ~ s(x, k = 25, bs = "tp", m = 1)) +
  geom_smooth(data = heat.day |> filter(year(date_time) > 2020),
              aes(x = date_time, y = (heat_J-heatIce_J)/Area_2D/1e6, color = location_name), method = 'lm', se = FALSE) +
  geom_point(data = heat.day_DecJan, 
             aes(x = date_time, y = (heat_J-heatIce_J)/Area_2D/1e6, fill = location_name), shape = 22, stroke = 0.2, alpha = 0.5) +
  geom_point(aes(x = date_time, y = (heat_J-heatIce_J)/Area_2D/1e6, fill = location_name), shape = 21, stroke = 0.2, size = 1.2) +
  scale_color_manual(values = usecolors, name = 'Lake') +
  scale_fill_manual(values = usecolors, name = 'Lake') +
  ylab('Heat storage (MJ m^<sup>-2</sup>)') +
  theme_bw(base_size = 9) +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_markdown(),
        legend.position = 'bottom',
        legend.title = element_blank(),
        legend.text = element_text(size = 7),
        legend.key.size = unit(0.2,'cm'),
        legend.margin = margin(0, 0, 0, 0)); h.ts

# ggsave('figures/Fig2_Heat_TimeSeries.png', width = 4, height = 2.5, dpi = 500)

# Combine heat plots 
layout <- "
AB
AB
CC
CC
CC
"

h.ice / h.wc / h.ts +
  plot_layout(design = layout) + plot_annotation(tag_levels = 'a', tag_suffix = ')') &
  theme(plot.tag = element_text(size = 8))

ggsave('figures/Fig4_HeatContent.png', width = 6, height = 4, dpi = 500)

# Output heat data. 
write_csv(heat.day, 'dataout/MDVLakes_dailyHeatStorage.csv')

ggplot(heat.day) +
  geom_path(aes(x = date_time, y = tempUse)) +
  facet_wrap(~location_name)

# Output SI figure comparing 1-D temp to area-weighted temp

ggplot(heat.day, aes(x = dec.date, y = tempV, fill = location_name)) +
  geom_point(size = 0.4, col = 'grey50') +
  geom_smooth(col = 'grey50', size = 0.3, fill = 'grey80') +
  geom_point(aes(x = dec.date, y = tempUse, col = location_name), size = 0.4) +
  geom_smooth(aes(x = dec.date, y = tempUse, col = location_name), size = 0.4) +
  scale_color_manual(values = usecolors) +
  labs(y = "Mean Temp (°C)") +
  plotCustom

ggsave(paste0('figures/SI_tempComp.png'), width = 6.5, height = 2.5, dpi = 500)

