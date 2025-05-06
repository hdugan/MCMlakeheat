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
makeTemp <- function(name, filllimits = c(NA,NA)) {
  ggplot(hypo.join %>% 
           filter(month(date_time) %in% c(11,12,1)) |> 
           filter(location_name == name)) + 
    geom_tile(aes(x = date_time, y = depth.asl, fill = ctd_temp_c), width = 150, height = 0.1) +
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
t1 = makeTemp('Lake Fryxell', filllimits = c(-4.5,7))
t2 = makeTemp('Lake Hoare', filllimits = c(-4.5,7))
t3 = makeTemp('East Lake Bonney', filllimits = c(-4.5,7))
t4 = makeTemp('West Lake Bonney', filllimits = c(-4.5,7))

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

# Calculate heat flux between profiles. Use chosen dates to keep profiles of interest (see samplingdays.png)
usedates = chosendates2 |> 
  mutate(date1 = if_else(closest == 'second', NA, date1)) |> 
  mutate(date2 = if_else(closest == 'first', NA, date2)) |> 
  select(location_name, wateryear, date1, date2) |> 
  pivot_longer(cols = 3:4, values_to = 'date_time') |> 
  filter(!is.na(date_time))

firstprofile = hypo.fill |> 
  mutate(tempV = tempUse * vol_layer_m3) |> 
  select(location_name, date_time, year, depth.asl.char, depth.asl, tempUse, tempV, salinity_g_kg, spHeat_J_kgK, density_kg_m3, 
         ice.approx, masl.approx, volUse, Area_2D) |> 
  inner_join(usedates |> select(-name), by = join_by(location_name, date_time)) |> # select only chosen dates
  group_by(location_name, wateryear, depth.asl.char) |> 
  summarise_all(mean, na.rm = T) |> 
  mutate(Q_layer = density_kg_m3 * spHeat_J_kgK * tempUse * Area_2D * 0.1) |>  # 0.1 is dz layer depth, in meters 
  left_join(chosendates2 |> select(location_name, wateryear, chosen_date)) |> 
  arrange(location_name, wateryear, depth.asl)

# Test plot to see if profile averages looks good           
ggplot(firstprofile) +
  geom_path(aes(x = tempUse, y = as.numeric(depth.asl.char), group = wateryear)) +
  facet_wrap(~location_name, scales = 'free')

# Sum total heat content per profile and then calculate heat flux between profiles
heat_flux <- firstprofile %>%
  group_by(location_name, wateryear, chosen_date) %>%
  summarise(
    Q_total = sum(Q_layer, na.rm = TRUE),
    vol = sum(volUse, na.rm = T), 
    tempV = sum(tempV, na.rm = T)/vol, tempUse = mean(tempUse, na.rm = T),
    LL = first(masl.approx),
    ice.approx = mean(ice.approx, na.rm = T),
    .groups = "drop"
  ) %>%
  arrange(location_name, wateryear, chosen_date) |> 
  group_by(location_name) %>%
  mutate(
    delta_Q = Q_total - lag(Q_total),
    delta_t = as.numeric(difftime(chosen_date, lag(chosen_date), units = "secs")),
    flux_W = delta_Q / delta_t,
    flux_W_m2 = flux_W / mean(ctd$Area_2D, na.rm = TRUE)  # or use surface area
  ) |> 
  mutate(location_name = factor(location_name, levels = c('Lake Fryxell','Lake Hoare', 'East Lake Bonney', 'West Lake Bonney'))) |> 
  mutate(dec.date = decimal_date(chosen_date))

t5 = ggplot(heat_flux, aes(x = chosen_date, y = flux_W_m2, color = location_name)) +
  geom_line(linewidth = 1) +
  geom_point() +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
  scale_color_manual(values = usecolors, name = 'Lake') +
  # ylab("Heat Flux (W/m²)") +
  ylab('Heat Flux (W m^<sup>-2</sup>)') +
  theme_bw(base_size = 9) +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_markdown(),
        legend.position = 'bottom',
        legend.title = element_blank(),
        legend.text = element_text(size = 7),
        legend.key.size = unit(0.2,'cm'),
        plot.margin = margin(2,2,2,2),
        legend.margin = margin(0, 0, 0, 0))

t6 = t1 + t2 +t3 + t4 + plot_layout(guides = 'collect') &
  # plot_annotation(tag_levels = 'a', tag_suffix = ')') &
  theme(legend.position = 'bottom', 
  legend.key.height = unit(0.2, 'cm'),
  legend.margin = margin(0, 0, 0, 0),
  plot.margin = margin(2,2,2,2),
  legend.key.width = unit(1.5,'cm')); t6

plot_grid(t6, t5, nrow = 2, rel_heights = c(2,1), labels = c('a)', 'b)'), label_size = 9, label_fontface = "plain")

# t6 + t5 +
#   plot_layout(design = layout) + plot_annotation(tag_levels = 'a', tag_suffix = ')') &
#  theme(plot.tag = element_text(size = 8))
ggsave('figures/Fig3_HeatFlux.png', width = 6, height = 6, dpi = 500)


### Summarise by day ###
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


# Output heat data 

write_csv(hypo.fill |> select(-depth.asl.char, -specCond_5, -cutoffDepth), 'dataout/MDVLakes_depthDiscrete.csv')
write_csv(heat.day |> select(-dec.date, -yday), 'dataout/MDVLakes_heatStorage.csv')
write_csv(heat_flux |> select(-dec.date), 'dataout/MDVLakes_annualHeatFlux.csv')


