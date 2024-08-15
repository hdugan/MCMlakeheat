library(MetBrewer)

# Extended Seasons
exSeas = hypo.fill |> filter(date_time >= as.Date('2007-10-01'),  date_time <= as.Date('2008-05-01'))

exSeas |> summarise(heat_J = sum(heat_J, na.rm = T), Area_2D = first(Area_2D), heat_J_m2 = heat_J/Area_2D) |> 
  mutate(heat_J_zero = heat_J_m2 - first(heat_J_m2)) |> 
  ggplot() +
  geom_path(aes(x = date_time, y = heat_J_zero, color = location_name)) +
  geom_point(aes(x = date_time, y = heat_J_zero, fill = location_name), shape = 21, stroke = 0.2, size = 2) +
  scale_color_manual(values = c('#4477c9', '#e3dc10', '#b34f0c', '#4c944a'), name = 'Lake') +
  scale_fill_manual(values = c('#4477c9', '#e3dc10', '#b34f0c', '#4c944a'), name = 'Lake') +
  ylab('Heat storage (J)') +
  theme_bw(base_size = 9) +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_markdown(),
        legend.position = 'bottom',
        legend.title = element_blank(),
        legend.text = element_text(size = 7),
        legend.key.size = unit(0.2,'cm'),
        legend.margin = margin(0, 0, 0, 0))
ggsave('figures/SI_extendedseason.png', width = 4, height = 2.5, dpi = 500)


# Check plot - temperature Extended Seasons
ggplot(exSeas) +
  geom_path(aes(x = tempUse, y = depth.asl, group = date_time, color = as.factor(date_time))) +
  ylab('Elevation (m asl)') + xlab('Temperature (°C)') +
  scale_colour_viridis_d(option = 'F') +
  theme_bw(base_size = 9) +
  facet_wrap(~location_name, scales = 'free', ncol = 1)

# Check plot - temperature
ggplot(df.full.ice |> filter(location_name == 'East Lake Bonney', date_time >= as.Date('2022-10-01'))) +
  geom_rect(data = icebox |> filter(location_name == 'East Lake Bonney', date_time >= as.Date('2022-10-01')),
            aes(xmin = ctd_temp_c, xmax = ctd_temp_c, ymin = max.depth, ymax = min.depth, group = date_time), 
            color = 'grey50',size = 0.3) +
  geom_path(aes(x = tempUse, y = depth.asl, group = date_time, color = as.factor(date_time))) +
  ylab('Elevation (m asl)') + xlab('Temperature (°C)') +
  scale_colour_viridis_d(option = 'F') +
  theme_bw(base_size = 9) +
  facet_wrap(~location_name, scales = 'free', ncol = 1)


################## Heat gain in the summer ##################
cast.df = hypo.fill |> #filter(location_name == 'Lake Fryxell') |> 
  filter(date_time != '2023-11-13') |>  # filter out 2nd cast from 2023, because it will be coded as last
  mutate(wyear = if_else(month(date_time) >= 10, year +1, year)) |> 
  filter(month(date_time) >= 10 | month(date_time) <= 2) |> # Filter out casts in March +
  group_by(location_name, wyear) |> 
  mutate(cast = case_when(date_time == first(date_time) ~ 'first',
                          date_time == last(date_time) ~ 'last', 
                          TRUE ~ 'mid')) |> 
  filter(cast %in% c('first','last')) |>
  mutate(tempV = tempUse * vol_layer_m3)

wy = cast.df |> 
  group_by(location_name, wyear, date_time, cast) |> 
  arrange(location_name, date_time, desc(depth.asl)) |> 
  summarise(vol = sum(volUse, na.rm = T), tempV = sum(tempV, na.rm = T)/vol, tempUse = mean(tempUse, na.rm = T), 
            ice.approx = mean(ice.approx, na.rm = T), 
            heat_J = sum(heat_J, na.rm = T), heatIce_J = sum(heatIce_J, na.rm = T), 
            Area_2D = first(Area_2D), kd.ice = first(kd.ice)) |> 
  group_by(location_name, wyear) |> 
  mutate(daydiff = last(date_time) - first(date_time), tempdiff = last(tempUse) - first(tempUse), 
         kd.mean = mean(kd.ice, na.rm =  T), ice.mean = mean(ice.approx, na.rm = T))

# Heat gain versus days between casts 
ggplot(wy) +
  geom_point(aes(x = daydiff, y = tempdiff, size = -ice.approx, fill = -ice.approx), shape = 21) +
  facet_wrap(~location_name) +
  geom_hline(aes(yintercept = 0), linetype = 2)

# Ice thickness vs heat gain
wy.summer = wy |> filter(daydiff >= 25 & daydiff <=60) |>
  select(location_name, wyear, daydiff, kd.mean, ice.mean, kd.ice, tempUse, ice.approx, cast) |> 
  pivot_wider(names_from = cast, values_from = c(tempUse, ice.approx, kd.ice)) |> 
  mutate(lastTemp = tempUse_last-tempUse_first, firstTemp = 0, 
         lastIce = ice.approx_last-ice.approx_first, firstIce = 0) 

ggplot(wy.summer) +
  geom_hline(aes(yintercept = 0), linetype = 2) +
  geom_point(aes(x = -ice.approx_last, y = lastTemp, fill = daydiff), shape = 21, size = 2) +
  scale_fill_met_c('Tam', name = 'Day Diff') +
  ylab('Mean Temperature Difference (°C)') + 
  xlab('Ice Thickness (m)') +
  theme_bw() +
  facet_wrap(~location_name)

ggplot(wy.summer) +
  geom_hline(aes(yintercept = 0), linetype = 2) +
  geom_point(aes(x = kd.mean * ice.mean, y = lastTemp, fill = -ice.approx_first), shape = 21, size = 2) +
  scale_fill_met_c('Tam', name = 'Ice\nThickness') +
  ylab('Mean Temperature Difference (°C)') + 
  xlab('Kd * Ice thickness') +
  theme_bw() +
  facet_wrap(~location_name)

################## Heat loss in the winter ##################
wi = cast.df |> 
  mutate(iyear = if_else(yday(date_time) >= 333, year +1, year)) |> 
  group_by(location_name, iyear, date_time, cast) |> 
  arrange(location_name, date_time, desc(depth.asl)) |> 
  summarise(vol = sum(volUse, na.rm = T), tempV = sum(tempV, na.rm = T)/vol,  tempUse = mean(tempUse, na.rm = T), 
            ice.approx = mean(ice.approx, na.rm = T), 
            heat_J = sum(heat_J, na.rm = T), heatIce_J = sum(heatIce_J, na.rm = T), 
            Area_2D = first(Area_2D), kd.ice = first(kd.ice)) |> 
  group_by(location_name, iyear) |> 
  mutate(daydiff = last(date_time) - first(date_time), tempdiff = last(tempUse) - first(tempUse), 
         kd.mean = mean(kd.ice, na.rm =  T), ice.mean = mean(ice.approx, na.rm = T)) |> 
  filter(yday(first(date_time)) >= 349 | yday(first(date_time)) <= 90 ) # Dec 15th - spring

# Heat gain versus days between casts 
ggplot(wi |> filter(daydiff > 100)) +
  geom_point(aes(x = daydiff, y = tempdiff, size = -ice.approx, fill = -ice.approx), shape = 21) +
  facet_wrap(~location_name)

# Ice thickness vs heat gain
wi |> filter(daydiff >= 290) |> 
  ggplot() +
  geom_hline(aes(yintercept = 0), linetype = 2) +
  geom_point(aes(x = -ice.approx, y = tempdiff, fill = daydiff), shape = 21, size = 2) +
  scale_fill_met_c('Tam', name = 'Days\nbetween\ncasts') +
  ylab('Mean Temperature Difference (°C)') + 
  xlab('Ice Thickness (m)') +
  theme_bw() +
  facet_wrap(~location_name)
 
wi |> filter(tempdiff < -0.25, location_name == 'East Lake Bonney')


# ggplot(cast.df) +
#   geom_path(aes(x = tempUse, y = depth.asl, group = date_time, color = factor(month(date_time)))) +
#   ylab('Elevation (m asl)') + xlab('Temperature (°C)') +
#   scale_color_manual(values = c('lightblue3','orange','red3','green4')) +
#   theme_bw(base_size = 9) +
#   facet_wrap(~wyear)
 
# ggplot(frx1) +
#   geom_path(aes(x = tempUse, y = depth.asl, group = date_time, color = factor(month(date_time)))) +
#   ylab('Elevation (m asl)') + xlab('Temperature (°C)') +
#   scale_color_manual(values = c('lightblue3','orange','red3','green4')) +
#   theme_bw(base_size = 9) +
#   facet_wrap(~iyear)
