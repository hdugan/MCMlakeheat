library(MetBrewer)

# Check plot - temperature
ggplot(df.full.ice |> filter(location_name == 'Lake Fryxell', date_time >= as.Date('2007-10-01'),  date_time <= as.Date('2009-03-01'))) +
  geom_rect(data = icebox |> filter(location_name == 'Lake Fryxell', date_time >= as.Date('2007-10-01'),  date_time <= as.Date('2009-03-01')),
            aes(xmin = ctd_temp_c, xmax = ctd_temp_c, ymin = max.depth, ymax = min.depth, group = date_time), 
            color = 'grey50',size = 0.3) +
  geom_path(aes(x = tempUse, y = depth.asl, group = date_time, color = as.factor(date_time))) +
  ylab('Elevation (m asl)') + xlab('Temperature (°C)') +
  scale_colour_viridis_d(option = 'F') +
  theme_bw(base_size = 9) +
  facet_wrap(~location_name, scales = 'free', ncol = 1)


################## Heat gain in the summer ##################
cast.df = hypo.fill |> #filter(location_name == 'Lake Fryxell') |> 
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
  summarise(vol = first(cum_vol_m3), tempV = sum(tempV, na.rm = T)/vol, ice.approx = mean(ice.approx, na.rm = T), 
            heat_J = sum(heat_J, na.rm = T), heatIce_J = sum(heatIce_J, na.rm = T), 
            Area_2D = first(Area_2D)) |> 
  group_by(location_name, wyear) |> 
  mutate(daydiff = last(date_time) - first(date_time), tempdiff = last(tempV) - first(tempV))

# Heat gain versus days between casts 
ggplot(wy) +
  geom_point(aes(x = daydiff, y = tempdiff, size = -ice.approx, fill = -ice.approx), shape = 21) +
  facet_wrap(~location_name)

# Ice thickness vs heat gain
wy |> filter(daydiff >= 30 & daydiff <=60) |>
  group_by(location_name, wyear) |> 
  summarise(ice.approx = mean(ice.approx), daydiff = mean(daydiff), tempdiff = mean(tempdiff)) |> 
  ggplot() +
  geom_hline(aes(yintercept = 0), linetype = 2) +
  geom_point(aes(x = -ice.approx, y = tempdiff, fill = daydiff), shape = 21, size = 2) +
  scale_fill_met_c('Tam', name = 'Days\nbetween\ncasts') +
  ylab('Mean Temperature Difference (°C)') + 
  xlab('Ice Thickness (m)') +
  theme_bw() +
  facet_wrap(~location_name)


# Ice thickness vs heat gain
wy |> filter(daydiff >= 30 & daydiff <=60) |>
  select(location_name, wyear, daydiff, tempV, ice.approx, cast) |> 
  pivot_wider(names_from = cast, values_from = c(tempV, ice.approx)) |> 
  mutate(lastTemp = tempV_last-tempV_first, firstTemp = 0, 
         lastIce = ice.approx_last-ice.approx_first, firstIce = 0) |> 
  ggplot() +
  # geom_hline(aes(yintercept = 0), linetype = 2) +
  geom_point(aes(x = -lastIce, y = lastTemp, fill = -ice.approx_first), shape = 21, size = 2) +
  scale_fill_met_c('Tam', name = 'Ice\nThickness') +
  ylab('Mean Temperature Difference (°C)') + 
  xlab('Ice Thickness (m)') +
  theme_bw() +
  facet_wrap(~location_name)



################## Heat loss in the winter ##################
wi = cast.df |> 
  mutate(iyear = if_else(yday(date_time) >= 333, year +1, year)) |> 
  group_by(location_name, iyear, date_time, cast) |> 
  arrange(location_name, date_time, desc(depth.asl)) |> 
  summarise(vol = first(cum_vol_m3), tempV = sum(tempV, na.rm = T)/vol, ice.approx = mean(ice.approx, na.rm = T), 
            heat_J = sum(heat_J, na.rm = T), heatIce_J = sum(heatIce_J, na.rm = T), 
            Area_2D = first(Area_2D)) |> 
  group_by(location_name, iyear) |> 
  mutate(daydiff = last(date_time) - first(date_time), tempdiff = last(tempV) - first(tempV)) |> 
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


# ggplot(frx1) +
#   geom_path(aes(x = tempUse, y = depth.asl, group = date_time, color = factor(month(date_time)))) +
#   ylab('Elevation (m asl)') + xlab('Temperature (°C)') +
#   scale_color_manual(values = c('lightblue3','orange','red3','green4')) +
#   theme_bw(base_size = 9) +
#   facet_wrap(~wyear)
# 
# ggplot(frx1) +
#   geom_path(aes(x = tempUse, y = depth.asl, group = date_time, color = factor(month(date_time)))) +
#   ylab('Elevation (m asl)') + xlab('Temperature (°C)') +
#   scale_color_manual(values = c('lightblue3','orange','red3','green4')) +
#   theme_bw(base_size = 9) +
#   facet_wrap(~iyear)
