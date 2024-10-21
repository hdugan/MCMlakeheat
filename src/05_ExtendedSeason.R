library(MetBrewer)

# Extended Seasons
exSeas = hypo.fill |> filter(date_time >= as.Date('2007-10-01'),  date_time <= as.Date('2008-05-01'))

ex.sum = exSeas |> 
  mutate(tempV = tempUse * vol_layer_m3) |> 
  summarise(heat_J = sum(heat_J, na.rm = T), Area_2D = first(Area_2D), heat_J_m2 = heat_J/Area_2D, 
            tempUse = mean(tempUse, na.rm = T),
            vol = sum(volUse, na.rm = T), tempV = sum(tempV, na.rm = T)/vol) |> 
  mutate(heat_J_zero = heat_J_m2 - first(heat_J_m2), 
         temp_zero = tempUse - first(tempUse), 
         tempV_zero = tempV - first(tempV), 
         vol_zero = vol - first(vol))

useTheme =  list(scale_color_manual(values = c('#4477c9', '#e3dc10', '#b34f0c', '#4c944a'), name = 'Lake'),
  scale_fill_manual(values = c('#4477c9', '#e3dc10', '#b34f0c', '#4c944a'), name = 'Lake'),
  theme_bw(base_size = 9),
  theme(axis.title.x = element_blank(),
        axis.title.y = element_markdown(),
        legend.position = 'bottom',
        legend.title = element_blank(),
        legend.text = element_text(size = 7),
        legend.key.size = unit(0.2,'cm'),
        legend.margin = margin(0, 0, 0, 0))
  )

# Plot volume
ex1 = ggplot(ex.sum) +
  geom_path(aes(x = date_time, y = vol_zero, color = location_name)) +
  geom_point(aes(x = date_time, y = vol_zero, fill = location_name), shape = 21, stroke = 0.2, size = 2) +
  ylab('Vol Change (m3)') +
  useTheme

# Plot heat storage change 
ex2 = ggplot(ex.sum) +
  geom_path(aes(x = date_time, y = heat_J_zero, color = location_name)) +
  geom_point(aes(x = date_time, y = heat_J_zero, fill = location_name), shape = 21, stroke = 0.2, size = 2) +
  ylab('Heat storage (J)') +
  useTheme

# Plot temp increase 
ex3 = ggplot(ex.sum) +
  geom_path(aes(x = date_time, y = temp_zero, color = location_name)) +
  geom_point(aes(x = date_time, y = temp_zero, fill = location_name), shape = 21, stroke = 0.2, size = 2) +
  ylab('Mean (non-volumetric) temp increase') +
  useTheme

ex1 / ex2 / ex3

ggsave('figures/SI_extendedseason.png', width = 4, height = 6, dpi = 500)


# Check plot - temperature Extended Seasons
ggplot(exSeas |> filter(location_name == 'Lake Hoare')) +
  geom_path(aes(x = tempUse, y = depth.asl, group = date_time, color = as.factor(date_time))) +
  ylab('Elevation (m asl)') + xlab('Temperature (°C)') +
  scale_colour_viridis_d(option = 'F') +
  theme_bw(base_size = 9) +
  facet_wrap(~location_name, scales = 'free', ncol = 1)


