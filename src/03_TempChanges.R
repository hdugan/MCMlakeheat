
# hypo.fill is the master data.frame 

# Summarise by day 
heat.day = hypo.fill |> 
  summarise(heat_J = sum(heat_J, na.rm = T), heatIce_J = sum(heatIce_J, na.rm = T), 
            Area_2D = first(Area_2D)) |> 
  mutate(heatTot_J_m2 = (heat_J - heatIce_J)/Area_2D) |> 
  ungroup() |> 
  mutate(location_name = factor(location_name, levels = c('Lake Fryxell','Lake Hoare', 'East Lake Bonney', 'West Lake Bonney')))

# % of ice heat
heat.day |> mutate(icep = 100*heatIce_J/heat_J) |> 
  group_by(location_name) |> 
  summarise(mean(icep, na.rm = T))

## If all water was 1°C
hypo.thetical = hypo.fill |>
  mutate(ctd_temp_c = 1, sal.pred2 = 1) |> 
  filter(!is.na(ctd_temp_c), !is.na(depth_m)) |> 
  mutate(spHeat_J_kgK = SW_SpcHeat(Temp = ctd_temp_c, S = sal.pred2, P = 1 + (depth_m/10))) |> #units deafult, °C, ppt, bar
  mutate(density_kg_m3 = sw_dens(S = sal.pred2, t = ctd_temp_c, p = 1 + (depth_m/10))) |> 
  mutate(temp_K = 1 + 273.15) %>% 
  mutate(spHeat_J_m3K = spHeat_J_kgK * 1000) %>% 
  # latent heat of ice = density * thickness *  latent heat of ice (334000 J/kg)
  mutate(LHice_J_m3 = iceDensity_kgm3 * 334000) |> 
  mutate(heatIce_J = LHice_J_m3 * vol_layer_m3) |> 
  mutate(heat_J = spHeat_J_m3K * vol_layer_m3 * temp_K) |> 
  summarise(heat_J = sum(heat_J, na.rm = T), heatIce_J = sum(heatIce_J, na.rm = T), 
            Area_2D = first(Area_2D)) |> 
  mutate(heatTot_J_m2 = (heat_J - heatIce_J)/Area_2D) |> 
  ungroup() |> 
  mutate(location_name = factor(location_name, levels = c('Lake Fryxell','Lake Hoare', 'East Lake Bonney', 'West Lake Bonney')))

######### Plot timeseries ##########
ggplot(heat.day) +
  geom_smooth(aes(x = date_time, y = heatTot_J_m2/1e6, color = location_name), method = 'gam') +
  geom_point(aes(x = date_time, y = heatTot_J_m2/1e6, fill = location_name), shape = 21, stroke = 0.2) +
  
  geom_smooth(data = hypo.thetical, aes(x = date_time, y = heatTot_J_m2/1e6, group = location_name), color = 'grey70', method = 'gam', alpha = 0.5) +
  geom_point(data = hypo.thetical, aes(x = date_time, y = heatTot_J_m2/1e6, fill = location_name), shape = 21, stroke = 0.2, alpha = 0.5) +
  
  scale_color_manual(values = c('#4477c9', '#e3dc10', '#b34f0c', '#4c944a'), name = 'Lake') +
  scale_fill_manual(values = c('#4477c9', '#e3dc10', '#b34f0c', '#4c944a'), name = 'Lake') +
  ylab('Heat storage (MJ m^2 )') +
  theme_bw(base_size = 9) +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_markdown(),
        legend.position = 'bottom',
        legend.title = element_blank(),
        legend.text = element_text(size = 7),
        legend.key.size = unit(0.2,'cm'),
        legend.margin = margin(0, 0, 0, 0))
# facet_wrap(~location_name) 

ggsave('figures/Fig2_Heat_TimeSeries.png', width = 4, height = 2.5, dpi = 500)

# Output heat data. 
write_csv(heat.day, 'dataout/MDVLakes_dailyHeatStorage.csv')

meanTemp = hypo.fill |> 
  filter(month(date_time) %in% c(11,12,1)) |> 
  mutate(tempV = tempUse * vol_layer_m3) |> 
  group_by(location_name, date_time) |> 
  arrange(location_name, date_time, desc(depth.asl)) |> 
  summarise(vol = first(cum_vol_m3), tempV = sum(tempV, na.rm = T)/vol, ice.approx = mean(ice.approx, na.rm = T), 
            heat_J = sum(heat_J, na.rm = T), heatIce_J = sum(heatIce_J, na.rm = T), 
                      Area_2D = first(Area_2D))

ggplot(meanTemp) +
  geom_path(aes(x = date_time, y = tempV)) +
  geom_point(aes(x = date_time, y = tempV)) +
  geom_path(aes(x = date_time, y = (heat_J - heatIce_J)/1e16), col = 'red3') +
  geom_point(aes(x = date_time, y = (heat_J - heatIce_J)/1e16), col = 'red3') +
  geom_path(aes(x = date_time, y = -ice.approx), col = 'lightblue2') +
  geom_point(aes(x = date_time, y = -ice.approx), col = 'lightblue2') +
  facet_wrap(~location_name) +
  theme_bw()

