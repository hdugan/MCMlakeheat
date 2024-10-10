source('src/functions/getBestCastDay.R')
# hypo.fill is the master data.frame 

# Summarise by day 
heat.day = hypo.fill |> 
  mutate(tempV = tempUse * vol_layer_m3) |> 
  summarise(ice.approx = mean(ice.approx, na.rm = T), heat_J = sum(heat_J, na.rm = T), heatIce_J = sum(heatIce_J, na.rm = T), 
            Area_2D = first(Area_2D), vol = sum(volUse, na.rm = T), tempV = sum(tempV, na.rm = T)/vol, tempUse = mean(tempUse, na.rm = T),
            kd.ice = first(kd.ice)) |> 
  mutate(heatTot_J_m2 = (heat_J - heatIce_J)/Area_2D) |> 
  ungroup() |> 
  mutate(location_name = factor(location_name, levels = c('Lake Fryxell','Lake Hoare', 'East Lake Bonney', 'West Lake Bonney')))

# % of ice heat
heat.day |> mutate(icep = 100*heatIce_J/heat_J) |> 
  group_by(location_name) |> 
  summarise(mean(icep, na.rm = T))

## If all water was 1°C
hypo.thetical = hypo.fill |>
  mutate(ctd_temp_c = if_else(!is.na(ctd_temp_c), 1, ctd_temp_c)) |> 
  mutate(tempUse = if_else(!is.na(tempUse), 1, tempUse)) |> 
  filter(!is.na(depth_m)) |>
  mutate(spHeat_J_kgK = SW_SpcHeat(Temp = ctd_temp_c, S = sal.pred2, P = 1 + (depth_m/10))) |> #units deafult, °C, ppt, bar
  mutate(density_kg_m3 = sw_dens(S = sal.pred2, t = ctd_temp_c, p = 1 + (depth_m/10))) |> 
  mutate(temp_K = tempUse + 273.15) %>% 
  mutate(spHeat_J_m3K = spHeat_J_kgK * density_kg_m3) %>% 
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
  # geom_smooth(data = hypo.thetical, aes(x = date_time, y = heatTot_J_m2/1e6, group = location_name), color = 'grey70', method = 'gam', alpha = 0.5) +
  # geom_point(data = hypo.thetical, aes(x = date_time, y = heatTot_J_m2/1e6, fill = location_name), shape = 21, stroke = 0.2, alpha = 0.5) +
  # 
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

############ Timeseries plot of heat vs vol, ice, and mean temp ############
ll.interp = ll.interp |> mutate(masl.zero = masl.approx - first(masl.approx))

cols <- c("tempVol" = "black", "tempMean" = 'gray50', "heat" = "red3","ice" = "lightblue4","vol" = 'gold')
ggplot(heat.day) +
  geom_path(data = ll.interp, aes(x = date_time, y = masl.zero), size = 0.5) +
  # geom_smooth(aes(x = date_time, y = tempV, col = 'tempVol'), method = 'gam', formula = y ~ s(x, bs = "cs", k = 20)) +
  # geom_point(aes(x = date_time, y = tempV, col = 'tempVol'), size = 0.5) +
  geom_smooth(aes(x = date_time, y = tempUse, col = 'tempMean'), method = 'gam', formula = y ~ s(x, bs = "cs", k = 20)) +
  geom_point(aes(x = date_time, y = tempUse, col = 'tempMean'), size = 0.5) +
  geom_smooth(aes(x = date_time, y = (heat_J - heatIce_J)/1e16, col = 'heat'), method = 'gam', formula = y ~ s(x, bs = "cs", k = 20)) +
  geom_point(aes(x = date_time, y = (heat_J - heatIce_J)/1e16, col = 'heat'), size = 0.5) +
  geom_smooth(aes(x = date_time, y = -ice.approx, col = 'ice'), method = 'gam', formula = y ~ s(x, bs = "cs", k = 20)) +
  geom_point(aes(x = date_time, y = -ice.approx, col = 'ice'), size = 0.5) +
  geom_smooth(aes(x = date_time, y = vol/1e7, col = 'vol'), method = 'gam', formula = y ~ s(x, bs = "cs", k = 20)) +
  geom_point(aes(x = date_time, y = vol/1e7, col = 'vol'), size = 0.5) +
  
  scale_colour_manual(values = cols) +
  ylab('Temp and Ice thickness, ignore heat/vol units') +
  facet_wrap(~location_name) +
  theme_bw(base_size = 9) +
  theme(axis.title.x = element_blank())

ggsave('figures/SI_TimeSeries.png', width = 6, height = 6, dpi = 500)

ggplot(heat.day) +
  geom_path(data = ll.interp, aes(x = date_time, y = masl.zero), size = 0.5) +
  # geom_smooth(aes(x = date_time, y = tempV, col = 'tempVol'), method = 'gam', formula = y ~ s(x, bs = "cs", k = 20)) +
  # geom_point(aes(x = date_time, y = tempV, col = 'tempVol'), size = 0.5) +
  geom_smooth(aes(x = date_time, y = tempUse, col = 'tempMean'), method = 'gam', formula = y ~ s(x, bs = "cs", k = 20)) +
  geom_point(aes(x = date_time, y = tempUse, col = 'tempMean'), size = 0.5) +
  geom_smooth(aes(x = date_time, y = (heat_J - heatIce_J)/1e16, col = 'heat'), method = 'gam', formula = y ~ s(x, bs = "cs", k = 20)) +
  geom_point(aes(x = date_time, y = (heat_J - heatIce_J)/1e16, col = 'heat'), size = 0.5) +
  geom_smooth(aes(x = date_time, y = -ice.approx, col = 'ice'), method = 'gam', formula = y ~ s(x, bs = "cs", k = 20)) +
  geom_point(aes(x = date_time, y = -ice.approx, col = 'ice'), size = 0.5) +
  geom_smooth(aes(x = date_time, y = vol/1e7, col = 'vol'), method = 'gam', formula = y ~ s(x, bs = "cs", k = 20)) +
  geom_point(aes(x = date_time, y = vol/1e7, col = 'vol'), size = 0.5) +
  
  scale_colour_manual(values = cols) +
  ylab('Temp and Ice thickness, ignore heat/vol units') +
  facet_wrap(~location_name) +
  theme_bw(base_size = 9) +
  theme(axis.title.x = element_blank()) +
  xlim(as.Date('2004-10-01'), NA) +
  geom_vline(aes(xintercept = as.Date('2015-10-01')), linetype = 2)

ggsave('figures/SI_TimeSeries_2005.png', width = 6, height = 6, dpi = 500)


########### Annual change ##############
# Find case date that minimizes RMSE of days between casts 
expand_grid(location_name = c('Lake Fryxell', 'Lake Hoare', 'East Lake Bonney', 'West Lake Bonney'), useday = 1:30, usemonth = c(11,12)) |> 
  rowwise() %>%
  mutate(day.rmse = getBestCastDay(heat.day, location_name, useday, usemonth)) |> 
  arrange(day.rmse) |> 
  ungroup() |> 
  group_by(location_name) |> 
  slice(1)

LF.date = yday(as.Date('2001-12-09'))
LH.date = yday(as.Date('2001-12-12'))
ELB.date = yday(as.Date('2001-12-14'))
WLB.date = yday(as.Date('2001-12-24'))


# # pull that cast 
# annual.df = pullBestCastDay(heat.day, 'Lake Fryxell', 9, 12) |> 
#   bind_rows(pullBestCastDay(heat.day, 'Lake Hoare', 12, 12)) |> 
#   bind_rows(pullBestCastDay(heat.day, 'East Lake Bonney', 14, 12)) |> 
#   bind_rows(pullBestCastDay(heat.day, 'West Lake Bonney', 24, 12)) |> 
#   mutate(deltaVol = c(NA, diff(vol)), deltatempV = c(NA, diff(tempV)), deltaIce = c(NA, -diff(ice.approx)))

# pull that cast 
annual.df = pullBestCastDay(heat.day, 'Lake Fryxell', 1, 11) |> 
  bind_rows(pullBestCastDay(heat.day, 'Lake Hoare', 1, 11)) |> 
  bind_rows(pullBestCastDay(heat.day, 'East Lake Bonney', 1, 11)) |> 
  bind_rows(pullBestCastDay(heat.day, 'West Lake Bonney', 1, 11)) |> 
  mutate(deltaVol = c(NA, diff(vol)), deltatempV = c(NA, diff(tempUse)), 
         deltaIce = c(NA, -diff(ice.approx)), deltaHeatTot = c(NA, diff(heatTot_J_m2)),
         ice.mean = (ice.approx + lag(ice.approx))/2, 
         kd.mean = (kd.ice + lag(kd.ice))/2) |>
  mutate(castoff = if_else(cast.diff > 30 | lag(cast.diff) > 30, TRUE, FALSE))
  

ggplot(annual.df) +
  geom_point(aes(x = -ice.mean, y = deltatempV, color = castoff)) + 
  scale_color_manual(values = c('lightblue4','red3')) +
  facet_wrap(~location_name) +
  theme_bw(base_size = 9) 

ggplot(annual.df) +
  geom_hline(aes(yintercept = 0), linetype = 2) +
  geom_vline(aes(xintercept = 0), linetype = 2) +
  geom_point(aes(x = deltaVol/1e6, y = deltatempV, fill = castoff), shape = 21, stroke = 0.2) + 
  scale_color_manual(values = c('#4477c9', '#e3dc10', '#b34f0c', '#4c944a'), name = 'Lake') +
  scale_fill_manual(values = c('#4477c9', '#e3dc10', '#b34f0c', '#4c944a'), name = 'Lake') +
  ylab('Annual change mean temp') + xlab('Annual change volume (x1e6)') +
  facet_wrap(~location_name, scales = 'free', nrow = 1) +
  theme_bw(base_size = 9) +
  theme(legend.position = 'none')

ggplot(annual.df) +
  geom_hline(aes(yintercept = 0), linetype = 2) +
  geom_smooth(aes(x = deltaIce, y = deltatempV), method = 'lm') + 
  geom_vline(aes(xintercept = 0), linetype = 2) +
  geom_point(aes(x = deltaIce, y = deltatempV, fill = castoff), shape = 21, stroke = 0.2) + 
  scale_color_manual(values = c('#4477c9', '#e3dc10', '#b34f0c', '#4c944a'), name = 'Lake') +
  scale_fill_manual(values = c('#4477c9', '#e3dc10', '#b34f0c', '#4c944a'), name = 'Lake') +
  ylab('Annual change mean temp') + xlab('Annual change ice thickness') +
  facet_wrap(~location_name, scales = 'free', nrow = 1) +
  theme_bw(base_size = 9) +
  theme(legend.position = 'none')

ggplot(annual.df) +
  geom_hline(aes(yintercept = 0), linetype = 2) +
  geom_smooth(aes(x = ice.mean, y = deltatempV), method = 'lm') + 
  geom_point(aes(x = ice.mean, y = deltatempV, fill = castoff), shape = 21, stroke = 0.2) + 
  scale_color_manual(values = c('#4477c9', '#e3dc10', '#b34f0c', '#4c944a'), name = 'Lake') +
  scale_fill_manual(values = c('#4477c9', '#e3dc10', '#b34f0c', '#4c944a'), name = 'Lake') +
  ylab('Annual change mean temp') + xlab('Ice thickness') +
  facet_wrap(~location_name, scales = 'free', nrow = 1) +
  theme_bw(base_size = 9) +
  theme(legend.position = 'none')

ggplot(annual.df |> filter(cast.diff < 30)) +
  geom_hline(aes(yintercept = 0), linetype = 2) +
  geom_point(aes(x = ice.mean, y = deltatempV, fill = castoff), shape = 21, stroke = 0.2) + 
  scale_color_manual(values = c('#4477c9', '#e3dc10', '#b34f0c', '#4c944a'), name = 'Lake') +
  scale_fill_manual(values = c('#4477c9', '#e3dc10', '#b34f0c', '#4c944a'), name = 'Lake') +
  ylab('Annual change mean temp') + xlab('Ice Thickness') +
  facet_wrap(~location_name, scales = 'free', nrow = 1) +
  theme_bw(base_size = 9) +
  theme(legend.position = 'none')

# 
# p1/p2
# ggsave('figures/SI_Deltas.png', width = 6, height = 6, dpi = 500)



### Some results? 
# Lake Bonney, relationship between total ice thickness and annual temp change
# Lake Hoare, relationship between annual change in ice thickness and mean temp 

