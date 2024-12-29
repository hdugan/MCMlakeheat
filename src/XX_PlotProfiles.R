library(tidyverse)

usename = 'Lake Hoare'
useshort = 'LH'

############## Raw Plots #############
test = ctd |> filter(location_name == usename) |> group_by(date_time)
for (i in 1:length(unique(test$date_time))) {
  
  ggplot(test |> filter(cur_group_id() <= i)) +
    geom_path(aes(x = ctd_temp_c, y = depth.asl, group = date_time, color = year(date_time))) +
    geom_path(data = test |> filter(cur_group_id() == i), aes(x = ctd_temp_c, y = depth.asl), color = 'gold', linewidth = 2) +
    ylab('Elevation (m asl)') + xlab('Temperature (°C)') +
    scale_colour_viridis_c(option = 'F', name = 'Year') +
    theme_bw(base_size = 9) +
    labs(subtitle = test |> filter(cur_group_id() == i) |> pull(date_time) |> unique()) +
    facet_wrap(~location_name, scales = 'free', ncol = 1) +
    theme_bw(base_size = 8)

ggsave(paste0('figures/TempPlots_',useshort,'_raw/',useshort,'_temp_',i,'.png'), width = 5, height = 5, dpi = 500)

ggplot(test |> filter(cur_group_id() <= i)) +
  geom_path(aes(x = ctd_conductivity_mscm, y = depth.asl, group = date_time, color = year(date_time))) +
  geom_path(data = test |> filter(cur_group_id() == i), aes(x = ctd_conductivity_mscm, y = depth.asl), color = 'gold', linewidth = 2) +
  ylab('Elevation (m asl)') + xlab('Conductivity (mS/cm)') +
  scale_colour_viridis_c(option = 'F', name = 'Year') +
  theme_bw(base_size = 9) +
  labs(subtitle = test |> filter(cur_group_id() == i) |> pull(date_time) |> unique()) +
  facet_wrap(~location_name, scales = 'free', ncol = 1) +
  theme_bw(base_size = 8)

ggsave(paste0('figures/CondPlots_',useshort,'_raw/',useshort,'_cond_',i,'.png'), width = 5, height = 5, dpi = 500)

}


############## Cleaned Plots #############
test = df.full.ice |> filter(location_name == usename) |> group_by(date_time)
for (i in 1:length(unique(test$date_time))) {
  
  ggplot(test |> filter(cur_group_id() <= i)) +
    geom_path(aes(x = tempUse, y = depth.asl, group = date_time, color = year(date_time))) +
    geom_path(data = test |> filter(cur_group_id() == i), aes(x = tempUse, y = depth.asl), color = 'gold', linewidth = 2) +
    ylab('Elevation (m asl)') + xlab('Temperature (°C)') +
    scale_colour_viridis_c(option = 'F', name = 'Year') +
    theme_bw(base_size = 9) +
    labs(subtitle = test |> filter(cur_group_id() == i) |> pull(date_time) |> unique()) +
    facet_wrap(~location_name, scales = 'free', ncol = 1) +
    theme_bw(base_size = 8)
  
  ggsave(paste0('figures/TempPlots_',useshort,'/',useshort,'_temp_',i,'.png'), width = 5, height = 5, dpi = 500)
  
  ggplot(test |> filter(cur_group_id() <= i)) +
    geom_path(aes(x = condUse, y = depth.asl, group = date_time, color = year(date_time))) +
    geom_path(data = test |> filter(cur_group_id() == i), aes(x = condUse, y = depth.asl), color = 'gold', linewidth = 2) +
    ylab('Elevation (m asl)') + xlab('Conductivity (mS/cm)') +
    scale_colour_viridis_c(option = 'F', name = 'Year') +
    theme_bw(base_size = 9) +
    labs(subtitle = test |> filter(cur_group_id() == i) |> pull(date_time) |> unique()) +
    facet_wrap(~location_name, scales = 'free', ncol = 1) +
    theme_bw(base_size = 8)
  
  ggsave(paste0('figures/CondPlots_',useshort,'/',useshort,'_cond_',i,'.png'), width = 5, height = 5, dpi = 500)  
}
