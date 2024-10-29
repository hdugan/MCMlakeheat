

test = df.full.ice |> filter(location_name == 'West Lake Bonney')
for (i in 1:100) {
  
  ggplot(test |> filter(cur_group_id() <= i)) +
    # geom_rect(data = icebox |> filter(location_name == 'Lake Fryxell'),
    #           aes(xmin = ctd_temp_c, xmax = ctd_temp_c, ymin = max.depth, ymax = min.depth, group = year(date_time)), 
    #           color = 'grey50',size = 0.3) +
    geom_path(aes(x = tempUse, y = depth.asl, group = date_time, color = year(date_time))) +
    geom_path(data = test |> filter(cur_group_id() == i), aes(x = tempUse, y = depth.asl), color = 'gold', linewidth = 2) +
    ylab('Elevation (m asl)') + xlab('Temperature (°C)') +
    scale_colour_viridis_c(option = 'F', name = 'Year') +
    theme_bw(base_size = 9) +
    labs(subtitle = test |> filter(cur_group_id() == i) |> pull(date_time) |> unique()) +
    facet_wrap(~location_name, scales = 'free', ncol = 1) +
    theme_bw(base_size = 8)

ggsave(paste0('figures/TempPlots_WLB/WLB_temp_',i,'.png'), width = 5, height = 5, dpi = 500)

}


test = df.full.ice |> filter(location_name == 'East Lake Bonney')
for (i in 1:100) {
  
  ggplot(test |> filter(cur_group_id() <= i)) +
    # geom_rect(data = icebox |> filter(location_name == 'Lake Fryxell'),
    #           aes(xmin = ctd_temp_c, xmax = ctd_temp_c, ymin = max.depth, ymax = min.depth, group = year(date_time)), 
    #           color = 'grey50',size = 0.3) +
    geom_path(aes(x = tempUse, y = depth.asl, group = date_time, color = year(date_time))) +
    geom_path(data = test |> filter(cur_group_id() == i), aes(x = tempUse, y = depth.asl), color = 'gold', linewidth = 2) +
    ylab('Elevation (m asl)') + xlab('Temperature (°C)') +
    scale_colour_viridis_c(option = 'F', name = 'Year') +
    theme_bw(base_size = 9) +
    labs(subtitle = test |> filter(cur_group_id() == i) |> pull(date_time) |> unique()) +
    facet_wrap(~location_name, scales = 'free', ncol = 1) +
    theme_bw(base_size = 8)
  
  ggsave(paste0('figures/TempPlots_ELB/ELB_temp_',i,'.png'), width = 5, height = 5, dpi = 500)
  
}

test = df.full.ice |> filter(location_name == 'West Lake Bonney')
for (i in 1:100) {
  
  ggplot(test |> filter(cur_group_id() <= i)) +
    geom_path(aes(x = condUse, y = depth.asl, group = date_time, color = year(date_time))) +
    geom_path(data = test |> filter(cur_group_id() == i), aes(x = condUse, y = depth.asl), color = 'gold', linewidth = 2) +
    ylab('Elevation (m asl)') + xlab('Cond (mS/cm)') +
    scale_colour_viridis_c(option = 'F', name = 'Year') +
    theme_bw(base_size = 9) +
    labs(subtitle = test |> filter(cur_group_id() == i) |> pull(date_time) |> unique()) +
    facet_wrap(~location_name, scales = 'free', ncol = 1) +
    theme_bw(base_size = 8)
  
  ggsave(paste0('figures/CondsPlots_WLB/WLB_cond_',i,'.png'), width = 5, height = 5, dpi = 500)
  
}
