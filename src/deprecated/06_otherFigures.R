##########################################################################################
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
# ggsave('figures/Fig3_HeatMap.png', width = 6, height = 4, dpi = 500)

##########################################################################################
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

h1 + h5 + h2 + h6 + h3 + h7 + h4 + h8 +
  plot_layout(design = layout) + plot_annotation(tag_levels = 'a', tag_suffix = ')') &
  theme(plot.tag = element_text(size = 8))
ggsave('figures/SI_HeatMap2.png', width = 6, height = 6, dpi = 500)

##########################################################################################
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

ggsave('figures/SI_HeatContent.png', width = 6, height = 4, dpi = 500)


