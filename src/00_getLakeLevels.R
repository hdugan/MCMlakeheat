# Load libraries
library(tidyverse)
library(lubridate)
library(zoo)

source('src/00_gethypso.R')

# Doran, P. and M. Gooseff. 2023. Lake level surveys in the McMurdo Dry Valleys, Antarctica (1991-2023, ongoing) 
# ver 13. Environmental Data Initiative. 
# https://doi.org/10.6073/pasta/927439563d37c9461011e0060a5c1a87 (Accessed 2024-06-25).

inUrl1  <- "https://pasta.lternet.edu/package/data/eml/knb-lter-mcm/68/13/76751b6f6ffa289845a830d6581d52fe" 
infile1 <- tempfile()
download.file(inUrl1,infile1,method="curl")

ll2023 = read_csv('datain/511_2023/ll_2023.csv')

ll <- read_csv(infile1) |> 
  mutate(date_time = as.Date(mdy_hm(date_time))) |> 
  bind_rows(ll2023) |> 
  rename(masl = `lake level(masl)`) |> 
  select(-`reference benchmark`, -`surveying benchmark`, -comments, -dataset_code) |> 
  filter(lake %in% c('Lake Bonney', 'Lake Fryxell','Lake Hoare')) |> 
  mutate(location_name = case_when(lake == 'Lake Fryxell' ~ 'Lake Fryxell', 
                                   lake == 'Lake Hoare' ~ 'Lake Hoare', 
                                   lake == 'Lake Bonney' ~ 'East Lake Bonney')) %>% 
  bind_rows(. |> filter(lake == 'Lake Bonney') |> mutate(location_name = 'West Lake Bonney')) |> # Duplicate Bonney
  select(-lake) |> 
  group_by(date_time, location_name) |>
  summarise_all(mean, na.rm = T) |> 
  ungroup()

# Set up lake volume object
lake.volume = ll |> 
  filter(!is.na(masl)) |> 
  mutate(masl = as.character(round(masl,1))) |> 
  left_join(hypo_new |> ungroup() |> select(-lake) |> 
              mutate(Elevation_masl = as.character(round(Elevation_masl,1))), 
            by = c('masl' = 'Elevation_masl', 'location_name')) |> 
  select(-vol_layer_m3) |> 
  mutate(masl = as.numeric(masl)) |> 
  mutate(location_name = factor(location_name, levels = c('Lake Fryxell','Lake Hoare', 'East Lake Bonney', 'West Lake Bonney')))

# Approximate and duplicate for Lake Bonney
ll.interp = expand_grid(date_time = seq.Date(as.Date('1991-01-26'), as.Date('2023-11-23'), by = 'day'),
                        location_name = c('Lake Fryxell','Lake Hoare', 'East Lake Bonney', 'West Lake Bonney')) |> 
  left_join(lake.volume |> select(date_time, location_name, masl, Area_2D, cum_vol_m3)) |> 
  arrange(location_name, date_time, masl) |> 
  group_by(location_name) |>
  mutate(masl.approx = na.approx(masl, na.rm = FALSE)) |> 
  mutate(Area_2D.approx = na.approx(Area_2D, na.rm = FALSE)) |> 
  mutate(Vol_3D.approx = na.approx(cum_vol_m3, na.rm = FALSE)) |> 
  mutate(location_name = factor(location_name, levels = c('Lake Fryxell','Lake Hoare', 'East Lake Bonney', 'West Lake Bonney')))


# Priscu, J. 2023. Lake ice thickness and density measurements, McMurdo Dry Valleys, Antarctica (1989-2023, ongoing) ver 14. 
# Environmental Data Initiative. https://doi.org/10.6073/pasta/515c54434ee203a7611ed7db1e2501ae (Accessed 2024-07-02).
# Package ID: knb-lter-mcm.67.14 Cataloging System:https://pasta.edirepository.org.
inUrl1  <- "https://pasta.lternet.edu/package/data/eml/knb-lter-mcm/67/14/204ecca57a10a759532ba520376433ab" 
infile1 <- tempfile()
download.file(inUrl1,infile1,method="curl")

# Get 2023 ice thickness (will be online soon)
ice2023 = read_csv('datain/511_2023/ice_2023.csv')

ice <- read_csv(infile1) |> bind_rows(ice2023) |> 
  mutate(date_time = as.Date(mdy_hm(date_time))) |> 
  filter(year(date_time) > 1992) |> 
  filter(lake %in% c('East Lake Bonney', 'West Lake Bonney', 'Lake Fryxell','Lake Hoare')) |> 
  mutate(location_name = lake) |> 
  select(-lake, -dataset_code, -filename, -density, -tool, -z_diff_m, -comments, -lat, -lon) |> 
  filter(!(location_name == 'West Lake Bonney' & z_water_m > -2.5)) |> # remove outlier
  filter(!(location_name == 'Lake Fryxell' & date_time == as.Date('2022-11-28'))) |>  # remove outlier
  group_by(location_name, date_time) |> 
  summarise_all(mean) |> 
  ungroup() |> 
  mutate(location_name = factor(location_name, levels = c('Lake Fryxell','Lake Hoare', 'East Lake Bonney', 'West Lake Bonney')))

# Interpolate ice thickness
ice.interp = expand_grid(location_name = c('East Lake Bonney', 'West Lake Bonney', 'Lake Fryxell','Lake Hoare'),
                         date_time = seq.Date(as.Date('1993-12-09'), as.Date('2023-11-23'), by = 'day')) |> 
  left_join(ice) |> 
  group_by(location_name) |> 
  mutate(ice.approx = na.approx(z_water_m, na.rm = FALSE, rule = 2)) |> 
  left_join(ll.interp, by = join_by(location_name, date_time)) |>  # Join by masl 
  mutate(ice.asl = masl.approx + ice.approx) |> 
  select(location_name, date_time, ice.approx, ice.asl) |> 
  mutate(location_name = factor(location_name, levels = c('Lake Fryxell','Lake Hoare', 'East Lake Bonney', 'West Lake Bonney')))


# Join ice to lake level to ice to get volume of lake under ice. 
lake.volume.ice = ll |> 
  left_join(ice.interp, by = join_by(date_time, location_name)) |> 
  filter(!is.na(ice.approx)) |> 
  mutate(ice.asl = as.character(round(ice.asl,1))) |> 
  left_join(hypo_new |> ungroup() |> select(-lake) |> 
              mutate(Elevation_masl = as.character(round(Elevation_masl,1))), 
            by = c('ice.asl' = 'Elevation_masl', 'location_name')) |> 
  select(-vol_layer_m3) |> 
  mutate(ice.asl = as.numeric(ice.asl)) |> 
  rename(cum_vol_m3_ice = cum_vol_m3, Area_2D_ice = Area_2D) |> 
  mutate(location_name = factor(location_name, levels = c('Lake Fryxell','Lake Hoare', 'East Lake Bonney', 'West Lake Bonney')))


################################### STATS ###################################
### Calculate current area
ll.interp |> group_by(location_name) |> 
  filter(!is.na(masl)) |> 
  summarise_all(last) |> 
  mutate(Areakm2 = Area_2D/1e6)

################################### GRAPHS ###################################
# Graph with linear interpolation
ggplot(ll.interp) +
  geom_path(aes(x = date_time, y = masl.approx), col = 'lightblue4') +
  geom_point(aes(x = date_time, y = masl, fill = location_name), shape = 21, stroke = 0.2) +
  scale_fill_manual(values = c('#4477c9', '#e3dc10', '#b34f0c','#4c944a'), name = 'Lake') +
  facet_wrap(~location_name, scales = 'free', nrow = 4) +
  ylab('Lake Level (m asl)') +
  theme_bw(base_size = 9) +
  theme(axis.title.x = element_blank(), 
        legend.position = 'none')

ggsave('figures/SI_lakelevel.png', width = 6, height = 4, dpi = 500)

ggplot(ice.interp) +
  geom_path(aes(x = date_time, y = -ice.approx, color = location_name)) +
  geom_point(data = ice, aes(x = date_time, y = -z_water_m, fill = location_name), shape = 21, stroke = 0.2, size = 1) +
  # geom_path(aes(x = date_time, y = ice.approx)) +
  scale_color_manual(values = c('#4477c9', '#e3dc10', '#b34f0c', '#4c944a'), name = 'Lake') +
  scale_fill_manual(values = c('#4477c9', '#e3dc10', '#b34f0c', '#4c944a'), name = 'Lake') +
  ylab('Ice Thickness (m)') +
  theme_bw(base_size = 9) +
  # facet_wrap(~location_name, scales = 'free', nrow = 1) +
  theme(axis.title.x = element_blank(),
        legend.position = 'bottom',
        legend.title = element_blank(),
        legend.text = element_text(size = 7), 
        legend.key.size = unit(0.2,'cm'),
        legend.margin = margin(0, 0, 0, 0))

ggsave('figures/SI_IceThickness.png', width = 6, height = 2, dpi = 500)


ggplot(lake.volume) +
  geom_smooth(aes(x = date_time, y = cum_vol_m3, color = location_name),
              method = 'gam', formula = y ~ s(x, bs = "cs", k = 15)) +
  geom_point(aes(x = date_time, y = cum_vol_m3, fill = location_name), shape = 21, stroke = 0.2, size = 1) +
  geom_smooth(data = lake.volume.ice, aes(x = date_time, y = cum_vol_m3_ice, color = location_name), linewidth = 0.4, 
              method = 'gam', formula = y ~ s(x, bs = "cs", k = 15)) + #k = number of inflection points
  geom_point(data = lake.volume.ice, aes(x = date_time, y = cum_vol_m3_ice, fill = location_name), shape = 22, stroke = 0.2, size = 1) +
  # geom_path(aes(x = date_time, y = ice.approx)) +
  scale_color_manual(values = c('#4477c9', '#e3dc10', '#b34f0c', '#4c944a'), name = 'Lake') +
  scale_fill_manual(values = c('#4477c9', '#e3dc10', '#b34f0c', '#4c944a'), name = 'Lake') +
  ylab('Volume (m^3 )') +
  theme_bw(base_size = 9) +
  facet_wrap(~location_name, scales = 'free', nrow = 4) +
  theme(axis.title.x = element_blank(),
        legend.position = 'bottom',
        legend.title = element_blank(),
        legend.text = element_text(size = 7), 
        legend.key.size = unit(0.2,'cm'),
        legend.margin = margin(0, 0, 0, 0), 
        axis.title.y = element_markdown())

ggsave('figures/SI_Volume.png', width = 6, height = 6, dpi = 500)
