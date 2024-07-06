# Load libraries
library(tidyverse)
library(lubridate)
library(patchwork)

source('src/00_getLakeLevels.R')

# Priscu, J. 2023. Lake ice thickness and density measurements, McMurdo Dry Valleys, Antarctica (1989-2023, ongoing) ver 14. 
# Environmental Data Initiative. https://doi.org/10.6073/pasta/515c54434ee203a7611ed7db1e2501ae (Accessed 2024-07-02).


# Package ID: knb-lter-mcm.67.14 Cataloging System:https://pasta.edirepository.org.
inUrl1  <- "https://pasta.lternet.edu/package/data/eml/knb-lter-mcm/67/14/204ecca57a10a759532ba520376433ab" 
infile1 <- tempfile()
download.file(inUrl1,infile1,method="curl")

ice <- read_csv(infile1) |> 
  mutate(date_time = as.Date(mdy_hm(date_time))) |> 
  filter(year(date_time) > 1992) |> 
  filter(lake %in% c('East Lake Bonney', 'West Lake Bonney', 'Lake Fryxell','Lake Hoare')) |> 
  select(-location_name, -dataset_code, -filename, -density, -tool, -z_diff_m, -comments) |> 
  rename(location_name = lake) |> 
  filter(!(location_name == 'West Lake Bonney' & z_water_m > -2.5)) |> # remove outlier
  filter(!(location_name == 'Lake Fryxell' & date_time == as.Date('2022-11-28'))) |>  # remove outlier
  group_by(location_name, date_time) |> 
  summarise_all(mean) |> 
  ungroup() |> 
  mutate(location_name = factor(location_name, levels = c('Lake Fryxell','Lake Hoare', 'East Lake Bonney', 'West Lake Bonney')))


ice.interp = expand_grid(location_name = c('East Lake Bonney', 'West Lake Bonney', 'Lake Fryxell','Lake Hoare'),
                         date_time = seq.Date(as.Date('1993-12-09'), as.Date('2023-01-22'), by = 'day')) |> 
  left_join(ice) |> 
  mutate(lake = case_when(location_name == 'Lake Fryxell' ~ 'Lake Fryxell', 
                          location_name == 'Lake Hoare' ~ 'Lake Hoare', 
                          location_name == 'East Lake Bonney' ~ 'Lake Bonney', 
                          location_name == 'West Lake Bonney' ~ 'Lake Bonney')) |> 
  group_by(location_name) |> 
  mutate(ice.approx = na.approx(z_water_m, na.rm = FALSE, rule = 2)) |> 
  left_join(ll.interp, by = join_by(lake, date_time, location_name)) |>  # Join by masl 
  mutate(ice.asl = masl.approx + ice.approx) |> 
  select(location_name, date_time, ice.approx, ice.asl) |> 
  mutate(location_name = factor(location_name, levels = c('Lake Fryxell','Lake Hoare', 'East Lake Bonney', 'West Lake Bonney')))


# Check for wonkiness
ggplot(ice) +
  geom_point(aes(x = date_time, y = z_water_m)) +
  geom_path(aes(x = date_time, y = z_water_m)) +
  facet_wrap(~location_name)

ggplot(ice.interp) +
  geom_path(aes(x = date_time, y = -ice.approx, color = location_name), width = 0.2) +
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
