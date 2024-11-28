# Load libraries
library(tidyverse)
library(lubridate)
library(patchwork)

source('src/00_getLakeLevels.R')

# Takacs-Vesbach, C. and J. Priscu. 2024. Underwater photosynthetically active radiation (PAR) vertical profiles 
# collected from lakes in the McMurdo Dry Valleys, Antarctica (1993-2023, ongoing) ver 16. Environmental Data Initiative. 
# https://doi.org/10.6073/pasta/89139a7d28cef6b4dce8b4ff29b39d0c (Accessed 2024-07-22).


# Package ID: knb-lter-mcm.46.16 Cataloging System:https://pasta.edirepository.org.
inUrl1  <- "https://pasta.lternet.edu/package/data/eml/knb-lter-mcm/46/16/2dc94aebae0730cf5a8c1f075a52c814" 
infile1 <- tempfile()
download.file(inUrl1,infile1,method="curl")

par <- read_csv(infile1) |> 
  mutate(date_time = as.Date(mdy_hm(date_time))) |> 
  mutate(par = uwpar_umol_photons_m2s/ambient_par_umol_photons_m2s) |> 
  select(location_name, date_time, depth_m, par) |>
  filter(location_name %in% c('Lake Fryxell','Lake Hoare', 'East Lake Bonney', 'West Lake Bonney')) |> 
  mutate(location_name = factor(location_name, levels = c('Lake Fryxell','Lake Hoare', 'East Lake Bonney', 'West Lake Bonney'))) |> 
  left_join(ll.interp, by = join_by(location_name, date_time)) |> 
  mutate(depth.asl = masl.approx - depth_m)


# Depth profiles
ggplot(par) +
  geom_path(aes(y = depth_m, x = par, color = location_name, group = date_time)) +
  geom_point(aes(y = depth_m, x = par, color = location_name, group = date_time)) +
  scale_color_manual(values = c('#4477c9', '#e3dc10', '#b34f0c', '#4c944a'), name = 'Lake') +
  scale_fill_manual(values = c('#4477c9', '#e3dc10', '#b34f0c', '#4c944a'), name = 'Lake') +
  scale_y_reverse(limits = c(10,0)) +
  xlab('PAR (µmol photons m^-2 m^-1)') +
  ylab('Depth (m asl)') +
  ylim(0,7) +
  theme_bw(base_size = 9) +
  facet_wrap(~location_name, nrow = 1) +
  theme(
    legend.position = 'none',
    legend.title = element_blank(),
    legend.text = element_text(size = 7), 
    legend.key.size = unit(0.2,'cm'),
    legend.margin = margin(0, 0, 0, 0),
    axis.title.x = element_markdown(),
    axis.title.y = element_markdown())

ggsave('figures/SI_PAR.png', width = 6, height = 2, dpi = 500)

# Timeseries 
ggplot(par |> filter(depth_m == 5)) +
  geom_path(aes(x = date_time, y = par)) +
  geom_point(aes(x = date_time, y = par)) +
  theme_bw(base_size = 9) +
  facet_wrap(~location_name, nrow = 1)

# By month
ggplot(par |> filter(depth_m == 8)) +
  geom_point(aes(x = month(date_time), y = par)) +
  theme_bw(base_size = 9) +
  facet_wrap(~location_name, nrow = 1)

# Join with ice thickness
parice = par |> left_join(ice.interp) 

getKd = parice |> 
  group_by(location_name, date_time) |> 
  # mutate(zdiff = abs(depth_m + ice.approx)) |> 
  # filter(zdiff == min(zdiff)) |> 
  filter(depth_m == 4.5) |> 
  mutate(kd.ice = -log(par)/(-ice.approx)) |> 
  select(location_name, date_time, kd.ice)

# Timeseries Kd
ggplot(getKd) +
  geom_path(aes(x = date_time, y = kd.ice)) +
  geom_point(aes(x = date_time, y = kd.ice)) +
  theme_bw(base_size = 9) +
  facet_wrap(~location_name, nrow = 1)

ggplot(parice |> filter(depth_m == 4.5)) +
  geom_point(aes(x = ice.approx, y = par)) +
  geom_smooth(aes(x = ice.approx, y = par), method = 'lm') +
  theme_bw(base_size = 9) +
  facet_wrap(~location_name, nrow = 1)


