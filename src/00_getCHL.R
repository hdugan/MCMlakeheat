# Load libraries
library(tidyverse)
library(lubridate)
library(patchwork)

source('src/00_getLakeLevels.R')

# Priscu, J. 2023. Chlorophyll-a concentrations in discrete water column samples collected from 
# lakes in the McMurdo Dry Valleys, Antarctica (1993-2023, ongoing) ver 12. Environmental Data Initiative. 
# https://doi.org/10.6073/pasta/8621e8d2332c8b65aae4d272d055eb1d (Accessed 2024-07-05).


# Package ID: knb-lter-mcm.44.12 Cataloging System:https://pasta.edirepository.org.
inUrl1  <- "https://pasta.lternet.edu/package/data/eml/knb-lter-mcm/44/12/252bf37cb50780d5d03b56cc377c6a78" 
infile1 <- tempfile()
download.file(inUrl1,infile1,method="curl")

chl <- read_csv(infile1) |> 
  mutate(date_time = as.Date(mdy_hm(date_time))) |> 
  select(location_name, date_time, depth_m, chl_ug_chla_l) |> 
  filter(location_name %in% c('Lake Fryxell','Lake Hoare', 'East Lake Bonney', 'West Lake Bonney')) |> 
  mutate(location_name = factor(location_name, levels = c('Lake Fryxell','Lake Hoare', 'East Lake Bonney', 'West Lake Bonney'))) |> 
  left_join(ll.interp, by = join_by(location_name, date_time)) |> 
  mutate(depth.asl = masl.approx - depth_m)



ggplot(chl) +
  geom_point(aes(y = depth.asl, x = chl_ug_chla_l, fill = location_name), shape = 21, stroke = 0.2, size = 1) +
  # geom_path(aes(y = depth.asl, x = chl_ug_chla_l, group = date_time, color = location_name), width = 0.1) +
  scale_color_manual(values = c('#4477c9', '#e3dc10', '#b34f0c', '#4c944a'), name = 'Lake') +
  scale_fill_manual(values = c('#4477c9', '#e3dc10', '#b34f0c', '#4c944a'), name = 'Lake') +
  xlab('Chlorophyll (µg L^-1 )') +
  ylab('Depth (m asl)') +
  theme_bw(base_size = 9) +
  facet_wrap(~location_name, scales = 'free', nrow = 1) +
  theme(
        legend.position = 'none',
        legend.title = element_blank(),
        legend.text = element_text(size = 7), 
        legend.key.size = unit(0.2,'cm'),
        legend.margin = margin(0, 0, 0, 0),
        axis.title.x = element_markdown(),
        axis.title.y = element_markdown())

ggsave('figures/SI_Chlorophyll.png', width = 6, height = 2, dpi = 500)

