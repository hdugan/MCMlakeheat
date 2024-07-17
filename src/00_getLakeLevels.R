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

ll <- read_csv(infile1) |> 
  mutate(date_time = as.Date(mdy_hm(date_time))) |> 
  rename(masl = `lake level(masl)`) |> 
  select(-`reference benchmark`, -`surveying benchmark`, -comments, -dataset_code) |> 
  filter(lake %in% c('Lake Bonney', 'Lake Fryxell','Lake Hoare')) |> 
  mutate(location_name = case_when(lake == 'Lake Fryxell' ~ 'Lake Fryxell', 
                                   lake == 'Lake Hoare' ~ 'Lake Hoare', 
                                   lake == 'Lake Bonney' ~ 'East Lake Bonney')) %>% 
  bind_rows(. |> filter(lake == 'Lake Bonney') |> mutate(location_name = 'West Lake Bonney')) |> # Duplicate Bonney
  select(-lake)

# Set up lake volume object
lake.volume = ll |> 
  filter(!is.na(masl)) |> 
  mutate(masl = as.character(round(masl,1))) |> 
  left_join(hypo_new |> ungroup() |> select(-lake) |> 
            mutate(Elevation_masl = as.character(round(Elevation_masl,1))), 
            by = c('masl' = 'Elevation_masl', 'location_name')) |> 
  select(-vol_layer_m3) |> 
  mutate(masl = as.numeric(masl))


# Approximate and duplicate for Lake Bonney
ll.interp = expand_grid(date_time = seq.Date(as.Date('1991-01-26'), as.Date('2023-01-22'), by = 'day'),
                        location_name = c('Lake Fryxell','Lake Hoare', 'East Lake Bonney', 'West Lake Bonney')) |> 
  left_join(lake.volume |> select(date_time, location_name, masl, Area_2D, cum_vol_m3)) |> 
  arrange(location_name, date_time, masl) |> 
  group_by(location_name) |>
  mutate(masl.approx = na.approx(masl, na.rm = FALSE)) |> 
  mutate(Area_2D.approx = na.approx(Area_2D, na.rm = FALSE)) |> 
  mutate(Vol_3D.approx = na.approx(cum_vol_m3, na.rm = FALSE)) |> 
  mutate(location_name = factor(location_name, levels = c('Lake Fryxell','Lake Hoare', 'East Lake Bonney', 'West Lake Bonney')))


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

### Calculate current area
ll.interp |> group_by(location_name) |> 
  filter(!is.na(masl)) |> 
  summarise_all(last) |> 
  mutate(Areakm2 = Area_2D/1e6)

