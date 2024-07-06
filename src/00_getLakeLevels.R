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
  filter(lake %in% c('Lake Bonney', 'Lake Fryxell','Lake Hoare'))

# Approximate and duplicate for Lake Bonney
ll.interp = expand_grid(date_time = seq.Date(as.Date('1991-01-26'), as.Date('2023-01-22'), by = 'day'),
                        lake = c('Lake Bonney', 'Lake Fryxell','Lake Hoare')) |> 
  left_join(ll |> select(date_time, lake, masl)) |> 
  arrange(lake, date_time, masl) |> 
  group_by(lake) |>
  mutate(masl.approx = na.approx(masl, na.rm = FALSE)) |> 
  mutate(location_name = case_when(lake == 'Lake Fryxell' ~ 'Lake Fryxell', 
                                   lake == 'Lake Hoare' ~ 'Lake Hoare', 
                                   lake == 'Lake Bonney' ~ 'East Lake Bonney')) %>% 
  bind_rows(. |> filter(lake == 'Lake Bonney') |> mutate(location_name = 'West Lake Bonney')) |> 
  mutate(lake = factor(lake, levels = c('Lake Fryxell','Lake Hoare', 'Lake Bonney')))


# Graph with linear interpolation
ggplot(ll.interp) +
  geom_path(aes(x = date_time, y = masl.approx), col = 'lightblue4') +
  geom_point(aes(x = date_time, y = masl, fill = lake), shape = 21, stroke = 0.2) +
  scale_fill_manual(values = c('#4477c9', '#e3dc10', '#b34f0c'), name = 'Lake') +
  facet_wrap(~lake, scales = 'free', nrow = 3) +
  ylab('Lake Level (m asl)') +
  theme_bw(base_size = 9) +
  theme(axis.title.x = element_blank(), 
        legend.position = 'none')

ggsave('figures/SI_lakelevel.png', width = 6, height = 4, dpi = 500)

### Calculate current area

currentArea = ll |> group_by(lake) |> filter(date_time == last(date_time)) |> 
  ungroup() |> 
  mutate(location_name = case_when(lake == 'Lake Fryxell' ~ 'Lake Fryxell', 
                                   lake == 'Lake Hoare' ~ 'Lake Hoare', 
                                   lake == 'Lake Bonney' ~ 'East Lake Bonney')) %>%
  bind_rows(. |> filter(lake == 'Lake Bonney') |> mutate(location_name = 'West Lake Bonney')) |> 
  select(location_name, Elevation_masl = masl) |> 
  mutate(Elevation_masl = as.character(round(Elevation_masl,1))) |> 
  left_join(hypo_new |>  mutate(Elevation_masl = as.character(round(Elevation_masl,1)))) |> 
  mutate(Areakm2 = Area_2D/1e6)

