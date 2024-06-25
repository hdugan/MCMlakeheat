# Load libraries
library(tidyverse)
library(lubridate)
library(zoo)

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

ll.interp = expand_grid(date_time = seq.Date(as.Date('1991-01-26'), as.Date('2023-01-22'), by = 'day'),
                        lake = c('Lake Bonney', 'Lake Fryxell','Lake Hoare')) |> 
  left_join(ll |> select(date_time, lake, masl)) |> 
  arrange(lake, date_time, masl) |> 
  group_by(lake) |>
  mutate(masl.approx = na.approx(masl, na.rm = FALSE))

# Graph with linear interpolation
ggplot(ll.interp) +
  geom_point(aes(x = date_time, y = masl)) +
  geom_path(aes(x = date_time, y = masl.approx), col = 'red4') +
  facet_wrap(~lake, scales = 'free', nrow = 3)
