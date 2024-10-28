# Load libraries
library(tidyverse)
library(lubridate)
library(patchwork)

# Dugan, H., P. Doran, and A. Fountain. 2024. Daily measurement summaries from Lake Hoare Meteorological Station (HOEM), 
# McMurdo Dry Valleys, Antarctica (1987-2022, ongoing) ver 8. Environmental Data Initiative. 
# https://doi.org/10.6073/pasta/4677be41bb961d12e4bc7ddb916cfffe (Accessed 2024-10-18).
inUrl1  <- "https://pasta.lternet.edu/package/data/eml/knb-lter-mcm/7111/8/bc9a2c9e926ceff4ff642622525db453" 
infile1 <- tempfile()
download.file(inUrl1,infile1,method="curl")

hoem.airt <- read_csv(infile1) |> 
  mutate(date_time = as.POSIXct(mdy(date_time)))

ggplot(hoem.airt) +
  geom_path(aes(x = date_time, y = avg_airt3m))

inUrl4  <- "https://pasta.lternet.edu/package/data/eml/knb-lter-mcm/7111/8/e7a1ebe6f4479d13962088977254d272" 
infile1 <- tempfile()
download.file(inUrl4,infile1,method="curl")

hoem.rad <- read_csv(infile1) |> 
  mutate(date_time = as.POSIXct(mdy(date_time))) |> 
  mutate(wyear = if_else(month(date_time) >= 10, year(date_time) + 1, year(date_time))) 
ggplot(hoem.rad) +
  geom_path(aes(x = date_time, y = avg_swradin))

hoem.rad.a = hoem.rad |> 
  group_by(wyear) |> 
  summarise(swradin = sum(avg_swradin, na.rm = T)) |> 
  mutate(swradin = ifelse(wyear %in% c(2015, 2023), NA, swradin)) |> 
  mutate(wyear = wyear - 1)

ggplot(hoem.rad.a) +
  geom_col(aes(x = wyear, y = swradin))

# Doran, P. and A. Fountain. 2023. High frequency measurements from Lake Fryxell Meteorological Station 
# (FRLM), McMurdo Dry Valleys, Antarctica (1993-2022, ongoing) ver 17. Environmental Data Initiative. 
# https://doi.org/10.6073/pasta/015d3ff36195d0531dff560f0ca28634 (Accessed 2024-10-10).

# Package ID: knb-lter-mcm.7010.17 Cataloging System:https://pasta.edirepository.org.
inUrl1  <- "https://pasta.lternet.edu/package/data/eml/knb-lter-mcm/7010/17/1aafff5c9473045c30275cb2c839904d" 
infile1 <- tempfile()
download.file(inUrl1,infile1,method="curl")

frlm.airt <- read_csv(infile1) |> 
  mutate(date_time = as.POSIXct(mdy_hm(date_time))) 

inUrl2 <- "https://pasta.lternet.edu/package/data/eml/knb-lter-mcm/7010/17/1b7b71ad5c135e42783e6a83f8a14b27" 
infile1 <- tempfile()
download.file(inUrl2,infile1,method="curl")

frlm.rad <- read_csv(infile1) |> 
  mutate(date_time = as.POSIXct(mdy_hm(date_time))) 

frlm.airt |> filter(date_time >= as.POSIXct('2007-11-01') & date_time <= as.POSIXct('2008-04-01')) |> 
  ggplot() +
  geom_path(aes(x = date_time, y = airt3m)) +
  geom_hline(aes(yintercept = 0), linetype = 2)

frlm.rad |> filter(date_time >= as.POSIXct('2007-11-01') & date_time <= as.POSIXct('2008-04-01')) |> 
  ggplot() +
  geom_path(aes(x = date_time, y = swradin)) +
  geom_hline(aes(yintercept = 0), linetype = 2)



# Gooseff, M. and D. McKnight. 2024. Seasonal high-frequency measurements of discharge, 
# water temperature, and specific conductivity from Von Guerard Stream at F6, McMurdo 
# Dry Valleys, Antarctica (1990-2023, ongoing) ver 20. Environmental Data Initiative. 
# https://doi.org/10.6073/pasta/5260bdcfca3c9c23f87393222e55d222 (Accessed 2024-10-10).

# Package ID: knb-lter-mcm.9027.20 Cataloging System:https://pasta.edirepository.org.
inUrl1  <- "https://pasta.lternet.edu/package/data/eml/knb-lter-mcm/9027/20/74257dfd5f98e51c6b325b3672951d61" 
infile1 <- tempfile()
download.file(inUrl1,infile1,method="curl")

stream.vg <- read_csv(infile1) |> 
  mutate(date_time = as.POSIXct(mdy_hm(date_time))) 

# Annual timeseries
stream.vg |>  
  mutate(wyear = if_else(month(date_time) >= 10, year(date_time) + 1, year(date_time))) |> 
  group_by(wyear) |> 
  summarise(discharge_rate = sum(discharge_rate, na.rm = T)) |> 
  ggplot() +
  geom_col(aes(x = wyear, y = discharge_rate))

