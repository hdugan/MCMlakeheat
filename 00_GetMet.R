# Load libraries
library(tidyverse)
library(lubridate)
library(patchwork)

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

stream.vg |> filter(date_time >= as.POSIXct('2007-11-01') & date_time <= as.POSIXct('2008-04-01')) |> 
  ggplot() +
  geom_path(aes(x = date_time, y = discharge_rate)) +
  geom_hline(aes(yintercept = 0), linetype = 2)

stream.vg |> filter(date_time >= as.POSIXct('2007-11-01') & date_time <= as.POSIXct('2008-04-01')) |> 
  ggplot() +
  geom_path(aes(x = date_time, y = water_temp)) +
  geom_hline(aes(yintercept = 0), linetype = 2)
