# Load libraries
library(tidyverse)
library(lubridate)
library(zoo)

# source DIC function 
source('https://raw.githubusercontent.com/hdugan/lake_DIC/master/lake_DIC.R')

# Priscu, J., K. Welch, and W. Lyons. 2022. 
# Ion concentrations in discrete water column samples collected from lakes in the McMurdo Dry Valleys, 
# Antarctica (1991-2019, ongoing) ver 12. Environmental Data Initiative. 
# https://doi.org/10.6073/pasta/31f7354d1a05679eb3ce7c384c6e2b22 (Accessed 2024-06-26).

inUrl1  <- "https://pasta.lternet.edu/package/data/eml/knb-lter-mcm/62/12/4c4202afce78edbff1f889e462407beb" 
infile1 <- tempfile()
download.file(inUrl1,infile1,method="curl")

ions <- read_csv(infile1) |> 
  mutate(date_time = as.Date(mdy_hm(date_time)))
  

# Priscu, J.C. 2023. Dissolved inorganic carbon (DIC) concentrations in discrete water column samples 
# collected from lakes in the McMurdo Dry Valleys, Antarctica (1993-2022, ongoing). Environmental Data Initiative. 
# DOI: 10.6073/pasta/fadcc745b6ecb6d1942dda95ef373eab. Dataset accessed 26 June 2024.

inUrl1  <- "https://pasta.lternet.edu/package/data/eml/knb-lter-mcm/59/11/9b9e39a6b2c5f7a4322a806062376181" 
infile1 <- tempfile()
download.file(inUrl1,infile1,method="curl")

dic <- read_csv(infile1) |> 
  mutate(date_time = as.Date(mdy_hm(date_time))) |> 
  filter(location_name %in% c('West Lake Bonney', 'East Lake Bonney', 'Lake Fryxell','Lake Hoare'))

# Takacs-vesbach, C.D., Priscu, J.C. 2024. Hydrogen ion concentrations (pH) in discrete water column samples 
# collected from lakes in the McMurdo Dry Valleys, Antarctica (1993-2022, ongoing). Environmental Data Initiative. 
# DOI: 10.6073/pasta/4a9c2ed67ccb6e2dd4b5ece36719d590. Dataset accessed 26 June 2024.

inUrl1  <- "https://pasta.lternet.edu/package/data/eml/knb-lter-mcm/63/14/ed5ec547fcb1aa64206cdb91c55b74e2" 
infile1 <- tempfile()
download.file(inUrl1,infile1,method="curl")

ph <- read_csv(infile1) |> 
  mutate(date_time = as.Date(mdy_hm(date_time))) |> 
  filter(location_name %in% c('West Lake Bonney', 'East Lake Bonney', 'Lake Fryxell','Lake Hoare'))

###### CTD Round #########
# Priscu, J. 2023. Conductivity, temperature, and depth (CTD) vertical profiles collected from lakes in the 
# McMurdo Dry Valleys, Antarctica (1993-2023, ongoing) ver 17. Environmental Data Initiative. 
# https://doi.org/10.6073/pasta/650871571843bde5e0db6fb52cf549a4 (Accessed 2024-06-25).

inUrl1  <- "https://pasta.lternet.edu/package/data/eml/knb-lter-mcm/88/17/91474a205d3dd99cc794f8510d2d99c5" 
infile1 <- tempfile()
download.file(inUrl1,infile1,method="curl")

ctd.round <- read_csv(infile1) |> 
  mutate(date_time = as.Date(mdy_hm(date_time))) |> 
  filter(location_name %in% c('West Lake Bonney', 'East Lake Bonney', 'Lake Fryxell','Lake Hoare')) |> 
  select(location_name, date_time, depth_m, ctd_temp_c, ctd_conductivity_mscm) |> 
  mutate(depth_1m = round(depth_m, 1)) |> 
  mutate(depthdiff = abs(depth_1m - depth_m)) |> 
  group_by(location_name, date_time, depth_1m) |> 
  arrange(location_name, date_time, depth_1m, depthdiff) |> 
  summarise_all(first) |> 
  ungroup() |> 
  select(-depthdiff, -depth_m) |> 
  rename(depth_m = depth_1m)
  
ctd.dates = ctd.round |> 
  mutate(datepre7 = date_time - 7, datepost7 = date_time + 7) |> 
  rename(dateCTD = date_time)

##### Combine data #####

dic.join = dic |> select(location_name, date_time, depth_m, dic_mg_cl) |> 
  full_join(ph |> select(location_name, date_time, depth_m, ph), by = join_by(location_name, date_time, depth_m)) |> 
  left_join(ctd.dates, by = join_by(location_name, date_time >= datepre7, date_time <= datepost7, depth_m)) |> 
  group_by(location_name, date_time) |> 
  mutate(ph.approx = na.approx(ph, na.rm = FALSE))

# Use temp, DIC, ph to estimate carbonate   
dic.carbonate = dic.join |> 
  mutate(carbonate = carbonate(TEMP = ctd_temp_c, DIC = dic_mg_cl, pH = ph.approx, output = "mg"))  %>%
  unnest_wider(carbonate) |> 
  filter(!is.na(bicarbonate_mgkg))

ions.df = ions |> select(-contains('comments'), -dataset_code, -limno_run, -filename) |> 
  select(location_name:depth_m, depth_masl, everything()) |> 
  rowwise() |> 
  mutate(cation_sum = sum(li_mgl, na_mgl, k_mgl, mg_mgl, ca_mgl, f_mgl, na.rm = T)) |> 
  mutate(anion_sum = sum(cl_mgl, br_mgl, so4_mgl, si_mgl, na.rm = T)) |> 
  mutate(cation_sumM = sum(li_mm, na_mm, k_mm, mg_mm, ca_mm, f_mm, na.rm = T)) |> 
  mutate(anion_sumM = sum(cl_mm, br_mm, so4_mm, na.rm = T)) |> # remove silica
  select(location_name:depth_masl, cation_sum, anion_sum, cation_sumM, anion_sumM)

# Full join of data

full.df = ions.df |> left_join(dic.carbonate, by = join_by(location_name, date_time, depth_m))

salinity.df = full.df |> 
  select(location_name:depth_m, cation_sum, anion_sum, bicarbonate_mgkg, carbonate_mgkg) |> 
  filter(!is.na(bicarbonate_mgkg)) |> 
  mutate(salinity = sum(cation_sum, anion_sum, bicarbonate_mgkg, carbonate_mgkg))

ggplot(salinity.df) +
  geom_path(aes(x = salinity/1000, y = depth_m, group = date_time, col = year(date_time))) +
  geom_point(aes(x = salinity/1000, y = depth_m, group = date_time, col = year(date_time))) +
  scale_y_reverse() +
  xlab('Salinity (g/L)') +
  facet_wrap(~location_name, scales = 'free')

         