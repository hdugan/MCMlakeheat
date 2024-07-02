# Load libraries
library(tidyverse)
library(lubridate)
library(zoo)
library(mgcv)
library(marelac)
library(wql)

# source lake lakels
source('src/00_getLakeLevels.R')
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
  filter(!is.na(cl_mgl)) |>  # dominant anion
  filter(!is.na(na_mgl)) |>  # dominant cation
  mutate(cation_sum = sum(li_mgl, na_mgl, k_mgl, mg_mgl, ca_mgl, f_mgl, na.rm = T)) |> 
  mutate(anion_sum = sum(cl_mgl, br_mgl, so4_mgl, na.rm = T)) |> # remove silica
  mutate(cation_sumM = sum(li_mm, na_mm, k_mm, mg_mm, ca_mm, f_mm, na.rm = T)) |> 
  mutate(anion_sumM = sum(cl_mm, br_mm, so4_mm, na.rm = T)) |> # remove silica
  select(location_name:depth_masl, cation_sum, anion_sum, cation_sumM, anion_sumM) |> 
  filter(!(location_name == 'West Lake Bonney' & date_time == as.Date('1998-11-11') & depth_m == 30))

# Full join of data

full.df = ions.df |> left_join(dic.carbonate, by = join_by(location_name, date_time, depth_m))

salinity.df = full.df |> 
  select(location_name:depth_m, ctd_temp_c, ctd_conductivity_mscm, cation_sum, anion_sum, bicarbonate_mgkg, carbonate_mgkg) |> 
  filter(!is.na(bicarbonate_mgkg)) |> 
  mutate(salinity = sum(cation_sum, anion_sum, bicarbonate_mgkg, carbonate_mgkg)) |> 
  mutate(density = sw_dens(S = salinity/1000, t = ctd_temp_c, p = 1 + (depth_m/10))) |> 
  mutate(specCond = ctd_conductivity_mscm/(1 + 0.020*(ctd_temp_c - 5))) |> 
  mutate(salinity.derivedCond = convert_RtoS(R = ctd_conductivity_mscm/42.914, t = ctd_temp_c, P = 1 + (depth_m/10))) |> 
  mutate(salinity.derivedCond2 = ec2pss(ctd_conductivity_mscm, t = ctd_temp_c, p = 1 + depth_m)) |> 
  left_join(ll.interp |> select(-lake, -masl), by = join_by(location_name, date_time)) |> 
  mutate(depth.asl = masl.approx - depth_m)
  
# C(35, 15, 0) is the conductivity of standard seawater (35‰) at 15 °C which by
# definition has a conductivity equal to that of the standard KCl solution at
# that temperature and its value is 4.2914 Siemens/meter or 42.914 mS/cm
# Density calculated here using Gibbs function
# Feistel R, 2008. A Gibbs function for seawater thermodynamics for -6 to 80 dgC and salinity up to 120 g/kg. Deep-Sea Research I, 55, 1639-1671.



################################# PLOTS #################################
ggplot(salinity.df) +
  geom_path(aes(x = salinity/1000, y = depth.asl, group = date_time, col = year(date_time))) +
  geom_point(aes(x = salinity/1000, y = depth.asl, group = date_time, col = year(date_time))) +
  # geom_path(aes(x = salinity.derivedCond, y = depth.asl, group = date_time), col = 'red') +
  # scale_y_reverse() +
  geom_point(data = df.pred.salinty, aes(x = pred, depth.asl), col = 'red') +
  xlab('Salinity (g/L)') +
  facet_wrap(~location_name, scales = 'free')

ggplot(salinity.df |> filter(year(date_time) == 2019)) +
  geom_path(aes(x = density, y = depth_m, group = date_time)) +
  geom_point(aes(x = density, y = depth_m, group = date_time)) +
  geom_path(data = density.df, aes(x = density_kgm3*1000, y = depth_m), col = 'red3') +
  geom_point(data = density.df, aes(x = density_kgm3*1000, y = depth_m), col = 'red3') +
  scale_y_reverse() +
  xlab('Density (g/m3)') + ylab('Depth (m)') +
  facet_wrap(~location_name, scales = 'free') +
  labs(caption = 'Red = measured 2022, Black = sum ions -> density using Gibbs equation')
# ggsave('figures/densityComp.png', width = 5, height = 5, dpi = 500)

ggplot(salinity.df) +
  geom_point(aes(y = salinity/1000, x = specCond, group = date_time), col = 'red3', size = 0.7) +
  geom_point(aes(y = salinity/1000, x = ctd_conductivity_mscm, group = date_time), size = 0.7) +
  scale_y_reverse() +
  ylab('Salinity (g/L)') + xlab('Conductivity (mS/cm)') +
  facet_wrap(~location_name, scales = 'free') +
  labs(caption = 'CTD casts vs ') +
  labs(caption = 'Black = raw conductivity, Red = temp corrected to 5°C')
# ggsave('figures/densityCondComp.png', width = 5, height = 5, dpi = 500) 

ggplot(salinity.df |> filter(year(date_time) == 2019)) +
  geom_path(aes(x = ctd_temp_c , y = depth_m, group = date_time)) +
  geom_point(aes(x = ctd_temp_c , y = depth_m, group = date_time)) +
  geom_path(data = density.df, aes(x = temp_C, y = depth_m), col = 'red3') +
  geom_point(data = density.df, aes(x = temp_C, y = depth_m), col = 'red3') +
  scale_y_reverse() +
  xlab('Density (g/m3)') +
  facet_wrap(~location_name, scales = 'free')

         