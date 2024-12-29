source('src/functions/salinity_fromDensity.R')
library(oce)
# Density measurements taken by Limno Team winter 2022

density.df = read_csv('datain/density_2022.csv') |> 
  mutate(density_kgm3 = density_kgm3*1000) |> 
  mutate(location_name = case_when(lake == 'LF' ~ 'Lake Fryxell', 
                                 lake == 'LH' ~ 'Lake Hoare', 
                                 lake == 'ELB' ~ 'East Lake Bonney', 
                                 lake == 'WLB' ~ 'West Lake Bonney'))

density.df = density.df |> 
  rowwise() |> 
  mutate(salinity_fromDensity = optimizeSalinity(temp_C, density_kgm3, depth_m)$minimum) |> 
  mutate(oceSalinity.GSW = swSTrho(temperature = temp_C, density = density_kgm3, pressure = 1+depth_m, eos = "gsw")) |> 
  mutate(oceSalinity.UNESCO = swSTrho(temperature = temp_C, density = density_kgm3, pressure = 1+depth_m, eos = "unesco")) |> 
  mutate(salinity_fromDensity = if_else(salinity_fromDensity < 2, oceSalinity.GSW, salinity_fromDensity))
