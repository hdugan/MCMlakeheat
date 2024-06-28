# Load libraries
library(tidyverse)
library(lubridate)
library(patchwork)

source('src/getCTD.R')
source('src/gethypso.R')

# Join CTD to hypsometry by lake name and elevation/depth masl 
ctd.join = ctd |> 
  mutate(year = year(date_time)) |> 
  select(date_time, year, location_name, masl.approx, depth.asl, depth_m, ctd_conductivity_mscm, ctd_temp_c) |> 
  mutate(depth.asl = round(depth.asl, 1)) |> 
  left_join(hypo_new |> rename(depth.asl = Elevation_masl) |>  mutate(depth.asl = round(depth.asl, 1)), 
            by = c('location_name', 'depth.asl'))


# Calculate salinity based on year and depth
salinity.pred = read_csv('dataout/salinityTransferTable.csv')

