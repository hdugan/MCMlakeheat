# Load libraries
library(tidyverse)
library(lubridate)
library(patchwork)

source('src/gethypso.R')

# Get hypsometry and create character elevation
hypo.use = hypo_new |> 
  ungroup() |> 
  mutate(depth.asl.char = as.character(round(Elevation_masl,1))) |> 
  select(-lake, -Elevation_masl)

# Join hyspometry
df.spcH |> left_join(hypo.use, by = join_by(depth.asl.char, location_name))

  