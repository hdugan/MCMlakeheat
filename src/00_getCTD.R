# Load libraries
library(tidyverse)
library(lubridate)
library(patchwork)

source('src/00_getLakeLevels.R')

# Older data 
shirtcliffe_bonney_1963 = read_csv('datain/papers/shirtcliffe_bonney_1963.csv') |> 
  mutate(depth.asl = (62.2-4) - depth_m) |> 
  mutate(location_name = 'East Lake Bonney') |> 
  mutate(date_time = as.Date('1963-11-01'))

spigel_ELB = read_csv('datain/papers/Spigel_Oct_1991_ELB.csv') |> 
  mutate(location_name = 'East Lake Bonney') |> 
  mutate(date_time = as.Date('1991-10-30')) |> 
  left_join(ll.interp, by = join_by(location_name, date_time)) |>  # Join by masl 
  mutate(depth.asl = masl.approx - depth_m)
  
spigel_WLB = read_csv('datain/papers/Spigel_Dec_1993_WLB.csv') |> 
  mutate(location_name = 'West Lake Bonney') |> 
  mutate(date_time = as.Date('1993-12-01')) |> 
  left_join(ll.interp, by = join_by(location_name, date_time)) |>  # Join by masl 
  mutate(depth.asl = masl.approx - depth_m)

spigel_LF = read_csv('datain/papers/Spigel_Jan_1991_LF.csv') |> 
  mutate(location_name = 'Lake Fryxell') |> 
  mutate(date_time = as.Date('1991-01-26')) |> #actually Jan 21, using 26 for lake level 
  left_join(ll.interp, by = join_by(location_name, date_time)) |>  # Join by masl 
  mutate(depth.asl = masl.approx - depth_m)

# Priscu, J. 2023. Conductivity, temperature, and depth (CTD) vertical profiles collected from lakes in the 
# McMurdo Dry Valleys, Antarctica (1993-2023, ongoing) ver 17. Environmental Data Initiative. 
# https://doi.org/10.6073/pasta/650871571843bde5e0db6fb52cf549a4 (Accessed 2024-06-25).

# Package ID: knb-lter-mcm.88.17 Cataloging System:https://pasta.edirepository.org.
inUrl1  <- "https://pasta.lternet.edu/package/data/eml/knb-lter-mcm/88/17/91474a205d3dd99cc794f8510d2d99c5" 
infile1 <- tempfile()
download.file(inUrl1,infile1,method="curl")

# Read in 2023 data (will be online soon)
ctd2023 = read_csv('datain/ctd_2023.csv')

ctd <- read_csv(infile1) |> 
  bind_rows(ctd2023) |> 
  mutate(date_time = as.Date(mdy_hm(date_time))) |> 
  # rename(lake = location_name) |> 
  mutate(lake = case_when(location_name == 'Lake Fryxell' ~ 'Lake Fryxell', 
                          location_name == 'Lake Hoare' ~ 'Lake Hoare', 
                          location_name == 'East Lake Bonney' ~ 'Lake Bonney', 
                          location_name == 'West Lake Bonney' ~ 'Lake Bonney')) |> 
  filter(lake %in% c('Lake Bonney', 'Lake Fryxell','Lake Hoare')) |> 
  left_join(ll.interp, by = join_by(location_name, date_time)) |>  # Join by masl 
  mutate(depth.asl = masl.approx - depth_m)
  
#### plot functions ######

plotCTD <- function(lakename) {
  
  ctd.lake = ctd |> filter(location_name == lakename) 
  
  p1 = ggplot(ctd.lake) +
    geom_path(aes(x = ctd_temp_c, y = depth.asl, group = limno_run, color = year(date_time))) +
    ylab('Elevation (m asl)') + xlab('Temp (°C)') +
    scale_colour_viridis_c(option = 'F', name = 'Year') +
    labs(title = lakename) +
    theme_bw(base_size = 9)
  
  if (lakename == 'East Lake Bonney'){
    p1 = p1 + geom_path(data = shirtcliffe_bonney_1963, aes(x = Temp_C, y = depth.asl), color = 'gold') 
  }
  if (lakename == 'Lake Fryxell'){
    p1 = p1 + geom_path(data = spigel_LF, aes(x = Temp_C, y = depth.asl), color = 'gold') 
  }
  
  p2 = ggplot(ctd.lake) +
    geom_path(aes(x = ctd_conductivity_mscm, y = depth.asl, group = limno_run, color = year(date_time))) +
    ylab('Elevation (m asl)') + xlab('Cond (mS/cm)') +
    scale_colour_viridis_c(option = 'F', name = 'Year') +
    theme_bw(base_size = 9)
  
  p1 + p2 + plot_layout(guides = 'collect') + plot_annotation(title = lakename)
}


plotCTD('Lake Fryxell') /
plotCTD('Lake Hoare') /
plotCTD('East Lake Bonney') /
plotCTD('West Lake Bonney') 

# ggsave('figures/ctdprofiles.png', width = 6, height = 12, dpi = 500)  

### Get maximum depths
ctd |> group_by(location_name) |> summarise(max(depth_m, na.rm = T))

