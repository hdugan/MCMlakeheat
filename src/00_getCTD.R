# Load libraries
library(tidyverse)
library(lubridate)
library(patchwork)
library(ggnewscale)

source('src/00_getLakeLevels.R')

# Older data 
# A sun-heated antarctic lake
shirtcliffe_bonney_1963 = read_csv('datain/papers/shirtcliffe_bonney_1963.csv') |> 
  mutate(depth.asl = (62.2-4) - depth_m) |> 
  mutate(location_name = 'East Lake Bonney') |> 
  mutate(date_time = as.Date('1963-11-01')) |> 
  mutate(location_name = factor(location_name, levels = c('Lake Fryxell','Lake Hoare', 'East Lake Bonney', 'West Lake Bonney')))

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
  mutate(depth.asl = masl.approx - depth_m) |> 
  mutate(location_name = factor(location_name, levels = c('Lake Fryxell','Lake Hoare', 'East Lake Bonney', 'West Lake Bonney')))

# Solar heating of Lake Fryxell, a permanently ice-covered Antarctic lake
hoare_LF = read_csv('datain/papers/Hoare_Nov_1963.csv') |> 
  mutate(location_name = 'Lake Fryxell') |> 
  mutate(date_time = as.Date('1963-11-20')) |> #Late november
  mutate(depth.asl = (17.5-1.9) - depth_m) |> 
  mutate(location_name = factor(location_name, levels = c('Lake Fryxell','Lake Hoare', 'East Lake Bonney', 'West Lake Bonney')))
  
# Chinn [1993] presents comparisonsfor all lakes that show increases in water levels between 1974 and 1990
# 4 meters for Lake Bonney
# 1.9 meters for Lake Fryxell 

# Priscu, J. 2023. Conductivity, temperature, and depth (CTD) vertical profiles collected from lakes in the 
# McMurdo Dry Valleys, Antarctica (1993-2023, ongoing) ver 17. Environmental Data Initiative. 
# https://doi.org/10.6073/pasta/650871571843bde5e0db6fb52cf549a4 (Accessed 2024-06-25).

# Package ID: knb-lter-mcm.88.17 Cataloging System:https://pasta.edirepository.org.
# inUrl1  <- "https://pasta.lternet.edu/package/data/eml/knb-lter-mcm/88/17/91474a205d3dd99cc794f8510d2d99c5" 
inUrl1  <- "https://pasta.lternet.edu/package/data/eml/knb-lter-mcm/88/18/91474a205d3dd99cc794f8510d2d99c5" 
infile1 <- tempfile()
download.file(inUrl1,infile1,method="curl")

# # Attempt to download the file from EDI, if not locally
# infile1 = tryCatch(
#   expr = {
#     download.file(inUrl1,infile1,method="curl")
#   },
#   error = function(e) {  # If download fails, read the local file
#     message('Caught an error!')
#     return('datain/mcm_lter/mcmlter-lake-ctd-20231023.csv')
#   }
# )


ctd <- read_csv(infile1) |> 
  mutate(date_time = as.Date(mdy_hm(date_time))) |> 
  # rename(lake = location_name) |> 
  mutate(lake = case_when(location_name == 'Lake Fryxell' ~ 'Lake Fryxell', 
                          location_name == 'Lake Hoare' ~ 'Lake Hoare', 
                          location_name == 'East Lake Bonney' ~ 'Lake Bonney', 
                          location_name == 'West Lake Bonney' ~ 'Lake Bonney')) |> 
  filter(lake %in% c('Lake Bonney', 'Lake Fryxell','Lake Hoare')) |> 
  mutate(depth_m = wire_corrected_depth_m) |> 
  left_join(ll.interp, by = join_by(location_name, date_time)) |>  # Join by masl 
  mutate(depth.asl = masl.approx - depth_m)
  
#### plot functions ######

plotCTD <- function(lakename) {
  
  ctd.lake = ctd |> filter(location_name == lakename) 
  
  p1 = ggplot(ctd.lake) +
    geom_path(aes(x = ctd_temp_c, y = depth.asl, group = limno_run, color = year(date_time))) +
    ylab('Elevation (m asl)') + xlab('Temp (°C)') +
    scale_colour_viridis_c(option = 'F', name = 'Year', limits = c(1993,2023)) +
    labs(title = lakename) +
    theme_bw(base_size = 9)
  
  if (lakename == 'East Lake Bonney'){
    p1 = p1 + 
      new_scale_color() + 
      geom_path(data = shirtcliffe_bonney_1963, aes(x = Temp_C, y = depth.asl), color = 'gold3', linewidth = 1.5) }
  if (lakename == 'Lake Fryxell'){
    p1 = p1 + 
      new_scale_color() + 
      geom_path(data = spigel_LF, aes(x = Temp_C, y = depth.asl, color = factor(year(date_time))), linewidth = 1.5) +
      geom_path(data = hoare_LF, aes(x = Temp_C, y = depth.asl, color = factor(year(date_time))), linewidth = 1.5) +
      scale_color_manual(values = c('gold','gold3'), name = 'Year')
  }
  
  p2 = ggplot(ctd.lake) +
    geom_path(aes(x = ctd_conductivity_mscm, y = depth.asl, group = limno_run, color = year(date_time))) +
    ylab('Elevation (m asl)') + xlab('Cond (mS/cm)') +
    scale_colour_viridis_c(option = 'F', name = 'Year', limits = c(1993,2023)) +
    theme_bw(base_size = 9)
  
  p1 + p2 + plot_annotation(title = lakename)
}


(plotCTD('Lake Fryxell') /
plotCTD('Lake Hoare') /
plotCTD('East Lake Bonney') /
plotCTD('West Lake Bonney')) +
  plot_layout(guides = 'collect')

# ggsave('figures/ctdprofiles.png', width = 6, height = 9, dpi = 500)  

### Get maximum depths
ctd |> group_by(location_name) |> summarise(max(depth_m, na.rm = T))

