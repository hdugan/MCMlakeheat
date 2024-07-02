# Load libraries
library(tidyverse)
library(lubridate)
library(patchwork)
library(zoo)
library(mgcv)
library(marelac)
library(wql)

source('src/00_getCTD.R')
source('src/00_gethypso.R')
source('src/functions/SpecHeat_Water.R')

# Create SpecCond at 0.1 increments every 0.1 m
ctd.join = ctd |> 
  mutate(year = year(date_time)) |> 
  select(date_time, year, location_name, masl.approx, depth.asl, depth_m, ctd_conductivity_mscm, ctd_temp_c) |> 
  mutate(depth.asl = round(depth.asl, 1)) |> 
  group_by(date_time, year, location_name, depth.asl) |> 
  summarise_all(mean, na.rm = T) |> 
  ungroup() |> 
  # remove bad data 
  filter(!(location_name == 'West Lake Bonney' & date_time == as.Date('2002-12-16'))) |> 
  filter(!(location_name == 'East Lake Bonney' & date_time == as.Date('2002-11-14'))) |> 
  mutate(ctd_conductivity_mscm = if_else(location_name == 'West Lake Bonney' & date_time == as.Date('2002-11-14') & ctd_conductivity_mscm > 85, 
                                         NA, ctd_conductivity_mscm)) |> 
  mutate(ctd_conductivity_mscm = if_else(location_name == 'West Lake Bonney' & date_time == as.Date('2002-11-14') & 
                                           ctd_conductivity_mscm < 75 & depth.asl <25, 
                                           NA, ctd_conductivity_mscm)) |> 
  mutate(ctd_conductivity_mscm = if_else(location_name == 'East Lake Bonney' & date_time == as.Date('2002-12-11') & 
                                           ctd_conductivity_mscm > 120, 
                                         NA, ctd_conductivity_mscm)) # These are bad data points


lakenames = c('West Lake Bonney', 'East Lake Bonney', 'Lake Fryxell','Lake Hoare')

# a = ctd.join |> filter(location_name %in% c('West Lake Bonney', 'East Lake Bonney') & year %in% c(2002, 2003))
# ggplot(a) +
#   geom_point(aes(x = ctd_conductivity_mscm, y = depth.asl, group = as.factor(date_time), color = as.factor(date_time))) +
#   ylab('Elevation (m asl)') + xlab('Temp (°C)') +
#   theme_bw(base_size = 9) +
#   facet_wrap(~location_name, scales = 'free')

########################## Create clean dataframe for lake ########################################

clean.ctd <- function(lakename) {
  ctd.wlb = ctd.join |> filter(location_name == lakename) 
  samplingdates = unique(ctd.wlb$date_time)
  df.lake.list = list()
  for (i in 1:length(samplingdates)) {
    usedate = samplingdates[i]
    
    df.lake = ctd.wlb |> filter(date_time == usedate) |> 
      arrange(depth_m)
    if(nrow(df.lake) < 10) {
      next
    }
    
    # Have to assume top of lake is 0°C
    df.lake = df.lake |> 
      mutate(ctd_temp_c = if_else(row_number() == 1 & is.na(ctd_temp_c), 0, ctd_temp_c)) |> 
      mutate(depth.asl.char = as.character(depth.asl)) # joining by numbers impossible with decimal places
    
    df.lake.list[[usedate]] = expand_grid(depth.asl.char = as.character(seq(max(df.lake$depth.asl, na.rm = T), min(df.lake$depth.asl, na.rm = T), by = -0.1))) |> 
      left_join(df.lake, join_by(depth.asl.char)) |> 
      mutate(date_time = usedate) |> 
      mutate(year = year(usedate)) |> 
      mutate(location_name = lakename) |> 
      mutate(across(c(depth.asl:ctd_temp_c), ~ na.approx(.x, na.rm = FALSE, maxgap = 50, rule = 2)))
    
  }
  df.lake.out = bind_rows(df.lake.list)
  checkdates = df.lake.out |> filter(is.na(ctd_temp_c)) |> group_by(date_time) |> summarise_all(first)
  df.lake.out = df.lake.out |> filter(!date_time %in% checkdates$date_time) # filter out profiles with too much missing data 
  return(df.lake.out)
}

df.wlb.clean = clean.ctd(lakename = 'West Lake Bonney') |> 
  filter(date_time != as.Date('1995-01-13')) #temp profile looks wrong
df.lf.clean = clean.ctd(lakename = 'Lake Fryxell')
df.lh.clean = clean.ctd(lakename = 'Lake Hoare')
df.elb.clean = clean.ctd(lakename = 'East Lake Bonney') |> 
  filter(date_time != as.Date('1995-01-14')) #temp profile looks wrong

# Combine the four lakes 
df.clean = df.lf.clean |> bind_rows(df.lh.clean, df.elb.clean, df.wlb.clean)

# Check plots 
p1 = ggplot(df.clean) +
  geom_path(aes(x = ctd_temp_c, y = depth.asl, group = date_time, color = year(date_time))) +
  ylab('Elevation (m asl)') + xlab('Temp (°C)') +
  scale_colour_viridis_c(option = 'F', name = 'Year') +
  theme_bw(base_size = 9) +
  facet_wrap(~location_name, scales = 'free')

p2 = ggplot(df.clean) +
  geom_path(aes(x = ctd_conductivity_mscm, y = depth.asl, group = date_time, color = year(date_time))) +
  ylab('Elevation (m asl)') + xlab('Cond (mS/cm)') +
  scale_colour_viridis_c(option = 'F', name = 'Year') +
  theme_bw(base_size = 9) +
  facet_wrap(~location_name, scales = 'free')

# Check plots
p1 + p2 + plot_layout(guides = 'collect') 

##################### Add SpecCond and calculate salinity ########################

df.spc = df.clean |> 
  mutate(specCond.raw = ctd_conductivity_mscm/(1 + 0.020*(ctd_temp_c - 5))) |> # standardize to 5°C
  mutate(specCond = as.character(round(specCond.raw / 0.1) * 0.1)) |>  # round to nearest 0.1 m depth, change to character for join
  mutate(specCond = if_else(location_name == 'Lake Hoare', as.character(round(specCond.raw / 0.01) * 0.01), specCond)) # For Lake Hoare, round spC to 2 decimals

# Load conductivity/salinity relationship
# Not applicable for ELB salinity > 180
sal.pred = read_csv('dataout/condSalTransfer.csv') |> 
  mutate(specCond = as.character(specCond)) |> 
  rename(sal.pred = pred)

df.spc = df.spc |> 
  left_join(sal.pred, by = join_by(location_name, specCond))
df.spc |> filter(is.na(sal.pred))

# Load salinity/depth relationship
salz.pred = read_csv('dataout/salinityTransferTable.csv') |> 
  filter(location_name == 'East Lake Bonney') %>%
  mutate(depth.asl.char = as.character(depth.asl)) %>%
  select(-depth.asl) %>%
  bind_rows(. |> filter(year == 1995) |> mutate(year = 1993)) %>% # ions started in 1995, so use 1995 profiles for 1993 and 1994
  bind_rows(. |> filter(year == 1995) |> mutate(year = 1994)) %>%
  bind_rows(. |> filter(year == 2019) |> mutate(year = 2020)) %>%
  bind_rows(. |> filter(year == 2019) |> mutate(year = 2021)) %>%
  bind_rows(. |> filter(year == 2019) |> mutate(year = 2022)) %>%
  bind_rows(. |> filter(year == 2019) |> mutate(year = 2023))

# If no conductivity value, replace with salinity table 
df.spc2 = df.spc |> left_join(salz.pred, by = join_by(year, location_name, depth.asl.char)) |> 
  mutate(sal.pred2 = if_else(is.na(sal.pred), pred, sal.pred))

# Ok, but some of the transfer table doesn't go deep enough. Interpolate with constant 
df.spc3 = df.spc2 |> 
  group_by(location_name, date_time) |> 
  mutate(across(c(sal.pred2), ~ na.approx(.x, na.rm = FALSE, maxgap = 50, rule = 2))) |> 
  mutate(sal.pred2 = if_else(sal.pred2 < 0, 0, sal.pred2)) %>% 
  select(-sal.pred, -pred)

# Check plot 
ggplot(df.spc3) +
  geom_path(aes(x = sal.pred2, y = depth.asl, group = date_time, color = year(date_time))) +
  ylab('Elevation (m asl)') + xlab('Salinity (mg/L)') +
  scale_colour_viridis_c(option = 'F', name = 'Year') +
  theme_bw(base_size = 9) +
  facet_wrap(~location_name, scales = 'free')

##################### Add specific heat capacity and density ########################
# equation not build for temperatures < 0°C or S > 180, but what can you do
# Units J/kg K
df.spcH = df.spc3 |> 
  mutate(spHeat_J_kgK = SW_SpcHeat(Temp = ctd_temp_c, S = sal.pred2, P = 1 + (depth_m/10))) |> #units deafult, °C, ppt, bar
  mutate(density_kg_m3 = sw_dens(S = sal.pred2, t = ctd_temp_c, p = 1 + (depth_m/10)))

quantile(df.spcH$spHeat_J_kgK)
quantile(df.spcH$density_kg_m3)


