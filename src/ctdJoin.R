# Load libraries
library(tidyverse)
library(lubridate)
library(patchwork)

source('src/getCTD.R')
source('src/gethypso.R')

a = ctd |> filter(location_name == 'West Lake Bonney' & date_time == as.Date('2002-12-16'))

# Create SpecCond at 0.1 increments every 0.1 m
ctd.join = ctd |> 
  mutate(year = year(date_time)) |> 
  select(date_time, year, location_name, masl.approx, depth.asl, depth_m, ctd_conductivity_mscm, ctd_temp_c) |> 
  mutate(depth.asl = round(depth.asl, 1)) |> 
  group_by(date_time, year, location_name, depth.asl) |> 
  summarise_all(mean, na.rm = T) |> 
  ungroup() |> 
  mutate(ctd_conductivity_mscm = if_else(location_name == 'West Lake Bonney' & date_time == as.Date('2002-12-16') & ctd_conductivity_mscm > 85, 
                                         NA, ctd_conductivity_mscm)) |> 
  mutate(ctd_conductivity_mscm = if_else(location_name == 'West Lake Bonney' & date_time == as.Date('2002-12-16') & 
                                           depth_m > 30 & ctd_conductivity_mscm < 77, 
                                         NA, ctd_conductivity_mscm)) # These are bad data points


lakenames = c('West Lake Bonney', 'East Lake Bonney', 'Lake Fryxell','Lake Hoare')

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

df.clean = df.lf.clean |> bind_rows(df.lh.clean, df.elb.clean, df.wlb.clean)

df.wlb.clean |> filter(ctd_conductivity_mscm > 85 & depth.asl > 28)

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
  mutate(specCond = as.character(round(specCond.raw / 0.1) * 0.1)) # round to nearest 0.5 m depth, change to character for join

# Load conductivity/salinity relationship
# Not applicable for ELB salinity > 180
sal.pred = read_csv('dataout/condSalTransfer.csv') |> 
  mutate(specCond = as.character(specCond)) |> 
  rename(sal.pred = pred)

df.spc = df.spc |> 
  left_join(sal.pred) |> 
  mutate(depth.asl = as.character(depth.asl)) #need to be character to join
df.spc |> filter(is.na(sal.pred))

# Load salinity/depth relationship
salz.pred = read_csv('dataout/salinityTransferTable.csv') |> 
  filter(location_name == 'East Lake Bonney') %>%
  mutate(depth.asl = as.character(depth.asl)) %>%
  bind_rows(. |> filter(year == 1995) |> mutate(year = 1993)) %>% # ions started in 1995, so use 1995 profiles for 1993 and 1994
  bind_rows(. |> filter(year == 1995) |> mutate(year = 1994)) %>%
  bind_rows(. |> filter(year == 2019) |> mutate(year = 2020)) %>%
  bind_rows(. |> filter(year == 2019) |> mutate(year = 2021)) %>%
  bind_rows(. |> filter(year == 2019) |> mutate(year = 2022)) %>%
  bind_rows(. |> filter(year == 2019) |> mutate(year = 2023))

# If no conductivity value, replace with salinity table 
df.spc2 = df.spc |> left_join(salz.pred, by = join_by(year, location_name, depth.asl)) |> 
  mutate(sal.pred2 = if_else(is.na(sal.pred), pred, sal.pred))

# Ok, but some of the transfer table doesn't go deep enough. Interpolate with constant 
df.spc3 = df.spc2 |> 
  mutate(across(c(sal.pred2), ~ na.approx(.x, na.rm = FALSE, maxgap = 50, rule = 2)))

# Check plot 

##################### 

a = ctd.join |> left_join(sal.pred, by = join_by(location_name, specCond))
a |> filter(is.na(pred)) |> filter(location_name == 'Lake Hoare')

table(b$location_name)

# Join hyspometry
left_join(hypo_new |> rename(depth.asl = Elevation_masl) |>  mutate(depth.asl = round(depth.asl, 1)), 
          by = c('location_name', 'depth.asl')) |> 

# Calculate salinity based on year and depth
# Every 0.5 m
salinity.pred = read_csv('dataout/salinityTransferTable.csv') |> 
  bind_rows(salinity.pred |> filter(year == 1995) |> mutate(year = 1993)) |> # ions started in 1995, so use 1995 profiles for 1993 and 1994
  bind_rows(salinity.pred |> filter(year == 1995) |> mutate(year = 1994)) 
  

a = ctd.join |> left_join(salinity.pred, by = join_by(year, location_name, depth.asl))

View(a |> filter(is.na(pred)))

ggplot(a) + 
  geom_point(aes(x = ctd_conductivity_mscm, y = pred)) +
  facet_wrap(~location_name, scales = 'free')

salinity.df

ggplot(salinity.df) + 
  geom_point(aes(x = specCond, y = salinity)) +
  facet_wrap(~location_name, scales = 'free')
