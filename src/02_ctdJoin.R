# Load libraries
library(tidyverse)
library(lubridate)
library(patchwork)
library(zoo)
library(mgcv)
library(marelac)
library(wql)
library(ggtext)
library(gridtext)
library(scico)


source('src/00_getCTD.R')
source('src/00_gethypso.R')
source('src/functions/SpecHeat_Water_vector.R')
source('src/functions/salinity_fromDensity.R')

# Round depth to 0.1 m increment
ctd.join = ctd |> 
  mutate(year = year(date_time)) |> 
  select(date_time, year, location_name, masl.approx, depth.asl, depth_m, ctd_conductivity_mscm, ctd_temp_c) |> 
  mutate(depth.asl = round(depth.asl, 1)) |> 
  group_by(date_time, year, location_name, depth.asl) |> 
  summarise_all(mean, na.rm = T) |> 
  ungroup() |> 
  # remove bad data 
  filter(!(location_name == 'West Lake Bonney' & date_time == as.Date('1995-01-13'))) |> 
  filter(!(location_name == 'West Lake Bonney' & date_time == as.Date('2002-12-16'))) |> 
  filter(!(location_name == 'East Lake Bonney' & date_time == as.Date('1995-01-14'))) |> 
  filter(!(location_name == 'East Lake Bonney' & date_time == as.Date('2002-11-14'))) |> 
  # 2002 and 2005 data looks about 1 m too low?
  mutate(depth.asl = if_else(year(date_time) == 2005 & month(date_time) >= 10, depth.asl +1, depth.asl)) |> 
  mutate(depth.asl = if_else(year(date_time) == 2002 & month(date_time) >= 10, depth.asl +1, depth.asl)) |> 
  
  # Bad data points 
  mutate(ctd_conductivity_mscm = if_else(location_name == 'West Lake Bonney' & date_time == as.Date('2002-11-14') & ctd_conductivity_mscm > 85, 
                                         NA, ctd_conductivity_mscm)) |> 
  mutate(ctd_conductivity_mscm = if_else(location_name == 'West Lake Bonney' & date_time == as.Date('2002-11-14') & 
                                           ctd_conductivity_mscm < 75 & depth.asl <25, 
                                           NA, ctd_conductivity_mscm)) |> 
  mutate(ctd_conductivity_mscm = if_else(location_name == 'East Lake Bonney' & date_time == as.Date('2002-12-11') & 
                                           ctd_conductivity_mscm > 120, 
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
    
    df.lake.list[[usedate]] = expand_grid(depth.asl.char = as.character(round(seq(round(max(df.lake$masl.approx, na.rm = T),1), min(df.lake$depth.asl, na.rm = T), by = -0.1),1))) |> 
      left_join(df.lake, join_by(depth.asl.char)) |> 
      mutate(depth_m = if_else(row_number() == 1 & is.na(depth_m), 0, depth_m)) |>
      mutate(ctd_conductivity_mscm = if_else(row_number() == 1 & is.na(ctd_conductivity_mscm), 0, ctd_conductivity_mscm)) |>
      mutate(ctd_temp_c = if_else(row_number() == 1 & is.na(ctd_temp_c), 0, ctd_temp_c)) |>
      mutate(depth.asl = if_else(row_number() == 1 & is.na(depth.asl), as.numeric(depth.asl.char), depth.asl)) |>
      mutate(date_time = usedate) |> 
      mutate(year = year(usedate)) |> 
      mutate(location_name = lakename) |> 
      mutate(across(c(depth.asl:ctd_temp_c), ~ na.approx(.x, na.rm = FALSE, maxgap = 50, rule = 2)))
    
  }
  
  # tail(df.lake.list[[as.Date('2019-11-25')]])
  useBottomCond <- function(list, usedate) {
    list |> 
      full_join(df.lake.list[[as.Date(usedate)]] |> filter(depth_m > 10) |> 
                  select(depth.asl.char, ctd_conductivity_mscm), by = "depth.asl.char", suffix = c("_df1", "_df2")) %>%
      mutate(ctd_conductivity_mscm = coalesce(ctd_conductivity_mscm_df1, ctd_conductivity_mscm_df2)) %>%
      select(names(df.lake.list[[as.Date(usedate)]])) |> 
      mutate(date_time = first(date_time), year = first(year), location_name = first(location_name), 
             masl.approx = first(masl.approx), depth.asl = as.numeric(depth.asl.char)) |> 
      mutate(depth_m = ifelse(is.na(depth_m), masl.approx - depth.asl, depth_m))
  }
  useBottomTemp <- function(list, usedate) {
    list |> 
      full_join(df.lake.list[[as.Date(usedate)]] |> filter(depth_m > 10) |> 
                  select(depth.asl.char, ctd_temp_c), by = "depth.asl.char", suffix = c("_df1", "_df2")) %>%
      mutate(ctd_temp_c = coalesce(ctd_temp_c_df1, ctd_temp_c_df2)) %>%
      select(names(df.lake.list[[as.Date(usedate)]])) |> 
      mutate(date_time = first(date_time), year = first(year), location_name = first(location_name), 
             masl.approx = first(masl.approx), depth.asl = as.numeric(depth.asl.char)) |> 
      mutate(depth_m = ifelse(is.na(depth_m), masl.approx - depth.asl, depth_m))
  }
  
  if(lakename == 'East Lake Bonney') {
    # Replace missing conductivity in ELB
    df.lake.list[[as.Date('2008-03-26')]] = df.lake.list[[as.Date('2008-03-26')]] |> 
      mutate(ctd_conductivity_mscm = NA) %>%
      useBottomCond(., '2008-03-19')
    df.lake.list[[as.Date('2019-11-25')]] = useBottomCond(df.lake.list[[as.Date('2019-11-25')]], '2018-12-22')
    df.lake.list[[as.Date('2019-12-16')]] = useBottomCond(df.lake.list[[as.Date('2019-12-16')]], '2018-12-22')
    df.lake.list[[as.Date('2020-01-07')]] = useBottomCond(df.lake.list[[as.Date('2020-01-07')]], '2018-12-22')
    df.lake.list[[as.Date('2022-11-29')]] = useBottomCond(df.lake.list[[as.Date('2022-11-29')]], '2022-01-07')
    df.lake.list[[as.Date('2022-12-19')]] = useBottomCond(df.lake.list[[as.Date('2022-12-19')]], '2022-01-07')
    df.lake.list[[as.Date('2023-01-05')]] = useBottomCond(df.lake.list[[as.Date('2023-01-05')]], '2022-01-07')
    
    df.lake.list[[as.Date('2019-12-16')]] = useBottomTemp(df.lake.list[[as.Date('2019-12-16')]], '2019-11-25')
    df.lake.list[[as.Date('2022-11-29')]] = useBottomTemp(df.lake.list[[as.Date('2022-11-29')]], '2022-01-07')
    df.lake.list[[as.Date('2022-12-19')]] = useBottomTemp(df.lake.list[[as.Date('2022-12-19')]], '2022-01-07')
  }
  if(lakename == 'West Lake Bonney') {
    df.lake.list[[as.Date('2002-11-14')]] = df.lake.list[[as.Date('2002-11-14')]] |> 
      mutate(ctd_conductivity_mscm = ifelse(depth.asl <= 45, NA, ctd_conductivity_mscm)) %>%
      useBottomCond(., '2001-12-29')
  }
  
  df.lake.out = bind_rows(df.lake.list)
  checkdates = df.lake.out |> filter(is.na(ctd_temp_c)) |> group_by(date_time) |> summarise_all(first)
  df.lake.out = df.lake.out |> filter(!date_time %in% checkdates$date_time) # filter out profiles with too much missing data 
  return(df.lake.out)
}

df.wlb.clean = clean.ctd(lakename = 'West Lake Bonney') 
df.lf.clean = clean.ctd(lakename = 'Lake Fryxell')
df.lh.clean = clean.ctd(lakename = 'Lake Hoare')
df.elb.clean = clean.ctd(lakename = 'East Lake Bonney')

# Combine the four lakes 
df.clean = df.lf.clean |> bind_rows(df.lh.clean, df.elb.clean, df.wlb.clean)

# # Check plots 
# p1 = ggplot(df.clean) +
#   geom_path(aes(x = ctd_temp_c, y = depth.asl, group = date_time, color = year(date_time))) +
#   ylab('Elevation (m asl)') + xlab('Temp (°C)') +
#   scale_colour_viridis_c(option = 'F', name = 'Year') +
#   theme_bw(base_size = 9) +
#   facet_wrap(~location_name, scales = 'free')
# 
# p2 = ggplot(df.clean) +
#   geom_path(aes(x = ctd_conductivity_mscm, y = depth.asl, group = date_time, color = year(date_time))) +
#   ylab('Elevation (m asl)') + xlab('Cond (mS/cm)') +
#   scale_colour_viridis_c(option = 'F', name = 'Year') +
#   theme_bw(base_size = 9) +
#   facet_wrap(~location_name, scales = 'free')
# 
# # Check plots
# p1 + p2 + plot_layout(guides = 'collect')

##################### Calculate salinity ########################
# Density equations from Spigel and Priscu 1996: Evolution of temperature and salt structure of Lake Bonney, a chemically stratified Antarctic lake
# East Lake Bonney, where p = density (kg m -3), PUN = density (kg m_3 ) predicted by unmodified UNESCO equations at specified S, t, p
# and S = practical salinity predicted by the UNESCO equations for specified C, t, p.
alpha.ELB = 15.299; beta.ELB = 133.36

df.sal = df.clean |> 
  mutate(ctd_conductivity_mscm = if_else(ctd_conductivity_mscm <= 0 , 0, ctd_conductivity_mscm)) |> # cannot be negative
  mutate(specCond_5 = ctd_conductivity_mscm/(1 + 0.020*(ctd_temp_c - 5))) |> # standardize to 5°C
  mutate(salinity.UNESCO = ec2pss(ctd_conductivity_mscm, t = ctd_temp_c, p = 1 + depth_m)) |> 
  mutate(salinity.UNESCO = if_else(salinity.UNESCO <= 0 , 0, salinity.UNESCO)) |> # cannot be negative
  rowwise() |> 
  mutate(density.UNESCO = ifelse(is.na(salinity.UNESCO)| is.na(ctd_temp_c),
                                  NA,
                                  sw_dens(S = salinity.UNESCO, t = ctd_temp_c, p = 1 + (depth_m/10), method = 'UNESCO'))) |> 
  # SP salinity adjustment (Note: Correction for high salinities to account for discrepancy between conductivities measured by the 
  # Radiometer CDM83 conductivity meter (used in lab analyses for Spigel & Priscu 1996 eqn. of state paper) and Seabird conductivity sensors (both SBE25 and SBE19))
  mutate(salinity.SP96 = ifelse(salinity.UNESCO>66, 
                                 (((-0.0000002832456*salinity.UNESCO)-0.0008991763)*salinity.UNESCO+1.0609)*salinity.UNESCO,
                                 salinity.UNESCO)) |>
  mutate(density.UNESCO.SP96 = ifelse(is.na(salinity.SP96)| is.na(ctd_temp_c),
                                  NA,
                                  sw_dens(S = salinity.SP96, t = ctd_temp_c, p = 1 + (depth_m/10), method = 'UNESCO'))) |>
  mutate(density_kg_m3 = ifelse(location_name == 'East Lake Bonney' & salinity.SP96 > 100, 
                                (alpha.ELB*(salinity.SP96 - 42)/(beta.ELB-(salinity.SP96-42))) + density.UNESCO.SP96, 
                               density.UNESCO)) |> 
  # abck out salinity from unesco equation 
  mutate(salinity_g_kg = ifelse(location_name == 'East Lake Bonney' & salinity.SP96 > 100, 
                                optimizeSalinity(ctd_temp_c, density_kg_m3, depth_m)$minimum, 
                                salinity.UNESCO))

# Check plot 
ggplot(df.sal) +
  geom_path(aes(x = salinity_g_kg, y = depth.asl, group = date_time, color = year(date_time))) +
  ylab('Elevation (m asl)') + xlab('Salinity (g/kg)') +
  scale_colour_viridis_c(option = 'F', name = 'Year') +
  theme_bw(base_size = 9) +
  facet_wrap(~location_name, scales = 'free')

# Check plot 
ggplot(df.sal) +
  geom_path(aes(x = density_kg_m3, y = depth.asl, group = date_time, color = year(date_time))) +
  ylab('Elevation (m asl)') + xlab('Density (kg/m3)') +
  scale_colour_viridis_c(option = 'F', name = 'Year') +
  theme_bw(base_size = 9) +
  facet_wrap(~location_name, scales = 'free')

# Check plot 
ggplot(df.sal) +
  geom_path(aes(x = specCond_5, y = depth.asl, group = date_time, color = year(date_time))) +
  geom_path(aes(x = ctd_conductivity_mscm, y = depth.asl, group = date_time, color = year(date_time))) +
  ylab('Elevation (m asl)') + xlab('Density (kg/m3)') +
  scale_colour_viridis_c(option = 'F', name = 'Year') +
  theme_bw(base_size = 9) +
  facet_wrap(~location_name, scales = 'free')

# ####### Old cold
# df.spc = df.clean |> 
#   mutate(specCond.raw = ctd_conductivity_mscm/(1 + 0.020*(ctd_temp_c - 5))) |> # standardize to 5°C
#   mutate(specCond = as.character(round(specCond.raw / 0.1) * 0.1)) |>  # round to nearest 0.1 m depth, change to character for join
#   mutate(specCond = if_else(location_name == 'Lake Hoare', as.character(round(specCond.raw / 0.01) * 0.01), specCond)) # For Lake Hoare, round spC to 2 decimals
# 
# # Load conductivity/salinity relationship # Not applicable for ELB salinity > 180
# sal.pred = read_csv('dataout/condSalTransfer.csv') |> 
#   mutate(specCond = as.character(specCond)) |> 
#   rename(sal.pred = pred)
# 
# df.spc = df.spc |> 
#   left_join(sal.pred, by = join_by(location_name, specCond))
# df.spc |> filter(is.na(sal.pred))
# 
# # Load salinity/depth relationship
# salz.pred = read_csv('dataout/salinityTransferTable.csv') |> 
#   filter(location_name == 'East Lake Bonney') %>%
#   mutate(depth.asl.char = as.character(depth.asl)) %>%
#   select(-depth.asl) %>%
#   bind_rows(. |> filter(year == 1995) |> mutate(year = 1993)) %>% # ions started in 1995, so use 1995 profiles for 1993 and 1994
#   bind_rows(. |> filter(year == 1995) |> mutate(year = 1994)) %>%
#   bind_rows(. |> filter(year == 2019) |> mutate(year = 2020)) %>%
#   bind_rows(. |> filter(year == 2019) |> mutate(year = 2021)) %>%
#   bind_rows(. |> filter(year == 2019) |> mutate(year = 2022)) %>%
#   bind_rows(. |> filter(year == 2019) |> mutate(year = 2023))
# 
# # If no conductivity value, replace with salinity table 
# df.spc2 = df.spc |> left_join(salz.pred, by = join_by(year, location_name, depth.asl.char)) |> 
#   mutate(sal.pred2 = if_else(is.na(sal.pred), pred, sal.pred))
# 
# # Ok, but some of the transfer table doesn't go deep enough. Interpolate with constant 
# df.spc3 = df.spc2 |> 
#   group_by(location_name, date_time) |> 
#   mutate(across(c(sal.pred2), ~ na.approx(.x, na.rm = FALSE, maxgap = 50, rule = 2))) |> 
#   mutate(sal.pred2 = if_else(sal.pred2 < 0, 0, sal.pred2)) %>% 
#   select(-sal.pred, -pred)
# 
# # Check plot 
# ggplot(df.spc3) +
#   geom_path(aes(x = sal.pred2, y = depth.asl, group = date_time, color = year(date_time))) +
#   ylab('Elevation (m asl)') + xlab('Salinity (mg/L)') +
#   scale_colour_viridis_c(option = 'F', name = 'Year') +
#   theme_bw(base_size = 9) +
#   facet_wrap(~location_name, scales = 'free')

##################### Add specific heat capacity and density ########################
# equation not build for temperatures < 0°C or S > 180, but what can you do
# Units J/kg K
df.spcH = df.sal |> 
  ungroup() |> 
  mutate(spHeat_J_kgK = SW_SpcHeat_vector(Temp = ctd_temp_c, S = salinity_g_kg, P = 1 + (depth_m/10))) #vectorized the function
  # mutate(spHeat_J_kgK = SW_SpcHeat(Temp = ctd_temp_c, S = salinity_g_kg, P = 1 + (depth_m/10))) #units deafult, °C, ppt, bar

quantile(df.spcH$spHeat_J_kgK, na.rm = T)
quantile(df.spcH$density_kg_m3, na.rm = T)

##################### Add freezing point of water ########################
# This is complicated because no equation exists for salinities > 40. 
# Using a lookup table from Bodnar 1993
# https://www-sciencedirect-com.ezproxy.library.wisc.edu/science/article/pii/001670379390378A?via%3Dihub
bodnar = read_csv('datain/papers/Bodnar_1993_FreezingPoint_Lookup.csv') %>% 
  mutate(FPD = FPD1+FPD2) %>% 
  mutate(salinity_g_kg = Salinity_perWt * 10) |> 
  select(salinity_g_kg, FPD) %>% 
  arrange(salinity_g_kg) %>% 
  filter(!is.na(salinity_g_kg))

bodnar.poly = lm(FPD ~ poly(salinity_g_kg, degree = 3), data = bodnar)
# #create scatterplot
# df = data.frame(sal.pred2 = 1:213) %>% mutate(my_model = predict(bodnar.poly, .))
# ggplot(bodnar) + geom_point(aes(x = sal.pred2, y = FPD)) +
#   geom_point(data = df, aes(x = sal.pred2, y = my_model), col = 'red')

# Lookup table only goes up to eutectic point of pure NaCl (21.2 %wt, FPD = -23.18)
# Everything above this salinity set FPD to 23.2
df.spcH = df.spcH %>% 
  ungroup() %>%
  mutate(FPD = predict(bodnar.poly, .)) |> # predict freezing point depressing based on polynomial
  mutate(FPD = -FPD) |> 
  mutate(FPD = if_else(FPD < -21.21, -21.21, FPD)) #eutectic point of NaCl

df.spcH %>% 
  ungroup() %>%
  mutate(FPD = predict(bodnar.poly, .)) |> # predict freezing point depressing based on polynomial
  mutate(FPD = -FPD) |> 
  filter(FPD < -21.21)

ggplot(df.spcH) +
  geom_point(aes(x = salinity_g_kg, y = FPD))
  
##################### Add lake ice thickness ########################
df.full.ice = df.spcH |> 
  left_join(ice.interp, by = join_by(date_time, location_name)) |> 
  mutate(tempUse = if_else(depth.asl > ice.asl, NA, ctd_temp_c)) |> 
  mutate(condUse = if_else(depth.asl > ice.asl, NA, ctd_conductivity_mscm)) |> 
  mutate(isIce = if_else(depth.asl > ice.asl, TRUE, FALSE)) |> 
  mutate(iceDensity_kgm3 = if_else(depth.asl > ice.asl, 900, NA)) |> 
  mutate(location_name = factor(location_name, levels = c('Lake Fryxell','Lake Hoare', 'East Lake Bonney', 'West Lake Bonney'))) 

###################### CTD PLOTS ########################
# specHeatIce = 0.506 cal/degC/g # 2108 J/kgK
# latentHeatIce = 70.8 cal/g (or 334000 J/kg)

# The latent heat of fusion of ice is 33,600 joules per kilogram
# latent heat of ice = density * thickness *  latent heat of ice (334000 J/kg)

# for ice plotting
toptemp = df.full.ice |> filter(!isIce) |> group_by(date_time, location_name) |> filter(depth.asl == max(depth.asl, na.rm = T)) |> 
  select(location_name, date_time, depth.asl, ctd_temp_c, ctd_conductivity_mscm)
icebox = df.full.ice |> filter(isIce == TRUE) |> group_by(location_name, date_time) |> 
  summarise(min.depth = min(depth.asl), max.depth = max(depth.asl)) |> 
  left_join(toptemp)

# Check plot - temperature
ct1 = ggplot(df.full.ice) +
  geom_rect(data = icebox,
            aes(xmin = ctd_temp_c, xmax = ctd_temp_c, ymin = max.depth, ymax = min.depth, group = year(date_time)), 
            color = 'grey50',size = 0.3) +
  geom_path(aes(x = tempUse, y = depth.asl, group = date_time, color = year(date_time))) +
  ylab('Elevation (m asl)') + xlab('Temperature (°C)') +
  scale_color_scico(palette = 'bilbao', name = 'Year', direction = -1, end = 0.9) +
  theme_bw(base_size = 9) +
  facet_wrap(~location_name, scales = 'free', nrow = 1) +
  new_scale_color() + 
  geom_path(data = spigel_LF, aes(x = Temp_C, y = depth.asl, color = factor(year(date_time))), linewidth = 1) +
  geom_path(data = hoare_LF, aes(x = Temp_C, y = depth.asl, color = factor(year(date_time))), linewidth = 1) +
  geom_path(data = shirtcliffe_bonney_1963, aes(x = Temp_C, y = depth.asl), color = 'gold3', linewidth = 1) +
  scale_color_manual(values = c('gold','gold3'), name = 'Year')

# Check plot - conductivity
ct2 = ggplot(df.full.ice) +
  geom_rect(data = icebox,
            aes(xmin = ctd_conductivity_mscm, xmax = ctd_conductivity_mscm, ymin = max.depth, ymax = min.depth, group = year(date_time)), 
            color = 'grey50',size = 0.3) +
  geom_path(aes(x = condUse, y = depth.asl, group = date_time, color = year(date_time))) +
  ylab('Elevation (m asl)') + xlab('Conductivity (mS cm^-1 )') +
  scale_color_scico(palette = 'bilbao', name = 'Year', direction = -1, end = 0.9) +
  theme_bw(base_size = 9) +
  facet_wrap(~location_name, scales = 'free', nrow = 1) +
  theme(axis.title.x = element_markdown())

ct1 / ct2 + plot_layout(guides = 'collect') +
  plot_annotation(tag_levels = 'a', tag_suffix = ')') &
  theme(plot.tag = element_text(size = 8), legend.position = 'bottom', 
        legend.key.width = unit(0.5, 'cm'), legend.key.height = unit(0.3, 'cm'),
        legend.title = element_blank(),
        legend.margin = margin(0, 0, 0, 0))
ggsave('figures/Fig1_CTD.png', width = 6, height = 6, dpi = 500)


# Figure 2: Same plot but of CTD changes 
# Temperature and Conductivity Changes
useprofile = df.full.ice |> 
  group_by(location_name) |> 
  # filter(date_time == first(date_time)) |> 
  filter(date_time %in% as.Date(c('1995-10-15', '1995-11-07','1993-12-09'))) |> 
  select(date_time, depth.asl, location_name, condUse, tempUse) |> 
  rename(initialCond = condUse, initialTemp = tempUse, initialDate = date_time)

diffprofile = df.full.ice |> 
  group_by(date_time, location_name) |> 
  select(depth.asl, location_name, condUse) |> 
  left_join(useprofile) |> 
  mutate(newCond = condUse - initialCond) |> 
  filter(!is.na(newCond))

c2 = ggplot(diffprofile) +
  geom_path(aes(x = newCond, y = depth.asl, group = date_time, color = year(date_time))) +
  ylab('Elevation (m asl)') + xlab('\u0394  Conductivity (mS cm^-1 )') +
  scale_color_scico(palette = 'bilbao', name = 'Year', direction = -1, end = 0.9) +
  theme_bw(base_size = 9) +
  theme(axis.title.x = element_markdown()) +
  facet_wrap(~location_name, scales = 'free', nrow = 1)

# Join
diffprofile = df.full.ice |> 
  group_by(date_time, location_name) |> 
  select(depth.asl, location_name, tempUse) |> 
  left_join(useprofile) |> 
  mutate(newTemp = tempUse - initialTemp) |> 
  filter(!is.na(newTemp))
c1 = ggplot(diffprofile) +
  geom_path(aes(x = newTemp, y = depth.asl, group = date_time, color = year(date_time))) +
  ylab('Elevation (m asl)') + xlab('\u0394 Temperature (°C)') +
  scale_color_scico(palette = 'bilbao', name = 'Year', direction = -1, end = 0.9) +
  theme_bw(base_size = 9) +
  theme(axis.title.x = element_markdown()) +
  facet_wrap(~location_name, scales = 'free', nrow = 1); c1

diffprofile |> group_by(location_name) |> summarise(first(initialDate))

c1 / c2 + plot_layout(guides = 'collect') +
  plot_annotation(tag_levels = 'a', tag_suffix = ')') &
  theme(plot.tag = element_text(size = 8), legend.position = 'bottom', 
        legend.key.width = unit(0.5, 'cm'), legend.key.height = unit(0.3, 'cm'),
        legend.title = element_blank(),
        legend.margin = margin(0, 0, 0, 0))

ggsave('figures/Fig2_CTDchange.png', width = 6, height = 6, dpi = 500)

####  Sampling Days Plot ####
samplingdays = df.full.ice |> 
  group_by(location_name, date_time) |> 
  summarise() |> 
  mutate(fakeyear = `year<-`(date_time, 2024)) |> 
  mutate(fakeyear2 = if_else(month(fakeyear) >= 10, `year<-`(fakeyear, 2023), fakeyear)) |> 
  mutate(location_name = factor(location_name, levels = c('Lake Fryxell','Lake Hoare', 'East Lake Bonney', 'West Lake Bonney'))) |> 
  mutate(wateryear = if_else(month(date_time) >= 10, year(date_time) + 1, year(date_time)))

# Highlight profiles used for averging 
source('src/functions/profileAvgDates.R')
# this returns chosen dates for each lake
bestdates
# 1 East Lake Bonney   338     114 1995-12-04 
# 2 Lake Fryxell       329     170 1995-11-25 
# 3 Lake Hoare         345     246 1995-12-11 
# 4 West Lake Bonney   327     107 1995-11-23 

# Removedate where it's really challenging to get a consistency year to year
chosendates2 = chosendates |> 
  filter(!(location_name == 'Lake Hoare' & wateryear <= 2004)) |> 
  filter(!(location_name == 'Lake Fryxell' & wateryear <= 1995))  

ggplot(samplingdays) +
  geom_point(data = chosendates, aes(x = fakeyear2, y = wateryear), color = 'red') +
  geom_point(data = chosendates2, aes(x = fakeyear2, y = wateryear), color = 'gold') +
  geom_tile(aes(x = fakeyear2, y = wateryear), linewidth  = 200) +
  theme_bw(base_size = 9) +
  scale_y_continuous(breaks = seq(1993,2025, by = 2), name = 'Water Year') +
  scale_x_date(date_labels = '%b', date_breaks = '1 months', name = 'Day') +
  facet_wrap(~location_name) +
  theme(axis.title.x = element_blank())

ggsave('figures/SI_SamplingDays.png', width = 6, height = 4, dpi = 500)

# # Check plot - temperature
# ggplot(df.full.ice |> dplyr::filter(location_name %in% c('East Lake Bonney', 'West Lake Bonney'))) +
#   geom_rect(data = icebox |> dplyr::filter(location_name %in% c('East Lake Bonney', 'West Lake Bonney')),
#             aes(xmin = ctd_temp_c, xmax = ctd_temp_c, ymin = max.depth, ymax = min.depth, group = year(date_time)), 
#             color = 'grey50',size = 0.3) +
#   geom_path(aes(x = tempUse, y = depth.asl, group = date_time, color = year(date_time))) +
#   ylab('Elevation (m asl)') + xlab('Temperature (°C)') +
#   scale_colour_viridis_c(option = 'F', name = 'Year') +
#   theme_bw(base_size = 9) +
#   facet_wrap(~location_name, scales = 'free', ncol = 2)
# ggsave('figures/SI_LB.png', width = 6, height = 6, dpi = 500)
