##############################################################################
# This code does a lot with ions + salinity. In the end outputs figure of
# Density comparisons for the bottom of ELB and WLB using the Gibbs function (Feistel, 2008), the
# UNESCO equation of state (Fofonoff and Millard, 1983), and equations from Spigel and Priscu (1996). 
# Blue circles are measured density using Anton Paar DMA 35 density meter.
##############################################################################


# Load libraries
library(tidyverse)
library(lubridate)
library(zoo)
library(mgcv)
library(marelac)
library(wql)
library(oce)
library(gsw)

# source lake lakels
source('src/00_getLakeLevels.R')
source('src/00_getDensity.R')
# source DIC function 
# source('https://raw.githubusercontent.com/hdugan/lake_DIC/master/lake_DIC.R')
source('src/functions/carbonate_speciation.R')

### Get measured density values, calculate mean ###
density.mean = density.df |> 
  select(-lake, -specGravity, -date_time) |> 
  group_by(location_name, depth_m) |> 
  summarise_all(mean, na.rm = T)

###### CTD Round #########
# Takacs-Vesbach, C. and J. Priscu. 2025. Conductivity, temperature, and depth (CTD) vertical profiles collected from lakes in the McMurdo Dry Valleys, Antarctica (1993-2023, ongoing) ver 18. 
# Environmental Data Initiative. https://doi.org/10.6073/pasta/004de32b27fc88954abdce0ff8a3bbb3 (Accessed 2025-06-29).

# Package ID: knb-lter-mcm.88.17 Cataloging System:https://pasta.edirepository.org.
inUrl1  <- "https://pasta.lternet.edu/package/data/eml/knb-lter-mcm/88/18/91474a205d3dd99cc794f8510d2d99c5" 
infile1 <- tempfile()
download.file(inUrl1,infile1,method="curl")

ctd.2022 <- read_csv(infile1) |> 
  mutate(date_time = as.Date(mdy_hm(date_time))) |>
  filter(year(date_time) %in% c(2022,2023), month(date_time) %in% c(11,12)) |> 
  filter(location_name %in% c('West Lake Bonney', 'East Lake Bonney', 'Lake Fryxell','Lake Hoare')) |> 
  select(location_name, date_time, depth_m = wire_corrected_depth_m, ctd_temp_c, ctd_conductivity_mscm) |> 
  mutate(depth_m = round(depth_m, 1)) |> 
  mutate(depth.asl.char = as.character(depth_m)) |> 
  group_by(location_name, date_time, depth.asl.char) |> 
  summarise_all(first) |> 
  ungroup() |> 
  # mutate(depthdiff = abs(depth_1m - depth_m)) |> 
  group_by(location_name, date_time) |> 
  filter(n() >= 5) |> 
  mutate(date_time = `year<-`(date_time, 2022))

ctd2023 = read_csv('datain/ctd_2023.csv') |> 
  mutate(date_time = as.Date(mdy_hm(date_time))) |> 
  bind_rows(ctd.2022) |> 
  filter(location_name %in% c('West Lake Bonney', 'East Lake Bonney', 'Lake Fryxell','Lake Hoare')) |> 
  select(location_name, date_time, depth_m = wire_corrected_depth_m, ctd_temp_c, ctd_conductivity_mscm) |> 
  mutate(depth_m = round(depth_m, 1)) |> 
  mutate(depth.asl.char = as.character(depth_m)) |> 
  group_by(location_name, date_time, depth.asl.char) |> 
  summarise_all(first) |> 
  ungroup() |> 
  # mutate(depthdiff = abs(depth_1m - depth_m)) |> 
  group_by(location_name, date_time) |> 
  filter(n() >= 5) |> 
  mutate(date_time = `year<-`(date_time, 2022))

# Function to fill missing depths and interpolate values
fill_and_interpolate <- function(df, maxgap = 25) {
  df %>%
    group_by(location_name, date_time) %>% 
    # Create a complete sequence of depths for each group
    complete(depth.asl.char = as.character(seq(min(depth_m), max(depth_m), by = 0.1))) %>%
    mutate(depth_m = as.numeric(depth.asl.char)) |> 
    # Interpolate missing values
    arrange(location_name, date_time, depth_m) %>%
    # Flag large gaps
    mutate(
      gap = ifelse(is.na(lag(depth_m)) | is.na(lead(depth_m)), NA, lead(depth_m) - depth_m),
      gap = if_else(depth_m == first(depth_m), 0, gap),
      interpolate = ifelse(is.na(gap) | gap > maxgap, NA, TRUE)
    ) |> 
    mutate(ctd_temp_c = ifelse(interpolate == TRUE, approx(depth_m, ctd_temp_c, depth_m, method = "linear", rule = 1)$y, NA)) %>%
    mutate(ctd_conductivity_mscm = ifelse(interpolate == TRUE, approx(depth_m, ctd_conductivity_mscm, depth_m, method = "linear", rule = 1)$y, NA)) %>%
    ungroup()
}

# Apply the function
ctd.round.fill = fill_and_interpolate(ctd.2022)
ctd.round.fill |> group_by(location_name, date_time) |> summarise_all(first)

ctd.dates = ctd.round.fill |> 
  mutate(datepre7 = date_time - 7, datepost7 = date_time + 7) |> 
  rename(dateCTD = date_time)

p.cond = ggplot(ctd.round.fill |> filter(location_name %in% c('West Lake Bonney', 'East Lake Bonney'))) +
  geom_point(aes(x = ctd_conductivity_mscm, y = depth_m)) +
  scale_y_reverse() +
  xlab('Conductivity (mS/cm)') + ylab('Depth (m)') +
  theme_bw(base_size = 9) +
  facet_wrap(~location_name)

# Priscu, J., K. Welch, and W. Lyons. 2022. 
# Ion concentrations in discrete water column samples collected from lakes in the McMurdo Dry Valleys, 
# Antarctica (1991-2019, ongoing) ver 12. Environmental Data Initiative. 
# https://doi.org/10.6073/pasta/31f7354d1a05679eb3ce7c384c6e2b22 (Accessed 2024-06-26).
ions_2022 = read_csv('datain/limno_IC2022-23.csv') 

# Priscu, J.C. 2023. Dissolved inorganic carbon (DIC) concentrations in discrete water column samples 
# collected from lakes in the McMurdo Dry Valleys, Antarctica (1993-2022, ongoing). Environmental Data Initiative. 
# DOI: 10.6073/pasta/fadcc745b6ecb6d1942dda95ef373eab. Dataset accessed 26 June 2024.

inUrl1  <- "https://pasta.lternet.edu/package/data/eml/knb-lter-mcm/59/11/9b9e39a6b2c5f7a4322a806062376181" 
infile1 <- tempfile()
download.file(inUrl1,infile1,method="curl")

dic <- read_csv(infile1) |> 
  mutate(date_time = as.Date(mdy_hm(date_time))) |> 
  filter(location_name %in% c('West Lake Bonney', 'East Lake Bonney', 'Lake Fryxell','Lake Hoare')) |> 
  filter(year(date_time) == 2022 & month(date_time) %in% c(11,12))

# Takacs-vesbach, C.D., Priscu, J.C. 2024. Hydrogen ion concentrations (pH) in discrete water column samples 
# collected from lakes in the McMurdo Dry Valleys, Antarctica (1993-2022, ongoing). Environmental Data Initiative. 
# DOI: 10.6073/pasta/4a9c2ed67ccb6e2dd4b5ece36719d590. Dataset accessed 26 June 2024.

inUrl1  <- "https://pasta.lternet.edu/package/data/eml/knb-lter-mcm/63/14/ed5ec547fcb1aa64206cdb91c55b74e2" 
infile1 <- tempfile()
download.file(inUrl1,infile1,method="curl")

ph <- read_csv(infile1) |> 
  mutate(date_time = as.Date(mdy_hm(date_time))) |> 
  filter(location_name %in% c('West Lake Bonney', 'East Lake Bonney', 'Lake Fryxell','Lake Hoare')) |> 
  filter(year(date_time) == 2022 & month(date_time) %in% c(11,12))

##### Combine data #####

dic.join = dic |> select(location_name, date_time, depth_m, dic_mg_cl) |> 
  full_join(ph |> select(location_name, date_time, depth_m, ph), by = join_by(location_name, date_time, depth_m)) |> 
  left_join(ctd.dates, by = join_by(location_name, date_time >= datepre7, date_time <= datepost7, depth_m)) |> 
  group_by(location_name, date_time) |> 
  mutate(ph.approx = na.approx(ph, na.rm = FALSE))

# Use temp, DIC, ph to estimate carbonate   
# Bicarbonate = 61 mg/meq
# Carbonate = 30.5 mg/meq

dic.carbonate = dic.join |> 
  mutate(carbonate = carbonate_speciation(TEMP = ctd_temp_c, DIC = dic_mg_cl, pH = ph.approx, output = "mg"))  %>%
  unnest_wider(carbonate) |> 
  filter(!is.na(bicarbonate_mgkg)) |> 
  select(-date_time, -dateCTD, -interpolate, -gap, -datepre7, -datepost7, -depth.asl.char) |> 
  group_by(location_name, depth_m) |> 
  summarise_all(mean, na.rm = T)

dic.carbonate |> select(dic_mg_cl, ph, ctd_temp_c, bicarbonate_mgkg, carbonate_mgkg)
# dic.carbonate |> select(dic_mg_cl, ph, ctd_temp_c, bicarbonate_mgCkg, carbonate_mgCkg)

# combine 
ions.df = ions_2022 |> 
  rowwise() |> 
  filter(!is.na(cl_mgl)) |>  # dominant anion
  filter(!is.na(na_mgl)) |>  # dominant cation
  mutate(cation_sum = sum(li_mgl, na_mgl, k_mgl, mg_mgl, ca_mgl, na.rm = T)) |> 
  mutate(anion_sum = sum(cl_mgl, br_mgl, so4_mgl,f_mgl, na.rm = T)) |> 
  select(-date_time) |> 
  group_by(location_name, depth_m) |> 
  summarise_all(mean, na.rm = T)

# Full join of data

full.df = ions.df |> full_join(dic.carbonate |> select(location_name:ctd_conductivity_mscm, co2_mgkg:alkalinity_uEqkg), 
                               by = join_by(location_name, depth_m))

# Density equations from Spigel and Priscu 1996: Evolution of temperature and salt structure of Lake Bonney, a chemically stratified Antarctic lake
# East Lake Bonney 
# where p = density (kg m -3), PUN = density (kg m_3 ) predicted by unmodified UNESCO equations at specified S, t, p (see appendix), a = 133.36, b = 15 .299,
# and S = practical salinity predicted by the UNESCO equations for specified C, t, p.
alpha.ELB = 15.299; beta.ELB = 133.36
# West Lake Bonney. density - density.UNESCO = alpha.ELB(salinity.UNESCO - 42)/[beta.ELB - (salinity.UNESCO-42)], S>42
alpha.WLB = 0.13696

salinity.df = full.df |> 
  select(location_name:depth_m, ctd_temp_c, ctd_conductivity_mscm, cation_sum, anion_sum, bicarbonate_mgkg, carbonate_mgkg) |> 
  arrange(location_name, depth_m) |> 
  filter(!is.na(bicarbonate_mgkg)) |> 
  filter(!is.na(ctd_conductivity_mscm)) |> 
  filter(!is.na(cation_sum)) |> 
  mutate(salinity.mg_l = (cation_sum + anion_sum + bicarbonate_mgkg + carbonate_mgkg)/1000) |> 
  # Compare to UNESCO equations
  # mutate(salinity.UNESCO2 = convert_RtoS(R = ctd_conductivity_mscm/42.914, t = ctd_temp_c, P = 1 + (depth_m/10))) |> # same as equation below (checked)
  mutate(salinity.UNESCO = ec2pss(ctd_conductivity_mscm, t = ctd_temp_c, p = 1 + depth_m)) |> 
  mutate(density.UNESCO = sw_dens(S = salinity.UNESCO, t = ctd_temp_c, p = 1 + (depth_m/10), method = 'UNESCO')) |>
  mutate(density.GIBBS = sw_dens(S = salinity.UNESCO, t = ctd_temp_c, p = 1 + (depth_m/10), method = 'Gibbs')) |>
  mutate(density.CHEN = sw_dens(S = salinity.UNESCO, t = ctd_temp_c, p = 1 + (depth_m/10), method = 'Chen')) |>
  left_join(ll.interp |> filter(date_time == as.Date('2022-11-15')) |> select(masl.approx), by = join_by(location_name),  relationship = "many-to-many") |> 
  mutate(depth.asl = masl.approx - depth_m) |>
  
  mutate(salinity.SP96 = if_else(salinity.UNESCO>66, 
                                 (((-0.0000002832456*salinity.UNESCO)-0.0008991763)*salinity.UNESCO+1.0609)*salinity.UNESCO,
                                 salinity.UNESCO)) |>
  mutate(density.UNESCO.SP96 = sw_dens(S = salinity.SP96, t = ctd_temp_c, p = 1 + (depth_m/10), method = 'UNESCO')) |>
  mutate(density.SP96 = if_else(location_name == 'East Lake Bonney' & salinity.SP96 > 42, 
                                (alpha.ELB*(salinity.SP96 - 42)/(beta.ELB-(salinity.SP96-42))) + density.UNESCO.SP96, 
                                density.UNESCO.SP96)) |> 
  mutate(density.SP96 = if_else(location_name == 'West Lake Bonney' & salinity.SP96 > 42, (alpha.WLB*(salinity.SP96 - 42)) + density.UNESCO.SP96, 
                                density.SP96)) |> 
  left_join(density.mean, by = join_by(location_name, depth_m)) 

########### optimize SP96 to 2022 data ###########
# elb.den = salinity.df2 |> filter(location_name == 'East Lake Bonney', ctd_conductivity_mscm > 90)
# 
# minDens <- function(pars, df) {
#   alpha = pars[1]
#   beta = pars[2]
#   
#   df = df |> 
#     select(location_name:ctd_conductivity_mscm, salinity.mg_l:density.UNESCO, density_kgm3, salinity.SP96, density.Tadj) |> 
#     mutate(testDens = (alpha*(salinity.SP96 - 42)/(beta-(salinity.SP96-42))) + density.UNESCO) |> 
#     mutate(diffDens = abs(testDens - density.Tadj))
#   
#   return(sum(df$diffDens^2, na.rm = T))
# }
# 
# optim(c(15, 133), fn = minDens, df = elb.den, method = "Nelder-Mead")
# # answer 13.83, 158.9
# # 11.27409 132.824973
# 
# salinity.df = salinity.df |> 
#   mutate(density.SP22 = if_else(location_name == 'East Lake Bonney' & ctd_conductivity_mscm > 90, 
#                                 (11.27*(salinity.SP96 - 42)/(132.82-(salinity.SP96-42))) + density.UNESCO, 
#                                 density.UNESCO), density.UNESCO)

########### ########### ########### ########### ########### 

adjDensity = salinity.df |> 
  filter(!is.na(salinity_fromDensity)) |> 
  mutate(density.Tadj = sw_dens(S = salinity_fromDensity, t = ctd_temp_c, p = 1 + (depth_m/10), method = 'UNESCO')) |> 
  select(location_name, depth_m, density_kgm3, salinity_fromDensity, density.Tadj)

salinity.df2 = salinity.df |> 
  left_join(adjDensity, by = join_by(location_name, depth_m, density_kgm3, salinity_fromDensity)) |> 
  mutate(salinity_gkg_SP96 = salinity.mg_l/density.SP96*1000) |>
  mutate(salinity_gkg_adj = salinity.mg_l/density.Tadj*1000) |> 
  # add back in missing rows 
  full_join(full.df |> select(location_name, depth_m, ctd_temp_c, ctd_conductivity_mscm)) |> 
  rowwise() |> 
  mutate(salinity_gkg_SP96_back = optimizeSalinity(ctd_temp_c, density.SP96, depth_m)$minimum) |> 
  mutate(salinity_gkg_SP96_backTEOS = gsw_SA_from_rho(density.SP96, ctd_temp_c, p = 1 + depth_m))

################################# SP24 #################################
ggplot(salinity.df2 |> filter(location_name == 'East Lake Bonney')) +
  geom_point(aes(x = salinity_gkg_SP96_back, y = salinity_gkg_SP96_backTEOS)) +
  geom_abline()

################################# PLOTS #################################

# Compare Derived Densities with SP 96
p.dens = ggplot(salinity.df2 |> 
                  filter(location_name %in% c('East Lake Bonney', 'West Lake Bonney'))) +
  geom_path(aes(x = density.CHEN, y = depth_m), col = 'red1') +
  geom_point(aes(x = density.CHEN, y = depth_m), col = 'red1') +
  geom_path(aes(x = density.UNESCO, y = depth_m), col = 'red3') +
  geom_point(aes(x = density.UNESCO, y = depth_m), col = 'red3') +
  geom_path(aes(x = density.GIBBS, y = depth_m), col = 'red4') +
  geom_point(aes(x = density.GIBBS, y = depth_m), col = 'red4') +
  geom_path(aes(x = density.SP96, y = depth_m), col = 'gold') +
  geom_point(aes(x = density.SP96, y = depth_m), col = 'gold') +
  # geom_path(aes(x = density.Tadj, y = depth_m), col = 'green3') +
  geom_point(aes(x = density.Tadj, y = depth_m), col = 'green3', size = 2) +
  # geom_path(aes(x = density_kgm3, y = depth_m), col = 'green1') +
  geom_point(aes(x = density_kgm3, y = depth_m), col = 'green1', size = 2) +
  scale_y_reverse() +
  xlab('Density (kg/m3)') + ylab('Depth (m)') +
  facet_wrap(~location_name, scales = 'free') +
  theme_bw(base_size = 9); p.dens

# Compare Derived salinities
p.sal = ggplot(salinity.df2 |> 
         filter(location_name %in% c('East Lake Bonney', 'West Lake Bonney'))) +
  geom_path(aes(x = salinity.mg_l, y = depth_m)) +
  geom_point(aes(x = salinity.mg_l, y = depth_m)) +
  geom_path(aes(x = salinity.UNESCO, y = depth_m), col = 'red3') +
  geom_point(aes(x = salinity.UNESCO, y = depth_m), col = 'red3') +
  geom_path(aes(x = salinity.SP96, y = depth_m), col = 'gold') +
  geom_point(aes(x = salinity.SP96, y = depth_m), col = 'gold') +
  geom_point(aes(x = salinity_fromDensity, y = depth_m), col = 'green3', size = 1.5) +
  geom_point(aes(x = salinity_gkg_SP96, y = depth_m), col = 'blue1', size = 1.5, shape = 15) +
  geom_point(aes(x = salinity_gkg_adj, y = depth_m), col = 'blue4', size = 1.5) +
  geom_point(aes(x = salinity_gkg_SP96_back, y = depth_m), col = 'purple', size = 1.5) +
  geom_point(aes(x = salinity_gkg_SP96_backTEOS, y = depth_m), col = 'purple', size = 1.5) +
  scale_y_reverse() +
  xlab('Salinity (g/kg)') + ylab('Depth (m)') +
  facet_wrap(~location_name, scales = 'free')  +
  theme_bw(base_size = 9); p.sal

# Save figures
p.cond / p.dens / p.sal
# ggsave('figures/densityComp.png', width = 5, height = 8, dpi = 500)
 
# Zoomed in
ggplot(salinity.df2 |> 
         filter(location_name %in% c('East Lake Bonney')) |> 
         filter(salinity.mg_l > 100)) +
  geom_path(aes(x = salinity.mg_l, y = depth_m)) +
  geom_point(aes(x = salinity.mg_l, y = depth_m)) +
  geom_path(aes(x = salinity.UNESCO, y = depth_m), col = 'red3') +
  geom_point(aes(x = salinity.UNESCO, y = depth_m), col = 'red3') +
  geom_path(aes(x = salinity.SP96, y = depth_m), col = 'gold') +
  geom_point(aes(x = salinity.SP96, y = depth_m), col = 'gold') +
  geom_point(aes(x = salinity_fromDensity, y = depth_m), col = 'green3', size = 1.5) +
  geom_point(aes(x = salinity_gkg_SP96, y = depth_m), col = 'blue1', size = 1.5, shape = 15) +
  geom_point(aes(x = salinity_gkg_adj, y = depth_m), col = 'blue4', size = 1.5) +
  geom_point(aes(x = salinity_gkg_SP96_back, y = depth_m), col = 'purple', size = 1.5) +
  scale_y_reverse() +
  xlab('Salinity (g/kg)') + ylab('Depth (m)') +
  facet_wrap(~location_name, scales = 'free')  +
  theme_bw(base_size = 9)

# Zoomed in
df.zoom = salinity.df2 |> 
  filter(location_name %in% c('East Lake Bonney', 'West Lake Bonney')) |> 
  filter(salinity.mg_l > 100) |> 
  select(location_name, depth_m, density.UNESCO, density.GIBBS, density.SP96, density.Tadj) |> 
  pivot_longer(cols = 3:6)

ggplot(df.zoom) +
  geom_path(data = df.zoom |> filter(name != 'density.Tadj'), aes(x = value, y = depth_m, col = name)) +
  geom_point(aes(x = value, y = depth_m, col = name)) +
  scale_color_manual(values = c('red4','gold','lightblue3','red3'), name = '') +
  scale_y_reverse() +
  xlab('Density (kg m<sup>-3</sup>)') + ylab('Depth (m)') +
  facet_wrap(~location_name, scales = 'free')  +
  theme_bw(base_size = 9) +
  theme(strip.background = element_rect(fill = "white", color = NA),  # Set background to white and remove border
        strip.text = element_text(color = "black", face = "bold", size = 10), 
        axis.title.x = element_markdown())
# theme(axis.title.x = element_markdown())

ggsave('figures/SI_DensityComp.png', width = 6, height = 2.5, dpi = 500, units = 'in')

# density anomaly from pure water at the same temperature and pressure
salinity.df2 |> 
  filter(location_name %in% c('East Lake Bonney')) |> 
  filter(salinity.mg_l > 100) |> 
  select(location_name, depth_m, density.SP96, density.Tadj) |> 
  filter(!is.na(density.Tadj)) |> 
  mutate(diff = 100*abs(density.Tadj - density.SP96)/ (density.Tadj - 1000))

salinity.df2 |> 
  filter(location_name %in% c('West Lake Bonney')) |> 
  filter(salinity.mg_l > 100) |> 
  select(location_name, depth_m, density.UNESCO, density.Tadj) |> 
  filter(!is.na(density.Tadj)) |> 
  mutate(diff = 100*abs(density.Tadj - density.UNESCO)/ (density.Tadj - 1000))
