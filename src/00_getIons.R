# Load libraries
library(tidyverse)
library(lubridate)
library(zoo)
library(mgcv)
library(marelac)
library(wql)

# source lake lakels
source('src/00_getLakeLevels.R')
source('src/00_getDensity.R')
# source DIC function 
# source('https://raw.githubusercontent.com/hdugan/lake_DIC/master/lake_DIC.R')
source('src/functions/carbonate_speciation.R')

###### CTD Round #########
# Priscu, J. 2023. Conductivity, temperature, and depth (CTD) vertical profiles collected from lakes in the 
# McMurdo Dry Valleys, Antarctica (1993-2023, ongoing) ver 17. Environmental Data Initiative. 
# https://doi.org/10.6073/pasta/650871571843bde5e0db6fb52cf549a4 (Accessed 2024-06-25).
# Read in 2023 data (will be online soon)
ctd2023 = read_csv('datain/ctd_2023.csv') |> 
  mutate(date_time = as.Date(mdy_hm(date_time))) |> 
  mutate(depth_m = wire_corrected_depth_m)

inUrl1  <- "https://pasta.lternet.edu/package/data/eml/knb-lter-mcm/88/17/91474a205d3dd99cc794f8510d2d99c5" 
infile1 <- tempfile()
download.file(inUrl1,infile1,method="curl")

ctd.round <- read_csv(infile1) |> 
  mutate(date_time = as.Date(mdy_hm(date_time))) |> 
  bind_rows(ctd2023) |> 
  filter(location_name %in% c('West Lake Bonney', 'East Lake Bonney', 'Lake Fryxell','Lake Hoare')) |> 
  select(location_name, date_time, depth_m = wire_corrected_depth_m, ctd_temp_c, ctd_conductivity_mscm) |> 
  mutate(depth_m = round(depth_m, 1)) |> 
  mutate(depth.asl.char = as.character(depth_m)) |> 
  group_by(location_name, date_time, depth.asl.char) |> 
  summarise_all(first) |> 
  ungroup() |> 
  # mutate(depthdiff = abs(depth_1m - depth_m)) |> 
  group_by(location_name, date_time) |> 
  filter(n() >= 5)

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
ctd.round.fill = fill_and_interpolate(ctd.round)

ctd.dates = ctd.round.fill |> 
  mutate(datepre7 = date_time - 7, datepost7 = date_time + 7) |> 
  rename(dateCTD = date_time)

# Priscu, J., K. Welch, and W. Lyons. 2022. 
# Ion concentrations in discrete water column samples collected from lakes in the McMurdo Dry Valleys, 
# Antarctica (1991-2019, ongoing) ver 12. Environmental Data Initiative. 
# https://doi.org/10.6073/pasta/31f7354d1a05679eb3ce7c384c6e2b22 (Accessed 2024-06-26).
ions_2022 = read_csv('datain/limno_IC2022-23.csv') 

inUrl1  <- "https://pasta.lternet.edu/package/data/eml/knb-lter-mcm/62/12/4c4202afce78edbff1f889e462407beb" 
infile1 <- tempfile()
download.file(inUrl1,infile1,method="curl")

ions <- read_csv(infile1) |> 
  mutate(date_time = as.Date(mdy_hm(date_time))) |> 
  filter(location_name %in% c('West Lake Bonney', 'East Lake Bonney', 'Lake Fryxell','Lake Hoare')) |> 
  bind_rows(ions_2022)

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
  filter(!is.na(bicarbonate_mgkg))

dic.carbonate |> select(dic_mg_cl, ph, ctd_temp_c, bicarbonate_mgkg, carbonate_mgkg)
# dic.carbonate |> select(dic_mg_cl, ph, ctd_temp_c, bicarbonate_mgCkg, carbonate_mgCkg)

dic.carbonate.mol = dic.join |> 
  mutate(carbonate = carbonate_speciation(TEMP = ctd_temp_c, DIC = dic_mg_cl, pH = ph.approx, output = "mol"))  %>%
  unnest_wider(carbonate) |> 
  filter(!is.na(bicarbonate_molkg)) |> 
  select(location_name, date_time, depth_m, dic_mg_cl, ph, ctd_temp_c, bicarbonate_molkg, carbonate_molkg)

# combine 
ions.df = ions |> select(-contains('comments'), -dataset_code, -limno_run, -filename) |> 
  select(location_name:depth_m, depth_masl, everything()) |> 
  rowwise() |> 
  filter(!is.na(cl_mgl)) |>  # dominant anion
  filter(!is.na(na_mgl)) |>  # dominant cation
  mutate(cation_sum = sum(li_mgl, na_mgl, k_mgl, mg_mgl, ca_mgl, na.rm = T)) |> 
  mutate(anion_sum = sum(cl_mgl, br_mgl, so4_mgl,f_mgl, na.rm = T)) |> # remove silica
  mutate(cation_sumMeq = sum(li_mm, na_mm, k_mm, mg_mm*2, ca_mm*2, na.rm = T)) |> 
  mutate(anion_sumMeq = sum(cl_mm, br_mm, so4_mm*2, f_mm, na.rm = T)) |> # remove silica
  mutate(CB = 100*(cation_sumMeq - anion_sumMeq) / (cation_sumMeq + anion_sumMeq)) |> 
  select(location_name:depth_masl, cation_sum, anion_sum, cation_sumMeq, anion_sumMeq, CB) |> 
  filter(!(location_name == 'West Lake Bonney' & date_time == as.Date('1998-11-11') & depth_m == 30))

# ionic strength check 
ions.CB = ions.df |> inner_join(dic.carbonate.mol, by = join_by(location_name, date_time, depth_m)) |> 
  mutate(anion_sumMeq2 = sum(anion_sumMeq,bicarbonate_molkg*1000, carbonate_molkg*2*1000, na.rm = T)) |> # remove silica
  mutate(CB = 100*(cation_sumMeq - anion_sumMeq) / (cation_sumMeq + anion_sumMeq)) |> 
  mutate(CB2 = 100*(cation_sumMeq - anion_sumMeq2) / (cation_sumMeq + anion_sumMeq2)) 

ggplot(ions.CB |> filter(CB2 > -50)) +
  geom_histogram(aes(x = CB), binwidth = 2, color = 'black') +
  geom_histogram(aes(x = CB2), fill = 'red', binwidth = 2, color = 'black') +
  facet_wrap(~location_name, scales = 'free') +
  xlab('Charge Balance (%)')

# Full join of data

full.df = ions.df |> full_join(dic.carbonate |> select(location_name:ctd_conductivity_mscm, co2_mgkg:alkalinity_uEqkg), 
                               by = join_by(location_name, date_time, depth_m))

salinity.df = full.df |> 
  select(location_name:depth_m, ctd_temp_c, ctd_conductivity_mscm, cation_sum, anion_sum, bicarbonate_mgkg, carbonate_mgkg) |> 
  filter(!is.na(bicarbonate_mgkg)) |> 
  filter(!is.na(ctd_conductivity_mscm)) |> 
  filter(!is.na(cation_sum)) |> 
  mutate(sum_ions = cation_sum + anion_sum) |> 
  mutate(density_freshwater = sw_dens(S = 0, t = ctd_temp_c, p = 1 + (depth_m/10)), method = 'Gibbs') |> 
  mutate(salinity = sum(cation_sum, anion_sum, bicarbonate_mgkg, carbonate_mgkg)/1000) |> 
  mutate(salinity_gkg = salinity/(salinity+density_freshwater)*1000) |> 
  mutate(density = sw_dens(S = salinity, t = ctd_temp_c,p = 1 + (depth_m/10)), method = 'Gibbs') |> 
  mutate(density2 = sw_dens(S = salinity_gkg, t = ctd_temp_c,p = 1 + (depth_m/10)), method = 'Gibbs') |> 
  mutate(specCond = ctd_conductivity_mscm/(1 + 0.020*(ctd_temp_c - 5))) |> 
  # Compare to UNESCO equations
  mutate(salinity.UNESCO2 = convert_RtoS(R = ctd_conductivity_mscm/42.914, t = ctd_temp_c, P = 1 + (depth_m/10))) |> # same as equation below (checked)
  mutate(salinity.UNESCO = ec2pss(ctd_conductivity_mscm, t = ctd_temp_c, p = 1 + depth_m)) |> 
  mutate(density.CHEN = sw_dens(S = salinity.UNESCO, t = ctd_temp_c, p = 1 + (depth_m/10), method = 'Chen')) |>
  mutate(density.UNESCO = sw_dens(S = salinity.UNESCO, t = ctd_temp_c, p = 1 + (depth_m/10), method = 'UNESCO')) |>
  mutate(density.GIBBS = sw_dens(S = salinity.UNESCO, t = ctd_temp_c, p = 1 + (depth_m/10), method = 'Gibbs')) |>
  left_join(ll.interp |> select(-masl), by = join_by(location_name, date_time)) |> 
  mutate(depth.asl = masl.approx - depth_m) |> 
  # add back in misssing rows 
  full_join(full.df |> select(location_name, date_time, depth_m, ctd_temp_c, ctd_conductivity_mscm))

# Get salinity range for manuscript
salinity.df |> group_by(location_name) |> summarise(min(salinity, na.rm = T), max(salinity, na.rm = T))

# # ggplot(salinity.df) +
# #   geom_point(aes(x = salinity.UNESCO, y = salinity.UNESCO2)) +
# #   facet_wrap(~location_name, scales = 'free')
# 
# # C(35, 15, 0) is the conductivity of standard seawater (35‰) at 15 °C which by
# # definition has a conductivity equal to that of the standard KCl solution at
# # that temperature and its value is 4.2914 Siemens/meter or 42.914 mS/cm
# # Density calculated here using Gibbs function
# # Feistel R, 2008. A Gibbs function for seawater thermodynamics for -6 to 80 dgC and salinity up to 120 g/kg. Deep-Sea Research I, 55, 1639-1671.
# 
# # Add in density equations from Spigel and Priscu 1996: Evolution of temperature and salt structure of Lake Bonney, a chemically stratified Antarctic lake
# # East Lake Bonney 
# # where p = density (kg m -3), PUN = density (kg m_3 ) predicted by unmodified UNESCO equations at specified S, t, p (see appendix), a = 133.36, b = 15 .299,
# # and S = practical salinity predicted by the UNESCO equations for specified C, t, p.
# alpha.ELB = 15.299
# beta.ELB = 133.36
# # beta.ELB = 315
# # density - density.UNESCO = alpha.ELB(salinity.UNESCO - 42)/[beta.ELB - (salinity.UNESCO-42)], S>42
# # West Lake Bonney
# alpha.WLB = 0.13696
# 
# salinity.df = salinity.df |> 
#   mutate(salinity.SP96 = if_else(salinity.UNESCO>66, 
#                                  (((-0.0000002832456*salinity.UNESCO)-0.0008991763)*salinity.UNESCO+1.0609)*salinity.UNESCO,
#                                  salinity.UNESCO)) |> 
#   mutate(density.SP96 = if_else(location_name == 'East Lake Bonney' & salinity.SP96 > 42, 
#                                 (alpha.ELB*(salinity.SP96 - 42)/(beta.ELB-(salinity.SP96-42))) + density.UNESCO, 
#                                 density.UNESCO)) |> 
#   mutate(density.SP96 = if_else(location_name == 'West Lake Bonney' & salinity.SP96 > 42, (alpha.WLB*(salinity.SP96 - 42)) + density.UNESCO, 
#                                 density.SP96))

         