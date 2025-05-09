
########################## Autocorrelation in timeseries ##############################
for (i in 1:4) {
  uselake = lakecolor$uselake[i]
  nrows = length(annual.list[[i]]$flux[-1])
  print(acf(annual.list[[i]]$flux[-1], main = uselake, ci = 0.95, na.action = na.pass))
  # Compute confidence interval 
  # Use 95 or 99% confidence? 
  ci = qnorm((1 + 0.95)/2)/sqrt(nrows)
  
  sig = acf(annual.list[[i]]$flux[-1], plot = F, na.action = na.pass)$acf[2] > ci
  print(paste0(uselake, ' acf flux: ', sig))
}

for (i in 1:4) {
  uselake = lakecolor$uselake[i]
  nrows = length(annual.list[[i]]$ice.diff[-1])
  print(acf(annual.list[[i]]$ice.diff[-1], main = uselake, ci = 0.95, na.action = na.pass))
  # Compute confidence interval 
  # Use 95 or 99% confidence? 
  ci = qnorm((1 + 0.95)/2)/sqrt(nrows)
  
  sig = acf(annual.list[[i]]$ice.diff[-1], plot = F, na.action = na.pass)$acf[2] > ci
  print(paste0(uselake, ' acf ice.diff: ', sig))
}

for (i in 1:4) {
  uselake = lakecolor$uselake[i]
  nrows = length(annual.list[[i]]$LL.diff[-1])
  print(acf(annual.list[[i]]$LL.diff[-1], main = uselake, ci = 0.95, na.action = na.pass))
  # Compute confidence interval 
  # Use 95 or 99% confidence? 
  ci = qnorm((1 + 0.95)/2)/sqrt(nrows)
  
  sig = acf(annual.list[[i]]$LL.diff[-1], plot = F, na.action = na.pass)$acf[2] > ci
  print(paste0(uselake, ' acf ll.diff: ', sig))
}

################################ Variable-lag Granger Causality ################################
for (i in 1:4) {
  print(i)
  print(VLTimeCausality::VLGrangerFunc(Y = annual.list[[i]]$temp, X = annual.list[[i]]$iceZ, gamma = 0.5)$XgCsY)
  print(VLTimeCausality::VLGrangerFunc(Y = annual.list[[i]]$temp, X = annual.list[[i]]$LL, gamma = 0.5)$XgCsY)
  print(VLTimeCausality::VLGrangerFunc(Y = annual.list[[i]]$temp, X = annual.list[[i]]$ice.diff, gamma = 0.5)$XgCsY)
  print(VLTimeCausality::VLGrangerFunc(Y = annual.list[[i]]$temp, X = annual.list[[i]]$LL.diff, gamma = 0.5)$XgCsY)
  
}

for (i in 1:4) {
  print(i)
  print(VLTimeCausality::VLGrangerFunc(Y = annual.list[[i]]$temp, X = annual.list[[i]]$iceZ, gamma = 0.5)$XgCsY)
  print(VLTimeCausality::VLGrangerFunc(Y = annual.list[[i]]$temp, X = lead(annual.list[[i]]$iceZ), gamma = 0.5)$XgCsY)
}

for (i in 1:4) {
  print(i)
  print(VLTimeCausality::VLGrangerFunc(Y = annual.list[[i]]$temp.diff, X = annual.list[[i]]$iceZ, gamma = 0.5)$XgCsY)
  print(VLTimeCausality::VLGrangerFunc(Y = annual.list[[i]]$temp.diff, X = lead(annual.list[[i]]$iceZ), gamma = 0.5)$XgCsY)
}

###################### Table 1 ############################
# Check ranges for manuscript 
ctd.join |> 
  group_by(location_name) |> 
  mutate(location_name = factor(location_name, levels = c('Lake Fryxell','Lake Hoare', 'East Lake Bonney', 'West Lake Bonney'))) |> 
  summarise(minT = min(ctd_temp_c, na.rm = T), maxT = max(ctd_temp_c, na.rm = T),
            minC = min(ctd_conductivity_mscm, na.rm = T), maxC = max(ctd_conductivity_mscm, na.rm = T))

df.full.ice |> 
  group_by(location_name) |> 
  mutate(location_name = factor(location_name, levels = c('Lake Fryxell','Lake Hoare', 'East Lake Bonney', 'West Lake Bonney'))) |> 
  summarise(minT = min(tempUse, na.rm = T), maxT = max(tempUse, na.rm = T),
            minC = min(condUse, na.rm = T), maxC = max(condUse, na.rm = T))

# salinity (g/kg) - except using g/ml in manuscipt 
df.sal |>   
  group_by(location_name) |> 
  mutate(location_name = factor(location_name, levels = c('Lake Fryxell','Lake Hoare', 'East Lake Bonney', 'West Lake Bonney'))) |> 
  summarise(minS = min(salinity_g_kg, na.rm = T), maxS = max(salinity_g_kg, na.rm = T))

# spHeat
df.spcH |> group_by(location_name) |> 
  mutate(location_name = factor(location_name, levels = c('Lake Fryxell','Lake Hoare', 'East Lake Bonney', 'West Lake Bonney'))) |> 
  summarise(min(spHeat_J_kgK, na.rm = T), max(spHeat_J_kgK, na.rm = T))

firstprofile |> group_by(location_name) |> 
  mutate(location_name = factor(location_name, levels = c('Lake Fryxell','Lake Hoare', 'East Lake Bonney', 'West Lake Bonney'))) |> 
  summarise(min(spHeat_J_kgK, na.rm = T), max(spHeat_J_kgK, na.rm = T))

# Density
df.sal |> group_by(location_name) |> summarise(min(density_kg_m3, na.rm = T), max(density_kg_m3, na.rm = T))

# Heat Flux
heat_flux |> group_by(location_name) |> 
  mutate(location_name = factor(location_name, levels = c('Lake Fryxell','Lake Hoare', 'East Lake Bonney', 'West Lake Bonney'))) |> 
  summarise(minHeat3 = min(flux_W_m2, na.rm = T), maxHeat3 = max(flux_W_m2, na.rm = T))

###################### FLOOD YEAR LAKE LEVEL RISE ############################
ll |> filter(year(date_time) %in% c(2001,2002)) |> 
  arrange(location_name) |> 
  group_by(location_name) |> 
  summarise(minLL = min(masl), maxLL = max(masl)) |> 
  mutate(diff = maxLL - minLL)

###################### % water level ############################ 
# calculate a more robust total heat content of the lake in order to
# compare heat storage through time. For all lakes, we instituted a bottom
# elevation cutoff so profiles ended at the same depth [Lake Fryxell = 2.5 masl,
# Lake Hoare = 48 masl, ELB and WLB = 25 masl].
cutoff = data.frame(lake = c('LF','LH','ELB','WLB'),
           cutoff = c(2.5, 48, 25, 25))

max.ll = ll |> group_by(location_name) |> summarise(masl = max(masl))

hypo_new |> left_join(max.ll) |> 
  filter(Elevation_masl < masl) |> 
  left_join(cutoff) |> 
  group_by(location_name) |> 
  mutate(perVol = 100*cum_vol_m3/max(cum_vol_m3)) |> 
  filter(Elevation_masl < cutoff) |> 
  summarise(max(perVol))
  

