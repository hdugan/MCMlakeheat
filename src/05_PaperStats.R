library(synchrony)

# Impact on melting ice (adding water) versus warming water column based on Lake Fryxell 
a = hypo.join |> filter(location_name == 'Lake Fryxell' & date_time == as.Date('2023-11-13'))
# current heat content
a |> 
  summarise(ice.approx = mean(ice.approx, na.rm = T), heat_J = sum(heat_J, na.rm = T), heatIce_J = sum(heatIce_J, na.rm = T), 
            Area_2D = first(Area_2D)) |> 
  mutate(heatTot_J_m2 = (heat_J - heatIce_J)/Area_2D/1e6)

# if you melted a layer of ice
a |> 
  mutate(tempUse = if_else(depth.asl >= 14.7 & depth.asl <= 15.7, 0, tempUse)) |>
  mutate(iceDensity_kgm3 = if_else(depth.asl >= 14.7 & depth.asl <= 15.7, NA, iceDensity_kgm3)) |>
  mutate(temp_FPD = tempUse - FPD) %>% # set baseline temperature to -5°C
  mutate(spHeat_J_m3K = spHeat_J_kgK * density_kg_m3) %>% 
  # latent heat of ice = density * thickness *  latent heat of ice (334000 J/kg)
  mutate(LHice_J_m3 = iceDensity_kgm3 * 334000) |> 
  mutate(heatIce_J = LHice_J_m3 * vol_layer_m3) |> 
  mutate(heat_J = spHeat_J_m3K * vol_layer_m3 * temp_FPD) %>% 
  mutate(heat_J_m2 = heat_J/Area_2D) |> 
  summarise(ice.approx = mean(ice.approx, na.rm = T), heat_J = sum(heat_J, na.rm = T), heatIce_J = sum(heatIce_J, na.rm = T), 
            Area_2D = first(Area_2D)) |> 
  mutate(heatTot_J_m2 = (heat_J - heatIce_J)/Area_2D/1e6)

# If you warmed the water column 2°C
a |> 
  mutate(tempUse = tempUse + 2) |>
  # mutate(tempUse = if_else(depth.asl >= 14.7 & depth.asl <= 15.7, 0, tempUse)) |>
  mutate(temp_FPD = tempUse - FPD) %>% # set baseline temperature to -5°C
  mutate(spHeat_J_m3K = spHeat_J_kgK * density_kg_m3) %>% 
  # latent heat of ice = density * thickness *  latent heat of ice (334000 J/kg)
  mutate(LHice_J_m3 = iceDensity_kgm3 * 334000) |> 
  mutate(heatIce_J = LHice_J_m3 * vol_layer_m3) |> 
  mutate(heat_J = spHeat_J_m3K * vol_layer_m3 * temp_FPD) %>% 
  mutate(heat_J_m2 = heat_J/Area_2D) |> 
  summarise(ice.approx = mean(ice.approx, na.rm = T), heat_J = sum(heat_J, na.rm = T), heatIce_J = sum(heatIce_J, na.rm = T), 
            Area_2D = first(Area_2D)) |> 
  mutate(heatTot_J_m2 = (heat_J - heatIce_J)/Area_2D/1e6)


############ Synchrony for Temperature ###########
sync1 = data.frame(LF_temp = c(output.predict.dec[[1]]$fit.temp), 
                   LH_temp = c(output.predict.dec[[2]]$fit.temp,NA), 
                   ELB_temp = output.predict.dec[[3]]$fit.temp, 
                   WLB_temp = output.predict.dec[[4]]$fit.temp)

options(digits=2)
correlation_table <- corrr::correlate(sync1, method = "pearson")
correlation_table
cor_2 <- rcorr(as.matrix(sync1[,-1]), type = 'pearson')
cor_2

#### Synchrony package

# Pair wise synchrony for temperature 
sync1.out = data.frame(pair = c('LF-LH','LF-ELB','LF-WLB','LH-ELB','LH-WLB','ELB-WLB'), 
                       meanCorr = NA, meanCorr.p = NA,
                       Concur = NA, Concur.p = NA,
                       Phase = NA, Phase.p = NA)

## Compute the concordance (and its statistical significance) between multiple variable
kendall.w(data = sync1, nrands = 999)
kendall.w(data = sync1[-3], nrands = 999)

col1 = c(1,1,1,2,2,3)
col2 = c(2,3,4,3,4,4)
for (i in 1:6) {
  ts1 = sync1[,col1[i]]
  ts2 = sync1[,col2[i]]
  
  ts1.na = !is.na(ts1)
  ts2.na = !is.na(ts2)
  na.inx = ts1.na & ts2.na
  
  ts1 = ts1[na.inx]
  ts2 = ts2[na.inx]
  
  #Correlation
  sync1.out$meanCorr[i] = rcorr(ts1, ts2)$r[1,2]
  sync1.out$meanCorr.p[i] = round(rcorr(ts1, ts2)$P[1,2],2)
  
  # (implemented in function peaks), which simply measures the proportion of concurrent peaks (local maxima) and
  # troughs (local minima) between pairs of time series (Buonaccorsi et al. 2001)
  # This metric varies between 0 when the time series never peak and trough together, and 1 when the time
  # series always peak and trough simultaneously
  concurence = peaks(ts1, ts2, nrands = 999)
  sync1.out$Concur[i] = concurence$obs
  sync1.out$Concur.p[i] = concurence$pval
  
  # phase.sync, measures phase synchrony between quasiperiodic times series
  # The strength of phase synchrony can be quantified by a Q index that falls between 0 (no phase synchrony) 
  # and 1 (full phase synchrony) 
  # Function to safely handle phase sync in cases where it fails 
  safe_phase.sync <- function(ts1, ts2) {
    tryCatch({
      phase.sync(ts1, ts2, mins = TRUE, nrands = 999)
    }, error = function(e) {
      if (grepl("need at least two non-NA values to interpolate", e$message)) {
        # return(NA) # Return NA for specific error
        return(data.frame(Q.obs = NA, pval = NA))
      } else {
        stop(e) # Rethrow other errors
      }
    })
  }
  phase = safe_phase.sync(ts1,ts2)
  # phase = phase.sync(ts1, ts2, mins = TRUE, nrands = 999)
  sync1.out$Phase[i] = phase$Q.obs
  sync1.out$Phase.p[i] = phase$pval
}

latexTable(sync1.out, usecols = 5)

############ Synchrony for Ice Thickness ###########
sync2 = data.frame(LF_temp = c(output.predict.dec[[1]]$fit.ice), 
                   LH_temp = c(output.predict.dec[[2]]$fit.ice,NA), 
                   ELB_temp = output.predict.dec[[3]]$fit.ice, 
                   WLB_temp = output.predict.dec[[4]]$fit.ice)

options(digits=2)
correlation_table <- corrr::correlate(sync2, method = "pearson")
correlation_table
cor_2 <- rcorr(as.matrix(sync2[,-1]), type = 'pearson')
cor_2

#### Synchrony package

# Pair wise synchrony for temperature 
sync2.out = data.frame(pair = c('LF-LH','LF-ELB','LF-WLB','LH-ELB','LH-WLB','ELB-WLB'), 
                       meanCorr = NA, meanCorr.p = NA,
                       Concur = NA, Concur.p = NA,
                       Phase = NA, Phase.p = NA)

## Compute the concordance (and its statistical significance) between multiple variable
kendall.w(data = sync2, nrands = 999)

col1 = c(1,1,1,2,2,3)
col2 = c(2,3,4,3,4,4)
for (i in 1:6) {
  ts1 = sync2[,col1[i]]
  ts2 = sync2[,col2[i]]
  
  ts1.na = !is.na(ts1)
  ts2.na = !is.na(ts2)
  na.inx = ts1.na & ts2.na
  
  ts1 = ts1[na.inx]
  ts2 = ts2[na.inx]
  
  #Correlation
  sync2.out$meanCorr[i] = rcorr(ts1, ts2)$r[1,2]
  sync2.out$meanCorr.p[i] = round(rcorr(ts1, ts2)$P[1,2],2)
  
  # (implemented in function peaks), which simply measures the proportion of concurrent peaks (local maxima) and
  # troughs (local minima) between pairs of time series (Buonaccorsi et al. 2001)
  # This metric varies between 0 when the time series never peak and trough together, and 1 when the time
  # series always peak and trough simultaneously
  concurence = peaks(ts1, ts2, nrands = 999)
  sync2.out$Concur[i] = concurence$obs
  sync2.out$Concur.p[i] = concurence$pval
  
  # phase.sync, measures phase synchrony between quasiperiodic times series
  # The strength of phase synchrony can be quantified by a Q index that falls between 0 (no phase synchrony) 
  # and 1 (full phase synchrony) 
  phase = phase.sync(ts1, ts2, mins = TRUE, nrands = 999)
  sync2.out$Phase[i] = phase$Q.obs
  sync2.out$Phase.p[i] = phase$pval
}

latexTable(sync2.out, usecols = 5)


########################## Autocorrelation in timeseries ##############################
for (i in 1:4) {
  uselake = lakecolor$uselake[i]
  nrows = length(interp.out[[i]]$temp.diff[-1])
  print(acf(interp.out[[i]]$temp.diff[-1], main = uselake, ci = 0.95, na.action = na.pass))
  # Compute confidence interval 
  # Use 95 or 99% confidence? 
  ci = qnorm((1 + 0.95)/2)/sqrt(nrows)
  
  sig = acf(interp.out[[i]]$temp.diff[-1], plot = F, na.action = na.pass)$acf[2] > ci
  print(paste0(uselake, ' acf temp.diff: ', sig))
}

for (i in 1:4) {
  uselake = lakecolor$uselake[i]
  nrows = length(interp.out[[i]]$ice.diff[-1])
  print(acf(interp.out[[i]]$ice.diff[-1], main = uselake, ci = 0.95, na.action = na.pass))
  # Compute confidence interval 
  # Use 95 or 99% confidence? 
  ci = qnorm((1 + 0.95)/2)/sqrt(nrows)
  
  sig = acf(interp.out[[i]]$ice.diff[-1], plot = F, na.action = na.pass)$acf[2] > ci
  print(paste0(uselake, ' acf ice.diff: ', sig))
}

for (i in 1:4) {
  uselake = lakecolor$uselake[i]
  nrows = length(interp.out[[i]]$LL.diff[-1])
  print(acf(interp.out[[i]]$LL.diff[-1], main = uselake, ci = 0.95, na.action = na.pass))
  # Compute confidence interval 
  # Use 95 or 99% confidence? 
  ci = qnorm((1 + 0.95)/2)/sqrt(nrows)
  
  sig = acf(interp.out[[i]]$LL.diff[-1], plot = F, na.action = na.pass)$acf[2] > ci
  print(paste0(uselake, ' acf ll.diff: ', sig))
}

################################ Variable-lag Granger Causality ################################
for (i in 1:4) {
  print(i)
  print(VLTimeCausality::VLGrangerFunc(Y = interp.out[[i]]$temp, X = interp.out[[i]]$iceZ, gamma = 0.5)$XgCsY)
  print(VLTimeCausality::VLGrangerFunc(Y = interp.out[[i]]$temp, X = interp.out[[i]]$LL, gamma = 0.5)$XgCsY)
  print(VLTimeCausality::VLGrangerFunc(Y = interp.out[[i]]$temp, X = interp.out[[i]]$ice.diff, gamma = 0.5)$XgCsY)
  print(VLTimeCausality::VLGrangerFunc(Y = interp.out[[i]]$temp, X = interp.out[[i]]$LL.diff, gamma = 0.5)$XgCsY)
  
}

for (i in 1:4) {
  print(i)
  print(VLTimeCausality::VLGrangerFunc(Y = interp.out[[i]]$temp, X = interp.out[[i]]$iceZ, gamma = 0.5)$XgCsY)
  print(VLTimeCausality::VLGrangerFunc(Y = interp.out[[i]]$temp, X = lead(interp.out[[i]]$iceZ), gamma = 0.5)$XgCsY)
}

for (i in 1:4) {
  print(i)
  print(VLTimeCausality::VLGrangerFunc(Y = interp.out[[i]]$temp.diff, X = interp.out[[i]]$iceZ, gamma = 0.5)$XgCsY)
  print(VLTimeCausality::VLGrangerFunc(Y = interp.out[[i]]$temp.diff, X = lead(interp.out[[i]]$iceZ), gamma = 0.5)$XgCsY)
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

# Density
df.sal |> group_by(location_name) |> summarise(min(density_kg_m3, na.rm = T), max(density_kg_m3, na.rm = T))

# Heat J/m3
hypo.join |> group_by(location_name) |> 
  mutate(location_name = factor(location_name, levels = c('Lake Fryxell','Lake Hoare', 'East Lake Bonney', 'West Lake Bonney'))) |> 
  summarise(minHeat3 = min(heat_J_m3/1e6, na.rm = T), maxHeat3 = max(heat_J_m3/1e6, na.rm = T))

heat.day |> group_by(location_name) |> 
  mutate(location_name = factor(location_name, levels = c('Lake Fryxell','Lake Hoare', 'East Lake Bonney', 'West Lake Bonney'))) |> 
  summarise( maxHeat2 = max(heatTot_J_m2/1e6, na.rm = T), minHeat2 = min(heatTot_J_m2/1e6, na.rm = T),
             minIce = min(-heatIce_J/Area_2D/1e6, na.rm = T), maxIce = max(-heatIce_J/Area_2D/1e6, na.rm = T),
             minWC = min(heat_J/Area_2D/1e6, na.rm = T), maxWC = max(heat_J/Area_2D/1e6, na.rm = T))


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
  

