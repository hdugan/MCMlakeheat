# Impact on melting ice (adding water) versus warming water column based on Lake Fryxell 

a = df.full.ice |> filter(location_name == 'Lake Fryxell' & date_time == as.Date('2023-11-13'))
# current heat content
a |> 
  summarise(ice.approx = mean(ice.approx, na.rm = T), heat_J = sum(heat_J, na.rm = T), heatIce_J = sum(heatIce_J, na.rm = T), 
            Area_2D = first(Area_2D)) |> 
  mutate(heatTot_J_m2 = (heat_J - heatIce_J)/Area_2D/1e6)

# if you melted a layer of ice
a |> 
  mutate(tempUse = if_else(depth.asl >= 14.7 & depth.asl <= 15.7, 0, tempUse)) |>
  mutate(iceDensity_kgm3 = if_else(depth.asl >= 14.7 & depth.asl <= 15.7, NA, iceDensity_kgm3)) |>
  
  mutate(temp_K = tempUse + 273.15) %>%
  mutate(spHeat_J_m3K = spHeat_J_kgK * density_kg_m3) %>% 
  # latent heat of ice = density * thickness *  latent heat of ice (334000 J/kg)
  mutate(LHice_J_m3 = iceDensity_kgm3 * 334000) |> 
  mutate(heatIce_J = LHice_J_m3 * vol_layer_m3) |> 
  mutate(heat_J = spHeat_J_m3K * vol_layer_m3 * temp_K) %>% 
  mutate(heat_J_m2 = heat_J/Area_2D) |> 
  summarise(ice.approx = mean(ice.approx, na.rm = T), heat_J = sum(heat_J, na.rm = T), heatIce_J = sum(heatIce_J, na.rm = T), 
            Area_2D = first(Area_2D)) |> 
  mutate(heatTot_J_m2 = (heat_J - heatIce_J)/Area_2D/1e6)

# If you warmed the water column 5°C
a |> 
  mutate(tempUse = tempUse + 5) |>
  # mutate(tempUse = if_else(depth.asl >= 14.7 & depth.asl <= 15.7, 0, tempUse)) |>
  mutate(temp_K = tempUse + 273.15) %>%
  mutate(spHeat_J_m3K = spHeat_J_kgK * density_kg_m3) %>% 
  # latent heat of ice = density * thickness *  latent heat of ice (334000 J/kg)
  mutate(LHice_J_m3 = iceDensity_kgm3 * 334000) |> 
  mutate(heatIce_J = LHice_J_m3 * vol_layer_m3) |> 
  mutate(heat_J = spHeat_J_m3K * vol_layer_m3 * temp_K) %>% 
  mutate(heat_J_m2 = heat_J/Area_2D) |> 
  summarise(ice.approx = mean(ice.approx, na.rm = T), heat_J = sum(heat_J, na.rm = T), heatIce_J = sum(heatIce_J, na.rm = T), 
            Area_2D = first(Area_2D)) |> 
  mutate(heatTot_J_m2 = (heat_J - heatIce_J)/Area_2D/1e6)

