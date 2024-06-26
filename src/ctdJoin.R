

# Join CTD to hypsometry by lake name and elevation/depth masl 
ctd.join = ctd |> select(date_time, location_name, masl.approx, depth.asl, depth_m, ctd_conductivity_mscm, ctd_temp_c) |> 
  mutate(depth.asl = round(depth.asl, 1)) |> 
  left_join(hypo_new |> rename(depth.asl = Elevation_masl) |>  mutate(depth.asl = round(depth.asl, 1)), 
            by = c('location_name', 'depth.asl'))


hypo_new |> filter(location_name == 'East Lake Bonney') |> arrange(desc(Elevation_masl)) |> 
  filter(Elevation_masl < 63)
