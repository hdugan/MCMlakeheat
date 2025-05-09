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


######### Old code for salinity ##########
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