#################### Create salinity transfer functions ###################
# Years are missing salinity data. This can be missing ions, DIC, or pH
# Use a 5 year window to subset salinity profiles
years = c(1995:2019) 
lakenames = c('West Lake Bonney', 'East Lake Bonney', 'Lake Fryxell','Lake Hoare')
total.pred = list()
for (l in lakenames) {
  df.elb.pred = list()
  for (i in 1:length(years)) {
    useyear = years[i]
    
    df.elb = salinity.df |> filter(location_name == l) |> 
      filter(year(date_time) >= useyear - 2 & year(date_time) <= useyear + 2) # create 5 year 
    
    m.elb <- gam(salinity/1000 ~ s(depth.asl, k = 20, m = 1), data = df.elb)
    df.elb.pred[[i]] <- data.frame(depth.asl = seq(floor(min(df.elb$depth.asl)), ceiling(max(df.elb$depth.asl)), by = 0.1)) %>%
      mutate(pred = predict(m.elb, .)) |> 
      mutate(location_name = l) |> 
      mutate(year = useyear)
  
  }
  total.pred[[l]] = bind_rows(df.elb.pred)
}

total.pred.df = as_tibble(bind_rows(total.pred)) |> 
  mutate(pred = as.numeric(pred))
write_csv(total.pred.df, 'dataout/salinityTransferTable.csv')

# Plot to check any wonkiness
ggplot(salinity.df) +
  geom_path(aes(x = salinity/1000, y = depth.asl, group = date_time, col = year(date_time))) +
  geom_point(aes(x = salinity/1000, y = depth.asl, group = date_time, col = year(date_time))) +
  geom_point(data = total.pred.df, aes(x = pred, depth.asl), col = 'red') +
  xlab('Salinity (g/L)') +
  facet_wrap(~location_name, scales = 'free')




# df.elb = salinity.df |> filter(location_name == 'East Lake Bonney')
# m.elb <- gam(salinity/1000 ~ s(depth.asl, k = 20), data = df.elb)
# df.elb.pred <- data.frame(depth.asl = seq(floor(min(df.elb$depth.asl)), ceiling(max(df.elb$depth.asl)), by = 0.5)) %>%
#   mutate(pred = predict(m.elb, .)) |> 
#   mutate(location_name = 'East Lake Bonney')
# 
# df.lf = salinity.df |> filter(location_name == 'Lake Fryxell')
# m.lf <- gam(salinity/1000 ~ s(depth.asl, k = 20), data = df.lf)
# df.lf.pred <- data.frame(depth.asl = seq(floor(min(df.lf$depth.asl)), ceiling(max(df.lf$depth.asl)), by = 0.5)) %>%
#   mutate(pred = predict(m.lf, .)) |> 
#   mutate(location_name = 'Lake Fryxell')
# 
# df.lh = salinity.df |> filter(location_name == 'Lake Hoare')
# m.lh <- gam(salinity/1000 ~ s(depth.asl, k = 20), data = df.lh)
# df.lh.pred <- data.frame(depth.asl = seq(floor(min(df.lh$depth.asl)), ceiling(max(df.lh$depth.asl)), by = 0.5)) %>%
#   mutate(pred = predict(m.lh, .)) |> 
#   mutate(location_name = 'Lake Hoare')
# 
# df.wlb = salinity.df |> filter(location_name == 'West Lake Bonney') |> 
#   filter(year(date_time) >= 2012)
# m.wlb <- gam(salinity/1000 ~ s(depth.asl, k = 20, m = 1), data = df.wlb)
# df.wlb.pred <- data.frame(depth.asl = seq(floor(min(df.wlb$depth.asl)), ceiling(max(df.wlb$depth.asl)), by = 0.5)) %>%
#   mutate(pred = predict(m.wlb, .)) |> 
#   mutate(location_name = 'West Lake Bonney')
# 
# df.pred.salinty = df.lh.pred |> bind_rows(df.lf.pred, df.elb.pred, df.wlb.pred) |> 
#   mutate(pred = as.numeric(pred))
