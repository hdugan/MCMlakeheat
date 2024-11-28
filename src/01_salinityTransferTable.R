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

ggplot(salinity.df |> dplyr::filter(location_name == 'East Lake Bonney')) +
  # geom_path(aes(x = salinity/1000, y = depth.asl, group = date_time, col = year(date_time))) +
  geom_point(aes(x = salinity/1000, y = depth.asl, group = date_time, col = year(date_time))) +
  geom_point(data = total.pred.df |> filter(location_name == 'East Lake Bonney'), aes(x = pred, depth.asl), col = 'red', size = 0.2) +
  xlab('Salinity (g/L)') +
  xlim(180, NA)
