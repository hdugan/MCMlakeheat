# Load libraries
library(tidyverse)
library(lubridate)
library(mgcv)

# source lake lakels
source('src/00_getIons.R')

############ Cond - Salinity transfer function ###############
ggplot(salinity.df) + 
  geom_point(aes(x = specCond, y = salinity)) +
  facet_wrap(~location_name, scales = 'free')

df.2 = salinity.df |> select(location_name, depth.asl, depth_m:ctd_conductivity_mscm, specCond, salinity) |> 
  filter(salinity/1000 <= 180)

lakenames = c('West Lake Bonney', 'East Lake Bonney', 'Lake Fryxell','Lake Hoare')
k.by = c(3,5,3,3)
use.by = c(0.1,0.1,0.1,0.01)
total.pred = list()
for (l in 1:4) {
    name = lakenames[l]
    df.lake = df.2 |> filter(location_name == name)
    
    m.lake <- gam(salinity/1000 ~ s(specCond, k = k.by[l]), data = df.lake)
    
    total.pred[[l]] <- data.frame(specCond = seq(0, ceiling(max(df.lake$specCond, na.rm = T)), by = use.by[l])) %>%
      mutate(pred = predict(m.lake, .)) |> 
      mutate(location_name = name) 
}

total.pred.df = as_tibble(bind_rows(total.pred)) |> 
  mutate(pred = as.numeric(pred))
write_csv(total.pred.df, 'dataout/condSalTransfer.csv')

# Plot to check any wonkiness
ggplot(df.2) +
  # geom_path(aes(x = specCond, y = salinity/1000)) +
  geom_point(aes(x = specCond, y = salinity/1000)) +
  geom_point(data = total.pred.df, aes(x = specCond, pred), col = 'red') +
  xlab('SpecCond (mS/cm)') +
  facet_wrap(~location_name, scales = 'free')

