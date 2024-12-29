# Load libraries
library(tidyverse)
library(lubridate)
library(mgcv)

# source raw data
source('src/00_getIons.R')

############ Cond - Salinity transfer function ###############
ggplot(salinity.df) + 
  geom_point(aes(x = specCond, y = salinity/1000)) +
  facet_wrap(~location_name, scales = 'free')

df.2 = salinity.df |> select(location_name, depth.asl, depth_m:ctd_conductivity_mscm, specCond, salinity) |> 
  filter(salinity/1000 <= 180) |> 
  mutate(location_name = factor(location_name, levels = c('Lake Fryxell','Lake Hoare', 'East Lake Bonney', 'West Lake Bonney')))


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
  mutate(pred = as.numeric(pred)) |> 
  mutate(location_name = factor(location_name, levels = c('Lake Fryxell','Lake Hoare', 'East Lake Bonney', 'West Lake Bonney')))
write_csv(total.pred.df, 'dataout/condSalTransfer.csv')

# Plot to check any wonkiness
ggplot(df.2) +
  # geom_path(aes(x = specCond, y = salinity/1000)) +
  geom_point(aes(x = specCond, y = salinity/1000, fill = location_name), shape = 21, stroke = 0.2) +
  geom_point(data = total.pred.df, aes(x = specCond, pred), col = 'grey30', size = 0.6) +
  scale_fill_manual(values = c('#4477c9', '#e3dc10', '#b34f0c', '#4c944a'), name = 'Lake') +
  xlab('SpecCond (mS cm^-1 )') +
  ylab('Salinity (g L^-1 )') +
  theme_bw(base_size = 9) +
  facet_wrap(~location_name, scales = 'free', nrow = 1) +
  theme(legend.position = 'none', 
        axis.title.x = element_markdown(),
        axis.title.y = element_markdown())
  # labs(caption = 'Salinities > 180 for ELB removed') 

ggsave('figures/SI_SpC_Salinity.png', width = 6, height = 2, dpi = 500)
 

