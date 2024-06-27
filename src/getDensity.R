
# Density measurements taken by Limno Team winter 2022

density.df = read_csv('datain/density_2022.csv') |> 
  mutate(location_name = case_when(lake == 'LF' ~ 'Lake Fryxell', 
                                 lake == 'LH' ~ 'Lake Hoare', 
                                 lake == 'ELB' ~ 'East Lake Bonney', 
                                 lake == 'WLB' ~ 'West Lake Bonney')) 
