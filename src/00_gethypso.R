library(tidyverse)
library(patchwork)

# Priscu, J. and J. Schmok. 2016. McMurdo Dry Valleys Bathymetric Hypsographic Function Values ver 5. 
# Environmental Data Initiative. https://doi.org/10.6073/pasta/0f4e4aab180775bcd23c64108882fea0 (Accessed 2024-06-26).

# Data was collected by Jeffrey Schmok in November 1995 in order to prepare
# the Golder report on the bathymetry of Lakes Hoare, Fryxell, and Bonney. A 0.0
# m contour is based on a shoreline survey of the frozen moat, and therefore is
# closer to piezometric water level Â than to top surface of floating permanent
# ice. Deeper contours are based on piezometric water level. Thus volume
# calculations within the ice cover represent volumes of liquid water as if the
# ice was melted. Third order polynomial equations were fit to the area vs.
# depth data both from Schmok's report and from the digitized map data, using
# SigmaPlot's curve fitting routine. However, since the contour data are already
# a "best-fit" of measured depth data, and Â contour intervals are relatively
# small, it was decided to linearly interpolate the depth:area relationships for
# depths between the measured depth contours, at 0.5m intervals. Volume was
# calculated for each 0.5m increment as a truncated cone
# (V=(h/3)*(A1+A2+sqrt(A1*A2)).

# hypo_old = read_csv('datain/hypsometry_old.csv')
# 
# ggplot(hypo_old) +
#   geom_point(aes(x = cumvol_per, y = depth_m, col = lake)) +
#   scale_y_reverse()


# New hypsometry made by Maciek Obryk 

hypo_new = read_csv('datain/hypsometry_lidar.csv') |> 
  select(-Dataset) |> 
  group_by(lake) |> 
  mutate(area.lag = lag(Area_2D, default = 0), elev.lag = lag(Elevation_masl, default = 0)) |> 
  # mutate(vol_layer_m3 = ((Elevation_masl-elev.lag)/3)*(Area_2D + area.lag + (sqrt(Area_2D*area.lag)))) |> 
  mutate(vol_layer_m3 = Area_2D * 0.1) %>% 
  mutate(cum_vol_m3 = cumsum(vol_layer_m3)) |> 
  select(-area.lag, -elev.lag, -`Depth (0 = bottom of the lake)`, -Plane_Height, -VolumeLayer, -Volume, -lake) |> 
  mutate(location_name = case_when(lake == 'LF' ~ 'Lake Fryxell', 
                                   lake == 'LH' ~ 'Lake Hoare', 
                                   lake == 'ELB' ~ 'East Lake Bonney', 
                                   lake == 'WLB' ~ 'West Lake Bonney')) 
 
# ggplot(hypo_new) +
#   geom_point(aes(x = Volume, y = Elevation_masl, col = lake))
# 
# 
# p1 = ggplot(hypo_new) +
#   geom_point(aes(x = Area_2D, y = `Depth (0 = bottom of the lake)`, col = lake))
# 
# 
# p2 = ggplot(hypo_old) +
#   geom_point(aes(x = area_m2, y = depth_m, col = lake)) +
#   scale_y_reverse()
# 
# p1 / p2
