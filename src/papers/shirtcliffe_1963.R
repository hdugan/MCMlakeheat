# A sun-heated antarctic lake
# T. G. L. Shirtcliffe, R. F. Benseman
# https://agupubs.onlinelibrary.wiley.com/doi/epdf/10.1029/JZ069i016p03355

shirtcliffe_bonney_1963 <- data.frame(
  Depth_m = c(4.6, 6.1, 7.6, 9.2, 10.7, 12.2, 13.7, 15.2, 16.8, 18.3, 19.8, 21.4, 22.9, 24.4, 25.9, 27.4, 29.0, 30.5),
  Density_g_ml = c(1.0014, 1.0022, 1.0062, 1.0096, 1.0195, 1.0508, 1.0885, 1.1266, 1.1510, 1.1660, 1.1751, 1.1801, 1.1846, 1.1868, 1.1888, 1.1895, 1.1982, 1.1985),
  Temperature_C = c(1.2, 3.0, 4.9, 6.2, 7.0, 7.4, 7.4, 7.1, 6.4, 5.0, 4.6, 3.6, 2.6, 1.5, 0.6, 0.0, -1.0, -2.0),
  Electrical_Conductivity_ohm_cm_X10_3 = c(3.40, 4.84, 11.75, 16.6, 29.8, 52.8, 66.4, 64.6, 64.6, 66.4, 68.4, 68.4, 68.4, 68.4, 68.4, 68.4, 68.4, 68.4),
  Chloride_Content_ppm = c(800, 1300, 4320, 7360, 16680, 44750, 78000, 119000, 138000, 151000, 164000, 175000, 180000, 180000, 180000, 180000, 180000, 180000)
)

# Q = incident radiation, 80,000 cal/cm2/year, 1.5% of this under ice 
# 0 = temperature.
# t = time
# z = depth (positive downward).
# K = thermal diffusivity.
# k = thermal conductivity, 0.0012 cgsunits as a reasonablevalue for the conductivity of saline water at these temperatures
# A = rate of heat production per unit volume
# n = n/is the extinction coefficient or reciprocalof the absorption length, absorption length of 8.2 m

# (1) 0.08 +- 0.04°C m-1 for the geothermal gradient, 
# correspondingto a flux (upward) of 130 +- 60 cal cm-2 yr-1

gettheta <- function(Q0, k, n, z, zm) {
  (100*Q0/k/(86400*365))*(1/n - z*exp(-n*zm) - (1/n)*exp(-n*z))
}

df = data.frame(z = 0:30) |> 
  mutate(theta = gettheta(Q0 = 80000*0.015, k = 0.0013, n = 1/8.2, z = z, zm = 13-4))
df

ggplot(df) +
  geom_vline(aes(xintercept = 0), linetype = 2) +
  geom_path(aes(x = theta, y = z+4)) +
  geom_point(aes(x = theta, y = z+4)) +
  geom_point(data = shirtcliffe_bonney_1963, aes(x = Temperature_C, y = Depth_m), fill = 'red2', shape = 21) +
  scale_y_reverse() +
  theme_bw()

