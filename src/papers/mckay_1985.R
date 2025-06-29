# Thickness of ice on perennially frozen lakes
# McKay 1985

# Equation [1]
# kdT/dz = S(z) + L + Fg 
# k = thermal conductivity of ice
# dT/dz = gradient of annual mean temperature (T) with depth (z) in the ice
# S(z) = annual mean flux of solar energy absorbed below z
# Fg = geothermal heat flux
# L = heat flux due to latent heat release at the ice-water interface

# L = vpl 
# v is the rate of formation of new ice averaged over the entire year, equal to the ablation rate, # m year-1
# p is the density of the ice and # kg/m3
# l is the latent heat of fusion of water # J/kg

# To solve equation [1] for the temperature of the ice cover as a function of depth
# S(z) = (1-a)(1-r)So exp(-z/h) 
# a is the albedo of the lake
# r is the fraction of the lake that is covered by dark absorbing material such as sand and silt, 
# So is the solar radiation incident on the lake surface and the exponential term gives attenuation
# with depth into the ice with an extinction path length of h (defined to be the cosine of the effective solar zenith angle
#   divided by the extinction coefficient). 

# The thermal conductivity of ice is approximated by 
# k = b/T-c 
# where b and c constants taken to be 780 W/m and 0.615 W/m/K respectively. 

# Making these substitutions into equation (1) and integrating from the ice-water interface to the surface gives 

# Equation [2]
Z = (b*ln(T0/Ts) + c*(Ts-T0) - S0*(1-a)*(1-r)*h*(1-exp(-Z/h))) / (v*p*l + Fg)

# T0 is the temperature of the ice-water interface
# Ts is the yearly averaged temperature of the surface, both in K
# Z is the equilibrium thickness of the ice cover. 

# Equation (2) is an implicit expression for the average yearly thickness of the ice
# cover. In the limit that the ice cover is much thicker than the extinction path length (Z» h) 
# and in the approximation that the thermal conductivity is independent of temperature (k =
# constant) and the geothermal flux is small, this equation reduces to

Z = (k*(T0-Ts) - S0*(1-a)*(1-r)*h) / (v*p*l/ 3.154e+7) # 3.154e+7 = seconds in a year

a = 0.6
Ts = -20 
T0 = 0
h = 1
Fg = 0.08 #Wm2
S0 = 104 #Wm2
r = 0.1 #%
v = 0.3 #m per year  ablation rate
p = 915 #kg.m3  
l = 3.36e5 #J kg−1 
# where ρi (=915 kg m−3) is ice density and Lf (=0.33 × 106 J kg−1) is the latent heat of freezing for water.
k = 2.3 # Wm−1 °C−1 

# output = data.frame(r = seq(0,1,0.1), Z = NA)
# for (i in 1:11) {
#   r = output$r[i]
#   output$Z[i] = (k*(T0-Ts) - S0*(1-a)*(1-r)*h) / (v*p*l/ 3.154e+7) # Convert J (W/S) to watts per year 
# }
#   
# ggplot(output) + 
#   geom_point(aes(x = r, y = Z))


output = data.frame(v = seq(0.1,1,0.1), Z.1 = NA, Z.5 = NA)
for (i in 1:10) {
  v = output$v[i]
  output$Z.1[i] = (k*(T0-Ts) - S0*(1-a)*(1-r)*h) / (v*p*l/ 3.154e+7) # Convert J (W/S) to watts per year 
  output$Z.5[i] = (k*(T0-Ts) - S0*(1-a)*(1-r)*0.5) / (v*p*l/ 3.154e+7) # Convert J (W/S) to watts per year 
}

ggplot(output) + 
  geom_path(aes(x = v, y = Z.1)) +
  geom_point(aes(x = v, y = Z.1)) +
  geom_path(aes(x = v, y = Z.5)) +
  geom_point(aes(x = v, y = Z.5)) +
  geom_point(aes(x = 0.20, y = 4.5), color = 'red3') +
  geom_point(aes(x = 0.80, y = 2), color = 'red3') +
  scale_y_continuous(breaks = 1:10, limits = c(0,10)) +
  scale_x_continuous(breaks = seq(0.1,1,0.1))

