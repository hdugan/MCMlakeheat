# SW_SpcHeat    Specific heat at constant pressure of seawater
# Source: https://web.mit.edu/seawater/
# =========================================================================
# USAGE:  cp = SW_SpcHeat(T,uT,S,uS,P,uP)
#
# DESCRIPTION:
#   Specific heat capacity of seawater at 0.1 MPa given by [1]
#   Calculation for non-atmospheric pressures (P_sat < P < 12 MPa)
#   given in [2]
#
# INPUT:
#   T  = temperature
#   uT = temperature unit
#        'C'  : [degree Celsius] (ITS-90)
#        'K'  : [Kelvin]
#        'F'  : [degree Fahrenheit]
#        'R'  : [Rankine]
#   S  = salinity
#   uS = salinity unit
#        'ppt': [g/kg]  (reference-composition salinity)
#        'ppm': [mg/kg] (in parts per million)
#        'w'  : [kg/kg] (mass fraction)
#        '#'  : [kg/kg] (in parts per hundred)
#   P  = pressure
#   uP = pressure unit
#        'MPa': [MPa]
#        'bar': [bar]
#        'kPa': [kPa]
#        'Pa' : [Pa]
#
#   Note: T, S and P must have the same dimensions 
#
# OUTPUT:
#   cp = specific heat [J/kg-K]
#
#   Note: cp will have the same dimensions as T, S and P
#
# VALIDITY: (1) 0 < T < 180 C; 0 < S < 180 g/kg; 0.1 < P = P0 < 1 MPa
#           (2) 0 < T < 180 C; S = 0 g/kg; 0 < P < 12 MPa
#           (3) 0 < T < 40 C; 0 < S < 42 g/kg; 0 < P < 12 MPa
#           (4) 40 < T < 180 C; 0 < S < 42 g/kg; 0 < P < 12 MPa
#           (5) 0 < T < 180 C; 42 < S < 180 g/kg; 0 < P < 12 MPa

# ACCURACY: (1) 1#
#           (2) 1#
#           (3) 1#
#           (4) 1# (Extrapolated)
#           (5) 1# (Extrapolated)
#
# REVISION HISTORY:
#   2009-12-18: Mostafa H. Sharqawy (mhamed@mit.edu), MIT
#               - Initial version
#   2012-06-06: Karan H. Mistry (mistry@alum.mit.edu), MIT
#               - Allow T,S input in various units
#               - Allow T,S to be matrices of any size
#   2015-04-15: Kishor G. Nayar (kgnayar@mit.edu), MIT
#               - Extended function to 12 MPa; corrected salinity to
#                 "Reference salinity" from "Salinity Product"
#   2016-04-10: Karan H. Mistry (mistry@alum.mit.edu), MIT
#               - Allow T,S to be matrices of any size
#
# DISCLAIMER:
#   This software is provided "as is" without warranty of any kind.
#   See the file sw_copy.m for conditions of use and licence.
#
# REFERENCES:
# [1] D. T. Jamieson, J. S. Tudhope, R. Morris, and G. Cartwright,
#      Desalination, 7(1), 23-30, 1969.
# [2]  K.G. Nayar, M. H. Sharqawy, L.D. Banchik and J. H. Lienhard V, Desalination,
#       390, 1-24, 2016. (http://web.mit.edu/seawater/) 
#=========================================================================

SW_SpcHeat <- function(Temp, uT = 'C', S, uS = 'ppt', P, uP = 'bar') {
  # Constants
  P0 <- 0.101325  # Reference pressure in MPa
  
  # Check that S, T & P have the same dimensions
  if (!identical(dim(S), dim(Temp)) || !identical(dim(Temp), dim(P))) {
    stop("S, Temp & P must have same dimensions")
  }
  
  # Convert temperature input to °C
  switch(tolower(uT),
         "c" = {},  # Already in °C
         "k" = { Temp <- Temp - 273.15 },
         "f" = { Temp <- 5/9 * (Temp - 32) },
         "r" = { Temp <- 5/9 * (Temp - 491.67) },
         stop("Not a recognized temperature unit. Please use 'C', 'K', 'F', or 'R'")
  )
  
  # Convert salinity to ppt
  switch(tolower(uS),
         "ppt" = {},  # Already in ppt
         "ppm" = { S <- S / 1000 },
         "w" = { S <- S * 1000 },
         "%" = { S <- S * 10 },
         stop("Not a recognized salinity unit. Please use 'ppt', 'ppm', 'w', or '%'")
  )
  
  # Convert pressure input to MPa
  switch(tolower(uP),
         "mpa" = {},  # Already in MPa
         "bar" = { P <- P / 10 },
         "kpa" = { P <- P / 1000 },
         "pa" = { P <- P / 1E6 },
         stop("Not a recognized pressure unit. Please use 'MPa', 'bar', 'kPa', or 'Pa'")
  )
  
  # Check that S, Temp & P are within the function range
  if (any(Temp < 0 | Temp > 180)) {
    warning("Temperature is out of range for specific heat function 0<T<180 C")
  }
  
  if (any(S < 0 | S > 180)) {
    warning("Salinity is out of range for specific heat function 0<S<180 g/kg")
  }
  
  Psat <- SW_Psat(Temp, 'C', S, 'ppt') / 1E6
  
  if (any(P < Psat | Psat > 12)) {
    warning("Pressure is out of range for specific heat function P_sat < P < 12 MPa")
  }
  
  # Begin calculation
  
  # Convert Temp to T_68 (from T_90)
  T68 <- 1.00024 * (Temp + 273.15)
  
  S_gkg <- S
  
  A <- 5.328 - 9.76e-2 * S + 4.04e-4 * S^2
  B <- -6.913e-3 + 7.351e-4 * S - 3.15e-6 * S^2
  C <- 9.6e-6 - 1.927e-6 * S + 8.23e-9 * S^2
  D <- 2.5e-9 + 1.666e-9 * S - 7.125e-12 * S^2
  
  cp_sw_P0 <- 1000 * (A + B * T68 + C * T68^2 + D * T68^3)
  
  # Pressure dependent terms
  c1 <- -3.1118
  c2 <- 0.0157
  c3 <- 5.1014e-5
  c4 <- -1.0302e-6
  c5 <- 0.0107
  c6 <- -3.9716e-5
  c7 <- 3.2088e-8
  c8 <- 1.0119e-9
  
  cp_sw_P <- (P - P0) * (c1 + c2 * Temp + c3 * Temp^2 + c4 * Temp^3 + S_gkg * (c5 + c6 * Temp + c7 * Temp^2 + c8 * Temp^3))
  
  cp <- cp_sw_P0 + cp_sw_P
  
  return(cp)
}

#####################################################

# SW_Psat    Saturation (vapor) pressure of seawater
#=========================================================================
# USAGE:  Pv = SW_Psat(T,uT,S,uS)
#
# DESCRIPTION:
  #   Vapor pressure of natural seawater given by [1] based on new correlation
#   The pure water vapor pressure is given by [2]
#
# INPUT:
#   T  = temperature
#   uT = temperature unit
#        'C'  : [degree Celsius] (ITS-90)
#        'K'  : [Kelvin]
#        'F'  : [degree Fahrenheit]
#        'R'  : [Rankine]
#   S  = salinity
#   uS = salinity unit
#        'ppt': [g/kg]  (reference-composition salinity)
#        'ppm': [mg/kg] (in parts per million)
#        'w'  : [kg/kg] (mass fraction)
#        '#'  : [kg/kg] (in parts per hundred)
#
#   Note: T and S must have the same dimensions
#
# OUTPUT:
#   Pv = vapor pressure [N/m^2]
#
#   Note: Pv will have the same dimensions as T and S
#
# VALIDITY: (1) 20 < T < 180 C; 0 < S < 160 g/kg
#           (2)  0 < T <  20 C; 0 < S < 160 g/kg
#
# ACCURACY: (1) 0.26#
#           (2) 0.91#
#
# REVISION HISTORY:
#   2009-12-18: Mostafa H. Sharqawy (mhamed@mit.edu), MIT
#               - Initial version based on correlation from [3]
#   2012-06-06: Karan H. Mistry (mistry@alum.mit.edu), MIT
#               - Allow T,S input in various units
#               - Allow T,S to be matrices of any size
#   2014-07-30: Kishor G. Nayar (kgnayar@mit.edu), MIT
#               - Revised version with new correlation
#
# DISCLAIMER:
#   This software is provided "as is" without warranty of any kind.
#   See the file sw_copy.m for conditions of use and licence.
#
# REFERENCES:
#   [1] K.G. Nayar, M. H. Sharqawy, L.D. Banchik and J. H. Lienhard V, Desalination,
#       390, 1-24, 2016. (http://web.mit.edu/seawater/) 
#   [2]  ASHRAE handbook: Fundamentals, ASHRAE; 2005.
#   [3] M. H. Sharqawy, J. H. Lienhard V, and S. M. Zubair, Desalination
#       and Water Treatment, 16, 354-380, 2010. (http://web.mit.edu/seawater/)

# Function to calculate saturation vapor pressure of seawater
SW_Psat <- function(Temp, uT, S, uS) {
  
  # Convert temperature to Celsius if necessary
  if (uT == "K") {
    T_C <- Temp - 273.15
  } else if (uT == "F") {
    T_C <- (Temp - 32) * 5/9
  } else if (uT == "R") {
    T_C <- (Temp - 491.67) * 5/9
  } else {
    T_C <- Temp  # Assume Celsius if not specified or already in Celsius
  }
  
  # Convert salinity to g/kg if necessary
  if (uS == "ppt") {
    S_gkg <- S
  } else if (uS == "ppm") {
    S_gkg <- S / 1000
  } else if (uS == "w" || uS == "%") {
    S_gkg <- S * 1000
  } else {
    stop("Unknown salinity unit. Valid units: ppt, ppm, w, %")
  }
  
  # Check that S, Temp & P are within the function range
  if (any(Temp < 0 | Temp > 180)) {
    warning("Temperature is out of range for specific heat function 0<T<180 C")
  }
  
  if (any(S < 0 | S > 160)) {
    warning("Salinity is out of range for specific heat function 0<S<160 g/kg")
  }
  
  # Given temperature T in Celsius
  Temp <- Temp + 273.15  # Convert temperature to Kelvin
  
  # Coefficients for Pv_w calculation
  a <- c(-5.8002206E+03, 1.3914993E+00, -4.8640239E-02,
         4.1764768E-05, -1.4452093E-08, 6.5459673E+00)
  
  # Calculate Pv_w
  Pv_w <- exp((a[1] / Temp) + a[2] + a[3] * Temp + a[4] * Temp^2 + a[5] * Temp^3 + a[6] * log(Temp))
  
  # Coefficients for Pv calculation
  b <- c(-4.5818 * 10^-4, -2.0443 * 10^-6)
  
  # Given salinity S
  # Calculate Pv
  Pv <- Pv_w * exp(b[1] * S + b[2] * S^2)
  
  return(Pv)
}



