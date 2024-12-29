# Define the salinity optimization function
findSalinity <- function(par, temp, density, depth) {
  densityUNESCO <- sw_dens(S = par, t = temp, p = 1 + (depth / 10), method = 'UNESCO')
  return(abs(density - densityUNESCO))
}

# Wrapper for optimization
optimizeSalinity <- function(temp, density, depth, lower = 0, upper = 300) {
  # Use optimize to find the optimal 'par' within the specified bounds
  result <- optimize(
    f = findSalinity, 
    interval = c(lower, upper), 
    temp = temp, 
    density = density, 
    depth = depth
  )
  return(result)
}
# 

# Define the salinity optimization function
findSalinity.Chen <- function(par, temp, density, depth) {
  densityChen <- sw_dens(S = par, t = temp, p = 1 + (depth / 10), method = 'Chen')
  return(abs(density - densityChen))
}

# Wrapper for optimization
optimizeSalinity.Chen <- function(temp, density, depth, lower = 0, upper = 2) {
  # Use optimize to find the optimal 'par' within the specified bounds
  result <- optimize(
    f = findSalinity.Chen, 
    interval = c(lower, upper), 
    temp = temp, 
    density = density, 
    depth = depth
  )
  return(result)
}

# Example usage
temp <- 0      # Temperature in °C
density <- 999.6  # Target density in kg/m³
depth <- 4    # Depth in meters

# Find the salinity value that minimizes the difference
optimizeSalinity(temp, density, depth)
optimizeSalinity.Chen(temp, density, depth)

sw_dens(0.1, t = 0, p = 1 + (4 / 10), method = 'UNESCO')
sw_dens(0.1, t = 0, p = 1 + (4 / 10), method = 'Chen')

findSalinity.Chen(1.9, 0, 999.6, 4)
findSalinity.Chen(0, 0, 999.6, 4)
