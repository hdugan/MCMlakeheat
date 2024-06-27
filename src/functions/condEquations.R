library(marelac)


sw_dens(S = 300, t = 4, p = 1, method = 'Gibbs')

# empirical model of Sen and Goode
# https://library.seg.org/doi/epdf/10.1190/1.1443191
# molality in mol/kg


# Conductivity in S/m
# 1 S/m = 10 mS/cm
condF <- function(temp, molality) {
  (5.6 + 0.27*temp - 1.5e-4*temp^2)*molality - (molality^(3/2)*(2.36 + 0.099*temp)/(1 + 0.214*(molality^0.5)))
}


molality = 104.626/58.44
condF(-1.72, molality) * 10


# Create the inverse
# Define the function representing the equation
equation <- function(molality, temp, cond) {
  term1 <- (5.6 + 0.27 * temp - 1.5e-4 * temp^2) * molality
  term2 <- molality^(3/2) * (2.36 + 0.099 * temp) / (1 + 0.214 * sqrt(molality))
  return(term1 - term2 - cond)
}

# Parameters
temp <- -1.7  # temperature in degrees Celsius
cond <- 71.6/10  # conductivity value

# Wrapper function for uniroot
equation_wrapper <- function(molality) {
  equation(molality, temp, cond)
}

# Solve for molality
# Use uniroot to find the solution
solution <- uniroot(equation_wrapper, lower = 1e-10, upper = 10)

# Print the molality solution
solution$root
solution$root*58.44 # in g/L

salinity.df |> select(ctd_temp_c, ctd_conductivity_mscm,salinity,density) |> 
  rename(temp = ctd_temp_c, cond = ctd_conductivity_mscm) |> 
  rowwise() |>
  mutate(derivedMolality = map(temp, cond, function(x) {
    as.data.frame(uniroot(equation_wrapper,lower = 0, upper = 10))
  })) %>%
  unnest()

