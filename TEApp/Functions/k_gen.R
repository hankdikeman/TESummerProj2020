### Function to generate k values (rate constant values) using the Arrhenius equation ####
# -----------------------------------
# k = A*exp(-Ea/R*T)
# -----------------------------------
### Arrhenius Function ###
k_gen <- function(T, Ea, A) {
  R <- 1.987 
  # Universal Gas COnstant in units of [cal mol^-1 K^-1]
  return(A*exp(-Ea/(R*T)))
  # return k value with given temperature, activation energy and pre-factor
}