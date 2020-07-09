### Function that returns 10 k values corresponding to relevant rate constants in kinetic model ### 
# -----------------------------------
# form of k values:
# Col1 = k1
# Col2 = k1r
# Col3 = k2
# Col4 = k2r
# Col5 = k3
# Col6 = k3r
# Col7 = k4
# Col8 = k5
# Col9 = k6
# Col10 = k7
# -----------------------------------

k_set <- function(T) {
  # Emperical data for activation energy and Arrhenius pre-factor
  Arrhvals <- data.frame(matrix(c(13145, 9932, 19860, 14369, 6421, 9588, 0, 13045, 19760, 6321, 
                                3.0409*exp(10), 6.4527*exp(7), 4.3770*exp(15), 2.9961*exp(11), 8.3880*exp(5), 8.4223*exp(6), 1.3300*exp(-2), 1.4817*exp(9), 2.9362*exp(14), 2.1695*exp(4)), 
                                ncol = 2))
  colnames(Arrhvals) <- c("Activation energy", "Arrhenius pre-factor")
  
  # Pre-allocation of space for rate constant values 
  kvals <- data.frame(matrix(0, nrow = 1, ncol = 10))
  colnames(kvals) <- c("k1","k1r","k2","k2r","k3","k3r","k4","k5","k6","k7")
  
  for(i in 1:length(kvals)) {
    k <- k_gen(T, Arrhvals[i, 1], Arrhvals[i, 2])
    kvals[1, i] <- k
  }
  return(kvals)
}