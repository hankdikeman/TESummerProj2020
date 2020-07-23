## table of kvals and arrVals for rxn
gen_table_vals <- function(kvalues, arrVals){
  kvals <- data.frame(matrix(0, nrow = 1, ncol = 10))
  colnames(kvals) <- c("k1","k1r","k2","k2r","k3","k3r","k4","k5","k6","k7")
  ##maybe use cbind 
  for(i in 1:length(kvals)) {
    kvals[i,] <- (kvalues[i])
    kvals
  }
  return (kvals)
}