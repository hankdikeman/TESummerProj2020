IC_wt_help <- function(p, MM, wt) {
  # Calculate moles
  n <- wt/MM
  
  # Calculate volume 
  vol <- wt/p
  
  # return in dataframe format 
  out <- data.frame(matrix(c(n, vol), ncol = 2))
  colnames(out) <- c("mol", "vol")
  
  return(out)
}