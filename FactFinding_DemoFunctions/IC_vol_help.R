IC_vol_help <- function(p, MM, vol) {
  # Calculate moles (divide volume by 1000 to get into units of Liters)
  n <- ((vol/1000)*p)/MM

  return(n)
}