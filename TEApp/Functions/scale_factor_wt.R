scale_factor_wt <- function(TGwt, Lwt, Mwt) {
  # function takes reactant weights inputs and returns TG concentration (scale factor)
  
  # Densitys of each species (g/L)
  # Density values were collected for conditions of 25 degrees celcius and 1 bar 
  pTG <- 0.93*1000 # value for general vegetable oil off google 
  pL <- 2.13*1000
  pM <- 0.791*1000
  
  # Molar masses of each species (g/mol)
  MMTG <- 885.4 # average value 
  MML <- 39.997
  MMM <- 32.04
  
  # Run helper function IC_wt_help.R to obtain moles and volume of each species
  a <- IC_wt_help(pTG, MMTG, TGwt)
  b <- IC_wt_help(pL, MML, Lwt)
  c <- IC_wt_help(pM, MMM, Mwt)
  
  # Obtain total volume by summing second index of a-c
  total_vol <- a[1,2] + c[1,2]
  
  # Return concentration of TG
  return(a[1,1]/total_vol)
}