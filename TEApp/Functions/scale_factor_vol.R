scale_factor_vol <- function(TGvol, Lvol, Mvol) {
  # function takes reactant volume inputs and returns TG concentration (scale factor)
  
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
  a <- IC_vol_help(pTG, MMTG, TGvol)
  
  # Obtain total volume by summing second index of a-c
  total_vol <- (TGvol + Mvol)/1000
  
  # Returns concentration of TG
  return(a/total_vol)
}