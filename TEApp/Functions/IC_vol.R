IC_vol <- function(TGvol, Lvol, Mvol) {
  # function IC_vol (Initial condition generator) takes in volume amounts (in mL)
  # of triglyceride, lye, and methanol. The function outputs the concentrations of 
  # the three mentioned species utilizing the helper function # IC_vol_help.R.
  
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
  b <- IC_vol_help(pL, MML, Lvol)
  c <- IC_vol_help(pM, MMM, Mvol)
  
  # Obtain total volume by summing second index of a-c
  total_vol <- (TGvol + Mvol)/1000
  
  # Concentrations of each species in a data frame [TG, L, MeOH]
  conc <- data.frame(matrix(c(0, a, 0, 0, b, 0, 0, c), ncol = 8))/total_vol
  
  # Return non-dimensionalized values in a dataframe by dividing by TG conc.
  Int_vals <- conc/conc[1,2]
  colnames(Int_vals) <- c("E", "TG", "DG", "MG", "OH", "G", "S", "M")
  return(Int_vals)
}