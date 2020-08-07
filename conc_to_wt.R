# function to convert timepoint concentration to weight amounts in grams 
conc_to_wt <- function(conc_df,TG, L, M, is_wt, mm_TG = NULL, mm_DG = NULL, mm_MG = NULL, mm_S = NULL, mm_E = NULL) {
  # conc_df = time point concentration data frame
  # TG, L, M initial reactant amounts can be either in wt or volume 
  # methyl ester 
  # triglyceride 
  # diglyceride
  # monoglyceride
  # methanol
  # glycerol
  # soap - salt of a fatty acid 
  # sodium hydroxide
  
  
  # Molar masses of each species (g/mol)
  mmE <- 300 # Needs to be entered 
  mmTG <- 885.4 # Also
  mmDG <- 665 # Also
  mmMG <- 445 # Also
  mmM <- 32.04
  mmG <- 92.09
  mmS <- 250 # Also
  mmL <- 39.997
  
  if (is_wt == 1) {
    # If original reactant amounts were given in grams 
    # Run helper function IC_wt_help.R to obtain moles and volume of each reactant species
    # Used to obtain volume (assumption of constant volume)
    a <- IC_wt_help(pTG, MMTG, TG)
    b <- IC_wt_help(pL, MML, L)
    c <- IC_wt_help(pM, MMM, M)
    
    # copies data frame format but appends original conc df to be moles 
    mol_df <- conc_df*(a[1,2]+b[1,2]+c[1,2])
    
    #Divide each species by its molar mass separately to obtain weight values (wt_df)
    wt_df[1,1] <- mol_df[1,1]/mmE
    wt_df[1,2] <- mol_df[1,2]/mmTG
    wt_df[1,3] <- mol_df[1,3]/mmDG
    wt_df[1,4] <- mol_df[1,4]/mmMG
    wt_df[1,5] <- mol_df[1,5]/mmM
    wt_df[1,6] <- mol_df[1,6]/mmG
    wt_df[1,7] <- mol_df[1,7]/mmS
    wt_df[1,8] <- mol_df[1,8]/mmL
    
    return(wt_df)
  }
  else {
    # else reactant values were given in volume 
    mol_df <- conc_df*(TG + L + M)
    
    #Divide each species by its molar mass separately to obtain weight values (wt_df)
    wt_df[1,1] <- mol_df[1,1]/mmE
    wt_df[1,2] <- mol_df[1,2]/mmTG
    wt_df[1,3] <- mol_df[1,3]/mmDG
    wt_df[1,4] <- mol_df[1,4]/mmMG
    wt_df[1,5] <- mol_df[1,5]/mmM
    wt_df[1,6] <- mol_df[1,6]/mmG
    wt_df[1,7] <- mol_df[1,7]/mmS
    wt_df[1,8] <- mol_df[1,8]/mmL
    
    return(wt_df)
  }
}
