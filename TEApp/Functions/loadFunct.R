loadfunct <- function(){
  # initial condition calculation functions
  source("./Functions/IC_vol.R")
  source("./Functions/IC_vol_help.R")
  source("./Functions/IC_wt.R")
  source("./Functions/IC_wt_help.R")
  # integration functions
  source("./Functions/RK4.R")
  source("./Functions/dC.R")
  # k calculation functions
  source("./Functions/k_gen.R")
  source("./Functions/k_set.R")
}