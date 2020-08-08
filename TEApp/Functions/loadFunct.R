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
  # graphing functions
  source("./Functions/totalConcPlot.R")
  source("./Functions/progConcBar.R")
  source("./Functions/pieYield.R")
  source("./Functions/CatActivity.R")
  source("./Functions/relativeRates.R")
  source("./Functions/emulRisk.R")
  # Scale factor functions
  source("./Functions/scale_factor_wt.R")
  source("./Functions/scale_factor_vol.R")
}