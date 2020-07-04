#### Function Explanation ####
# finds the rate of change of concentration for each species
# form of concentration dataframe: 
# Col1 = Ester E
# Col2 = Triglyceride TG
# Col3 = Diglyceride DG
# Col4 = Monoglyceride MG
# Col5 = Alcohol ROH
# Col6 = Water W
# Col7 = Glycerol G
# Col8 = Soap S
# form of k values
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
#### Derivative Function ####
dC <- function(k, C, sf) {
  ##
  # insert equations for reaction rates
  # i.e. r1 = k[1,1]*C[1,1]*C[1,3] - k[1,2]*C[1,2]*C[1,4]
  ##
  derivC <- data.frame()
  ##
  # insert calculation of derivatives
  # i.e. derivC[1,1] = r1 - r2 + r3
  ##
  return(derivC)
}