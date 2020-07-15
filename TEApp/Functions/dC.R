#### Function Explanation ####
# finds the rate of change of concentration for each species
# -----------------------------------
# form of concentration dataframe: 
# Col1 = Ester E
# Col2 = Triglyceride TG
# Col3 = Diglyceride DG
# Col4 = Monoglyceride MG
# Col5 = Alcohol ROH
# Col6 = Glycerol G
# Col7 = Soap S
# Col8 = Sodium Hydroxide OH-
# -----------------------------------
# form of k values:
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
# -----------------------------------
#### Derivative Function ####
dC <- function(k, C, sf) {
  #### Reaction Rates  ####
  # TE of TG
  r1 <- sf*C[1,8] * (k[1,1]*C[1,2]*C[1,5] - k[1,2]*C[1,3]*C[1,1])
  # TE of DG
  r2 <- sf*C[1,8] * (k[1,3]*C[1,3]*C[1,5] - k[1,4]*C[1,4]*C[1,1])
  # TE of MG
  r3 <- sf*C[1,8] * (k[1,5]*C[1,4]*C[1,5] - k[1,6]*C[1,6]*C[1,1])
  # sap of E
  r4 <- sf*C[1,8] * k[1,7]*C[1,1]
  # sap of TG
  r5 <- sf*C[1,8] * k[1,8]*C[1,2]
  # sap of DG
  r6 <- sf*C[1,8] * k[1,9]*C[1,3]
  # sap of MG
  r7 <- sf*C[1,8] * k[1,10]*C[1,4]
  #### Calculation of Derivatives ####
  derivC <- data.frame(matrix(0,nrow = 1, ncol = 8))
  # dE/dt
  derivC[1,1] <- r1 + r2 + r3 - r4
  # dTG/dt
  derivC[1,2] <- -1*r1 - r5
  # dDG/dt
  derivC[1,3] <- r1 - r2 + r5 - r6
  # dMG/dt
  derivC[1,4] <- r2 - r3 + r6 - r7
  # dROH/dt
  derivC[1,5] <- r4 - r1 - r2 - r3
  # dG/dt
  derivC[1,6] <- r3 + r7
  # dS/dt
  derivC[1,7] <- r4 + r5 + r6 + r7
  # dOH/dt
  derivC[1,8] <- -1*(r4 + r5 + r6 + r7)
  # return dataframe of derivatives
  return(derivC)
}