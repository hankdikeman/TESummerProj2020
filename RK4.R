#### Function Explanation ####
# integrates concentration using Runge-Kutta 4 method
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
#### RK4 Function ####
RK4 <- function(k, C0, time, dt, sf) {
  # determine number of steps and concentration dataframe
  t_steps <- (time / dt) %/% 1 + 1
  C <- C0
  # preallocate space
  C[2:t_steps,] <- 0
  # integrates using dC derivative function and RK4
  for(i in 2:t_steps) {
    # runs dC function and computes next conc step
    r1 <- dC(k, C[i-1,], sf)
    r2 <- dC(k, C[i-1,] + r1*dt/2, sf)
    r3 <- dC(k, C[i-1,] + r2*dt/2, sf)
    r4 <- dC(k, C[i-1,] + r3*dt, sf)
    C[i,] <- C[i-1,]+ dt/6 * (r1 + 2*r2 + 2*r3 + r4)
    ##
    # insert check functions here to check that concentrations match matl bal equations within error;
    # throw error from this function otherwise
    ##
  }
  return(C)
}