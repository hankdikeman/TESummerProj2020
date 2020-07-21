RK4Var <- function(k, C0, time_lim, sf) {
  # determine number of steps and concentration dataframe
  t_step_var <- 0.0001
  print(paste("t_step_var = ",t_step_var))
  t_total <- 0
  C <- C0
  step_num <- 0
  # preallocate space
  C[2:((time_lim %/% 1)*5),] <- 0
  # making time dataframe
  time_trace <- data.frame(matrix(0, nrow = nrow(C), ncol = 1))
  colnames(time_trace) <- c("minutes")

  # integrates using dC derivative function and RK4
  while(t_total <= time_lim) {
    # increment step number
    step_num <- step_num + 1
    # runs dC function and computes next conc step
    r1 <- dC(k, C[step_num,], sf)
    r2 <- dC(k, C[step_num,] + r1*t_step_var/2, sf)
    r3 <- dC(k, C[step_num,] + r2*t_step_var/2, sf)
    r4 <- dC(k, C[step_num,] + r3*t_step_var, sf)
    C[step_num+1,] <- C[step_num,]+ t_step_var/6 * (r1 + 2*r2 + 2*r3 + r4)
    # increment total time elapsed
    t_total <- t_total + t_step_var
    # debug help
    print(paste("step = ",t_step_var, " steps = ", step_num," Ester = ",C[step_num+1,1]," time = ",t_total))
    # set time values
    time_trace[step_num+1,1] <- t_total
    # assess how large time step should be
    inv_rate_cons <- abs((1/dC(k,C[step_num+1,],sf)[1,1])/100)
    if(inv_rate_cons < 1){
      t_step_var <- inv_rate_cons
    }
    else{
      t_step_var <- 1
    }
  }
  
  C[,ncol(C)+1] <- time_trace
  ConcPlot <- ggplot(data = C, aes(minutes)) +
    geom_point(aes(y = E/3, color = "Ester")) +
    geom_point(aes(y = TG, color = "Triglyceride")) +
    geom_point(aes(y = DG, color = "Diglyceride")) +
    geom_point(aes(y = MG, color = "Monoglyceride")) +
    geom_point(aes(y = ROH/C[1,5], color = "Alcohol")) +
    geom_point(aes(y = OH/C[1,8], color = "Hydroxide")) +
    geom_point(aes(y = G, color = "Glycerol")) +
    geom_point(aes(y = S, color = "Soap")) +
    labs(title = "Species Concentration as a Function of Time", subtitle = paste("Temp = ", 70, "ºC")) +
    xlab("time (min)") +
    ylab("Normalized Species Concentration") +
    scale_color_discrete(name = "Reaction Species")
  plot(ConcPlot)
  
  return(C)
}