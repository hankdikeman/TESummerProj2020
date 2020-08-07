emul_risk <- function(sim_df, scale_factor, vol) {
  print(paste("hello"))
  # Compute initial mass of all reactants (overall mass will stay constant)
  initial_mass <- sum(sim_df[1, 2:ncol(sim_df)]*scale_factor*vol*data.frame(matrix(c(300, 885.4, 665, 445, 32.04, 92.09, 250, 39.997), ncol = 8, nrow = 1)))
  
  # Create data frame with columns for time points and weight percents of soap 
  emul_df <- transmute(sim_vals, minutes = minutes, soap_wt_pc = (Soap*scale_factor*vol*250)/initial_mass)
  
  print(paste(c(emul_df, "emul")))

  # Create plot of time vs weight percent of soap using ggplot
  emulRisk <- ggplot(data = emul_df[, ]) +
    geom_line(
      mapping = aes(x = minutes, y = (soap_wt_pc), fill = risk),
      color = "black",
      show.legend = FALSE
    ) +
  scale_fill_manual(values = c("#00ff00", "#ff0000")) +
  labs(title = "Elmulsification Risk Assessement",
       subtitle = paste("Temp = ", temp, "ºC")) +
  xlab("time (min)") +
  ylab("Weight % of Soap")
  
  return (emulRisk)
}