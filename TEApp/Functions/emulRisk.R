emulRisk <- function(sim_df, scale_factor, vol, temp) {

  # Compute initial mass of all reactants (overall mass will stay constant)
  initial_mass <- sum(sim_df[1, 2:ncol(sim_df)]*scale_factor*vol*data.frame(matrix(c(300, 885.4, 665, 445, 32.04, 92.09, 250, 39.997), ncol = 8, nrow = 1)))
  
  # Create data frame with columns for time points and weight percents of soap 
  emul_df <- transmute(sim_df, minutes = minutes, S = (S*scale_factor*vol*250*100)/initial_mass)

  # Create plot of time vs weight percent of soap using ggplot
  emulRisk <- ggplot(data = emul_df[, ]) +
    geom_line(
      mapping = aes(x = minutes, y = S),
      color = "black",
      show.legend = FALSE
    ) +
  scale_fill_manual(values = c("#00ff00", "#ff0000")) +
  labs(title = "Emulsification Risk Assessement",
       subtitle = paste("Temp = ", temp, "ºC")) +
  xlab("time (min)") +
  ylab("Weight % of Soap")
  
  return (emulRisk)
}