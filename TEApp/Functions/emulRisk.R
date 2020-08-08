emulRisk <- function(sim_df, scale_factor, vol, temp) {

  # Compute initial mass of all reactants (overall mass will stay constant)
  initial_mass <- sum(sim_df[1, 1:(ncol(sim_df)-1)]*scale_factor*vol*data.frame(matrix(c(300, 885.4, 665, 445, 32.04, 92.09, 250, 39.997), ncol = 8, nrow = 1)))
  
  # Create data frame with columns for time points and weight percents of soap 
  emul_df <- transmute(sim_df, minutes = minutes, S = (S*scale_factor*vol*250*100)/initial_mass)
  
  # add representative values so all risk categories on legend, parse out emulsion risk levels
  emul_df[nrow(emul_df)+1,] <- c(-10,3)
  emul_df[nrow(emul_df)+1,] <- c(-10,5)
  emul_df <- mutate(emul_df, risk_cat = factor(ifelse(S < 1.5, "low risk", ifelse(S < 3.5, "mid risk","high risk")), levels = c("low risk", "mid risk", "high risk")))

  # Create plot of time vs weight percent of soap using ggplot
  emulRisk <- ggplot(data = emul_df, aes(x = minutes, y = S, fill = risk_cat)) +
    geom_blank() +
    geom_area(
      color = "black",
      alpha = 0.4
    ) +
    # low risk line
    geom_hline(yintercept = 1.5, color = "black", size = 1.25, linetype = "dashed") + 
    # low risk label
    geom_text(aes(
      x = 0.1 * max(minutes),
      y = 0.75,
      label = paste("Low Risk")
    ), 
    size = 6) +
    # mid risk line
    geom_hline(yintercept = 3.5, color = "black", size = 1.25, linetype = "dashed") + 
    # mid risk label
    geom_text(aes(
      x = 0.1 * max(minutes),
      y = 2.5,
      label = paste("Mid Risk")
    ), 
    size = 6) +
    # mid risk label
    geom_text(aes(
      x = 0.1 * max(minutes),
      y = 4.5,
      label = paste("High Risk")
    ), 
    size = 6) +
    scale_fill_manual(
      values = c("#00ff00", "#ffff00", "#ff0000"),
      name = "Emulsion Risk",
      labels = c("Low Risk", "Mid Risk", "High Risk"),
      breaks = c("low risk", "mid risk", "high risk")
    ) +
    labs(title = "Estimation of Emulsification Risk For Subsequent Purification",
         subtitle = "Low < 1.5wt% || 1.5wt% < Mid < 3.5wt% || 3.5wt% < High") + 
  xlab("time (min)") +
  ylab("Soap Concentration (wt%)") +
  ylim(0, max(c(8,max(emul_df$S)))) +
  xlim(0, max(sim_df$minutes))
  
  return (emulRisk)
}