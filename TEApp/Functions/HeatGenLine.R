HeatGenLine <- function(sim_vals, vol, scale_factor){
  # these should be the heats of formation PER FATTY ACID in kJ/mol. Must be also put manually onto graph
  delH_TE <- 10.742
  delH_SAP <- 110.7
  
  # calculation of released energy by reaction
  E_released <- transmute(sim_vals, minutes = minutes, E_tot = scale_factor*vol*(E*delH_TE + S*delH_SAP), typerxn = "total")
  E_released_TE <- transmute(sim_vals, minutes = minutes, E_tot = scale_factor*vol*(E*delH_TE), typerxn = "TE")
  E_released_SAP <- transmute(sim_vals, minutes = minutes, E_tot = scale_factor*vol*(S*delH_SAP), typerxn = "SAP")
  
  # combine all different energy releases
  E_rel_total <- rbind(E_released, E_released_TE, E_released_SAP)

  # plotting on line plot
  E_plot <- ggplot(data = E_rel_total, aes(x = minutes, y = E_tot, color = typerxn)) + 
    geom_line(size = 1.25) +
    labs(title = "Heat Released Through Time",
         subtitle = expression(paste(
           paste(Delta, "H", sep = "")[TE],
           " = ",
           -10.74,
           " kJ/mol,  ",
           paste(Delta, "H", sep = "")[SAP],
           " = ",
           -110.7,
           " kJ/mol"
         ))) +
    scale_color_manual(
      values = c("#000000", "#86BBD8", "#F5B7B1"),
      name = "Energy Source",
      labels = c("All Sources", "Transesterification", "Saponification"),
      breaks = c("total", "TE", "SAP")
    ) +
    xlab("time (min)") +
    ylab("Cumulative Energy Released (kJ)")
  
  return (E_plot)
}