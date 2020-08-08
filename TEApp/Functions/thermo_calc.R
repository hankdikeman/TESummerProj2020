## Enthalpies for ester estim. at -37 KJ/mol and soap and -20 KJ/mol
calcThermo <- function(sim_vals, temp, vol, accum_pt, scale_factor){
  constant <- scale_factor * vol * 100
  # Finds changes in concentration between timepoints and normalizes (essentially a crude derivative)
  added_heat <-
    transmute(sim_vals,
              minutes = minutes,
              heat = E * constant * (-37) + S * constant * (-20)) %>%
    filter(!(is.na(heat))) #%>%
  ##mutate(prod_created = heat / max(abs(heat))) %>%
    
  # calculate total accumulated product at given point
  accum_heat <- filter(sim_vals, minutes >= accum_pt)
  # round yield number
  accum_heat <- as.numeric(format(round(accumprod, 2), nsmall = 2))
  
  
  # Plot rate of change graph
  heat_rate <- ggplot(data = added_heat[, ]) +
    geom_area(
      mapping = aes(x = minutes, y = heat, fill = "green"),
      color = "black",
      show.legend = FALSE,
      alpha = 0.5
    ) +
    geom_vline(xintercept = accum_pt, color = "black", size = 1.25) + 
    geom_text(aes(
      x = ifelse(accum_pt < max(sim_vals$minutes) / 2, accum_pt + 0.1*max(sim_vals$minutes), accum_pt - 0.1*max(sim_vals$minutes)),
      y = 0.8,
      label = paste("Total Heat", accum_heat, "KJ")
    ), 
    size = 6) +
    scale_fill_manual(values = c("#00ff00", "#ff0000")) +
    labs(title = "Heat Released vs. Time",
         subtitle = paste("Temp = ", temp, "ºC")) +
    xlab("time (min)") +
    ylab("Heat Released (KJ)")
  
  return (heat_rate)
}