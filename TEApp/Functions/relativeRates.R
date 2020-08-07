rel_Rates <- function(sim_vals, temp) {
  added_prod <-
    # Finds changes in concentration between timepoints and normalizes (essentially a crude derivative)
    transmute(sim_vals,
              minutes = minutes,
              prod_rate = ((lead(E) - (E))/(lead(S) - (S)))) %>%
    filter(!(is.na(prod_rate))) %>%
    mutate(prod_rate = prod_rate / max(abs(prod_rate)))
  
  # Plot relative rates
  relRate <- ggplot(data = added_prod[, ]) +
    geom_area(
      mapping = aes(x = minutes, y = (prod_rate), fill = "orange"),
      color = "black",
      show.legend = FALSE
    ) +
    scale_fill_manual(values = c("#00ff00", "#ff0000")) +
    labs(title = "Relative Rates of Ester to Soap",
         subtitle = paste("Temp = ", temp, "ºC")) +
    xlab("time (min)") +
    ylab("Normalized Relative Rate")
  
  
  return(relRate)
}