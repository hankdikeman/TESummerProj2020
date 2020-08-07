rel_Rates <- function(sim_vals,temp) {
  added_prod <-
    # The relative rates of the reaction Ester:Soap
    transmute(sim_vals,
              minutes = minutes,
              prodE_created = (lead(E) - (E)),
              prodS_created = (lead(S) - (S))) %>%
    filter(!(is.na(prodE_created))) %>%
    mutate(prodE_created = prodE_created / max(abs(prodE_created))) %>%
    mutate(prodS_created = prodS_created / max(abs(prodS_created))) %>%
  
  # Plot relative rates
  relRate <- ggplot(data = added_prod[, ]) +
    geom_area(
      mapping = aes(x = minutes, y = (prodE_created/prodS_created), fill = "orange"),
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