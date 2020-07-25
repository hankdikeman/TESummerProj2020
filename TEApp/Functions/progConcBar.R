progConcBar <- function(sim_vals,temp) {
  # Finds changes in concentration between timepoints and normalizes (essentially a crude derivative)
  added_prod <-
    transmute(sim_vals,
              minutes = minutes,
              prod_created = (lead(E) - (E))) %>%
    filter(!(is.na(prod_created))) %>%
    mutate(prod_created = prod_created / max(abs(prod_created))) %>%
    mutate(posorneg = factor(ifelse(prod_created <= 0, "neg", "pos"), levels = c("pos", "neg")))

  # Plot rate of change graph
  progConc <- ggplot(data = added_prod[, ]) +
    geom_area(
      mapping = aes(x = minutes, y = prod_created, fill = posorneg),
      color = "black",
      show.legend = FALSE
    ) +
    scale_fill_manual(values = c("#00ff00", "#ff0000")) +
    labs(title = "Rate of Product Formation Through Time",
         subtitle = paste("Temp = ", temp, "ºC")) +
    xlab("time (min)") +
    ylab("Normalized Rate")

  return(progConc)
}