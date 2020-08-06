progConcBar <- function(sim_vals) {
  # Finds changes in concentration between timepoints and normalizes (essentially a crude derivative)
  added_prod <-
    transmute(sim_vals,
              minutes = time,
              prod_created = (lead(Ester) - (Ester))) %>%
    filter(!(is.na(prod_created))) %>%
    mutate(prod_created = prod_created / max(abs(prod_created))) %>%
    mutate(posorneg = factor(ifelse(prod_created <= 0, "neg", "pos"), levels = c("pos", "neg")))
  print(added_prod)
  # Plot rate of change graph
  progConc <- ggplot(data = added_prod[, ]) +
    geom_area(
      mapping = aes(x = minutes, y = prod_created, fill = posorneg),
      color = "black",
      show.legend = FALSE
    ) +
    scale_fill_manual(values = c("#00ff00", "#ff0000")) +
    labs(title = "Rate of Product Formation Through Time") +
    xlab("time (min)") +
    ylab("Normalized Rate")
  return(progConc)
}