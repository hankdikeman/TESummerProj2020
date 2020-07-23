progConcBar <- function(sim_vals) {
  # Finds changes in concentration between timepoints and normalizes (essentially a crude derivative)
  added_prod <-
    transmute(sim_vals,
              minutes = time,
              prod_created = (lead(Ester) - (Ester))) %>%
    filter(!(is.na(prod_created))) %>%
    mutate(prod_created = prod_created / max(abs(prod_created))) %>%
    mutate(posorneg = ifelse(prod_created <= 0, "neg", "pos"))

  # Plot rate of change graph
  ggplot(data = added_prod[,]) +
    geom_area(
      mapping = aes(x = minutes, y = prod_created, fill = posorneg),
      color = "black",
      show.legend = FALSE
    ) +
    scale_fill_manual(values = c("red", "green")) +
    labs(title = "Rate of Product Formation Through Time") +
    xlab("time (min)") +
    ylab("Normalized Rate of Change in Concentration")
}