emul_risk <- function(sim_vals,temp) {
  # Finds changes in concentration between timepoints and normalizes (essentially a crude derivative)
  added_prod <- transmute(sim_vals,
            minutes = minutes,
            prodS_created = (lead(S) - (S))) %>% 
    filter(!(is.na(prod_created))) %>%
    mutate(prodS_created = prodS_created / max(abs(prodS_created))) %>%
    mutate(risk = factor(ifelse(prod_created <= 0.05, "safe", "risky"), levels = c("risky", "safe")))
    
    
    emulRisk <- ggplot(data = added_prod[, ]) +
      geom_line(
        mapping = aes(x = minutes, y = (prodS_created), fill = risk),
        color = "black",
        show.legend = FALSE
      ) +
    scale_fill_manual(values = c("#00ff00", "#ff0000")) +
    labs(title = "Relative Rates of Ester to Soap",
         subtitle = paste("Temp = ", temp, "ºC")) +
    xlab("time (min)") +
    ylab("Normalized Relative Rate")
    
    return (emulRisk)
}