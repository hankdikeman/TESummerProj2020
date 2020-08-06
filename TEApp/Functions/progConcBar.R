progConcBar <- function(sim_vals,temp,accum_pt) {
  # Finds changes in concentration between timepoints and normalizes (essentially a crude derivative)
  added_prod <-
    transmute(sim_vals,
              minutes = minutes,
              prod_created = (lead(E) - (E))) %>%
    filter(!(is.na(prod_created))) %>%
    mutate(prod_created = prod_created / max(abs(prod_created))) %>%
    mutate(posorneg = factor(ifelse(prod_created <= 0, "neg", "pos"), levels = c("pos", "neg")))
  
  # calculate total accumulated product at given point
  accumprod <- filter(sim_vals, minutes >= accum_pt)[1,1]/3*100
  # round yield number
  accumprod <- as.numeric(format(round(accumprod, 2), nsmall = 2))

  # Plot rate of change graph
  progConc <- ggplot(data = added_prod[, ]) +
    geom_area(
      mapping = aes(x = minutes, y = prod_created, fill = posorneg),
      color = "black",
      show.legend = FALSE
    ) +
    geom_vline(xintercept = accum_pt, color = "black", size = 1.25) + 
    geom_text(aes(
                  x = ifelse(accum_pt < max(sim_vals$minutes) / 2, accum_pt + 0.1*max(sim_vals$minutes), accum_pt - 0.1*max(sim_vals$minutes)),
                  y = 0.8,
                  label = paste("Yield:", accumprod, "%")
                ), 
              size = 6) +
    scale_fill_manual(values = c("#00ff00", "#ff0000")) +
    labs(title = "Product Yield and Production Rate vs. Time",
         subtitle = paste("Temp = ", temp, "ºC")) +
    xlab("time (min)") +
    ylab("Normalized Rate")

  return(progConc)
}