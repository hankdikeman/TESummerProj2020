rel_Rates <- function(sim_vals, temp, accum_pt) {
  added_prod <-
    # Finds changes in concentration between timepoints and normalizes (essentially a crude derivative)
    transmute(sim_vals,
              minutes = minutes,
              prod_rate = ((lead(E) - (E))/(lead(S) - (S)))) %>%
    filter(!(is.na(prod_rate))) #%>%
    #mutate(prod_rate = prod_rate / max(abs(prod_rate)))
  
  # find rate ratio at selected point
  rate_ratio <- filter(added_prod, minutes >= accum_pt)$prod_rate[1]
  # round ratio number
  rate_ratio <- as.numeric(format(round(rate_ratio, 2), nsmall = 2))
  
  
  
  # Plot relative rates
  relRate <- ggplot(data = added_prod[, ]) +
    geom_area(
      mapping = aes(x = minutes, y = (prod_rate)),
      color = "black",
      show.legend = FALSE,
      fill="#86bbd8", 
      alpha=0.4
    ) +
    geom_vline(xintercept = accum_pt, color = "black", size = 1.25) + 
    geom_text(aes(
      x = ifelse(accum_pt < max(sim_vals$minutes) / 2, accum_pt + 0.2*max(sim_vals$minutes), accum_pt - 0.2*max(sim_vals$minutes)),
      y = 0.8*max(prod_rate),
      label = paste(rate_ratio,":1 Ester:Soap Rates", sep = "")
    ),
    size = 6) +
    labs(title = expression(paste("Relative Rates of Generation, Ester Formation Rate:Soap Formation Rate"))) +
    xlab("time (min)") +
    ylab(expression(paste("Relative Rates of Rxn (", scriptstyle(frac("Transesterification","Saponification")),")")))
  
  
  return(relRate)
}