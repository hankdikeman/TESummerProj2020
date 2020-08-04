CatActivity <- function(sim_vals, temp) {
  # normalize catalyst activity values from 0 -> 1
  cat_df <- transmute(sim_vals, minutes = minutes, OH = OH/max(sim_vals$OH), forfill = OH)

  # Plot catalyst activity graph
  CatAct <- ggplot(data = cat_df) +
    geom_tile(
      mapping = aes(x = minutes, y = OH, fill = OH, height = 0.02),
      show.legend = FALSE
    ) +
    scale_fill_distiller(palette = "RdYlGn", trans = "reverse",
                         limits = c(1,0)) +
    labs(title = "Normalized Catalyst Activity (0 -> 1) as a Function of Time",
         subtitle = paste("Initial Catalyst Loading = ", temp, "ºC")) +
    xlab("time (min)") +
    ylab("Normalized Catalyst Activity") + 
    ylim(0,1)

  return(CatAct)
}