CatActivity <- function(sim_vals) {
  # normalize catalyst activity values from 0 -> 1
  cat_df <- transmute(sim_vals, minutes = minutes, OH = OH/max(sim_vals$OH), forfill = OH)
  wtperc <- as.numeric(format(round(sim_vals[1,8]*39.997/(sim_vals[1,2]*885.4)*100,2), nsmall = 2))

  # Plot catalyst activity graph
  CatAct <- ggplot(data = cat_df) +
    geom_line(
      mapping = aes(x = minutes, y = OH, color = OH), size = 2, #stroke = 0,
      show.legend = FALSE
    ) +
    scale_color_distiller(palette = "RdYlGn", trans = "reverse",
                         limits = c(1,0)) +
    labs(title = "Normalized Catalyst Activity (0 to 1) as a Function of Time",
         subtitle = paste("Initial Catalyst Loading =", wtperc, "wt% Cat/Oil")) +
    xlab("time (min)") +
    ylab("Normalized Catalyst Activity") + 
    ylim(0,1)

  return(CatAct)
}