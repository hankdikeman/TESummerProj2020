totalConcPlot <- function(conc_df, temp, IC_df) {
  speciesPlot <-
    ggplot(data = conc_df, aes(x = minutes)) +
    geom_line(aes(y = E / (3), color = "Ester")) +
    geom_line(aes(y = TG, color = "Triglyceride")) +
    geom_line(aes(y = DG, color = "Diglyceride")) +
    geom_line(aes(y = MG, color = "Monoglyceride")) +
    geom_line(aes(y = ROH / IC_df[1, 5], color = "Alcohol")) +
    geom_line(aes(y = OH / IC_df[1, 8], color = "Hydroxide")) +
    geom_line(aes(y = G, color = "Glycerol")) +
    geom_line(aes(y = S, color = "Soap")) +
    labs(title = "Species Concentration as a Function of Time",
         subtitle = paste("Temp = ", input$temp_initial, "ºC")) +
    xlab("time (min)") +
    ylab("Normalized Species Concentration") +
    scale_color_discrete(name = "Reaction Species")
  return(speciesPlot)
}