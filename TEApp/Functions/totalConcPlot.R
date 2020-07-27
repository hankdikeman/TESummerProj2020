totalConcPlot <- function(sim_vals, temp, IC_df, disp_species) {
  species_plot <-
    ggplot(data = sim_vals, aes(x = minutes))
  
  # test conditions and add species to concentration plot as needed
  if(1 %in% disp_species) {
    species_plot <- species_plot + 
    geom_line(aes(y = E / (3), color = "Ester"))
  }
  if(2 %in% disp_species) {
    species_plot <- species_plot + 
    geom_line(aes(y = TG, color = "Triglyceride"))
  }
  if(3 %in% disp_species) {
    species_plot <- species_plot + 
    geom_line(aes(y = DG, color = "Diglyceride"))
  }
  if(4 %in% disp_species) {
    species_plot <- species_plot + 
    geom_line(aes(y = MG, color = "Monoglyceride"))
  }
  if(5 %in% disp_species) { 
    species_plot <- species_plot + 
    geom_line(aes(y = ROH / IC_df[1, 5], color = "Alcohol"))
  }
  if(6 %in% disp_species) { 
    species_plot <- species_plot + 
    geom_line(aes(y = G, color = "Glycerol"))
  }
  if(7 %in% disp_species) {
    species_plot <- species_plot + 
    geom_line(aes(y = S / (3), color = "Soap"))
  }
  if(8 %in% disp_species) {
    species_plot <- species_plot + 
      geom_line(aes(y = OH / IC_df[1, 8], color = "Hydroxide"))
  }
  
  # add other stuff
  species_plot <- species_plot + 
    labs(title = "Species Concentration as a Function of Time",
         subtitle = paste("Temp = ", temp, "ºC")) +
    xlab("time (min)") +
    ylab("Normalized Species Concentration") +
    scale_color_discrete(name = "Reaction Species") +
    ylim(0, 1)

  return(species_plot)
}