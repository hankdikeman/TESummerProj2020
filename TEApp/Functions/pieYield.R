pieYield <- function(tp_df){
  # timepoint of measured yield
  time_value <- as.numeric(format(round(tp_df$minutes[1],1),nsmall = 1))
  
  # product, starting material, and soap concentrations
  conv_vals <- c(tp_df$E[1], (tp_df$TG[1] * 3 + tp_df$DG[1] * 2 + tp_df$MG[1]), tp_df$S[1])
  
  # rounding of conversion values to fewer decimals and generation of labels
  percent_conversion <- as.numeric(c(format(round(conv_vals[1]/3*100,1), nsmall = 1), format(round(conv_vals[2]/3*100,1), nsmall = 1),format(round(conv_vals[3]/3*100,1), nsmall = 1)))
  labels <-
    c(
      paste("Converted:", percent_conversion[1], "%"),
      paste("Unconverted:", percent_conversion[2], "%"),
      paste("Saponified:", percent_conversion[3], "%")
    )
  
  # collection into dataframe
  conversion_df <- data.frame(labels,percent_conversion)
  
  piePlot <- ggplot(conversion_df, aes(x = 1, y = percent_conversion, fill = labels)) + 
    geom_bar(mapping = aes(x = 1, y = percent_conversion, fill = labels), stat = "identity") +#, color = "black") +
    geom_text(mapping = aes(label = labels), position = position_stack(vjust = 0.5), size = 4) +
    coord_polar(theta = 'y') + 
    theme(
      axis.ticks = element_blank(),
      axis.title = element_blank(),
      axis.text = element_blank(),
      panel.grid  = element_blank()
    ) +
    scale_fill_manual(values = c("#86BBD8","#F5B7B1","#D7DBDD"), name = "Fatty Acid Distribution (mol%)") +
    labs(title = "Reaction Yield and Soap Formation Data",
         subtitle = paste("At time = ", time_value, "minutes")) 

  return(piePlot)
}
