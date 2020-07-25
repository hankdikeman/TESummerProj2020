#Various stats

gen_prodStats <- function(volProd){
  graph <- ggplot(data = volProd, aes(x=minutes)) +
    geom_line(aes(y = Ester/(3*initialvals[1,2]), color = "Ester")) + 
    geom_line(aes(y = TG, color = "Triglyceride")) + 
    geom_line(aes(y = DG, color = "Diglyceride")) + 
    geom_line(aes(y = MG, color = "Monoglyceride")) + 
    geom_line(aes(y = ROH/initialvals[1,5], color = "Alcohol")) + 
    # geom_line(aes(y = OH/initialvals[1,8], color = "Hydroxide")) + 
    geom_line(aes(y = G, color = "Glycerol")) + 
    geom_line(aes(y = S, color = "Soap"))
  return (graph)
}

# Can add colors after certain time stamps to indicate worth-whileness
#of continuing the reaction.
gen_prodStatsHist <- function(volProd){
  graph <- ggplot(data = volProd, aes(x=minutes)) +
    geom_line(aes(y = Ester/(3*initialvals[1,2]), color = "Ester"))
  return (graph)
}