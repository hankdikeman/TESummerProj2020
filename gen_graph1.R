### Create histogram w/ accumulative prod of Ester
## Groups of prod:
#     Glycerides & Ester
#     Hydroxide, glycerol and soap
#     Ester, gylcerol, and soap
#
gen_hist <- function(ConcVals){
  graph <- ggplot(data = ConcVals, aes(x = minutes)) +
    geom_histogram(aes(y = Ester/(3*initialvals[1,2]), color = "Ester"))
  return (graph)
}