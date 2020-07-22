## Create histogram w/ accumulative prod of Ester
gen_hist <- function(ConcVals){
  graph <- ggplot(data = ConcVals, aes(x = minutes)) +
    geom_histogram(aes(y = Ester/(3*initialvals[1,2]), color = "Ester"))
}