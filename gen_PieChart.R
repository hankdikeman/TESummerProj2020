# Link for two type of pie charts. Does not solve problem when stuff is 0.
#https://www.statmethods.net/graphs/pie.html
# Need to install plotrix
library(plotrix)
pie_chart_3D <- function(concProd){
  lbls <- c("Ester","TG","DG","MG","ROH","G","S","OH")
  graph <- pie3D(concProd, labels = lbls, explode = 0.1, 
        main = "Pie chart of Concentrations")
  return (graph)
}

# Does not require pie chart
pie_chart <- function(concProd){
  lbls <- c("Ester","TG","DG","MG","ROH","G","S","OH")
  pct <- round(concProd/sum(slices)*100)
  lbls <- paste(lbls, pct) # add percents to labels
  lbls <- paste(lbls,"%",sep="") # ad % to labels
  graph <- pie(concProd,labels = lbls, col=rainbow(length(lbls)),
      main="Pie Chart of Concentrations")
  return (graph)
}

