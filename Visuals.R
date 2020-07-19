### Set of visuals to represent the TE.
#
install.packages("tidyverse")
library(tidyverse)
source("./k_gen.R")
source("./k_set.R")
source("./RK4.R")
source("./dC.R")

## Generating k values as shown in kGeneration.Rmd
# list of temperatures in kelvin
temps <- c(20, 25, 30, 35, 40, 50, 60, 70, 80, 90)
# list of k values
kvals <- data.frame(matrix(0, nrow = 10, ncol = 10))
colnames(kvals) <- c("k1","k1r","k2","k2r","k3","k3r","k4","k5","k6","k7")

for(i in 1:length(temps)) {
  kvals[i,] <- k_set(temps[i])
  kvals
}
# Display the k values
kvals

# Plot of k1 v temp
ggplot(data = kvals) +
  geom_point(mapping = aes(x = temps, y = k1), color = "red")

## Using values found in RK4
initialvals <- data.frame(matrix(c(0,1,0,0,6,0,0,0.01), nrow = 1, ncol = 8))
colnames(initialvals) <- c("Ester","TG","DG","MG","ROH","G","S","OH")
initialvals
# scale factor used to non-dimensionalize all concentrations
scale_factor <- 1.035

# total time of 600 minutes
time <- 1000
# 0.5 minute time step
dt <- 2
# integrate using initial conditions, scale factor, and time parameters
ConcVals <- RK4(kvals, initialvals, time, dt, scale_factor)
# first few lines of concentration matrix
head(ConcVals)

timevals <- data.frame(matrix(0, nrow = time/dt+1, ncol = 1))
for(i in 1:((time/dt)+1)){timevals[i,1] <- (i-1)*dt}
time_series <- cbind(ConcVals,timevals)
colnames(time_series) = c(colnames(ConcVals),"time")

## Plot of ester, glycerol, and soap v time
ggplot(data = time_series, aes(time)) + 
  geom_line(aes(y = Ester/(3*initialvals[1,2]), color = "Ester")) + 
  geom_line(aes(y = G, color = "Glycerol")) + 
  geom_line(aes(y = S, color = "Soap")) + 

  labs(title = "Species Concentration as a Function of Time", subtitle = "Room temp") +
  xlab("time (min)") + 
  ylab("Normalized Species Concentration") +
  scale_color_discrete(name = "Reaction Species")

