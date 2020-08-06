install.packages("tidyverse")
library(tidyverse)
source("./k_gen.R")
source("./k_set.R")
source("./RK4.R")
source("./dC.R")

###Same as visuals but can input temp & initial conc manually.

## Generating k values as shown in kGeneration.Rmd
# list of temperature(s) in kelvin
temp <- c(readline(prompt = "Enter reaction temperature: "))
temp <- as.integer(temp)
#Display temp
temp

# list of k values
kvals <- data.frame(matrix(0, nrow = 1, ncol = 10))
colnames(kvals) <- c("k1","k1r","k2","k2r","k3","k3r","k4","k5","k6","k7")

for(i in 1:length(temp)) {
  kvals[,i] <- k_set(temp[i])
  kvals
}
kvals

# Plot of k1 v temp
ggplot(data = kvals) +
  geom_point(mapping = aes(x = k2, y = temp), color = "red")

## Using values found in RK4
conc_TG <- readline(prompt = "Enter initial TG concentration: ")
conc_TG <- as.integer(conc_TG)
conc_ROH <- readline(prompt = "Enter initial ROH concentration: ")
conc_ROH <- as.integer(conc_ROH)
conc_OH <- readline(prompt = "Enter initial OH concentration: ")
conc_OH <- as.integer(conc_OH)

initialvals <- data.frame(matrix(c(0,conc_TG,0,0,conc_ROH,0,0,conc_OH), nrow = 1, ncol = 8))
colnames(initialvals) <- c("Ester","TG","DG","MG","ROH","G","S","OH")
# scale factor used to non-dimensionalize all concentrations
scale_factor <- 1.035
# total time of 600 minutes
time <- 1000
# 0.5 minute time step
dt <- 2
# integrate using initial conditions, scale factor, and time parameters
ConcVals <- RK4(kvals, initialvals, time, dt, scale_factor)

ggplot(data = time_series, aes(time)) + 
  geom_line(aes(y = Ester/(3*initialvals[1,2]), color = "Ester")) + 
  geom_line(aes(y = G, color = "Glycerol")) + 
  geom_line(aes(y = S, color = "Soap")) + 
  labs(title = "Species Concentration as a Function of Time", subtitle = "Room temp") +
  xlab("time (min)") + 
  ylab("Normalized Species Concentration") +
  scale_color_discrete(name = "Reaction Species")

## The temp doesn't seem to impact. The normalized species conc is E-128?!!


