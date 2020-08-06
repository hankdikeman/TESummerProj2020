# this is a simple driver file used to develop predicted models for yield

source("./RK4.R")
source("./dC.R")

#kvals dataframe
kvals <- data.frame(matrix(c(5.95,2.98,9.399,7.311,15.18,0.698,0.0133,0.3456,0.7474,0.4654), nrow = 1))/60
colnames(kvals) <- c("k1","k1r","k2","k2r","k3","k3r","k4","k5","k6","k7")
# initial species concentration values
initialvals <- data.frame(matrix(c(0,1,0,0,0,0,0,0.08), nrow = 1, ncol = 8))
colnames(initialvals) <- c("Ester","TG","DG","MG","ROH","G","S","OH")
# scale factor used to nondimensionalize all concentrations
scale_factor <- 1.035
# total time and time step
time <- 500
t_step <- 10

yield_by_alc <- data.frame(matrix(0, nrow = 50, ncol = 3))
colnames(yield_by_alc) <- c("Alc_IV","Yield","OH_Remain")

for(alc in 1:50){
  # calculate initial alcohol concentration (nondimensional)
  initialAlc <- 2 + 0.1*alc
  yield_by_alc[alc,"Alc_IV"] <- initialAlc/3*100
  initialvals[1,"ROH"] <- initialAlc
  # integrate using RK4 to find yield
  final_conc <- RK4(kvals,initialvals,time,t_step,scale_factor)[(time/t_step),]
  yield_by_alc[alc,"Yield"] <- final_conc[1,1]/3*100
  yield_by_alc[alc,"OH_Remain"] <- final_conc[1,8]/0.08*100
}

# get coefficients for modelling
yield_for_model <- transmute(yield_by_alc, Yield = Yield, ROH = Alc_IV, ROHsq = Alc_IV^2, ROHcub = Alc_IV^3)
# build 1st model, linear
linearMod1 <- lm(Yield ~ ROH, data=yield_for_model)
mod1_coeff <- summary(linearMod1)$coefficients
# build 2nd model, quadratic
linearMod2 <- lm(Yield ~ ROH + ROHsq, data=yield_for_model)
mod2_coeff <- summary(linearMod2)$coefficients
# build in predicted values from each model
yield_for_model <- mutate(yield_for_model, mod1Pred = mod1_coeff[1,1] + mod1_coeff[2,1]*ROH, mod2Pred = mod2_coeff[1,1] + mod2_coeff[2,1]*ROH + mod2_coeff[3,1]*ROHsq)

ggplot(data = yield_for_model) +
  geom_line(mapping = aes(x = ROH, y = Yield), color = 'blue') +
  geom_line(mapping = aes(x = ROH, y = mod1Pred), color = 'red') + 
  geom_line(mapping = aes(x = ROH, y = mod2Pred), color = 'green')
