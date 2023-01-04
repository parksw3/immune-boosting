library(dplyr)
library(deSolve)

library(shellpipes)
loadEnvironments()

S0 <- 0.5
I0 <- 1e-6
R0 <- 2.5
VE <- 0.6

out1 <- simulate_model_generalized(
	R0=R0, rho=0, VE_L=VE, VE_P=0, q=0,
	S0=S0-I0, V0=(1-S0)
) ## leaky
out2 <- simulate_model_generalized(
	R0=R0, rho=0, VE_L=0, VE_P=VE,
	S0=S0-I0, V0=(1-S0) * (1-VE), Rv0=(1-S0) * VE
) ## polarized vaccination
out3 <- simulate_model_generalized(
	R0=R0, rho=0, VE_L=VE, VE_P=0, q=1,
	S0=S0-I0, V0=(1-S0)
) ## boosting

saveEnvironment()
