library(dplyr)
library(tidyr)
library(deSolve)

library(shellpipes)
loadEnvironments()

S0 <- 0.5
I0 <- 1e-6
R0vec <- c(4, 8)
VEvec <- seq(0, 1, length.out=21)
qvec <- c(0, 0.5, 1)
scenariovec <- c(1, 2, 3) ## VE_L=VE, VE_L=sqrt(VE), VE_P=VE

allparam <- expand.grid(VEvec, qvec, scenariovec, R0vec)

reslist <- vector('list', nrow(allparam))

for (i in 1:nrow(allparam)) {
  pp <- allparam[i,]
  
  VE <- pp[[1]]
  scenario <- pp[[3]]
  
  if (scenario==1) {
    VE_L <- VE
    VE_P <- 0
  } else if (scenario==2) {
    VE_L <- 1-sqrt(1-VE)
    VE_P <- 1-sqrt(1-VE)
  } else {
    VE_L <- 0
    VE_P <- VE
  }
  
  out <- simulate_model_generalized(R0=pp[[4]], rho=0, 
                                    VE_L = VE_L,
                                    VE_P = VE_P,
                                    q=pp[[2]],
                                    S0=S0-I0, V0=(1-S0) * (1-VE_P), Rv0=(1-S0) * (VE_P),
                                    I0=I0)
  
  reslist[[i]] <- data.frame(
    finalsizeV=tail(out$Cv, 1)/(1-S0),
    finalsizeU=tail(out$Cu, 1)/S0,
    VE=pp[[1]],
    q=pp[[2]],
    scenario=pp[[3]],
    R0=pp[[4]]
  )
}

resdata <- (reslist %>% bind_rows
	%>% mutate(
		q=factor(q, levels=qvec, labels=as.character(qvec))
		, scenario=factor(scenario, labels=c("Leaky", "Mixed", "Polarized"))
		, R0=factor(R0)
	)
)

rdsSave(resdata)
