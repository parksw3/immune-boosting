library(dplyr)
library(tidyr)
library(deSolve)
library(ggplot2); theme_set(theme_bw())
library(ggpubr)
library(gridExtra, warn.conflicts=FALSE)
library(tikzDevice)

library(shellpipes)
loadEnvironments()

viridis::viridis(4, begin=0.1, alpha=0.4)

S0 <- 0.5
I0 <- 1e-6
R0vec <- c(2.5, 4, 8)
VEvec <- seq(0.05, 0.95, length.out=31)
qvec <- c(0, 1/3, 2/3, 1)
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

resdata <- reslist %>%
  bind_rows %>%
  mutate(
    q=factor(q, levels=c(0, 1/3, 2/3, 1), 
             labels=c("0", "1/3", "2/3", 1)),
    scenario=factor(scenario, levels=c(1, 2, 3),
                 labels=c("$\\textrm{VE}_L=\\textrm{VE}, \\textrm{VE}_P=0$", 
                          "$\\textrm{VE}_L=\\textrm{VE}_P=1-\\sqrt{1-\\textrm{VE}}$", 
                          "$\\textrm{VE}_L=0, \\textrm{VE}_P=\\textrm{VE}$")),
    R0=factor(R0, levels=c(2.5, 4, 8),
              labels=paste0("$\\mathcal{R}_0=", c(2.5, 4, 8), "$"))
  )

g1 <- ggplot(resdata) +
  geom_line(aes(VE, finalsizeV, col=factor(q)), lwd=1) +
  scale_x_continuous("Vaccine efficacy, $\\textrm{VE}$", limits=c(0, 1),
                     breaks=c(0, 0.2, 0.4, 0.6, 0.8, 1)) +
  scale_y_continuous("Proportion infected", limits=c(0, 1)) +
  scale_color_viridis_d("Proportion of unsuccessful challenges boosted, $q$", begin=1, end=0) +
  facet_grid(R0~scenario) +
  theme(
    legend.position = "top",
    panel.grid = element_blank()
  )

g2 <- ggplot(resdata) +
  geom_line(aes(VE, finalsizeU, col=factor(q)), lwd=1) +
  scale_x_continuous("Vaccine efficacy, $\\textrm{VE}$", limits=c(0, 1),
                     breaks=c(0, 0.2, 0.4, 0.6, 0.8, 1)) +
  scale_y_continuous("Proportion infected", limits=c(0, 1)) +
  scale_color_viridis_d("Proportion of unsuccessful challenges boosted, $q$", begin=1, end=0) +
  facet_grid(R0~scenario) +
  theme(
    legend.position = "none",
    panel.grid = element_blank()
  )

startGraphics(otype="tikz", ext="vaccinated.tikz"
	, width = 8, height = 6, standAlone = T
)
print(g1)

startGraphics(otype="tikz", ext="unvaccinated.tikz"
	, width = 8, height = 6, standAlone = T
)
print(g2)
