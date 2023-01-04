library(dplyr)
library(tidyr)
library(deSolve)
library(ggplot2); theme_set(theme_bw())
library(ggpubr)
library(gridExtra)
library(tikzDevice)

library(shellpipes)
loadEnvironments()
startGraphics(otype="tikz", width = 8, height = 6, standAlone = T)

viridis::viridis(4, begin=0.1, alpha=0.4)

S0 <- 0.5
I0 <- 1e-6
VE <- 0.6
R0 <- 2.5

R0vec <- c(2.5)
pvec <- c(0.6)
qvec <- c(0, 1/3, 2/3, 1)
thetavec <- c(1, 2, 3) ## VE_L=VE, VE_L=sqrt(VE), VE_P=VE

allparam <- expand.grid(pvec, qvec, thetavec, R0vec)

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
    time=out$time,
    VE=pp[[1]],
    q=pp[[2]],
    scenario=pp[[3]],
    R0=pp[[4]],
    cumulative=1-out$Cv/(1-S0)/(out$Cu/(S0)),
    hazard=1-out$inc_v/(1-S0-out$Cv)/(out$inc_s/out$S)
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
  geom_line(aes(time, cumulative*100, col=q), lwd=2) +
  geom_hline(yintercept=60, lty=2) +
  scale_x_continuous("Time (days)", expand=c(0, 0)) +
  scale_y_continuous("Estimated vaccine effecticacy (\\%)", limits=c(0, 100), expand=c(0, 0)) +
  scale_color_viridis_d("Proportion of unsuccessful challenges boosted, $q$") +
  facet_grid(~scenario) +
  ggtitle("A. Cumulative-incidence reduction") +
  theme(
    legend.position = "top",
    panel.grid = element_blank()
  )

g2 <- ggplot(resdata) +
  geom_line(aes(time, hazard*100, col=q), lwd=2) +
  geom_hline(yintercept=60, lty=2) +
  scale_x_continuous("Time (days)", expand=c(0, 0)) +
  scale_y_continuous("Estimated vaccine effecticacy (\\%)", limits=c(0, 100), expand=c(0, 0)) +
  facet_grid(~scenario) +
  ggtitle("B. Harzard reduction") +
  scale_color_viridis_d("Proportion of challenges boosted, $q$") +
  theme(
    panel.grid = element_blank()
  )

ggarrange(g1, g2, nrow=2, common.legend = TRUE)
