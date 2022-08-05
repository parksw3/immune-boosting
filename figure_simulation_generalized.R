library(dplyr)
library(tidyr)
library(deSolve)
library(ggplot2); theme_set(theme_bw())
library(ggpubr)
library(gridExtra)
library(tikzDevice)
source("simulate.R")
source("color_palette.R")

viridis::viridis(4, begin=0.1, alpha=0.4)

S0 <- 0.5
R0vec <- c(2.5, 4, 8)
pvec <- seq(0.05, 0.95, length.out=31)
qvec <- c(0, 1/3, 2/3, 1)
thetavec <- c(1, 2, 3) ## theta = p, theta = sqrt(p), theta=1

allparam <- expand.grid(pvec, qvec, thetavec, R0vec)

reslist <- vector('list', nrow(allparam))

for (i in 1:nrow(allparam)) {
  pp <- allparam[i,]
  
  theta <- switch (pp[[3]],
    `1` = pp[[1]],
    `2` = sqrt(pp[[1]]),
    `3` = 1
  )
  
  out <- simulate_model_generalized(R0=pp[[4]], rho=0, p=pp[[1]], theta=theta, q=pp[[2]],
                                    S0=S0-1e-6, V0=(1-S0) * theta, Rv0=(1-S0) * (1-theta))
  
  reslist[[i]] <- data.frame(
    finalsizeV=tail(out$Cv, 1)/(1-S0),
    finalsizeU=tail(out$Cu, 1)/S0,
    p=pp[[1]],
    q=pp[[2]],
    theta=pp[[3]],
    R0=pp[[4]]
  )
}

resdata <- reslist %>%
  bind_rows %>%
  mutate(
    q=factor(q, levels=c(0, 1/3, 2/3, 1), 
             labels=c("0", "1/3", "2/3", 1)),
    theta=factor(theta, levels=c(3, 2, 1),
                 labels=c("$\\theta=1$", "$\\theta=\\sqrt{p}$", "$\\theta=p$, polarized vaccination")),
    R0=factor(R0, levels=c(2.5, 4, 8),
              labels=paste0("$\\mathcal{R}_0=", c(2.5, 4, 8), "$"))
  )

g1 <- ggplot(resdata) +
  geom_line(aes(1-p, finalsizeV, col=factor(q)), lwd=1) +
  scale_x_continuous("Vaccine efficacy, $1-p$", limits=c(0, 1),
                     breaks=c(0, 0.2, 0.4, 0.6, 0.8, 1)) +
  scale_y_continuous("Proportion infected", limits=c(0, 1)) +
  scale_color_viridis_d("Proportion of unsuccessful challenges boosted, $q$") +
  facet_grid(R0~theta) +
  theme(
    legend.position = "top",
    panel.grid = element_blank()
  )

g2 <- ggplot(resdata) +
  geom_line(aes(1-p, finalsizeU, col=factor(q)), lwd=1) +
  scale_x_continuous("Vaccine efficacy, $1-p$", limits=c(0, 1),
                     breaks=c(0, 0.2, 0.4, 0.6, 0.8, 1)) +
  scale_y_continuous("Proportion infected", limits=c(0, 1)) +
  scale_color_viridis_d("Proportion of unsuccessful challenges boosted, $q$") +
  facet_grid(R0~theta) +
  theme(
    legend.position = "none",
    panel.grid = element_blank()
  )

tikz(file = "figure_simulation_generalized_vaccinated.tex", width = 8, height = 6, standAlone = T)
g1
dev.off()
tools::texi2dvi('figure_simulation_generalized_vaccinated.tex', pdf = T, clean = T)

tikz(file = "figure_simulation_generalized_unvaccinated.tex", width = 8, height = 6, standAlone = T)
g2
dev.off()
tools::texi2dvi('figure_simulation_generalized_unvaccinated.tex', pdf = T, clean = T)
