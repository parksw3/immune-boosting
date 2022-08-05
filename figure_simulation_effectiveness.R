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

R0vec <- c(2.5)
pvec <- c(0.4)
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
    time=out$time,
    p=pp[[1]],
    q=pp[[2]],
    theta=pp[[3]],
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
    theta=factor(theta, levels=c(3, 2, 1),
                 labels=c("$\\theta=1$", "$\\theta=\\sqrt{p}$", "$\\theta=p$, polarized vaccination")),
    R0=factor(R0, levels=c(2.5, 4, 8),
              labels=paste0("$\\mathcal{R}_0=", c(2.5, 4, 8), "$"))
  )

g1 <- ggplot(resdata) +
  geom_line(aes(time, cumulative*100, col=q), lwd=2) +
  geom_hline(yintercept=60, lty=2) +
  scale_x_continuous("Time (days)", expand=c(0, 0)) +
  scale_y_continuous("Estimated vaccine effectiveness (\\%)", limits=c(0, 100), expand=c(0, 0)) +
  scale_color_viridis_d("Proportion of challenges boosted, $q$") +
  facet_grid(~theta) +
  ggtitle("A. Cumulative-incidence-based estimates") +
  theme(
    legend.position = "top",
    panel.grid = element_blank()
  )

g2 <- ggplot(resdata) +
  geom_line(aes(time, hazard*100, col=q), lwd=2) +
  geom_hline(yintercept=60, lty=2) +
  scale_x_continuous("Time (days)", expand=c(0, 0)) +
  scale_y_continuous("Estimated vaccine effectiveness (\\%)", limits=c(0, 100), expand=c(0, 0)) +
  facet_grid(~theta) +
  ggtitle("B. Hazard-based estimates") +
  scale_color_viridis_d("Proportion of challenges boosted, $q$") +
  theme(
    panel.grid = element_blank()
  )

tikz(file = "figure_simulation_effectiveness.tex", width = 8, height = 6, standAlone = T)
ggarrange(g1, g2, nrow=2, common.legend = TRUE)
dev.off()
tools::texi2dvi('figure_simulation_effectiveness.tex', pdf = T, clean = T)
