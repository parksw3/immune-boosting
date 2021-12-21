library(dplyr)
library(tidyr)
library(deSolve)
library(ggplot2); theme_set(theme_bw())
library(ggpubr)
library(gridExtra)
library(tikzDevice)

## sessionInfo()
## quit()

source("simulate.R")
source("color_palette.R")

viridis::viridis(4, begin=0.1, alpha=0.4)

out1 <- simulate_model_generalized(R0=2.5, rho=0.04/7, p=0.4, theta=1, q=0)
out2 <- simulate_model_generalized(R0=2.5, rho=0.04/7, p=0.4, theta=1)
out3 <- simulate_model_generalized(R0=2.5, rho=0.04/7, p=0.4, theta=0.4)

g1 <- ggplot(out1) +
  geom_line(aes(time, Iu*100, col="$I_u$", lty="$I_u$"), lwd=2) +
  geom_line(aes(time, Iv*100, col="$I_v$", lty="$I_v$"), lwd=2) +
  scale_y_log10("Infected (\\%)", limits=c(1e-6, 0.2)*100, expand=c(0, 0)) +
  scale_x_continuous("Time (days)", expand=c(0, 0),
                     breaks=0:7*20) +
  scale_color_manual("", values=c(cpalette[6], cpalette[2])) +
  scale_linetype_manual("", values=c(1, 2)) +
  ggtitle("A") +
  theme(
    panel.grid=element_blank(),
    legend.position = c(0.9, 0.8),
    legend.background = element_rect(fill=NA),
    axis.line = element_line(size=1),
    panel.border = element_blank()
  )

g2 <- ggplot(out2) +
  geom_line(aes(time, Iu*100, col="$I_u$", lty="$I_u$"), lwd=2) +
  geom_line(aes(time, Iv*100, col="$I_v$", lty="$I_v$"), lwd=2) +
  scale_y_log10("Infected (\\%)", limits=c(1e-6, 0.2)*100, expand=c(0, 0)) +
  scale_x_continuous("Time (days)", expand=c(0, 0),
                     breaks=0:7*20) +
  scale_color_manual("", values=c(cpalette[6], cpalette[2])) +
  scale_linetype_manual("", values=c(1, 2)) +
  ggtitle("B") +
  theme(
    panel.grid=element_blank(),
    legend.position = "none",
    axis.line = element_line(size=1),
    panel.border = element_blank()
  )

g3 <- ggplot(out3) +
  geom_line(aes(time, Iu*100, col="$I_u$", lty="$I_u$"), lwd=2) +
  geom_line(aes(time, Iv*100, col="$I_v$", lty="$I_v$"), lwd=2) +
  scale_y_log10("Infected (\\%)", limits=c(1e-6, 0.2)*100, expand=c(0, 0)) +
  scale_x_continuous("Time (days)", expand=c(0, 0),
                     breaks=0:7*20) +
  scale_color_manual("", values=c(cpalette[6], cpalette[2])) +
  scale_linetype_manual("", values=c(1, 2)) +
  ggtitle("C") +
  theme(
    panel.grid=element_blank(),
    legend.position = "none",
    axis.line = element_line(size=1),
    panel.border = element_blank()
  )

out1a <- out1 %>%
  select(time, V, Rv, Ru) %>%
  gather(key, value, -time) %>%
  mutate(
    key=factor(key, levels=c("Ru", "Rv", "V"),
               labels=c("$R_u$", "$R_v$", "$V$"))
  )

out2a <- out2 %>%
  select(time, V, Rv, Ru) %>%
  gather(key, value, -time) %>%
  mutate(
    key=factor(key, levels=c("Ru", "Rv", "V"),
               labels=c("$R_u$", "$R_v$", "$V$"))
  )

out3a <- out3 %>%
  select(time, Rv, Ru) %>%
  gather(key, value, -time) %>%
  mutate(
    key=factor(key, levels=c("Ru", "Rv", "V"),
               labels=c("$R_u$", "$R_v$", "$V$"))
  )

g4 <- ggplot(out1a) +
  geom_area(aes(time, value*100, fill=key)) +
  geom_hline(yintercept=tail(out1$Ru+out1$V+out1$Rv, 1)*100, lty=2, lwd=2) +
  scale_x_continuous("Time (days)", expand=c(0, 0),
                     breaks=0:7*20) +
  scale_y_continuous("Seropositive (\\%)", limits=c(0, 1)*100, expand=c(0, 0)) +
  scale_fill_manual(values=c(cpalette[4], cpalette[7], cpalette[5])) +
  ggtitle("D") +
  theme(
    panel.grid = element_blank(),
    legend.title = element_blank(),
    legend.position = c(0.15, 0.60),
    legend.background = element_rect(fill=NA),
    axis.line = element_line(size=1),
    panel.border = element_blank()
  )

g5 <- ggplot(out2a) +
  geom_area(aes(time, value*100, fill=key)) +
  geom_hline(yintercept=tail(out2$Ru+out2$V+out2$Rv, 1)*100, lty=2, lwd=2) +
  scale_x_continuous("Time (days)", expand=c(0, 0),
                     breaks=0:7*20) +
  scale_y_continuous("Seropositive (\\%)", limits=c(0, 1)*100, expand=c(0, 0)) +
  scale_fill_manual(values=c(cpalette[4], cpalette[7], cpalette[5])) +
  ggtitle("E") +
  theme(
    panel.grid = element_blank(),
    legend.title = element_blank(),
    legend.position = "none",
    axis.line = element_line(size=1),
    panel.border = element_blank()
  )

g6 <- ggplot(out3a) +
  geom_area(aes(time, value*100, fill=key)) +
  geom_hline(yintercept=tail(out3$Ru+out3$Rv, 1)*100, lty=2, lwd=2) +
  scale_x_continuous("Time (days)", expand=c(0, 0),
                     breaks=0:7*20) +
  scale_y_continuous("Seropositive (\\%)", limits=c(0, 1)*100, expand=c(0, 0)) +
  scale_fill_manual(values=c(cpalette[4], cpalette[7], cpalette[5])) +
  ggtitle("F") +
  theme(
    panel.grid = element_blank(),
    legend.title = element_blank(),
    legend.position = "none",
    axis.line = element_line(size=1),
    panel.border = element_blank()
  )

gtot1 <- egg::ggarrange(g1, g4, nrow=2, draw=FALSE)
gtot1a <- annotate_figure(gtot1, top=text_grob("Standard vaccination model", size=14))

gtot2 <- egg::ggarrange(g2, g5, nrow=2, draw=FALSE)
gtot2a <- annotate_figure(gtot2, top=text_grob("Immune boosting model", size=14))

gtot3 <- egg::ggarrange(g3, g6, nrow=2, draw=FALSE)
gtot3a <- annotate_figure(gtot3, top=text_grob("Polarized vaccination model", size=14))

tikz(file = "figure_simulation_generalized.tex", width = 12, height = 6, standAlone = T)
grid.arrange(gtot1a, gtot2a, gtot3a, ncol=3)
dev.off()
tools::texi2dvi('figure_simulation_generalized.tex', pdf = T, clean = T)
