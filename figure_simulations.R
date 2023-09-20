library(dplyr)
library(tidyr)
library(ggplot2); theme_set(theme_bw(base_size=16))
library(ggpubr)
library(gridExtra)
library(tikzDevice)

## sessionInfo()
## quit()

library(shellpipes)
loadEnvironments()
startGraphics(otype="tikz", standAlone = T)

viridis::viridis(4, begin=0.1, alpha=0.4)

g1 <- ggplot(out1) +
  geom_line(aes(time, inc_s, col="Unvaccinated", lty="Unvaccinated"), lwd=2) +
  geom_line(aes(time, inc_v, col="Vaccinated", lty="Vaccinated"), lwd=2) +
  scale_y_log10("Incidence (1/day)", limits=c(1e-6, 0.05), expand=c(0, 0)) +
  scale_x_continuous("Time (days)", expand=c(0, 0)) +
  scale_color_manual("", values=c(cpalette[6], cpalette[2])) +
  scale_linetype_manual("", values=c(1, 2)) +
  theme(
    panel.grid=element_blank(),
    legend.position = c(0.8, 0.8),
    legend.background = element_rect(fill=NA),
    axis.line = element_line(size=1),
    panel.border = element_blank()
  )

g2 <- ggplot(out2) +
  geom_line(aes(time, inc_s, col="Unvaccinated", lty="Unvaccinated"), lwd=2) +
  geom_line(aes(time, inc_v, col="Vaccinated", lty="Vaccinated"), lwd=2) +
  scale_y_log10("Incidence (1/day)", limits=c(1e-6, 0.05), expand=c(0, 0)) +
  scale_x_continuous("Time (days)", expand=c(0, 0)) +
  scale_color_manual("", values=c(cpalette[6], cpalette[2])) +
  scale_linetype_manual("", values=c(1, 2)) +
  theme(
    panel.grid=element_blank(),
    legend.position = c(0.8, 0.8),
    legend.background = element_rect(fill=NA),
    axis.line = element_line(size=1),
    panel.border = element_blank()
  )

g3 <- ggplot(out3) +
  geom_line(aes(time, inc_s, col="Unvaccinated", lty="Unvaccinated"), lwd=2) +
  geom_line(aes(time, inc_v, col="Vaccinated", lty="Vaccinated"), lwd=2) +
  scale_y_log10("Incidence (1/day)", limits=c(1e-6, 0.05), expand=c(0, 0)) +
  scale_x_continuous("Time (days)", expand=c(0, 0)) +
  scale_color_manual("", values=c(cpalette[6], cpalette[2])) +
  scale_linetype_manual("", values=c(1, 2)) +
  theme(
    panel.grid=element_blank(),
    legend.position = c(0.8, 0.8),
    legend.background = element_rect(fill=NA),
    axis.line = element_line(size=1),
    panel.border = element_blank()
  )

print(g1)
print(g2)
print(g3)

out1a <- out1 %>%
  select(time, V, Rv, Ru) %>%
  gather(key, value, -time) %>%
  mutate(
    key=factor(key, levels=c("Ru", "Rv", "V"),
               labels=c("$R_u$", "$R_v$", "$S_v$"))
  )

out2a <- out2 %>%
  select(time, Rv, Ru) %>%
  gather(key, value, -time) %>%
  mutate(
    key=factor(key, levels=c("Ru", "Rv", "V"),
               labels=c("$R_u$", "$R_v$", "$S_v$"))
  )

out3a <- out3 %>%
  select(time, V, Rv, Ru) %>%
  gather(key, value, -time) %>%
  mutate(
    key=factor(key, levels=c("Ru", "Rv", "V"),
               labels=c("$R_u$", "$R_v$", "$S_v$"))
  )

g4 <- ggplot(out1a) +
  geom_area(aes(time, value*100, fill=key)) +
  geom_hline(yintercept=tail(out1$Ru+out1$V+out1$Rv, 1)*100, lty=2, lwd=2) +
  scale_x_continuous("Time (days)", expand=c(0, 0),
                     breaks=0:7*20) +
  scale_y_continuous("Seropositive (\\%)", limits=c(0, 1)*100, expand=c(0, 0)) +
  scale_fill_manual(values=c(cpalette[4], cpalette[7], cpalette[5])) +
  theme(
    panel.grid = element_blank(),
    legend.title = element_blank(),
    legend.position = c(0.15, 0.70),
    legend.background = element_rect(fill=NA),
    axis.line = element_line(size=1),
    panel.border = element_blank()
  )

g5 <- ggplot(out2a) +
  geom_area(aes(time, value*100, fill=key)) +
  geom_hline(yintercept=tail(out2$Ru+out2$Rv, 1)*100, lty=2, lwd=2) +
  scale_x_continuous("Time (days)", expand=c(0, 0),
                     breaks=0:7*20) +
  scale_y_continuous("Seropositive (\\%)", limits=c(0, 1)*100, expand=c(0, 0)) +
  scale_fill_manual(values=c(cpalette[4], cpalette[7], cpalette[5])) +
  theme(
    panel.grid = element_blank(),
    legend.title = element_blank(),
    legend.position = "none",
    axis.line = element_line(size=1),
    panel.border = element_blank()
  )

g6 <- ggplot(out3a) +
  geom_area(aes(time, value*100, fill=key)) +
  geom_hline(yintercept=tail(out3$Ru+out3$V+out3$Rv, 1)*100, lty=2, lwd=2) +
  scale_x_continuous("Time (days)", expand=c(0, 0),
                     breaks=0:7*20) +
  scale_y_continuous("Seropositive (\\%)", limits=c(0, 1)*100, expand=c(0, 0)) +
  scale_fill_manual(values=c(cpalette[4], cpalette[7], cpalette[5])) +
  theme(
    panel.grid = element_blank(),
    legend.title = element_blank(),
    legend.position = "none",
    axis.line = element_line(size=1),
    panel.border = element_blank()
  )
