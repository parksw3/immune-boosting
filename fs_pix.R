library(dplyr)
library(tidyr)
library(deSolve)
library(ggplot2); theme_set(theme_bw(base_size=14))
library(ggpubr)
library(gridExtra, warn.conflicts=FALSE)
library(tikzDevice)

library(shellpipes)
startGraphics()

## This should be upstream, but not resimulating for now
## Make a single list of q levels
## 2023 Nov 07 (Tue)

resdata <- rdsRead()

g1 <- (ggplot(resdata)
	+ geom_line(aes(VE, finalsizeV, col=factor(q)), lwd=1)
	+ scale_x_continuous("Vaccine efficacy"
		, limits=c(0, 1), breaks=c(0, 0.2, 0.4, 0.6, 0.8, 1)
	)
	+ scale_y_continuous("Proportion infected", limits=c(0, 1))
	+ scale_color_viridis_d("Boosting proportion", begin=1, end=0)
	+ theme(legend.position = "bottom")
)

print(g1
	%+% (resdata %>% filter(R0==8 & scenario=="Leaky"))
	+ ggtitle("Leaky")
)

print(g1
	%+% (resdata %>% filter(R0==8 & scenario=="Mixed"))
	+ ggtitle("Mixed")
)

print(g1
	%+% (resdata %>% filter(R0==8 & scenario=="Polarized"))
	+ ggtitle("Polarized")
)
