library(dplyr)
library(tidyr)
library(deSolve)
library(ggplot2); theme_set(theme_bw(base_size=14))
library(ggpubr)
library(gridExtra, warn.conflicts=FALSE)
library(tikzDevice)

library(shellpipes)
startGraphics()

resdata <- (rdsRead()
	|> mutate(prot = 1-finalsizeV/max(finalsizeV))
)

g1 <- (ggplot(resdata)
	+ geom_line(aes(VE, prot, col=factor(q)), lwd=1)
	+ scale_x_continuous("Cross immunity"
		, limits=c(0, 1), breaks=c(0, 0.2, 0.4, 0.6, 0.8, 1)
	)
	+ scale_y_continuous("Protection", limits=c(0, 1))
	+ scale_color_viridis_d("Boosting proportion", begin=1, end=0)
	+ theme(legend.position = "bottom")
)

print(g1
	%+% (resdata %>% filter(R0==8 & scenario=="Leaky"))
)

