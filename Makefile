## This is immune-boosting; Daniel's version of the immune-status unification model

current: target
-include target.mk
Ignore = target.mk

## pardirs = ""

# -include makestuff/perl.def

vim_session:
	bash -cl "vmt"

######################################################################

Sources += immune_boosting.tex
## immune_boosting.pdf: immune_boosting.tex

######################################################################

Sources += $(wildcard *.R)

autopipeR = defined

simulation_compare.Rout: simulation_compare.R simulate.rda
figure_simulation_compare.Rout: figure_simulation_compare.R simulation_compare.rda color_palette.rda

figure_simulation_efficacy.Rout: figure_simulation_efficacy.R simulate.rda color_palette.rda

figure_simulation_generalized.Rout: figure_simulation_generalized.R simulate.rda color_palette.rda

figure_simulation_generalized.Rout.%.tikz: figure_simulation_generalized.Rout ;

######################################################################

### Makestuff

Sources += Makefile

Ignore += makestuff
msrepo = https://github.com/dushoff

Makefile: makestuff/00.stamp
makestuff/%.stamp:
	- $(RM) makestuff/*.stamp
	(cd makestuff && $(MAKE) pull) || git clone $(msrepo)/makestuff
	touch $@

-include makestuff/os.mk

-include makestuff/pipeR.mk
-include makestuff/texi.mk

-include makestuff/git.mk
-include makestuff/visual.mk
