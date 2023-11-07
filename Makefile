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

## immune_boosting.tex.669d96b8.oldfile:
## immune_boosting.ld.pdf: immune_boosting.tex

######################################################################

Sources += $(wildcard *.R)

autopipeR = defined

simulation_compare.Rout: simulation_compare.R simulate.rda
## figure_simulation_compare.Rout.tikz.pdf: figure_simulation_compare.R
figure_simulation_compare.Rout: figure_simulation_compare.R simulation_compare.rda color_palette.rda

## Should be possible to refactor this with the paper pipeline
## figure_simulations.Rout.tikz.pdf: figure_simulations.R 
figure_simulations.Rout: figure_simulations.R simulation_compare.rda color_palette.rda

figure_simulation_efficacy.Rout: figure_simulation_efficacy.R simulate.rda color_palette.rda

## efficacy_panels.Rout.tikz.pdf: efficacy_panels.R 
efficacy_panels.Rout: efficacy_panels.R simulate.rda color_palette.rda

######################################################################

figure_simulation_generalized.Rout: figure_simulation_generalized.R simulate.rda color_palette.rda

figure_simulation_generalized.Rout.%.tikz: figure_simulation_generalized.Rout ;

######################################################################

figure_diagram_comb.pdf: figure_diagram_comb.tex

######################################################################

## Dushoff talk figures

# figure_simulation_generalized.Rout.vaccinated.tikz.pdf: figure_simulation_generalized.R

fs_sims.Rout: fs_sims.R simulate.rda

fs_pix.Rout: fs_pix.R fs_sims.rds simulate.rda color_palette.rda
## fs_pix.Rout.tikz.pdf: fs_pix.R

## figure_diagram_comb.pdf: figure_diagram_comb.tex
Sources += leaky.tex polarized.tex boosting.tex full.tex
## leaky.pdf: leaky.tex
## polarized.pdf: polarized.tex
## boosting.pdf: boosting.tex
## full.pdf: full.tex


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
