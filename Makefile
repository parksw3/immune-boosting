## This is immune-boosting; Daniel's version of the immune-status unification model

current: target
-include target.mk
Ignore = target.mk

# -include makestuff/perl.def

vim_session:
	bash -cl "vmt"

######################################################################

Sources += $(wildcard *.R)

autowrapR = defined

## immune_boosting.pdf: immune_boosting.tex

Ignore += figure_simulation_generalized.tex
Ignore += figure_simulation_generalized.pdf

figure_simulation_generalized.tex: figure_simulation_compare.Rout ;
figure_simulation_generalized.pdf: figure_simulation_generalized.Rout ;
figure_simulation_generalized_vaccinated.pdf: figure_simulation_generalized.Rout ;

figure_simulation_compare.Rout: figure_simulation_compare.R

figure_simulation_effectiveness.pdf: figure_simulation_effectiveness.Rout ;

######################################################################

## Haven't tracked this logic yet

Ignore += figure_simulation_compare.tex figure_simulation_effectiveness.tex figure_simulation_generalized_unvaccinated.tex figure_simulation_generalized_vaccinated.tex
Ignore += *.deps.out

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
