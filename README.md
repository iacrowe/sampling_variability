# Sampling variability in liver histology in NASH

This repo provides sample analytic code to recreate the simulations done to support an evaluation of the importance of sampling variability in the evaluation of liver histology for patients with NASH.

single_simulated_trial.R provides annotated code to explain how the simulations are built.  This can be explored by changing the proportion of patients apparently improving fibrosis due to sampling variability and changing the rate of fibrosis progression.

multiple_simulated_trials.R is an extension of this where trials are replicated to illustrate the distribution of outcomes for any given set of parameters.  In addition to changing the parameters above, the impact of changing trial participant number can be explored here.