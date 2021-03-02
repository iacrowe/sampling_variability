library(tidyverse)

set.seed(44)

# Single simulated trial, as per single_simulated_trial.R

nSim <- 120 # number of placebo-treated trial participants


TrialReplicate <- function(MeanFpr, SDFpr, PropSampleLess, PropSampleStable) {
  CohortSample <- tibble(sampleVar = runif(nSim)) %>%
    mutate(progRate = rgamma(nSim, 
                             shape = (MeanFpr/SDFpr)^2, 
                             rate = MeanFpr/SDFpr^2)) %>%
    mutate(varProg = sampleVar + progRate) %>%
    mutate(fibStageVar = cut(sampleVar, 
                             breaks = c(-Inf, PropSampleLess, 
                                        PropSampleLess + PropSampleStable, 
                                        +Inf),
                             labels = c("Improved", 
                                        "No change", 
                                        "Worsened"))) %>%
    mutate(fibStageVarProg = cut(varProg, 
                                 breaks = c(-Inf, PropSampleLess, 
                                            PropSampleLess + PropSampleStable, 
                                            +Inf),
                                 labels = c("Improved", 
                                            "No change", 
                                            "Worsened"))
    )
  
  OutputSim <- CohortSample %>% 
    group_by(fibStageVarProg) %>% 
    count(fibStageVarProg)
  
  return(OutputSim)
}


## This can now be replicated to illustrate variation between trials

nRep <- 1000 # number of simulated trials

multiple_trials <- 
  replicate(
    nRep, 
    TrialReplicate(
      MeanFpr = rnorm(1, 0.075, 0.02),
       SDFpr = rnorm(1, 0.075, 0.005),
       PropSampleLess = rnorm(1, 0.296, 0.04),
       PropSampleStable = rnorm(1, 0.557, 0.04)),
    simplify = FALSE)

multiple_trials <- 
  bind_rows(multiple_trials) %>%
  mutate(FibChange = n/nSim * 100)


multiple_trials_out <- 
  multiple_trials %>%
  rename(
    fibrosis_change = fibStageVarProg) %>%
  group_by(fibrosis_change) %>%
  arrange(FibChange) %>%
  summarise(
    average = mean(FibChange),
    tenth_percentile = nth(FibChange, nRep * 0.10),
    ninetyth_percentile = nth(FibChange, nRep * 0.90))



## Summary outputs

knitr::kable(multiple_trials_out, "simple")

violin_plot_wide <-
  ggplot(
    multiple_trials %>% drop_na(), 
    aes(fibStageVarProg, FibChange, colour = fibStageVarProg)) +
  geom_violin(size = 0.75) +
  geom_jitter(size = 0.75, alpha = 0.3) +
  scale_colour_manual(values = c("#4B644B", "#647D96", "#222B4C")) +
  coord_flip() +
  xlab("") +
  ylab("% participants") +
  ylim(c(0,80)) +
  theme_classic() +
  theme(legend.position = "none") +
  ggtitle("Fibrosis stage change in 1000 simulated trials of 120 placebo treated participants with NASH")

violin_plot_wide
