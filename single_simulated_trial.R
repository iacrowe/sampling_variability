library(tidyverse)

set.seed(796)

# Define function

## Included variables:
### MeanFpr, mean fibrosis progression rate (FPR, stages / year)
### SDFpr, standard deviation fibrosis progression rate (stages / year)

### PropSampleLess, proportion of persons with apparent improvement in simultaneous biopsy study
### PropSampleStable, proportion of persons with no change in fibrosis in simultaneous biopsy study

### nSim, number of simulated persons in trial

TrialReplicate <- function(MeanFpr, SDFpr, PropSampleLess, PropSampleStable) {
  CohortSample <- 
    tibble(sampleVar = runif(nSim)) %>% # random distribution to simulate sampling variability
    mutate(
      progRate = rgamma(nSim, 
                        shape = (MeanFpr/SDFpr)^2, 
                        rate = MeanFpr/SDFpr^2)) %>% # simulates FPR values with mean and SD defined
    mutate(
      varProg = sampleVar + progRate) %>% # addition of sampling variability to FPR
    mutate(
      sampl_var = cut(sampleVar, 
                        breaks = c(-Inf, 
                                   PropSampleLess, 
                                   PropSampleLess + PropSampleStable, 
                                   +Inf),
                        labels = c("Improved", 
                                   "No change", 
                                   "Worsened"))) %>% # delta fibrosis due to sampling
    mutate(sampl_var_prog = cut(varProg, 
                                 breaks = c(-Inf, 
                                            PropSampleLess, 
                                            PropSampleLess + PropSampleStable, 
                                            +Inf),
                                 labels = c("Improved", 
                                            "No change", 
                                            "Worsened"))) # delta fibrosis sampling plus FPR
  
  SamplVarOutput <- 
    CohortSample %>% 
    group_by(sampl_var) %>% 
    count(sampl_var) %>%
    transmute(
      sampl_var_only = n) 
  
  SamplVarProgOutput <- 
    CohortSample %>% 
    group_by(sampl_var_prog) %>% 
    count(sampl_var_prog) %>%
    transmute(sampl_var_plus_prog = n)
  
  OutputSim <- 
    left_join(
      SamplVarOutput, SamplVarProgOutput, by = c("sampl_var" = "sampl_var_prog")) %>%
    rename(
      fibrosis_change = sampl_var
    )
    
  
  return(OutputSim)
}


## Example trial

nSim <- 120

MeanFpr <- 0.05 * 1.5   # Trial duration 18 months:  annual FPR * 1.5
SDFpr <- 0.05 * 1.5
PropSampleLess <- 0.296 
PropSampleStable <- 0.557

TrialSim <- TrialReplicate(MeanFpr, SDFpr, PropSampleLess, PropSampleStable)

TrialSim



