
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Mapping the global distribution of seabird populations
## R script to aggregate results into a 5x5 degree grid
## Beth Clark 
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

library(tidyverse)

rm(list=ls()) 

######### GENERAL DIRECTIONS AND FILES ##############

## paste home directory here
dir <- paste0("C:/Users/bethany.clark/OneDrive - BirdLife International/",
              "Methods") 

dat <- read.csv(paste0(dir, "/outputs/05_exposure_scores_by_month.csv"))  

head(dat)

#combine by population

pop_exposure <- dat %>%
  group_by(sp_pop) %>%
  summarise(species = species[1],
    pop_exposure = mean(exposure_score)) %>%
  data.frame() ; pop_exposure

#combine by season
#need to bring in breeding schedules

#combine by species
#need to bring in breeding population sizes


