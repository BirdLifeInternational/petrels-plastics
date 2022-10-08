# Test correlation between exposure scores using high and low freq tracking data
#Beth Clark Oct 2022

library(dplyr)

rm(list=ls()) 

lowfreq <- read.csv("input_data/05_exposure_scores_by_population.csv")

highfreq <- read.csv("outputs/05_exposure_scores_by_month_highfreq.csv")

head(lowfreq)
head(highfreq)

lowfreq$sp_pop <- ifelse(lowfreq$sp_pop == "Pterodroma gouldi_Bethells Beach","Pterodroma gouldi_North Island",lowfreq$sp_pop)

pop_exposure <- highfreq %>%
  group_by(sp_pop) %>%
  summarise(species = species[1],
            population = population[1],
            n_months = n(),
            highfreq_exp = mean(exposure_score)) %>%
  data.frame() ; head(pop_exposure)

pop_exposure$lowfreq_exp <- lowfreq$population_exposure[match(pop_exposure$sp_pop,lowfreq$sp_pop)]
head(pop_exposure)

hist(pop_exposure$highfreq_exp) #not normal distribution, so should use kendall correlation

plot(pop_exposure$lowfreq_exp, pop_exposure$highfreq_exp)
cor.test(pop_exposure$lowfreq_exp, pop_exposure$highfreq_exp, method = "kendall")

write.csv(pop_exposure,"outputs/06_high_low_freq_population_exposure_scores.csv",
          row.names = F)




