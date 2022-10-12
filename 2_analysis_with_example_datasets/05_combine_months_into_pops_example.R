## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Combining rasters and scores for each month into averages by population
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

rm(list=ls()) 

################ LOADING PACKAGES ###################

library(raster)
library(rgdal)
library(tidyverse)

######### GENERAL DIRECTIONS AND FILES ##############

## DIRECTION TO YOUR RASTERS 
dir_1by1 <- "outputs/04_aggregate_1by1_grid"

dat <- read.csv("outputs/04_exposure_scores_by_month.csv") 
head(dat)

#combine by population

pop_exposure <- dat %>%
  group_by(sp_pop) %>%
  summarise(species = species[1],
            population = population[1],
            n_months = n(),
            population_exposure = mean(exposure_score)) %>%
  data.frame() ; head(pop_exposure)

#combine into maps per population ####
dir_out <- "outputs/05_populations"
dir.create(dir_out)

pop_exposure$density_sum <- NA

write.csv(pop_exposure,"outputs/05_exposure_scores_by_population.csv",
          row.names = F)  

#Write out bird location distribution rasters for each population

for (i in 1:nrow(pop_exposure)){
  
  months <- list.files(dir_1by1, pattern = pop_exposure$sp_pop[i])
  
  for(j in 1:length(months)){
    a <- raster(paste0(dir_1by1,"/",months[j]))
    if(j == 1){rast_sum <- a
    count <- 1
    } else { rast_sum <- a + rast_sum
    count <- count + 1
    }
  }
  rast_sum <- rast_sum/count
  raster_name <- paste0(dir_out,"/",pop_exposure$sp_pop[i],".tif")
  writeRaster(rast_sum, filename=raster_name, format="GTiff", overwrite=TRUE)
  
  rast_sum[is.na(rast_sum)] <- 0
  density_sum <- sum(getValues(rast_sum))
  pop_exposure$density_sum[i] <- density_sum
  
  print(pop_exposure$sp_pop[i])
  print(i)
}

range(pop_exposure$density_sum)



