## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Combining rasters and scores for each month into averages by population
## Beth Clark 2022
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

rm(list=ls()) 

################ LOADING PACKAGES ###################

library(raster)
library(rgdal)
library(tidyverse)

######### GENERAL DIRECTIONS AND FILES ##############

## DIRECTION TO RASTERS 
dir_1by1 <- "outputs/04_aggregate_1by1_grid"

dat <- read.csv("outputs/04_exposure_scores_by_month.csv")
head(dat)
summary(dat$exposure_score)

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

#42 failed "Fulmarus glacialis_Bj?rn?ya"
pop_exposure$sp_pop <- ifelse(pop_exposure$sp_pop == "Fulmarus glacialis_Bj?rn?ya","Fulmarus glacialis_Bjornoya",pop_exposure$sp_pop)

write.csv(pop_exposure,"outputs/05_exposure_scores_by_population.csv",
          row.names = F)  

#Write out bird location distribution rasters for each population

for (i in 1:nrow(pop_exposure)){
  
  months <- list.files(dir_1by1, pattern = paste0(pop_exposure$sp_pop[i], ".*\\.tif$"))  # updated to match only the .tif, not the .tif.aux.xml file.
  
  for(j in 1:length(months)){
    a <- raster::raster(paste0(dir_1by1,"/",months[j]))
    if(j == 1){rast_sum <- a
    count <- 1
    } else { rast_sum <- a + rast_sum
    count <- count + 1
    }
  }
  rast_sum <- rast_sum/count
  raster_name <- paste0(dir_out,"/",pop_exposure$sp_pop[i],".tif")
  raster::writeRaster(rast_sum, filename=raster_name, format="GTiff", overwrite=TRUE)
  
  rast_sum[is.na(rast_sum)] <- 0
  density_sum <- sum(raster::getValues(rast_sum))
  pop_exposure$density_sum[i] <- density_sum
  
  print(pop_exposure$sp_pop[i])
  print(i)
}

range(pop_exposure$density_sum)
#the ones with low numbers are seem to be high lat N species with NAs in the plastic data




