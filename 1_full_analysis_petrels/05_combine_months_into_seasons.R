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

## paste home directory here
dir <- paste0("C:/Users/bethany.clark/OneDrive - BirdLife International/",
              "Methods") 

land <- readOGR(dsn=paste0(dir,"/input_data/baselayer"), layer = "world-dissolved") 

## DIRECTION TO YOUR RASTERS 
dir_1by1 <- paste0(dir,"/outputs/04_aggregate_1by1_grid")

dat <- read.csv(paste0(dir,"/outputs/04_exposure_scores_by_month.csv"))  
head(dat)

#read in phenology data
pops <- read.csv(paste0(dir,"/outputs/06_phenology.csv"))
head(pops)




#combine by population
pop_exposure <- dat %>%
  group_by(sp_pop) %>%
  summarise(species = species[1],
            population = population[1],
            n_months = n(),
            population_exposure = mean(exposure_score)) %>%
  data.frame() ; head(pop_exposure)

#combine into maps per population ####
dir_out <- paste0(dir,"/outputs/05_populations")
dir.create(dir_out)

pop_exposure$density_sum <- NA

#42 failed "Fulmarus glacialis_Bjørnøya"
pop_exposure$sp_pop <- ifelse(pop_exposure$sp_pop == "Fulmarus glacialis_Bjørnøya","Fulmarus glacialis_Bjornoya",pop_exposure$sp_pop)

write.csv(pop_exposure,paste0(dir,"/outputs/05_exposure_scores_by_population.csv"),
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
#the ones with low numbers are seem to be high lat N species with NAs in the plastic data




