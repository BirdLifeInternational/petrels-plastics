## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Combining rasters and scores for each month into averages per season per population
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
files <- list.files(dir_1by1, full.names = TRUE,pattern="tif"); head(files)

dir_seasons <- paste0(dir,"/outputs/07_seasons")
dir.create(dir_seasons)
dir.create(paste0(dir_seasons,"/maps/")) 
dir.create(paste0(dir_seasons,"/maps_pdf/")) 

dat <- read.csv(paste0(dir,"/outputs/04_exposure_scores_by_month.csv"))  
head(dat)

#read in phenology data
pops <- read.csv(paste0(dir,"/outputs/06_phenology.csv"))
head(pops)
pops$br_n <- NA
pops$nonbr_n <- NA

#Write out bird location distribution rasters for each season
dir_out <- paste0(dir,"/outputs/05_populations")
dir.create(dir_out)

for (i in 1:nrow(pops)){
  
  pop <- pops$species_pop[i];pop
  
  br <- strsplit(pops$breeding[i],"_")[[1]]
  nonbr <- strsplit(pops$nonbreeding[i],"_")[[1]]
  
  br_rasters_possible <- paste0(dir_1by1,"/",pop,"_",br,".tif")
  nonbr_rasters_possible <- paste0(dir_1by1,"/",pop,"_",nonbr,".tif")
  
  br_rasters <- br_rasters_possible[br_rasters_possible %in% files]
  nonbr_rasters <- nonbr_rasters_possible[nonbr_rasters_possible %in% files]
  
  for(j in 1:length(br_rasters)){
    if(br_rasters[j] %in% files){
      a <- raster(br_rasters[j])
      if(j==1){         
        br_rast_sum <- a
        count <- 1
      } else {
        br_rast_sum <- a + br_rast_sum
        count <- count + 1
      }
      
    }
  }  
  pops$br_n[i] <- count
  br_rast_mean <- br_rast_sum / count
  count <- 0
  
  if(is.na(pops$nonbreeding[i])){
    print("no nonbr")
  } else {
    for(j in 1:length(nonbr_rasters)){
      if(nonbr_rasters[j] %in% files){
        a <- raster(nonbr_rasters[j])
        if(j == 1){
          nonbr_rast_sum <- a
          count <- 1
        } else {
          nonbr_rast_sum <- a + nonbr_rast_sum
          count <- count + 1
        }
      }
    }
  }
  pops$nonbr_n[i] <- count
  nonbr_rast_mean <- nonbr_rast_sum / count
  count <- 0  
  writeRaster(br_rast_mean, filename=paste0(dir_seasons,"/" ,pop,"_br.tif"), format="GTiff", overwrite=TRUE)
  writeRaster(nonbr_rast_mean, filename=paste0(dir_seasons,"/" ,pop,"_nonbr.tif"), format="GTiff", overwrite=TRUE)
  
  print(pops$species_pop[i])
  print(i)
}

range(pops$density_sum)
#the ones with low numbers are seem to be high lat N species with NAs in the plastic data



#combine by season
season_exposure <- dat %>%
  group_by(sp_pop) %>%
  summarise(species = species[1],
            population = population[1],
            n_months = n(),
            population_exposure = mean(exposure_score)) %>%
  data.frame() ; head(season_exposure)

#combine into maps per population ####
dir_out <- paste0(dir,"/outputs/05_populations")
dir.create(dir_out)

season_exposure$density_sum <- NA

#42 failed "Fulmarus glacialis_Bjørnøya"
pop_exposure$sp_pop <- ifelse(pop_exposure$sp_pop == "Fulmarus glacialis_Bjørnøya","Fulmarus glacialis_Bjornoya",pop_exposure$sp_pop)

write.csv(season_exposure,paste0(dir,"/outputs/05_exposure_scores_by_population.csv"),
          row.names = F)  



