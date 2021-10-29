## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Mapping the global distribution of seabird populations
## R script to aggregate results into a 5x5 degree grid
## Ana Carneiro and Anne-Sophie Bonnet-Lebrun
## July 2018
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#Adapted by Beth Clark Mar 2020 for overlap with 1x1 degree plastics data
rm(list=ls()) 

################ LOADING PACKAGES ###################

library(sp)
library(raster)
library(rgdal)
library(cowplot)
library(viridis)
library(stringr)
library(RColorBrewer)
library(geomerge)
library(maptools)
library(gridExtra)
library(sf)
library(tidyverse)
library(rgeos)

######### GENERAL DIRECTIONS AND FILES ##############

## GENERAL DIR
dir <- paste0("C:/Users/bethany.clark/OneDrive - BirdLife International/",
              "Methods") ## copy and paste here your working directory

## DIRECTION TO YOUR RASTERS (ALL DEM CLASSES COMBINED AND BY YEAR QUARTER)
#dir_demClasses <- paste0(dir,"/scripts_results/09_sum_demClasses")
land <- readOGR(dsn=paste0(dir,"/baselayer"), layer = "world-dissolved") #Changed - BC  

pops <- read.csv(paste0(dir,"/scripts_results/05_phenology/pops.csv"))


#combine into maps per population ####
dir_1by1 <- paste0(dir,"/scripts_results/04_aggregate_1by1_grid")

pops$density_sum <- NA

for (i in 1:nrow(pops)){
  
  months <- list.files(dir_1by1, pattern=pops$species_pop[i])
  
  for(j in 1:length(months)){
    a <- raster(paste0(dir_1by1,"/",months[j]))
    if(j == 1){rast_sum <- a
    count <- 1
    } else { rast_sum <- a + rast_sum
    count <- count + 1
    }
  }
  rast_sum <- rast_sum/count
  raster_name <- paste0(dir,"/scripts_results/06_pops/",pops$species_pop[i],".tif")
  writeRaster(rast_sum, filename=raster_name, format="GTiff", overwrite=TRUE)
  
  rast_sum[is.na(rast_sum)] <- 0
  density_sum <- sum(getValues(rast_sum))
  pops$density_sum[i] <- density_sum
  
  print(pops$species_pop[i])
  print(i)
}

range(pops$density_sum)
#the ones with low numbers are seem to be high lat N species with NAs in the plastic data




