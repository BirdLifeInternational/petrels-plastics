## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Mapping the global distribution of seabird populations
## R script to aggregate results into a 5x5 degree grid
## Ana Carneiro and Anne-Sophie Bonnet-Lebrun
## July 2018
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#Adapted by Beth Clark Mar 2020 for overlap with 1x1 degree plastics data
rm(list=ls()) 

################ LOADING PACKAGES ###################

#library(sp)
library(raster)
library(rgdal)
#library(cowplot)
#library(viridis)
#library(stringr)
#library(RColorBrewer)
#library(geomerge)
#library(maptools)
#library(gridExtra)
#library(sf)
#library(tidyverse)
#library(rgeos)

######### GENERAL DIRECTIONS AND FILES ##############

## paste home directory here
dir <- paste0("C:/Users/bethany.clark/OneDrive - BirdLife International/",
              "Methods") 

## DIRECTION TO YOUR RASTERS 
dir_1by1 <- paste0(dir,"/outputs/05_aggregate_1by1_grid")

land <- readOGR(dsn=paste0(dir,"/input_data/baselayer"), layer = "world-dissolved") 

pops <- read.csv(paste0(dir,"/outputs/04_phenology.csv"))
head(pops)

#combine into maps per population ####
dir_out <- paste0(dir,"/outputs/06_combine_by_population")
dir.create(dir_out)

pops$density_sum <- NA

#42 failed "Fulmarus glacialis_Bjørnøya"

for (i in 43:nrow(pops)){
  
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
  raster_name <- paste0(dir,"/outputs/06_combine_by_population/",pops$species_pop[i],".tif")
  writeRaster(rast_sum, filename=raster_name, format="GTiff", overwrite=TRUE)
  
  rast_sum[is.na(rast_sum)] <- 0
  density_sum <- sum(getValues(rast_sum))
  pops$density_sum[i] <- density_sum
  
  print(pops$species_pop[i])
  print(i)
}

range(pops$density_sum)
#the ones with low numbers are seem to be high lat N species with NAs in the plastic data




