## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Mapping the global distribution of seabird populations
## R script to aggregate results into a 5x5 degree grid
## Ana Carneiro and Anne-Sophie Bonnet-Lebrun
## July 2018
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#Adapted by Beth Clark Mar 2020 for overlap with 1x1 degree plastics data
rm(list=ls()) 

# Maps of all kernels combined for Win Cowger

################ LOADING PACKAGES ###################

library(raster)
library(rgdal)
library(cowplot)
library(viridis)
library(stringr)
library(tidyverse)
library(ggplot2)
library(RColorBrewer)

######### GENERAL DIRECTIONS AND FILES ##############

## GENERAL DIR
dir <- paste0("C:/Users/bethany.clark/OneDrive - BirdLife International/",
              "Methods") ## copy and paste here your working directory

## DIRECTION TO YOUR RASTERS (ALL DEM CLASSES COMBINED AND BY YEAR QUARTER)
land <- readOGR(dsn=paste0(dir,"/baselayer"), layer = "world-dissolved") #Changed - BC  

output <- paste0(dir,"/scripts_results/02_pops")

land <- readOGR(dsn=paste0(dir,"/baselayer"), layer = "world-dissolved")  ## changed - BC

dir_demClasses <- paste0(dir,"/scripts_results/06_pops")
files <- list.files(dir_demClasses, pattern="tif");files

####### CONVERT INTO A 1X1 DEGREE RESOLUTION ########

pops <- read.csv(paste0(dir,"/scripts_results/05_phenology/pops.csv"))

#add up the species ####
subset_safely <- function(x, index) {
  if (length(x) < index) {
    return(NA_character_)
  }
  x[[index]]
}
str_split_n <- function(string, pattern, n) {
  out <- str_split(string, pattern)
  vapply(out, subset_safely, character(1L), index = n)
}

pops$species <- str_split_n(pops$species_pop,"_",1)

head(pops)
pops$seasons <- ifelse(is.na(pops$nonbreeding),0.5,1)

#combine population maps into species maps, then rescale to 1 ####
#this will weight by number of tracked months
#read in pop sizes

pop_sizes <- read.csv("C:/Users/bethany.clark/OneDrive - BirdLife International/Data/population_sizes_csv.csv")
pop_sizes$site <- NULL
pop_sizes$colony <- NULL
pop_sizes$species_pop <- paste(pop_sizes$species,pop_sizes$population,sep="_")
pop_sizes$seasons <- pops$seasons[base::match(pop_sizes$species_pop,pops$species_pop)]
pop_sizes$pop_x_seasons <- pop_sizes$pop_arithmetric_mean*pop_sizes$seasons

multipop_species <- unique(pop_sizes$species)
for(i in 1:length(multipop_species)){
  species <- subset(pop_sizes,species == multipop_species[i])
  total <- sum(species$pop_x_seasons)
  species$popsize_weighting <- species$pop_x_seasons/total
  if(i == 1){
    all_sp <- species
  } else {
    all_sp <- rbind(all_sp,species)
  }
}


#match to pops
head(pops)
pops$weighting <- all_sp$popsize_weighting[match(pops$species_pop,all_sp$species_pop)]
pops$weighting <- ifelse(is.na(pops$weighting),1,pops$weighting)

write.csv(pops,paste0(dir,"/scripts_results/pop_size_weightings.csv"),row.names = F)

#what to about pops with only breeding season data????????        ######################################################

species <- unique(pops$species)
species_weights <- as.data.frame(species)
species_weights$seasons <- NA

for (i in 1:length(species)){
  
  sp_files <- list.files(dir_demClasses, pattern=species[i]);sp_files
  sp_weightings <- pops[pops$species == species[i],];sp_weightings
  
  species_weights$seasons[i] <- max(sp_weightings$seasons)
  
}

write.csv(species_weights,paste0(dir,"/species_weights_br.csv"),row.names = F)

species_weights <- read.csv(paste0(dir,"/species_weights_br.csv"))


for (i in 1:length(species)){
  
  sp_files <- list.files(dir_demClasses, pattern=species[i]);sp_files
  sp_weightings <- pops[pops$species == species[i],];sp_weightings
  
  for(j in 1:length(sp_files)){
    
    a <- raster(paste0(dir_demClasses,"/",sp_files[j]))
    
    if(j == 1){
      rast_sum <- a*sp_weightings$weighting[j]
    } else {
      rast_sum <- a*sp_weightings$weighting[j] + rast_sum
    }
    
  }
  species_weights$seasons[i] <- max(sp_weightings$seasons)
  raster_name <- paste0(dir,"/scripts_results/06_species/",species[i],".tif")
  writeRaster(rast_sum, filename=raster_name, format="GTiff", overwrite=TRUE)
  print(species[i])
  print(i)
  #plot(rast_sum,main=species[i])
  
}
all_no_br_adj <- a_all

#then plot all the species
for (i in 1:length(species)){
  sp <- raster(paste0(dir,"/scripts_results/06_species/",species[i],".tif"))
  
  a <- sp*species_weights$seasons[i]
  
  if(i == 1){
    a_all <- a
  } else {
    a_all <- a+a_all
  }
  plot(a_all,main=species[i])
}

plot(all_no_br_adj)
#could weight with 1 for br and 2 for all year

flat <- a_all
flat[flat!=0] <- 1

a_all[a_all==0] <- NA

colviri <- rev(viridis(20))
colsviri <- colorRampPalette(c(colviri))(255)
plot(a_all,col=colsviri)
plot(land,add=T)

#png(paste0(dir_1by1,"/maps/",name,".png"), width=1399,height=455)
par(mfrow=c(1,1))
plot(a_all,col=colsviri,legend=F)
plot(land,add=T)
#dev.off()

raster_name_2 <- paste0(dir,"/scripts_results/all_birds_distribution.tif")
writeRaster(a_all, filename=raster_name_2, 
            format="GTiff", overwrite=TRUE)
