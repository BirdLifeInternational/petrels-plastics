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

#read in plastics data
#m <- raster("C:/Users/bethany.clark/OneDrive - BirdLife International/Data/VanSebForBeth2.tif")
m <- raster("C:/Users/bethany.clark/OneDrive - BirdLife International/Data/AverageForBeth2.tif")
plot(log(m),col=rev(viridis(255)))

m

## rescale to 1
m2 <- m
m2[is.na(m2)] <- 0 
p_sum1 <- m2/sum(getValues(m2))
p_sum1[is.na(m)] <- NA


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

pops$breeding_country <- c("New Zealand",
                           "Australia","Australia","New Zealand",
                           "Chile",
                           "UK","UK",
                           "New Zealand","UK","New Zealand",
                           "USA","Australia","Australia","Mauritius","France","France","Seychelles",
                           "Australia",
                           "Portugal","Spain","Cape Verde","Portugal",
                           "Portugal","Portugal","Spain","Portugal",
                           "Spain","Spain","Italy","Italy","Malta","Italy","Italy","Tunisia",
                           "Cape Verde",
                           "Japan","South Korea","Japan","Japan",
                           "Antarctica","Antarctica",
                           "Norway","Iceland","Canada","Denmark","Norway","Norway","UK","Norway",
                           "Antarctica","Antarctica",
                           "France","UK",
                           "Portugal","Portugal","UK",
                           "Canada",
                           "Cape Verde",
                           "Canada","Canada","Canada",
                           "Portugal",
                           "Spain","Italy","Ireland","Malta","UK",
                           "UK",
                           "France","UK",
                           "France","UK",
                           "UK",
                           "New Zealand","Australia",
                           "UK","UK",
                           "Antarctica",
                           "Cape Verde","Portugal",
                           "Peru",
                           "UK",
                           "Australia","New Zealand","UK",
                           "New Zealand",
                           "New Zealand","France","France","South Africa","UK",
                           "New Zealand","UK","France","South Africa",
                           "UK",
                           "New Zealand",
                           "New Zealand",
                           "France","France",
                           "Mauritius",
                           "Brazil",
                           "New Zealand",
                           "France",
                           "UK",
                           "Australia",
                           "New Zealand","New Zealand",
                           "Portugal",
                           "Cape Verde",
                           "New Zealand",
                           "Dominican Republic",
                           "UK",
                           "New Zealand",
                           "New Zealand",
                           "Australia","France",
                           "South Africa",
                           "Portugal",
                           "New Zealand",
                           "New Zealand","UK",
                           "Australia",
                           "New Zealand","Australia",
                           "Ecuador",
                           "New Zealand",
                           "USA",
                           "Australia",
                           "UK",
                           "New Zealand","Australia",
                           "Seychelles",
                           "New Zealand",
                           "New Zealand",
                           "Portugal","Cape Verde","Portugal",
                           "Spain",
                           "USA",
                           "Mexico",
                           "Iceland","Ireland","UK",
                           "France","Malta",
                           "Antarctica","Antarctica")


#combine population maps into species maps, then rescale to 1 ####
#this will weight by number of tracked months
#read in pop sizes

pop_sizes <- read.csv("C:/Users/bethany.clark/OneDrive - BirdLife International/Data/population_sizes_csv.csv")
pop_sizes$site <- NULL
pop_sizes$colony <- NULL
pop_sizes$species_pop <- paste(pop_sizes$species,pop_sizes$population,sep="_")
head(pop_sizes)

pop_sizes$breeding_country <- pops$breeding_country[match(pop_sizes$species_pop,pops$species_pop)]

pop_sizes$seasons <- pops$seasons[match(pop_sizes$species_pop,pops$species_pop)]
pop_sizes$pop_x_seasons <- pop_sizes$pop_arithmetric_mean*pop_sizes$seasons

countries <- pop_sizes %>%
  group_by(breeding_country,species) %>%
  summarise(count = n())%>%
  data.frame()
multicountry <- countries[countries$count > 1,]

multipop_species <- unique(multicountry$species)
for(i in 1:length(multipop_species)){
  species <- subset(pop_sizes,species == multipop_species[i])
  species$sp_pop_country <- paste(species$species_pop,species$breeding_country,sep="_")
  species$popsize_weighting <- 1

  all_countries <- as.data.frame(table(species$breeding_country))
  multi_pop_countries <- all_countries[all_countries$Freq > 1,]
  
  if(nrow(multi_pop_countries) > 0){
  for(j in 1:nrow(multi_pop_countries)){

    sp_country <- subset(species,breeding_country == multi_pop_countries$Var1[j])
    sp_country$popsize_weighting <- NA
    
    total <- sum(sp_country$pop_x_seasons)
    sp_country$popsize_weighting <- sp_country$pop_x_seasons/total    
    
     
    for(k in 1:nrow(sp_country)){
      species$popsize_weighting[species$sp_pop_country == sp_country$sp_pop_country[k]] <- sp_country$popsize_weighting[k]
    }
    
  }
  }
  print(i)

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
pops$sp_country <- paste(pops$species,pops$breeding_country,sep="_")

write.csv(pops,paste0(dir,"/scripts_results/pop_size_weightings_countries.csv"),row.names = F)

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

sp_country <- unique(pops$sp_country)
sp_country_list <- as.data.frame(sp_country)
sp_country_list$seasons <- NA
sp_country_list$n_pops <- NA
sp_country_list$score <- NA

all_files <- list.files(dir_demClasses, pattern=".tif");all_files

for (i in 1:nrow(sp_country_list)){
  
  sp_country_df <- pops[pops$sp_country == sp_country_list$sp_country[i],]
  sp_country_list$n_pops[i] <- nrow(sp_country_df)
  
  sp_country_df_files <- paste0(sp_country_df$species_pop,".tif")

  for(j in 1:length(sp_country_df_files)){
    
    a <- raster(paste0(dir_demClasses,"/",sp_country_df_files[j]))
    
    if(j == 1){
      rast_sum <- a*sp_country_df$weighting[j]
    } else {
      rast_sum <- a*sp_country_df$weighting[j] + rast_sum
    }
    
  }
  sp_country_list$seasons[i] <- max(sp_country_df$seasons)
  #raster_name <- paste0(dir,"/scripts_results/06_countries_over/",
  #                      sp_country_list$sp_country[i],".tif")
  #writeRaster(rast_sum, filename=raster_name, format="GTiff", overwrite=TRUE)

  #plot(rast_sum,main=species[i])
  ## rescale to 1
  a_proj2 <- rast_sum
  a_proj2[is.na(rast_sum)] <- 0 
  
  b_sum1    <- rast_sum/sum(getValues(a_proj2))

  
  #overlap
  over <- b_sum1 * p_sum1
  over_score <- over
  summary(over_score@data@values)
  over_score[is.na(over_score)] <- 0
  over_val <- round(sum(getValues(over_score))*1000000,4)
  
  sp_country_list$score[i] <- over_val
  raster_name <- paste0(dir,"/scripts_results/06_countries_over/",
                        sp_country_list$sp_country[i],".tif")
  writeRaster(over, filename=raster_name, format="GTiff", overwrite=TRUE)
  print(i)
}

sp_country_list$species <- str_split_n(sp_country_list$sp_country,"_",1)
sp_country_list$breeding_country <- str_split_n(sp_country_list$sp_country,"_",2)
sp_country_list

write.csv(sp_country_list,
          paste0(dir,"/sp_country_scores.csv"),
          row.names = F)
