## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Combine populations by breeding country for EEZ analysis
## Beth Clark 2021
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

rm(list=ls()) 

## LOADING PACKAGES ####

library(raster)
library(rgdal)
library(cowplot)
library(viridis)
library(stringr)
library(tidyverse)
library(ggplot2)
library(RColorBrewer)

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

## GENERAL DIRECTIONS AND FILES ####
pop_rasters <- "outputs/05_populations"
files <- list.files(pop_rasters, pattern=".*\\.tif$");files

outputs <- "outputs/12_breeding_countries"
dir.create(outputs)

#read in plastics data
plastics <- raster("outputs/00_PlasticsRaster.tif")
plastics2 <- plastics
plastics2[is.na(plastics2)] <- 0 
p_sum1    <- plastics2/sum(getValues(plastics2))
p_sum1[is.na(plastics)] <- NA

####### CONVERT INTO A 1X1 DEGREE RESOLUTION ########

pops <- read.csv("outputs/07_exposure_scores_by_season.csv")
pops$tracks_breeding <- NULL
pops$tracks_nonbreeding <- NULL
pops$ref_breeding <- NULL
pops$ref_nonbreeding <- NULL

pop_exposure <- read.csv("outputs/05_exposure_scores_by_population.csv") 

#add up the species ####

pops$species <- str_split_n(pops$species_pop,"_",1)

head(pops)
pops$seasons <- ifelse(is.na(pops$nonbreeding),0.5,1)

pops$breeding_country <- c("Portugal","UK")
table(pops$breeding_country)
length(unique(pops$breeding_country))

#combine population maps into species maps, then rescale to 1 ####
#this will weight by number of tracked months
#read in pop sizes

pop_sizes <- read.csv("input_data/population_sizes.csv")
pop_sizes$site <- NULL
pop_sizes$colony <- NULL
pop_sizes$source_est_n_breeding_pairs <- NULL
pop_sizes$species_pop <- paste(pop_sizes$species,pop_sizes$population,sep="_")

#add breeding country and exposure
pop_sizes$breeding_country <- pops$breeding_country[match(pop_sizes$species_pop,pops$species_pop)]

pops$pop_size <- pop_sizes$est_n_breeding_pairs[match(pops$species_pop,pop_sizes$species_pop)]
pops$pop_size[is.na(pops$pop_size)] <- 1
pops$pop_x_seasons <- pops$pop_size*pops$seasons
head(pops)

pops$population_exposure <- pop_exposure$population_exposure

countries <- pops %>%
  group_by(breeding_country,species) %>%
  summarise(count = n(),
            country_exposure = stats::weighted.mean(population_exposure,
                                                    pop_x_seasons),
            unweighted_mean = mean(population_exposure),
            
  )%>%
  data.frame();countries

multicountry <- countries[countries$count > 1,]

if(nrow(multicountry)>0){
  
  multipop_species <- unique(multicountry$species)
  for(i in 1:length(multipop_species)){
    species <- subset(pops,species == multipop_species[i])
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

  pops$weighting <- all_sp$popsize_weighting[match(pops$species_pop,all_sp$species_pop)]
  pops$weighting <- ifelse(is.na(pops$weighting),1,pops$weighting)
} else {
  pops$weighting <- 1
}

#match to pops
pops$sp_country <- paste(pops$species,pops$breeding_country,sep="_")

species <- unique(pops$species)
species_weights <- as.data.frame(species)
species_weights$seasons <- NA

for (i in 1:length(species)){
  
  sp_files <- list.files(pop_rasters, pattern=paste0(species[i],".*\\.tif$"));sp_files
  sp_weightings <- pops[pops$species == species[i],];sp_weightings
  
  species_weights$seasons[i] <- max(sp_weightings$seasons)
  
}

sp_country <- unique(pops$sp_country)
sp_country_list <- as.data.frame(sp_country)
sp_country_list$seasons <- NA
sp_country_list$n_pops <- NA
sp_country_list$score <- NA

all_files <- list.files(pop_rasters, pattern=".tif$");all_files

for (i in 1:nrow(sp_country_list)){
  
  sp_country_df <- pops[pops$sp_country == sp_country_list$sp_country[i],]
  sp_country_list$n_pops[i] <- nrow(sp_country_df)
  
  sp_country_df_files <- paste0(sp_country_df$species_pop,".tif")
  
  for(j in 1:length(sp_country_df_files)){
    
    a <- raster(paste0(pop_rasters,"/",sp_country_df_files[j]))
    
    if(j == 1){
      rast_sum <- a*sp_country_df$weighting[j]
    } else {
      rast_sum <- a*sp_country_df$weighting[j] + rast_sum
    }
    
  }
  sp_country_list$seasons[i] <- max(sp_country_df$seasons)
  #raster_name <- paste0(dir,"/scripts_results/06_countries_over/",
  #                      sp_country_list$sp_country[i],".tif")
  #raster::writeRaster(rast_sum, filename=raster_name, format="GTiff", overwrite=TRUE)
  
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
  raster_name <- paste0("outputs/12_breeding_countries/",
                        sp_country_list$sp_country[i],".tif")
  writeRaster(over, filename=raster_name, format="GTiff", overwrite=TRUE)
  print(i)
}

sp_country_list$species <- str_split_n(sp_country_list$sp_country,"_",1)
sp_country_list$breeding_country <- str_split_n(sp_country_list$sp_country,"_",2)
sp_country_list

write.csv(sp_country_list,"outputs/12_species_country_scores.csv",
          row.names = F)
