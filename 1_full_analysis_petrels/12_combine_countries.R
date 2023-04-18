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

sessionInfo()
#R version 4.1.2 (2021-11-01)
#Platform: x86_64-w64-mingw32/x64 (64-bit)
#Running under: Windows 10 x64 (build 19045)

#Matrix products: default

#locale:
#[1] LC_COLLATE=English_United Kingdom.1252 
#[2] LC_CTYPE=English_United Kingdom.1252   
#[3] LC_MONETARY=English_United Kingdom.1252
#[4] LC_NUMERIC=C                           
#[5] LC_TIME=English_United Kingdom.1252    

#attached base packages:
#[1] stats     graphics  grDevices utils     datasets  methods  
#[7] base     

#other attached packages:
#[1] RColorBrewer_1.1-2 forcats_0.5.1      dplyr_1.0.8       
#[4] purrr_0.3.4        readr_2.1.2        tidyr_1.2.0       
#[7] tibble_3.1.6       ggplot2_3.3.5      tidyverse_1.3.2   
#[10] stringr_1.4.0      viridis_0.6.2      viridisLite_0.4.0 
#[13] cowplot_1.1.1      rgdal_1.4-8        raster_3.1-5      
#[16] sp_1.5-0          

#loaded via a namespace (and not attached):
#[1] tidyselect_1.1.2    haven_2.4.3         lattice_0.20-45    
#[4] gargle_1.2.0        colorspace_2.0-3    vctrs_0.3.8        
#[7] generics_0.1.2      utf8_1.2.2          rlang_1.0.6        
#[10] pillar_1.7.0        withr_2.5.0         glue_1.6.2         
#[13] DBI_1.1.2           dbplyr_2.1.1        readxl_1.3.1       
#[16] modelr_0.1.8        lifecycle_1.0.3     munsell_0.5.0      
#[19] gtable_0.3.0        cellranger_1.1.0    rvest_1.0.2        
#[22] codetools_0.2-18    tzdb_0.2.0          fansi_1.0.2        
#[25] broom_0.7.12        Rcpp_1.0.8          scales_1.2.1       
#[28] backports_1.4.1     googlesheets4_1.0.0 jsonlite_1.8.0     
#[31] fs_1.5.2            gridExtra_2.3       hms_1.1.1          
#[34] stringi_1.7.6       grid_4.1.2          cli_3.3.0          
#[37] tools_4.1.2         magrittr_2.0.2      crayon_1.5.0       
#[40] pkgconfig_2.0.3     ellipsis_0.3.2      xml2_1.3.3         
#[43] reprex_2.0.1        googledrive_2.0.0   lubridate_1.8.0    
#[46] assertthat_0.2.1    httr_1.4.2          rstudioapi_0.13    
#[49] R6_2.5.1            compiler_4.1.2   

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
p_sum1    <- plastics2/sum(raster::getValues(plastics2))
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

pops$breeding_country <- c("New Zealand", "Australia","Australia",
                           "New Zealand","Chile","UK","UK","New Zealand",
                           "UK","New Zealand","USA","Australia",
                           "Australia","Mauritius","France","France",
                           "Seychelles","Australia","Portugal","Spain",
                           "Cape Verde","Portugal","Portugal","Portugal",
                           "Spain","Portugal","Spain","Spain","Italy",
                           "Italy","Malta","Italy","Italy","Tunisia",
                           "Cape Verde","Japan","South Korea","Japan",
                           "Japan","Antarctica","Antarctica","Norway",
                           "Iceland","Canada","Denmark","Norway","Norway",
                           "UK","Norway","Antarctica","Antarctica",
                           "France","UK","Portugal","Portugal","UK",
                           "Canada","Cape Verde","Canada","Canada",
                           "Canada","Portugal","Spain","Italy","Ireland",
                           "Malta","UK","UK","France","UK","France","UK",
                           "UK","New Zealand","Australia","UK","UK",
                           "Antarctica","Cape Verde","Portugal","Peru",
                           "UK","Australia","New Zealand","UK","New Zealand",
                           "New Zealand","France","France","South Africa",
                           "UK","New Zealand","UK","France","South Africa",
                           "UK","New Zealand","New Zealand","France",
                           "France","Mauritius","Brazil","New Zealand",
                           "France","UK","Australia","New Zealand",
                           "New Zealand","Portugal","Cape Verde",
                           "New Zealand","Dominican Republic","UK",
                           "New Zealand","New Zealand","Australia","France",
                           "South Africa","Portugal","New Zealand",
                           "New Zealand","UK","Australia","New Zealand",
                           "Australia","Ecuador","New Zealand","USA",
                           "Australia","UK","New Zealand","Australia",
                           "Seychelles","New Zealand","New Zealand",
                           "Portugal","Cape Verde","Portugal","Spain",
                           "USA","Mexico","Iceland","Ireland","UK",
                           "France","Malta","Antarctica","Antarctica")
pops$species_pop <- ifelse(pops$species_pop == "Ardenna pacifica_Lowendal and Houtman Abrolhoss","Ardenna pacifica_Lowendal & Houtman Abrolhoss",pops$species_pop)
pops$species_pop <- ifelse(pops$species_pop == "Fulmarus glacialis_Bjornoya","Fulmarus glacialis_Bjørnøya",pops$species_pop)

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

head(pops)

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

#match to pops
pops$sp_country <- paste(pops$species,pops$breeding_country,sep="_")

species <- unique(pops$species)
species_weights <- as.data.frame(species)
species_weights$seasons <- NA

for (i in 1:length(species)){
  
  sp_files <- list.files(pop_rasters, pattern=species[i]);sp_files
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
  #raster_name <- paste0("scripts_results/06_countries_over/",
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
  raster_name <- paste0("outputs/12_breeding_countries/",
                        sp_country_list$sp_country[i],".tif")
  writeRaster(over, filename=raster_name, format="GTiff", overwrite=TRUE)
  print(i)
}

sp_country_list$species <- str_split_n(sp_country_list$sp_country,"_",1)
sp_country_list$breeding_country <- str_split_n(sp_country_list$sp_country,"_",2)
sp_country_list

write.csv(sp_country_list,
          "outputs/12_species_country_scores.csv",
          row.names = F)
