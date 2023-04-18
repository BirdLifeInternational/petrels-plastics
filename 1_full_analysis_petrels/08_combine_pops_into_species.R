## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Combine scores for populations into scores for species
## weighted by population size
## And export maps - Beth Clark Mar 2020  
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

rm(list=ls()) 

################ LOADING PACKAGES ###################

library(raster)
library(rgdal)
library(cowplot)
library(viridis)
library(stringr)
library(dplyr)
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
#[1] RColorBrewer_1.1-2 ggplot2_3.3.5      dplyr_1.0.8       
#[4] stringr_1.4.0      viridis_0.6.2      viridisLite_0.4.0 
#[7] cowplot_1.1.1      rgdal_1.4-8        raster_3.1-5      
#[10] sp_1.5-0          

#loaded via a namespace (and not attached):
#[1] Rcpp_1.0.8       pillar_1.7.0     compiler_4.1.2   tools_4.1.2     
#[5] lifecycle_1.0.3  tibble_3.1.6     gtable_0.3.0     lattice_0.20-45 
#[9] pkgconfig_2.0.3  rlang_1.0.6      cli_3.3.0        DBI_1.1.2       
#[13] rstudioapi_0.13  gridExtra_2.3    withr_2.5.0      generics_0.1.2  
#[17] vctrs_0.3.8      grid_4.1.2       tidyselect_1.1.2 glue_1.6.2      
#[21] R6_2.5.1         fansi_1.0.2      purrr_0.3.4      magrittr_2.0.2  
#[25] scales_1.2.1     codetools_0.2-18 ellipsis_0.3.2   assertthat_0.2.1
#[29] colorspace_2.0-3 utf8_1.2.2       stringi_1.7.6    munsell_0.5.0   
#[33] crayon_1.5.0    

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

######### GENERAL DIRECTIONS AND FILES ##############

## check we're still in rproj home directory "1_full_analysis_petrels"
getwd()

outputs <- "/outputs/08_species"
dir.create(outputs)

pop_rasters <- "outputs/05_populations"
files <- list.files(pop_rasters, pattern=".*\\.tif$");files  # adjusted to not return .tif.aux.xml

pops <- read.csv("outputs/05_exposure_scores_by_population.csv")  
tracked_seasons <- read.csv("input_data/breeding_months.csv")
head(tracked_seasons)
pops$breeding <- tracked_seasons$breeding
pops$nonbreeding <- tracked_seasons$nonbreeding

head(pops)
pops$seasons <- ifelse(is.na(pops$nonbreeding)|is.na(pops$nonbreeding),0.5,1)

#combine population maps into species maps ####
#this will weight by number of tracked season
#and the relative population sizes

pop_sizes <- read.csv("input_data/population_sizes.csv")
pop_sizes$site <- NULL
pop_sizes$colony <- NULL
pop_sizes$source_est_n_breeding_pairs <- NULL
pop_sizes$species_pop <- paste(pop_sizes$species,pop_sizes$population,sep="_")
pop_sizes$seasons <- pops$seasons[base::match(pop_sizes$species_pop,pops$sp_pop)]
pop_sizes$pop_x_seasons <- pop_sizes$est_n_breeding_pairs*pop_sizes$seasons
head(pop_sizes)

multipop_species <- unique(pop_sizes$species)
for(i in 1:length(multipop_species)){
  species <- subset(pop_sizes,species == multipop_species[i])
  species$popsize_weighting <- species$est_n_breeding_pairs/sum(species$est_n_breeding_pairs)
  species$pop_x_season_weighting <- species$pop_x_seasons/sum(species$pop_x_seasons)
  if(i == 1){
    all_sp <- species
  } else {
    all_sp <- rbind(all_sp,species)
  }
}
head(pops)

#match to pops
pops$pop_size_weight <- all_sp$popsize_weighting[match(pops$sp_pop,all_sp$species_pop)]
pops$pop_size_weight <- ifelse(is.na(pops$pop_size_weight),1,pops$pop_size_weight)

pops$pop_x_seasons_weight <- all_sp$pop_x_season_weighting[match(pops$sp_pop,
                                                                 all_sp$species_pop)]
pops$pop_x_seasons_weight <- ifelse(is.na(pops$pop_x_seasons_weight),
                                    1,pops$pop_x_seasons_weight)
head(pops)

#species, weighted by pop size ####
df_species <- pops %>% 
  group_by(species) %>%
  summarise(species_exposure = stats::weighted.mean(population_exposure,
                                                    pop_x_seasons_weight),
            unweighted_mean = mean(population_exposure),
            n_pops = n()
            ) %>%
  data.frame(); df_species

#test the correlation between weighted and unweighted means
df_species_multipop <- subset(df_species, n_pops != 1)

hist(df_species_multipop$species_exposure)
cor.test(df_species_multipop$species_exposure,
         df_species_multipop$unweighted_mean,
         method = "kendall")
plot(df_species_multipop$species_exposure,df_species_multipop$unweighted_mean)

df_species$seasons <- NA

for (i in 1:length(df_species$species)){
  
  sp_files <- list.files(pop_rasters, pattern= paste0(df_species$species[i],".*\\.tif$"));sp_files  
  sp_weightings <- subset(pops, pops$species == df_species$species[i]);sp_weightings
  
  for(j in 1:length(sp_files)){
    
    a <- raster(paste0(pop_rasters,"/",sp_files[j]))
    
    if(j == 1){
      rast_sum <- a*sp_weightings$pop_x_seasons_weight[j]
    } else {
      rast_sum <- a*sp_weightings$pop_x_seasons_weight[j] + rast_sum
    }
    
  }
  df_species$seasons[i] <- max(sp_weightings$seasons)
  raster_name <- paste0("outputs/08_species/",df_species$species[i],".tif")
  raster::writeRaster(rast_sum, filename=raster_name, format="GTiff", overwrite=TRUE)
  print(df_species$species[i])
  print(i)
}

write.csv(df_species,paste0("outputs/08_exposure_scores_by_species.csv"),
          row.names = F)

#then combine rasters for all the species
#weighting those with only breeding season tracking at 0.5
for (i in 1:nrow(df_species)){
  sp <- raster::raster(paste0("outputs/08_species/",df_species$species[i],".tif"))  # TODO: as above?
  
  a <- sp*df_species$seasons[i]
  
  a_flat <- a
  a_flat[a_flat != 0] <- 1
  
  if(i == 1){
    a_all <- a
    a_all_flat <- a_flat
  } else {
    a_all <- a+a_all
    a_all_flat <- a_flat + a_all_flat
  }
  print(i)
}

plot(a_all)
raster_name_dist <- paste0("outputs/08_all_species_distribution.tif")
raster::writeRaster(a_all, filename=raster_name_dist, 
                    format="GTiff", overwrite=TRUE)

plot(a_all_flat)
raster_name_richness <- paste0("outputs/08_species_richness.tif")
raster::writeRaster(a_all_flat, filename=raster_name_richness, 
                    format="GTiff", overwrite=TRUE)



