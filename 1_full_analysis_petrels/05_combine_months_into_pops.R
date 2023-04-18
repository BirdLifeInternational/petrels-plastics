## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Combining rasters and scores for each month into averages by population
## Beth Clark 2022
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

rm(list=ls()) 

################ LOADING PACKAGES ###################

library(raster)
library(rgdal)
library(tidyverse)

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
#[1] forcats_0.5.1   stringr_1.4.0   dplyr_1.0.8     purrr_0.3.4    
#[5] readr_2.1.2     tidyr_1.2.0     tibble_3.1.6    ggplot2_3.3.5  
#[9] tidyverse_1.3.2 rgdal_1.4-8     raster_3.1-5    sp_1.5-0       

#loaded via a namespace (and not attached):
#[1] tidyselect_1.1.2    haven_2.4.3         lattice_0.20-45    
#[4] gargle_1.2.0        colorspace_2.0-3    vctrs_0.3.8        
#[7] generics_0.1.2      utf8_1.2.2          rlang_1.0.6        
#[10] pillar_1.7.0        glue_1.6.2          withr_2.5.0        
#[13] DBI_1.1.2           dbplyr_2.1.1        modelr_0.1.8       
#[16] readxl_1.3.1        lifecycle_1.0.3     munsell_0.5.0      
#[19] gtable_0.3.0        cellranger_1.1.0    rvest_1.0.2        
#[22] codetools_0.2-18    tzdb_0.2.0          fansi_1.0.2        
#[25] broom_0.7.12        Rcpp_1.0.8          scales_1.2.1       
#[28] backports_1.4.1     googlesheets4_1.0.0 jsonlite_1.8.0     
#[31] fs_1.5.2            hms_1.1.1           stringi_1.7.6      
#[34] grid_4.1.2          cli_3.3.0           tools_4.1.2        
#[37] magrittr_2.0.2      crayon_1.5.0        pkgconfig_2.0.3    
#[40] ellipsis_0.3.2      xml2_1.3.3          reprex_2.0.1       
#[43] googledrive_2.0.0   lubridate_1.8.0     assertthat_0.2.1   
#[46] httr_1.4.2          rstudioapi_0.13     R6_2.5.1           
#[49] compiler_4.1.2

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




