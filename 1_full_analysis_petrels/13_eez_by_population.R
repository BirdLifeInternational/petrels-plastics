## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Use EEZ results and create summary statistics
## Beth Clark 2022
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

rm(list=ls()) 

library(sp)
library(raster)
library(rgdal)
library(stringr)
library(geomerge)
library(maptools)
library(gridExtra)
library(sf)
library(tidyverse)
library(rgeos)

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
#[1] rgeos_0.5-9     forcats_0.5.1   dplyr_1.0.8     purrr_0.3.4    
#[5] readr_2.1.2     tidyr_1.2.0     tibble_3.1.6    ggplot2_3.3.5  
#[9] tidyverse_1.3.2 sf_1.0-7        gridExtra_2.3   maptools_1.1-3 
#[13] geomerge_0.3.2  stringr_1.4.0   rgdal_1.4-8     raster_3.1-5   
#[17] sp_1.5-0       

#loaded via a namespace (and not attached):
#[1] httr_1.4.2          jsonlite_1.8.0      modelr_0.1.8       
#[4] assertthat_0.2.1    googlesheets4_1.0.0 cellranger_1.1.0   
#[7] pillar_1.7.0        backports_1.4.1     lattice_0.20-45    
#[10] glue_1.6.2          rvest_1.0.2         colorspace_2.0-3   
#[13] inlmisc_0.5.5       pkgconfig_2.0.3     broom_0.7.12       
#[16] haven_2.4.3         s2_1.0.7            scales_1.2.1       
#[19] tzdb_0.2.0          proxy_0.4-26        googledrive_2.0.0  
#[22] generics_0.1.2      ellipsis_0.3.2      withr_2.5.0        
#[25] cli_3.3.0           magrittr_2.0.2      crayon_1.5.0       
#[28] readxl_1.3.1        deldir_1.0-6        fs_1.5.2           
#[31] fansi_1.0.2         xml2_1.3.3          foreign_0.8-81     
#[34] class_7.3-19        tools_4.1.2         data.table_1.14.2  
#[37] hms_1.1.1           geosphere_1.5-14    gargle_1.2.0       
#[40] lifecycle_1.0.3     munsell_0.5.0       reprex_2.0.1       
#[43] compiler_4.1.2      e1071_1.7-9         rlang_1.0.6        
#[46] classInt_0.4-3      units_0.8-0         grid_4.1.2         
#[49] rstudioapi_0.13     igraph_1.3.5        boot_1.3-28        
#[52] wk_0.6.0            gtable_0.3.0        codetools_0.2-18   
#[55] DBI_1.1.2           R6_2.5.1            lubridate_1.8.0    
#[58] utf8_1.2.2          spdep_1.2-7         KernSmooth_2.23-20 
#[61] stringi_1.7.6       Rcpp_1.0.8          vctrs_0.3.8        
#[64] spData_2.2.0        dbplyr_2.1.1        tidyselect_1.1.2 

######### GENERAL DIRECTIONS AND FILES ##############

#Read in and tidy up eez data ####
#Flanders Marine Institute (2020). Union of the ESRI Country shapefile and the Exclusive Economic Zones (version 3). Available online at https://www.marineregions.org/. https://doi.org/10.14284/403. Consulted on 2021-03-04.
eez_file <- readOGR(dsn="input_data/EEZ_land_union_v3_202003", layer = "EEZ_Land_v3_202030") 

#make a polygon for the high seas
#1. make polygon covering whole earth
y_coords <- c(90,90,-90,-90,90)
x_coords <- c(-180,180,180,-180,-180)
poly1 <- sp::Polygon(cbind(x_coords,y_coords))
firstPoly <- sp::Polygons(list(poly1), ID = "High Seas")
str(firstPoly,1)
firstSpatialPoly <- sp::SpatialPolygons(list(firstPoly))
firstSpatialPoly

#2. intersect with eezs
crs(firstSpatialPoly) <- crs(eez_file)
highseas <- gDifference(firstSpatialPoly,eez_file)
plot(highseas,col="green")

eez_file$id <-  as.character(1:323)
id <- as.character(1:323)

plot(eez_file)

eez_proj <- bind(eez_file,highseas)

rm(eez_file)

plot(eez_proj,col="blue")

eez_proj@data$UNION[nrow(eez_proj@data)] <- "High Seas"
eez_proj@data$TERRITORY1[nrow(eez_proj@data)] <- "High Seas"
eez_proj@data$SOVEREIGN1[nrow(eez_proj@data)] <- "High Seas"
eez_proj@data$POL_TYPE[nrow(eez_proj@data)] <- "High Seas"

#link to directory containing rasters for each population
sp_files <- list.files("outputs/12_breeding_countries/", pattern = ".*\\.tif$");sp_files

#Create eezs table ####
eezs <- as.data.frame(sp_files)
eezs$n_used <- NA
eezs$eezs_used <- NA
eezs$eezs_sovereigns_used <- NA

#loop through each population
for (i in 1:length(sp_files)){
  
  #read in raster
  a <- raster(paste0("outputs/12_breeding_countries/",sp_files[i]))

  #overlap with eez polygons
  eez_over <- raster::extract(a,eez_proj)
  eez_proj$over <- unlist(lapply(eez_over,sum,na.rm = T))
  
  rank_eezs <- as.data.frame(eez_proj@data)
  
  #filter dataframe to only used eezs 
  rank_eezs <- rank_eezs[order(-rank_eezs$over),]
  head(rank_eezs)
  
  rank_used <- subset(rank_eezs,over != 0)
  rank_used$prop <- rank_used$over/sum(rank_used$over)
  
  #save results
  eezs$n_used[i] <- nrow(rank_used)
  eezs$eezs_used[i] <- paste(rank_used$TERRITORY1,collapse = "_")
  eezs$eezs_sovereigns_used[i] <- paste(rank_used$SOVEREIGN1,collapse = "_")
  
  rank_used$sp_country <- str_remove(sp_files[i],".tif")
  
  print(i)
  
  if(i==1){
    eezs_used <- rank_used
  } else {
    eezs_used <- rbind(eezs_used,rank_used)
  }
  
}

eezs_used$species <- str_split_fixed(eezs_used$sp_country,pattern = "_",n=2)[,1]
eezs_used$breeding_country <- str_split_fixed(eezs_used$sp_country,pattern = "_",n=2)[,2]

#save results ####
write.csv(eezs_used,"outputs/13_eezs_used_per_species.csv",row.names = F)
write.csv(eezs,"outputs/13_eezs_per_species.csv",row.names = F)

