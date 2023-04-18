## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Use eez results and create summary statistics
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
