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

library(raster) #3.1-5
library(rgdal)
library(cowplot)
library(viridis)
library(stringr)
library(RColorBrewer)
#library(terra)
#library(gdalUtils)

#install.packages("raster")
#install.packages("terra")
#install.packages("gdalUtils")

######### GENERAL DIRECTIONS AND FILES ##############

## GENERAL DIR
dir <- paste0("C:/Users/bethany.clark/OneDrive - BirdLife International/",
              "Methods") ## copy and paste here your working directory

## DIRECTION TO YOUR RASTERS (ALL DEM CLASSES COMBINED AND BY YEAR QUARTER)
#dir_demClasses <- paste0(dir,"/scripts_results/09_sum_demClasses")
land <- readOGR(dsn=paste0(dir,"/baselayer"), layer = "world-dissolved") #Changed - BC  

output <- paste0(dir,"/scripts_results/02_pops")

library(rgdal)
land <- readOGR(dsn=paste0(dir,"/baselayer"), layer = "world-dissolved")  ## changed - BC

plot(land, main = "All populations")

files <- list.files(output);files

r <- raster()

png(paste0(dir,"/scripts_results/all_locations.png"), width=2000,height=1125)


#all pops points locations ####
#for(i in 1:length(files)){
#  pop <- read.csv(paste0(output,"/",files[i]))
#  if(i==1){all_pops <- pop} else {all_pops <- rbind(all_pops,pop)}
#  print(i)
#}
#all_pops <- all_pops[!is.na(all_pops$scientific_name), ]
#write.csv(all_pops,paste0(dir,"/scripts_results/all_locations.csv"), row.names = F)

all_pops <- read.csv(paste0(dir,"/scripts_results/all_locations.csv"))

length(unique(all_pops$bird_id))

all_pops$scientific_name <- as.character(all_pops$scientific_name)
all_pops$group <- "Other Petrels"

unique(all_pops$scientific_name)

all_pops$group <- ifelse(grepl(all_pops$scientific_name,pattern = "Pterodroma"),
                         "Gadfly Petrels",all_pops$group)
all_pops$group <- ifelse(grepl(all_pops$scientific_name,pattern = "Hydrobates"),
                         "Storm-Petrels",all_pops$group)
all_pops$group <- ifelse(grepl(all_pops$scientific_name,pattern = "Oceanites"),
                         "Storm-Petrels",all_pops$group)
all_pops$group <- ifelse(grepl(all_pops$scientific_name,pattern = "Puffinus"),
                         "Shearwaters",all_pops$group)
all_pops$group <- ifelse(grepl(all_pops$scientific_name,pattern = "Ardenna"),
                         "Shearwaters",all_pops$group)
all_pops$group <- ifelse(grepl(all_pops$scientific_name,pattern = "Calonectris"),
                         "Shearwaters",all_pops$group)

#for webinar
yelblus <- c(brewer.pal(n = 9, name = "YlGnBu"),"#00172e")
cols <- colorRampPalette(yelblus)(6)

points(latitude~longitude,
       data=all_pops,
       pch=16, cex=0.05)
plot(land,col="#dbdbdb", 
     border = "#c7c7c7", add=T)
points(lat_colony~lon_colony,
       data=all_pops,
       pch=16, cex=1.5, col="red")

png(paste0(dir,"/scripts_results/all_locs_1col2.png"), width=3000,height=1688)
par(mfrow=c(1,1))
plot(land,col="#dbdbdb", 
     border = "#c7c7c7")
points(latitude~longitude,
       data=all_pops,
       pch=16, cex=0.05)
plot(land,col="#dbdbdb", 
     border = "#c7c7c7", add=T)
points(lat_colony~lon_colony,
       data=all_pops,
       pch=16, cex=1.5, col="red")
dev.off()
dev.off()


plot(land,col="#dbdbdb", 
     border = "#c7c7c7", main = "All populations")

points(latitude~longitude,
       data=all_pops[all_pops$group == "Gadfly Petrels",],
       pch=16, cex=0.05, col=cols[4])
plot(land,col="#dbdbdb", 
     border = "#c7c7c7", add=T)
points(lat_colony~lon_colony,
       data=all_pops[all_pops$group == "Gadfly Petrels",],
       pch=16, cex=1.5, col="red")


points(latitude~longitude,
       data=all_pops[all_pops$group == "Storm-Petrels",],
       pch=16, cex=0.05, col=cols[3])
plot(land,col="#dbdbdb", 
     border = "#c7c7c7", add=T)
points(lat_colony~lon_colony,
       data=all_pops[all_pops$group == "Storm-Petrels",],
       pch=16, cex=1.5, col="red")
points(lat_colony~lon_colony,
       data=all_pops[all_pops$group == "Gadfly Petrels",],
       pch=16, cex=1.5, col="red")

points(latitude~longitude,
       data=all_pops[all_pops$group == "Shearwaters",],
       pch=16, cex=0.05, col=cols[2])
plot(land,col="#dbdbdb", 
     border = "#c7c7c7", add=T)
points(lat_colony~lon_colony,
       data=all_pops[all_pops$group == "Shearwaters",],
       pch=16, cex=1.5, col="red")
points(lat_colony~lon_colony,
       data=all_pops[all_pops$group == "Gadfly Petrels",],
       pch=16, cex=1.5, col="red")
points(lat_colony~lon_colony,
       data=all_pops[all_pops$group == "Storm-Petrels",],
       pch=16, cex=1.5, col="red")

points(latitude~longitude,
       data=all_pops[all_pops$group == "Other Petrels",],
       pch=16, cex=0.05, col=cols[2])
plot(land,col="#dbdbdb", 
     border = "#c7c7c7", add=T)
points(lat_colony~lon_colony,
       data=all_pops,
       pch=16, cex=1.5, col="red")



#for paper

cols <- colorRampPalette(yelblus)(255)
colsviri <- cols[40:255]

files <- list.files(output);files

dir_demClasses <- paste0(dir,"/scripts_results/03_kernels")
files <- list.files(dir_demClasses, pattern="tif");files

####### CONVERT INTO A 1X1 DEGREE RESOLUTION ########

#read in plastics data
plastics <- raster("C:/Users/bethany.clark/OneDrive - BirdLife International/Data/AverageForBeth2.tif")
#plastics <- raster("C:/Users/bethany.clark/OneDrive - BirdLife International/Data/VanSebForBeth2.tif")

plot(plastics)

## resolution and projection
m <- plastics

## rescale to 1
m2 <- m
m2[is.na(m2)] <- 0 
p_sum1    <- m2/sum(getValues(m2))
p_sum1[is.na(m)] <- NA

plot((plastics), 
     main = "million plastic piece per 10km2", 
     col=cols)

png(paste0(dir,"/scripts_results/sqrt_plastic_original_scale.png"), width=2000,height=1125)
par(mfrow=c(1,1))
plot(sqrt(plastics), 
     main = "sqrt plastic pieces per 10km2", 
     col=cols)
dev.off()
dev.off()

RES <- res(m) # the resolution of the raster (in degrees)
# res_lon = RES[1]*pi/180 (in radians) and res_lat = RES[2]*pi/180 (in radians)
R <- 6371007.2 # the Earth's authalic radius (in meters)
lat <- yFromRow(m, 1:nrow(m)) # latitude of the centroid of each cell (in degrees, need to be converted in radians)
area <- (sin(pi/180*(lat + RES[2]/2)) - sin(pi/180*(lat - RES[2]/2))) * (RES[1] * pi/180) * R^2
r_area <- setValues(m, rep(area, each=ncol(m))) # gives the area of each grid cell in meters 

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

species <- unique(pops$species);species

#combine monthly maps into species maps, then rescale to 1 ####
#this will weight by number of tracked months
for (i in 1:length(species)){
  
  sp_files <- list.files(dir_demClasses, pattern=species[i]);sp_files
  
  for(j in 1:length(sp_files)){
    
    a <- raster(paste0(dir_demClasses,"/",sp_files[j]))
    a[is.na(a)] <- 0 
    b <- sum(getValues(a)) # total number of birds in the 10km x 10km raster
    
    ## reprojecting and rescaling
    a_proj <- projectRaster(a, m, method = "bilinear")
    a_proj2 <- a_proj * r_area / 100000000 # rescaling the values in each cell
    a_proj2[is.na(a_proj2)] <- 0 
    a_proj2[is.na(m)] <- NA
    if(j == 1){
      rast_sum <- a_proj2
      count <- 1
    } else {
      rast_sum <- a_proj2 + rast_sum
      count <- count + 1
    }
    
  }
  rast_sum <- rast_sum/count
  raster_name <- paste0(dir,"/scripts_results/06_species/",species[i],".tif")
  writeRaster(rast_sum, filename=raster_name, format="GTiff", overwrite=TRUE)
  print(species[i])
  print(i)
}

rast_sum[is.na(rast_sum)] <- 0

a_proj3[is.na(a_proj3)] <- 0

#then plot all the species
for (i in 1:length(species)){

  a <- raster(paste0(dir,"/scripts_results/06_species/",species[i],".tif"))
  
  ## reprojecting and rescaling
  a_proj <- projectRaster(a, r, method = "bilinear")
  a_proj[is.na(a_proj)] <- 0 
  
  if(i == 1){
    a_all <- a_proj
  } else {
    a_all <- a_proj+a_all
  }
  #plot(a_proj, main = species[i])
  #plot(land,add=T)
  print(length(species)-i)
}
a_all_species <- a_all

flat <- a_all_species
flat[flat!=0] <- 1

a_all_species[a_all_species==0] <- NA

plot(a_all,col=cols, main = "density by species")
plot(land,add=T)


raster_name_2 <- paste0(dir,"/all_birds_distribution_25nov20_bysp.tif")
writeRaster(a_all_species, filename=raster_name_2, format="GTiff", overwrite=TRUE)


######### GENERAL DIRECTIONS AND FILES ##############

## GENERAL DIR
dir <- paste0("C:/Users/bethany.clark/OneDrive - BirdLife International/",
              "Methods") ## copy and paste here your working directory

## DIRECTION TO YOUR RASTERS (ALL DEM CLASSES COMBINED AND BY YEAR QUARTER)
land <- readOGR(dsn=paste0(dir,"/baselayer"), layer = "world-dissolved") #Changed - BC  

dir_demClasses <- paste0(dir,"/scripts_results/03_kernels")

## DIRECTION TO YOUR RESULTS
#dir.create(paste0(dir,"/scripts_results/04_aggregate_1by1_grid_br/")) 
#dir.create(paste0(dir,"/scripts_results/04_aggregate_1by1_grid_br/maps/")) #added - BC
#dir.create(paste0(dir,"/scripts_results/04_aggregate_1by1_grid_br_mean/")) 
#dir.create(paste0(dir,"/scripts_results/04_aggregate_1by1_grid_br_mean/maps/")) #added - BC
dir_1by1 <- paste0(dir,"/scripts_results/04_aggregate_1by1_grid_br_p2")
#dir_1by1m <- paste0(dir,"/scripts_results/04_aggregate_1by1_grid_br_mean")

####### CONVERT INTO A 1X1 DEGREE RESOLUTION ########
a <- a_all
a[is.na(a)] <- 0 
b <- sum(getValues(a)) # total number of birds in the 10km x 10km raster

## reprojecting and rescaling
a_proj <- projectRaster(a, m, method = "bilinear")
a_proj2 <- a_proj * r_area / 100000000 # rescaling the values in each cell
a_proj2[is.na(a_proj2)] <- 0 
a_proj2[is.na(m)] <- NA

#breeding season overlap
over <- a_proj2 * p_sum1*20000
over_score <- over
summary(over_score@data@values)
over_score[is.na(over_score)] <- 0
over_val_br <- round(sum(getValues(over_score))*1000000,4)

plot(over)
plot(a_proj2,col=colsviri)

#combine into maps for each season per population ####
#then combine
dir_1by1 <- paste0(dir,"/scripts_results/04_aggregate_1by1_grid_br_p2")

for (i in 1:length(species)){
  
  sp_files <- list.files(dir_1by1, pattern=species[i]);sp_files
  
  br <- sp_files[grepl(x=sp_files,pattern="_br")];br
  nonbr <- sp_files[grepl(x=sp_files,pattern="_nonbr")];nonbr
  
  for(j in 1:length(br)){
    a <- raster(paste0(dir_1by1,"/",br[j]))
    if(j == 1){rast_sum <- a
      count <- 1
    } else { rast_sum <- a + rast_sum
      count <- count + 1
    }
    rast_sum <- rast_sum/count
    raster_name <- paste0(dir,"/scripts_results/07_seasons/",species[i],"_br.tif")
    writeRaster(rast_sum, filename=raster_name, format="GTiff", overwrite=TRUE)
  }
  
  if(length(nonbr) != 0){
  for(j in 1:length(nonbr)){
    a <- raster(paste0(dir_1by1,"/",nonbr[j]))
    if(j == 1){rast_sum <- a
    count <- 1
    } else { rast_sum <- a + rast_sum
    count <- count + 1
    }
    rast_sum <- rast_sum/count
    raster_name <- paste0(dir,"/scripts_results/07_seasons/",species[i],"_nonbr.tif")
    writeRaster(rast_sum, filename=raster_name, format="GTiff", overwrite=TRUE)
  }
  }
  print(species[i])
  print(i)
}


#then plot all the seasons ####

files <- list.files(paste0(dir,"/scripts_results/07_seasons/"));files

for (i in 1:length(files)){
  
  a <- raster(paste0(dir,"/scripts_results/07_seasons/",files[i]))
  
  ## rescaling
  rast_sum <- a
  rast_sum[is.na(rast_sum)] <- 0
  rast_sum1 <- rast_sum/sum(getValues(rast_sum))
  sum(getValues(rast_sum1))
  rast_sum1[is.na(a)] <- NA
  
  if(i == 1){
    a_all <- rast_sum1
  } else {
    a_all <- rast_sum1+a_all
  }
  print(length(files)-i)
}

a_all_seasons <- a_all
a_all_seasons[a_all_seasons == 0] <- NA



png(paste0(dir,"/scripts_results/all_seasons.png"), width=2000,height=1125)
par(mfrow=c(1,1))

cols_seasons <- colorRampPalette(yelblus)(length(files))
plot(a_all_seasons,col=cols_seasons,legend=F)
plot(land,col="#dbdbdb", 
     border = "#c7c7c7", add=T)
dev.off()

max(a_all_seasons@data@values,na.rm=T)

raster_name_2 <- paste0(dir,"/all_birds_distribution_by_season.tif")
writeRaster(a_all, filename=raster_name_2, format="GTiff", overwrite=TRUE)


#species richness map2 ####
files <- list.files(paste0(dir,"/scripts_results/06_species/"),pattern="tif");files
for (i in 1:length(files)){
  
  a <- raster(paste0(dir,"/scripts_results/06_species/",files[i]))
  
  a_flat <- a
  a_flat[a_flat != 0] <- 1

  if(i == 1){
    a_all <- a_flat
  } else {
    a_all <- a_flat+a_all
  }
  

  png(paste0(dir,"/scripts_results/06_species/flat_maps/",names(a),".png"), width=2200,height=1125)
  par(mfrow=c(1,2))
  plot(a)
  plot(land,col="#dbdbdb", 
       border = "#c7c7c7", add=T)
  plot(a_flat)
  plot(land,col="#dbdbdb", 
       border = "#c7c7c7", add=T)
  dev.off()
  print(length(files)-i)
}

sp_rich <- a_all

raster_name_2 <- paste0(dir,"/all_birds_sp_richness.tif")
writeRaster(sp_rich, filename=raster_name_2, format="GTiff", overwrite=TRUE)


range(sp_rich@data@values,na.rm=T)

cols_sp <- c("white",
  colorRampPalette(yelblus)(max(sp_rich@data@values,na.rm=T)))


par(mfrow=c(1,1))
plot(sp_rich,col=cols_sp)
plot(land,col="#dbdbdb", 
     border = "#c7c7c7", add=T)
points(lat_colony~lon_colony,
       data=all_pops,
       pch=18, cex=1, col="red")

png(paste0(dir,"/scripts_results/species_richness_cols.png"), width=2000,height=1125)
par(mfrow=c(1,1))
plot(sp_rich,col=cols_sp)
plot(land,col="#dbdbdb", 
     border = "#c7c7c7", add=T)
points(lat_colony~lon_colony,
       data=all_pops,
       pch=18, cex=3, col="red")
dev.off()



#breeding season overlap ####
over <- a_all_seasons * p_sum1*100000
over_score <- over
summary(over_score@data@values)

max(over_score@data@values,na.rm=T)

plot(over,col=cols_seasons2)
over_cap <- over
over_cap@data@values <- ifelse(over_cap@data@values > max(over_score@data@values,na.rm=T)/100, 
                               max(over_score@data@values,na.rm=T)/100,
                               over_cap@data@values)
plot(over_cap,col=cols)
plot(land,col="#dbdbdb", 
     border = "#dbdbdb", add=T)

png(paste0(dir,"/scripts_results/all_seasons_overlap_cap_cols.png"), width=2000,height=1125)
par(mfrow=c(1,1))
plot(over_cap,col=cols)
plot(land,col="#dbdbdb", 
     border = "#c7c7c7", add=T)
points(lat_colony~lon_colony,
       data=all_pops,
       pch=18, cex=6, col="red")
dev.off()
dev.off()

png(paste0(dir,"/scripts_results/all_seasons_overlap_cap_cols.png"), width=2000,height=1125)
par(mfrow=c(1,1))
plot(over_cap,col=cols)
plot(land,col="#dbdbdb", 
     border = "#c7c7c7", add=T)
points(lat_colony~lon_colony,
       data=all_pops,
       pch=18, cex=6, col="red")
dev.off()
dev.off()

points(lat_colony~lon_colony,
       data=all_pops,
       pch=18, cex=3, col="red")

#species richness overlap ####
over_sprich <- sp_rich * p_sum1*100000
over_sprich[over_sprich == 0] <- NA

over_sprich_score <- over_sprich
summary(over_sprich_score@data@values)

max(over_sprich_score@data@values,na.rm=T)

plot(over_sprich,col=cols_seasons2)
over_sprich_cap <- over_sprich
over_sprich_cap@data@values <- ifelse(over_sprich_cap@data@values > 
                                 max(over_sprich_score@data@values,na.rm=T)/10, 
                               max(over_sprich_score@data@values,na.rm=T)/10,
                               over_sprich_cap@data@values)
plot(over_sprich_cap,col=cols)
plot(land,col="#dbdbdb", 
     border = "#c7c7c7", add=T)


png(paste0(dir,"/scripts_results/species_richness_overlap.png"), width=2000,height=1125)
par(mfrow=c(1,1))
plot(over_sprich_cap,col=cols)
plot(land,col="#dbdbdb", 
     border = "#c7c7c7", add=T)
points(lat_colony~lon_colony,
       data=all_pops,
       pch=16, cex=1.5, col="red")
dev.off()
dev.off()

