#Case study figures for plastics project
#Bethany Clark 21/4/21

rm(list=ls()) 

library(RColorBrewer)
library(rgdal)
library(raster)
library(cowplot)
library(viridis)
library(tidyverse)
library(sf)

######### GENERAL DIRECTIONS AND FILES ##############

## GENERAL DIR
dir <- paste0("C:/Users/bethany.clark/OneDrive - BirdLife International/",
              "Methods") ## copy and paste here your working directory

dir_1by1 <- paste0(dir,"/scripts_results/04_aggregate_1by1_grid_br_p2")
dat <- read.csv(paste0(dir_1by1, "/results_rasters_br_p2.csv"))

land <- readOGR(dsn=paste0(dir,"/baselayer"), layer = "world-dissolved") #Changed - BC 

all_pops <- read.csv(paste0(dir,"/scripts_results/all_locations.csv"))

m <- raster("C:/Users/bethany.clark/OneDrive - BirdLife International/Data/AverageForBeth2.tif")
m2 <- m
m2[is.na(m2)] <- 0 
p_sum1    <- m2/sum(getValues(m2))
p_sum1[is.na(m)] <- NA

cols_inferno <- rev(inferno(20))
cols_inf <- colorRampPalette(c(cols_inferno))(255)

yelblus <- c(brewer.pal(n = 9, name = "YlGnBu"),"#00172e")
cols_sp <- colorRampPalette(yelblus)(255)

#1 storm petrel overlap for the breeding season for 5 colonies ####

#plot all cols on 1 distribution, labelling the colony locations

files <- list.files(dir_1by1,pattern = "pelagicus")
files <- files[files != "Hydrobates pelagicus_Malta_sum_nonbr.tif"]

for(i in 1:length(files)){
  pop <- raster(paste0(dir_1by1,"/",files[i]))
  p2 <- pop
  p2[is.na(p2)] <- 0 
  pop_sum1    <- p2/sum(getValues(p2))
  pop_sum1[is.na(pop)] <- NA
  
  pop_over <- p_sum1*pop_sum1
  
  if(i == 1){
    all_stormies <- pop_over
  } else {
    all_stormies <- all_stormies + pop_over
  }
  plot(all_stormies)
}

# define Robinson projection
proj <- "+proj=robin"

# project shapefile and raster to Robinson projection
land_sf <- st_as_sf(land)
world_prj <- land_sf %>% st_transform(proj)
b_dens_proj <- projectRaster(all_stormies, crs = proj)
#b_dens_proj[b_dens_proj == 0] <- NA

# convert raster to dataframe for ggplot
bat_df <- rasterToPoints(b_dens_proj) %>% as_tibble()

#add colony locations
cols <- all_pops[!duplicated(all_pops[
  c("lon_colony","lat_colony","scientific_name")]),]
cols <- subset(cols,!is.na(cols$lat_colony))
cols_stormies <- cols[cols$scientific_name == "Hydrobates pelagicus",]
sp::coordinates(cols_stormies) <- ~lon_colony+lat_colony
proj4string(cols_stormies) <- proj4string(land)
cols_prj <- spTransform(cols_stormies,proj)
storm_df <- as.data.frame(cols_prj)

# generate  ggplot figure
p1 <- ggplot() +
  theme_minimal() +  
  scale_fill_viridis(option="inferno",direction=-1)+
  geom_raster(aes(x = x, y = y, fill = layer), 
              data = bat_df) +
  geom_sf(aes(), colour = NA, fill = "grey75", 
          data = world_prj) +
  geom_point(aes(x=lon_colony, y=lat_colony),data=storm_df,
             colour="#004fd9",shape=18,size=20)+
  xlab("") + ylab("") ;p1

png(paste0(dir,"/scripts_results/stormies_rob.png"), 
    width=10000,height=5625)
p1
dev.off()
dev.off()

dat_stormies <- dat[dat$species == "Hydrobates pelagicus",];dat_stormies

#2 yelkoaun v scopoli's shearwaters ####
dat_sp <- read.csv(paste0(dir,"/scripts_results/pop_size_weightings.csv"))
head(dat_sp)
dat_med <- dat[dat$species == "Calonectris diomedea" |
                 dat$species == "Puffinus yelkouan",]

dat_med <- dat_med[complete.cases(dat_med),];dat_med

means_med <- dat_med %>%
  group_by(common_name) %>%
  summarise(., mean_nonbr = mean(mean_nonr), 
            mean_br = mean(mean_br)) %>%
  data.frame();means_med


files <- list.files(dir_1by1,pattern = "diom")
files <- files[str_detect(files,"nonbr")];files

for(i in 1:length(files)){
  pop <- raster(paste0(dir_1by1,"/",files[i]))
  p2 <- pop
  p2[is.na(p2)] <- 0 
  pop_sum1    <- p2/sum(getValues(p2))
  pop_sum1[is.na(pop)] <- NA

  if(i == 1){
    all_scopolis <- pop_sum1
  } else {
    all_scopolis <- all_scopolis + pop_sum1
  }
  plot(all_scopolis)
}

files <- list.files(dir_1by1,pattern = "yelk")
files <- files[str_detect(files,"nonbr")]

for(i in 1:length(files)){
  pop <- raster(paste0(dir_1by1,"/",files[i]))
  p2 <- pop
  p2[is.na(p2)] <- 0 
  pop_sum1    <- p2/sum(getValues(p2))
  pop_sum1[is.na(pop)] <- NA
  
  if(i == 1){
    all_yelk <- pop_sum1
  } else {
    all_yelk <- all_yelk + pop_sum1
  }
  plot(all_yelk)
}

# project shapefile and raster to Robinson projection
yelk_dens_proj <- projectRaster(all_yelk, crs = proj)
#yelk_dens_proj[yelk_dens_proj == 0] <- NA
yelk_df <- rasterToPoints(yelk_dens_proj) %>% as_tibble()

scop_dens_proj <- projectRaster(all_scopolis, crs = proj)
#scop_dens_proj[scop_dens_proj == 0] <- NA
scop_df <- rasterToPoints(scop_dens_proj) %>% as_tibble()

#add colony locations
cols_yelk <- cols[cols$scientific_name == "Puffinus yelkouan",]
sp::coordinates(cols_yelk) <- ~lon_colony+lat_colony
proj4string(cols_yelk) <- proj4string(land)
cols_yelk_prj <- spTransform(cols_yelk,proj)
cols_yelk_prj_df <- as.data.frame(cols_yelk_prj)

cols_scop <- cols[cols$scientific_name == "Calonectris diomedea",]
sp::coordinates(cols_scop) <- ~lon_colony+lat_colony
proj4string(cols_scop) <- proj4string(land)
cols_scop_prj <- spTransform(cols_scop,proj)
cols_scop_prj_df <- as.data.frame(cols_scop_prj)

yelk<- ggplot() +
  theme_minimal() +  
  scale_fill_gradientn(colors = cols_sp)+
  geom_raster(aes(x = x, y = y, fill = layer), 
              data = yelk_df) +
  geom_sf(aes(), colour = NA, fill = "grey75", 
          data = world_prj) +
  geom_point(aes(x=lon_colony, y=lat_colony),
             data=cols_yelk_prj_df,
             colour="red",shape=18,size=20)+
  xlab("") + ylab("") ;yelk

scop<- ggplot() +
  theme_minimal() +  
  scale_fill_gradientn(colors = cols_sp)+
  geom_raster(aes(x = x, y = y, fill = layer), 
              data = scop_df) +
  geom_sf(aes(), colour = NA, fill = "grey75", 
          data = world_prj) +
  geom_point(aes(x=lon_colony, y=lat_colony),
             data=cols_scop_prj_df,
             colour="red",shape=18,size=20)+
  xlab("") + ylab("") ;scop

png(paste0(dir,"/scripts_results/yelk_rob.png"), 
    width=10000,height=5625)
yelk
dev.off()
dev.off()

png(paste0(dir,"/scripts_results/scop_rob.png"), 
    width=10000,height=5625)
scop
dev.off()
dev.off()


#rather than both breeding on malta, we could include all the scopoli's
#and all the yelkuoan breeding in the med,

#plot the seabird dist v overlap for 2x in non-breeding season

#3 cook's petrels little barrier v codfish islands ####

#plot nbr maps for both colonies side by side?

dat_cookii <- dat[dat$species == "Pterodroma cookii",];dat_cookii

files <- list.files(dir_1by1,pattern = "cook")
files <- files[str_detect(files,"nonbr")]

pop <- raster(paste0(dir_1by1,"/",files[1]))

# project shapefile and raster to Robinson projection
pacific_proj <- "+proj=robin +lon_0=180 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs"

dens_proj <- projectRaster(pop, crs = pacific_proj)
plot(dens_proj)
df <- rasterToPoints(dens_proj) %>% as_tibble()
names(df)[3] <- "layer"

#add colony locations
col <- cols[cols$scientific_name == "Pterodroma cookii" &
               cols$colony_name == "Codfish Island",]
sp::coordinates(col) <- ~lon_colony+lat_colony
proj4string(col) <- proj4string(land)
cols_prj <- spTransform(col,pacific_proj)
cols_prj_df <- as.data.frame(cols_prj)



codfish<- ggplot() +
  theme_minimal() +  
  scale_fill_gradientn(colors = cols_sp)+
  geom_raster(aes(x = x, y = y, fill = layer), 
              data = df) +
  geom_map(data = pacific_proj, map = world_pacific,
           aes(x=x, y=y,map_id = id))+
  geom_sf(aes(), colour = NA, fill = "grey75", 
          data = pacific_proj) +
  geom_point(aes(x=lon_colony, y=lat_colony),
             data=cols_prj_df,
             colour="red",shape=18,size=20)+
  xlab("") + ylab("") ;codfish

png(paste0(dir,"/scripts_results/codfish.png"), 
    width=2000,height=1125)
par(mfrow=c(1,1))
plot(pop,col=cols_sp)
plot(land,col="#dbdbdb", 
     border = "#c7c7c7", add=T)
points(lat_colony~lon_colony,
       data=all_pops[all_pops$scientific_name == "Pterodroma cookii" &
                       all_pops$colony_name == "Codfish Island",],
       pch=18, cex=2,col="red")
dev.off()

pop <- raster(paste0(dir_1by1,"/",files[2]))


png(paste0(dir,"/scripts_results/littlebarrier.png"), 
    width=2000,height=1125)
plot(pop,col=cols_sp)
plot(land,col="#dbdbdb", 
     border = "#c7c7c7", add=T)
points(lat_colony~lon_colony,
       data=all_pops[all_pops$scientific_name == "Pterodroma cookii" &
                       all_pops$colony_name == "Little Barrier Island",],
       pch=18, cex=2,col="red")
dev.off()


