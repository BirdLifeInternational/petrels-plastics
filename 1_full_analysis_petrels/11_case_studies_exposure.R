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

dir_seasons <- paste0(dir,"/scripts_results/07_seasons")
files <- list.files(dir_seasons,pattern = "diom")

all_scopolis <- raster(paste0(dir_seasons,"/",files[str_detect(files,"nonbr")]))

files <- list.files(dir_seasons,pattern = "yelk")

all_yelk <- raster(paste0(dir_seasons,"/",files[str_detect(files,"nonbr")]))

# project shapefile and raster to Robinson projection
yelk_dens_proj <- projectRaster(all_yelk, crs = proj)
#yelk_dens_proj[yelk_dens_proj == 0] <- NA
yelk_df <- rasterToPoints(yelk_dens_proj) %>% as_tibble()
names(yelk_df)[3] <- "layer"

scop_dens_proj <- projectRaster(all_scopolis, crs = proj)
#scop_dens_proj[scop_dens_proj == 0] <- NA
scop_df <- rasterToPoints(scop_dens_proj) %>% as_tibble()
names(scop_df)[3] <- "layer"


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

#50% ud
no0_yelk <- yelk_dens_proj
no0_yelk[no0_yelk==0] <- NA
ud50_yelk <-  yelk_dens_proj
ud50_yelk[ud50_yelk>quantile(no0_yelk, probs = c((75/95)), type=7,names = FALSE)] <- 1
plot(ud50_yelk)
p_yelk <- rasterToContour(ud50_yelk)
plot(p_yelk)
p_yelk2 <- p_yelk[p_yelk$level == 1,]
poly_yelk <- st_as_sf(p_yelk2)

no0_scop <- scop_dens_proj
no0_scop[no0_scop==0] <- NA
ud50_scop <-  scop_dens_proj
ud50_scop[ud50_scop>quantile(no0_scop, probs = c((75/95)), type=7,names = FALSE)] <- 1
plot(ud50_scop)
p_scop <- rasterToContour(ud50_scop)
plot(p_scop)
p_scop2 <- p_scop[p_scop$level == 1,]
plot(p_scop2)
poly_scop <- st_as_sf(p_scop2)

#plot
yelk<- ggplot() +
  theme_minimal() +  
  scale_fill_gradientn(colors = cols_inf)+
  geom_raster(aes(x = x, y = y, fill = layer), 
              data = yelk_df) +
  geom_sf(aes(), colour = cols_sp[100], fill = NA, 
          data = poly_yelk, size = 4) +  
  geom_sf(aes(), colour = NA, fill = "grey75", 
          data = world_prj) +
  geom_point(aes(x=lon_colony, y=lat_colony),
             data=cols_yelk_prj_df,
             colour="#004fd9",shape=18,size=24)+
  xlab("") + ylab("") ;yelk



scop<- ggplot() +
  theme_minimal() +  
  scale_fill_gradientn(colors = cols_inf,
                       limits = c(0,max(yelk_df$layer)))+
  geom_raster(aes(x = x, y = y, fill = layer), 
              data = scop_df) +
  geom_sf(aes(), colour = cols_sp[100], fill = NA, 
          data = poly_scop, size = 4) +  
  geom_sf(aes(), colour = NA, fill = "grey75", 
          data = world_prj) +
  geom_point(aes(x=lon_colony, y=lat_colony),
             data=cols_scop_prj_df,
             colour="#004fd9",shape=18,size=24)+
  xlab("") + ylab("") ;scop

png(paste0(dir,"/scripts_results/yelk_exp.png"), 
    width=5000,height=2625)
yelk
dev.off()
dev.off()

png(paste0(dir,"/scripts_results/scop_exp.png"), 
    width=5000,height=2625)
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




lb_pop <- raster(paste0(dir_1by1,"/",files[2]))
m2 <- lb_pop
m2[is.na(m2)] <- 0 
lb_sum1    <- m2/sum(getValues(m2))
lb_sum1[is.na(m)] <- NA

lb_exp <- lb_sum1*p_sum1
lb_df <- rasterToPoints(lb_exp) %>% as_tibble()
names(lb_df)[3] <- "layer"

#add colony locations
col <- cols[cols$scientific_name == "Pterodroma cookii" &
               cols$colony_name == "Little Barrier Island",]
sp::coordinates(col) <- ~lon_colony+lat_colony
proj4string(col) <- proj4string(land)
#cols_prj <- spTransform(col,pacific_proj)
#cols_prj_df <- as.data.frame(cols_prj)
lb_prj_df <- as.data.frame(col)

#50% ud
no0_lb <- lb_pop
no0_lb[no0_lb==0] <- NA
ud50_lb <-  lb_pop
ud50_lb[ud50_lb>quantile(no0_lb, probs = c((75/95)), type=7,names = FALSE)] <- 1
plot(ud50_lb)
p_lb <- rasterToContour(ud50_lb)
plot(p_lb)
p_lb2 <- p_lb[p_lb$level == 1,]
plot(p_lb2)
poly_lb <- st_as_sf(p_lb2)

lb <- ggplot() +
  theme_minimal() +  
  scale_fill_gradientn(colors = cols_inf)+
  geom_raster(aes(x = x, y = y, fill = layer), 
              data = lb_df) +
  geom_sf(aes(), colour = NA, fill = "grey75", 
          data = land_sf) +
  geom_sf(aes(), colour = cols_sp[100], fill = NA, 
          data = poly_lb, size = 4) +
  geom_point(aes(x=lon_colony, y=lat_colony),
             data=lb_prj_df,
             colour="#004fd9",shape=18,size=24)+
  xlab("") + ylab("") ;lb

png(paste0(dir,"/scripts_results/littlebarrier_exp.png"), 
    width=5000,height=2625)

lb

dev.off()

cod_pop <- raster(paste0(dir_1by1,"/",files[1]))

m2 <- cod_pop
m2[is.na(m2)] <- 0 
cod_sum1    <- m2/sum(getValues(m2))
cod_sum1[is.na(m)] <- NA

cod_exp <- cod_sum1*p_sum1

# project shapefile and raster to Robinson projection
#pacific_proj <- "+proj=robin +lon_0=180 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs"

#dens_proj <- projectRaster(cod_exp, crs = proj)
#plot(dens_proj)
#df <- rasterToPoints(dens_proj) %>% as_tibble()
cf_df <- rasterToPoints(cod_exp) %>% as_tibble()
names(cf_df)[3] <- "layer"

#add colony locations
col <- cols[cols$scientific_name == "Pterodroma cookii" &
              cols$colony_name == "Codfish Island",]
sp::coordinates(col) <- ~lon_colony+lat_colony
proj4string(col) <- proj4string(land)
#cols_prj <- spTransform(col,pacific_proj)
#cols_prj_df <- as.data.frame(cols_prj)
cf_prj_df <- as.data.frame(col)

#50% ud
no0_cf <- cod_pop
no0_cf[no0_cf==0] <- NA
ud50_cf <-  cod_pop
ud50_cf[ud50_cf>quantile(no0_cf, probs = c((75/95)), type=7,names = FALSE)] <- 1
plot(ud50_cf)
p_cf <- rasterToContour(ud50_cf)
plot(p_cf)
p_cf2 <- p_cf[p_cf$level == 1,]
plot(p_cf2)
poly_cf <- st_as_sf(p_cf2)


cf <- ggplot() +
  theme_minimal() +  
  scale_fill_gradientn(colors = cols_inf,
                       limits = c(0,max(lb_df$layer)))+
  geom_raster(aes(x = x, y = y, fill = layer), 
              data = cf_df) +
  geom_sf(aes(), colour = NA, fill = "grey75", 
          data = land_sf) +
  geom_sf(aes(), colour = cols_sp[100], fill = NA, 
          data = poly_cf, size = 4) +
  geom_point(aes(x=lon_colony, y=lat_colony),
             data=cf_prj_df,
             colour="#004fd9",shape=18,size=24)+
  xlab("") + ylab("") ;cf

png(paste0(dir,"/scripts_results/codfish_exp.png"), 
    width=5000,height=2625)
cf

dev.off()
dev.off()


