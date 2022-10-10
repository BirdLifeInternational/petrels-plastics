## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Plot world maps for plastics, species richness, 
## all species distribution, exposure, and
## exposure with EEZ overlayed. Robinson projection
## Beth Clark 2022
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

rm(list=ls()) 

## Load packages ####
library(raster) #3.1-5
library(rgdal) #1.4-8
library(rgeos)#0.5-3
library(RColorBrewer)#1.1-2
library(sf)#0.9-4
library(tidyverse)#1.3.0
library(sp) #1.3-2
library(viridis)#0.5.1
library(cowplot)

#the above versions were used, but to reproject to the new equal earth project,
#the rgdal package needed to be updated. this might affect previous processes
#that use custom equal area projections centred on the geomean of the data
#and so we might need to return to previous versions.

## Input data ####
land <- rgdal::readOGR(dsn = paste0("input_data/baselayer"), layer = "world-dissolved") 

bird_dist <- raster::raster("outputs/08_all_species_distribution.tif")
b <- bird_dist
b[is.na(b)] <- 0 
b_sum1 <- b/sum(raster::getValues(b))
b_sum1[is.na(bird_dist)] <- NA
b_sum1[b_sum1 == 0] <- NA

#read in plastics data
plastics <- raster("outputs/00_PlasticsRaster.tif")

## rescale to 1
plastics2 <- plastics
plastics2[is.na(plastics2)] <- 0 
p_sum1    <- plastics2/sum(raster::getValues(plastics2))
p_sum1[is.na(plastics)] <- NA

yelblus <- c(brewer.pal(n = 9, name = "YlGnBu"),"#00172e")
col_birds <- c(colorRampPalette(yelblus)(1000))

# define Robinson projection
proj <- "+proj=robin"

# load an example land shapefile from rnaturalearth dataset
#world <- ne_countries(scale = "medium", returnclass = "sf")

# project shapefile and raster to Robinson projection
land_sf <- sf::st_as_sf(land)
world_prj <- land_sf %>% sf::st_transform(proj)
b_dens_proj <- raster::projectRaster(b_sum1, crs = proj)

# convert raster to dataframe for ggplot
bat_df <- raster::rasterToPoints(b_dens_proj) %>% as_tibble()

# if you can simply mask it then create a polygon that is a square larger than the map area
# then cookie cut out the map region

# define a box to clip shapefile to - make slightly bigger than required to avoid edge clip effects
CP <- sf::st_bbox(c(xmin = -180, xmax = 180, ymin = -90, ymax = 90)) %>%
  sf::st_as_sfc() %>%
  sf::st_segmentize(1) %>% st_set_crs(4326)
CP_prj <- CP %>% sf::st_transform(crs = proj)

# get the bounding box in transformed coordinates and expand by 10%
xlim <- sf::st_bbox(CP_prj)[c("xmin", "xmax")]*1.2
ylim <- sf::st_bbox(CP_prj)[c("ymin", "ymax")]*1.2

# turn into enclosing rectangle
encl_rect <- 
  list(
    cbind(
      c(xlim[1], xlim[2], xlim[2], xlim[1], xlim[1]), 
      c(ylim[1], ylim[1], ylim[2], ylim[2], ylim[1])
    )
  ) %>%
  sf::st_polygon() %>%
  sf::st_sfc(crs = proj)

# calculate the area outside the earth outline as the difference
# between the enclosing rectangle and the earth outline
cookie <- sf::st_difference(encl_rect, CP_prj)

#plot species richness ####
colonies <- read.csv("outputs/02_colony_locations.csv")
colonies <- subset(colonies,!is.na(colonies$lat_colony))

sp::coordinates(colonies) <- ~lon_colony+lat_colony
sp::proj4string(colonies) <- sp::proj4string(land)

colonies_prj <- sp::spTransform(colonies,proj)
colonies_df <- as.data.frame(colonies_prj)

sp_rich <- raster::raster("outputs/08_species_richness.tif")
cols_sp_rich <- c("white",
                  colorRampPalette(yelblus)(sp_rich@data@max))
# convert raster to dataframe for ggplot
sp_rich_prj <- raster::projectRaster(sp_rich, crs = proj)
r_df <- raster::rasterToPoints(sp_rich_prj) %>% as_tibble()

p1 <- ggplot() +
  theme_minimal() +
  scale_fill_gradientn(colors = cols_sp_rich)+
  theme(legend.position = "none")+
  geom_raster(aes(x = x, y = y, fill = X08_species_richness),
              data = r_df) +
  geom_sf(aes(), colour = NA, fill = "grey75", 
          data = world_prj) +
  geom_point(aes(x=lon_colony,y=lat_colony),
             data=colonies_df,colour="red",pch=18,size=6)+
  geom_sf(aes(), fill = "white", color = "black", 
          data = cookie, size = .5) +
  xlab("") + ylab("");p1

png("outputs/09_species_richness.png", 
    width=2000,height=1125)
p1
dev.off()
dev.off()

# cookie cut/ mask plot
p2 <- ggplot() +
  theme_minimal() +
  scale_fill_gradientn(colors = col_birds)+
  theme(legend.position = "none")+
  geom_raster(aes(x = x, y = y, fill = X08_all_species_distribution),
              data = bat_df) +
  geom_sf(aes(), colour = NA, fill = "grey75", data = world_prj) +
  geom_sf(aes(), fill = "white", color = "black", data = cookie, size = .5) +
  xlab("") + ylab("");p2

#plot bird density ####

png("outputs/09_allspecies_density.png",
    width=2000,height=1125)
p2
dev.off()
dev.off()

#plot plastic ####
p_cap <- plastics
p_max <- (max(raster::getValues(plastics),na.rm = T))/10
p_cap[p_cap > p_max] <- p_max
p_dens_proj <- raster::projectRaster(p_cap, crs = proj)

# convert raster to dataframe for ggplot
p_df <- raster::rasterToPoints(p_dens_proj) %>% as_tibble()

p3 <- ggplot() +
  theme_minimal() +
  scale_fill_viridis(option="inferno",direction=-1)+
  theme(legend.position = "none")+
  geom_raster(aes(x = x, y = y, fill = X00_PlasticsRaster),
              data = p_df) +
  geom_sf(aes(), colour = NA, fill = "grey75", 
          data = world_prj) +
  geom_sf(aes(), fill = "white", color = "black", 
          data = cookie, size = .5) +
  xlab("") + ylab("");p3

png("outputs/09_plastic_capped_10percent.png", 
    width=2000,height=1125)
p3
dev.off()
dev.off()

#plot exposure ####
exposure <- b_sum1 * p_sum1
exposure[exposure == 0] <- NA
e_cap <- exposure
e_max <- (max(raster::getValues(exposure),na.rm = T))/100
e_cap[e_cap > e_max] <- e_max
exposure_proj <- raster::projectRaster(e_cap, crs = proj)

# convert raster to dataframe for ggplot
exposure_df <- raster::rasterToPoints(exposure_proj) %>% as_tibble()
p4 <- ggplot() +
  theme_minimal() +
  scale_fill_viridis(option="inferno",direction=-1)+
  theme(legend.position = "none")+
  geom_raster(aes(x = x, y = y, fill = layer),
              data = exposure_df) +
  geom_sf(aes(), colour = NA, fill = "grey75", data = world_prj) +
  geom_sf(aes(), fill = "white", color = "black", 
          data = cookie, size = .5) +
  xlab("") + ylab("");p4

png("outputs/09_exposure_capped_1percent.png", 
    width=2000,height=1125)
p4
dev.off()
dev.off()

#plot overlap + eez ####

#Flanders Marine Institute (2020). Union of the ESRI Country shapefile and the Exclusive Economic Zones (version 3). Available online at https://www.marineregions.org/. https://doi.org/10.14284/403. Consulted on 2021-03-04.
eez <- rgdal::readOGR(dsn = "input_data/EEZ_land_union_v3_202003", layer = "EEZ_Land_v3_202030") 

eez_sf <- sf::st_as_sf(eez)
eez_prj <- eez_sf %>% sf::st_transform(proj)
p5 <- ggplot() +
  theme_minimal() +
  scale_fill_viridis(option="inferno",direction=-1)+
  theme(legend.position = "none")+
  geom_raster(aes(x = x, y = y, fill = layer),
              data = exposure_df) +
  geom_sf(aes(), fill = NA, colour = "#2684ff", data = eez_prj) +
  geom_sf(aes(), colour = NA, fill = "grey75", data = world_prj) +
  geom_sf(aes(), fill = "white", color = "black", data = cookie, size = .5) +
  xlab("") + ylab("");p5

png("outputs/09_exposure_capped1percent_eez.png", width=2000,height=1125)
p5
dev.off()
dev.off()

#Case study figures for manuscript #####

dir_1by1 <- "outputs/07_seasons"
dat <- read.csv("outputs/07_exposure_scores_by_season.csv")
dat$species <- gsub("_.*","",dat$species_pop)
head(dat)
#1 storm petrel overlap for the breeding season for 5 colonies ####

#plot all cols on 1 distribution, labelling the colony locations

files <- list.files(dir_1by1,pattern = "pelagicus")
files <- files[files != "Hydrobates pelagicus_Malta_nonbr.tif"]

for(i in 1:length(files)){
  pop <- raster::raster(paste0(dir_1by1,"/",files[i]))
  p2 <- pop
  p2[is.na(p2)] <- 0 
  pop_sum1    <- p2/sum(raster::getValues(p2))
  pop_sum1[is.na(pop)] <- NA
  
  pop_over <- p_sum1*pop_sum1
  
  if(i == 1){
    all_stormies <- pop_over
  } else {
    all_stormies <- all_stormies + pop_over
  }
  plot(all_stormies)
}

# project shapefile and raster to Robinson projection
b_dens_proj <- raster::projectRaster(all_stormies, crs = proj)

# convert raster to dataframe for ggplot
bat_df <- raster::rasterToPoints(b_dens_proj) %>% as_tibble()

#add colony locations
stormies_df <- colonies_df[colonies_df$species == "Hydrobates pelagicus",]

# generate  ggplot figure
p1 <- ggplot() +
  theme_minimal() +  
  scale_fill_viridis(option="inferno",direction=-1)+
  geom_raster(aes(x = x, y = y, fill = layer), 
              data = bat_df) +
  geom_sf(aes(), colour = NA, fill = "grey75", 
          data = world_prj) +
  geom_point(aes(x=lon_colony, y=lat_colony),
             data=stormies_df,
             colour="#004fd9",shape=18,size=30)+
  xlab("") + ylab("") ;p1

png("outputs/09_CaseStudy_Hydrobates_pelagicus.png", 
    width=10000,height=5625)
p1
dev.off()
dev.off()

dat_stormies <- dat[dat$species == "Hydrobates pelagicus",];dat_stormies

#2 yelkoaun v scopoli's shearwaters ####
dat_med <- dat[dat$species == "Calonectris diomedea" |
                 dat$species == "Puffinus yelkouan",]

dat_med <- dat_med[complete.cases(dat_med),];dat_med

#the two species share the colony "Malta", so plot
#data for this colony
dir_seasons <- "outputs/07_seasons"
files <- list.files(dir_seasons,pattern = "diom")
files <- files[str_detect(files,"Malta_nonbr")]
all_scopolis <- raster::raster(paste0(dir_1by1,"/",files[1]))

files <- list.files(dir_seasons,pattern = "yelk")
files <- files[str_detect(files,"Malta_nonbr")]
all_yelk <- raster::raster(paste0(dir_1by1,"/",files[1]))

# project shapefile and raster to Robinson projection
yelk_dens_proj <- raster::projectRaster(all_yelk, crs = proj)
yelk_df <- raster::rasterToPoints(yelk_dens_proj) %>% as_tibble()
names(yelk_df)[3] <- "layer"

scop_dens_proj <- raster::projectRaster(all_scopolis, crs = proj)
scop_df <- raster::rasterToPoints(scop_dens_proj) %>% as_tibble()
names(scop_df)[3] <- "layer"

#add colony locations
cols_yelk_prj_df <- colonies_df[colonies_df$species == "Puffinus yelkouan"&
                                colonies_df$colony == "Malta",]
cols_scop_prj_df <- colonies_df[colonies_df$species == "Calonectris diomedea"&
                                  colonies_df$colony == "Malta",]

#50% ud
no0_yelk <- yelk_dens_proj
no0_yelk[no0_yelk==0] <- NA
ud50_yelk <-  yelk_dens_proj
ud50_yelk[ud50_yelk>quantile(no0_yelk, probs = c((75/95)), type=7,names = FALSE)] <- 1
plot(ud50_yelk)
p_yelk <- raster::rasterToContour(ud50_yelk)
p_yelk2 <- p_yelk[p_yelk$level == 0.5,]
plot(p_yelk2)
poly_yelk <- st_as_sf(p_yelk2)

no0_scop <- scop_dens_proj
no0_scop[no0_scop==0] <- NA
ud50_scop <-  scop_dens_proj
ud50_scop[ud50_scop>quantile(no0_scop, probs = c((75/95)), type=7,names = FALSE)] <- 1
plot(ud50_scop)
p_scop <- raster::rasterToContour(ud50_scop)
p_scop2 <- p_scop[p_scop$level == 0.5,]
plot(p_scop2)
poly_scop <- st_as_sf(p_scop2)

#set colours
cols_inferno <- rev(inferno(20))
cols_inf <- colorRampPalette(c(cols_inferno))(255)

#plot
yelk<- ggplot() +
  theme_minimal() +  
  scale_fill_gradientn(colors = cols_inf)+
  geom_raster(aes(x = x, y = y, fill = layer), 
              data = yelk_df) +
  geom_sf(aes(), colour = "black", fill = NA, 
          data = poly_yelk, size = 1.5) +  
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
  geom_sf(aes(), colour = "black", fill = NA, 
          data = poly_scop, size = 1.5) +  
  geom_sf(aes(), colour = NA, fill = "grey75", 
          data = world_prj) +
  geom_point(aes(x=lon_colony, y=lat_colony),
             data=cols_scop_prj_df,
             colour="#004fd9",shape=18,size=24)+
  xlab("") + ylab("") ;scop

png("outputs/09_CaseStudy_YelkuoanShearwater.png", 
    width=5000,height=2625)
yelk
dev.off()
dev.off()

png("outputs/09_CaseStudy_ScopoliShearwater.png", 
    width=5000,height=2625)
scop
dev.off()
dev.off()

#3 cook's petrels little barrier v codfish islands ####

#plot nonbreeding exposure maps for both colonies side by side

dat_cookii <- dat[dat$species == "Pterodroma cookii",];dat_cookii

files <- list.files(dir_1by1,pattern = "cook")
files <- files[str_detect(files,"nonbr")]

lb_pop <- raster::raster(paste0(dir_1by1,"/",files[2]))
m2 <- lb_pop
m2[is.na(m2)] <- 0 
lb_sum1    <- m2/sum(raster::getValues(m2))
lb_sum1[is.na(plastics)] <- NA

lb_exp <- lb_sum1*p_sum1
lb_df <- raster::rasterToPoints(lb_exp) %>% as_tibble()
names(lb_df)[3] <- "layer"

#add colony locations
colonies_df2 <- as.data.frame(colonies)
lb_prj_df <- colonies_df2[colonies_df2$species == "Pterodroma cookii" &
                            colonies_df2$colony == "Little Barrier Island",]

#50% ud
no0_lb <- lb_pop
no0_lb[no0_lb==0] <- NA
ud50_lb <-  lb_pop
ud50_lb[ud50_lb>quantile(no0_lb, probs = c((75/95)), type=7,names = FALSE)] <- 1
plot(ud50_lb)
p_lb <- raster::rasterToContour(ud50_lb)
p_lb2 <- p_lb[p_lb$level == 0.5,]
plot(p_lb2)
poly_lb <- st_as_sf(p_lb2)

lb <- ggplot() +
  theme_minimal() +  
  scale_fill_gradientn(colors = cols_inf) +
  geom_raster(aes(x = x, y = y, fill = layer), 
              data = lb_df) +
  geom_sf(aes(), colour = NA, fill = "grey75", 
          data = land_sf) +
  geom_sf(aes(), colour = "black", fill = NA, 
          data = poly_lb, size = 1.5) +
  geom_point(aes(x=lon_colony, y=lat_colony),
             data=lb_prj_df,
             colour="#004fd9",shape=18,size=24)+
  xlab("") + ylab("") ;lb

png("outputs/09_CaseStudy_CooksPetrel_LittleBarrier.png", 
    width=5000,height=2625)
lb
dev.off()

cod_pop <- raster::raster(paste0(dir_1by1,"/",files[1]))

m2 <- cod_pop
m2[is.na(m2)] <- 0 
cod_sum1    <- m2/sum(raster::getValues(m2))
cod_sum1[is.na(m)] <- NA

cod_exp <- cod_sum1*p_sum1

# project shapefile and raster to Robinson projection
#pacific_proj <- "+proj=robin +lon_0=180 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs"

#dens_proj <- projectRaster(cod_exp, crs = proj)
#plot(dens_proj)
#df <- rasterToPoints(dens_proj) %>% as_tibble()
cf_df <- raster::rasterToPoints(cod_exp) %>% as_tibble()
names(cf_df)[3] <- "layer"

#add colony locations
cf_prj_df <- colonies_df2[colonies_df2$species == "Pterodroma cookii" &
                           colonies_df2$colony == "Codfish Island",]

#50% ud
no0_cf <- cod_pop
no0_cf[no0_cf==0] <- NA
ud50_cf <-  cod_pop
ud50_cf[ud50_cf>quantile(no0_cf, probs = c((75/95)), type=7,names = FALSE)] <- 1
plot(ud50_cf)
p_cf <- raster::rasterToContour(ud50_cf)
p_cf2 <- p_cf[p_cf$level == 0.5,]
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
  geom_sf(aes(), colour = "black", fill = NA, 
          data = poly_cf, size = 1.5) +
  geom_point(aes(x=lon_colony, y=lat_colony),
             data=cf_prj_df,
             colour="#004fd9",shape=18,size=24)+
  xlab("") + ylab("") ;cf

png("outputs/09_CaseStudy_CooksPetrel_Codfish.png", 
    width=5000,height=2625)
cf
dev.off()
dev.off()

