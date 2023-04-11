## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Plot world maps for plastics, species richness, 
## all species distribution, exposure, and
## exposure with EEZ overlaid. Robinson projection
## Beth Clark 2022
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

rm(list=ls()) 

################ LOADING PACKAGES ###################
library(raster) #3.1-5
library(rgdal) #1.4-8
library(rgeos)#0.5-3
library(RColorBrewer)#1.1-2
library(sf)#0.9-4
library(tidyverse)#1.3.0
library(sp) #1.3-2
library(viridis)#0.5.1

#the above versions were used, but to reproject to the new equal earth project,
#the rgdal package needed to be updated. this might affect previous processes
#that use custom equal area projections centred on the geomean of the data
#and so we might need to return to previous versions.

#Read in land file for visualisation:
#Natural Earth land 1:10m polygons version 5.1.1 
#downloaded from www.naturalearthdata.com/
land <- rgdal::readOGR(dsn = "input_data/baselayer", layer = "ne_10m_land")

bird_dist <- raster("outputs/08_all_species_distribution.tif")
b <- bird_dist
b[is.na(b)] <- 0 
b_sum1 <- b/sum(getValues(b))
b_sum1[is.na(bird_dist)] <- NA
b_sum1[b_sum1 == 0] <- NA

#read in plastics data
plastics <- raster("outputs/00_PlasticsRaster.tif")

## rescale to 1
plastics2 <- plastics
plastics2[is.na(plastics2)] <- 0 
p_sum1    <- plastics2/sum(getValues(plastics2))
p_sum1[is.na(plastics)] <- NA

yelblus <- c(brewer.pal(n = 9, name = "YlGnBu"),"#00172e")
col_birds <- c(colorRampPalette(yelblus)(1000))

# define Robinson projection
proj <- "+proj=robin"

# project shapefile and raster to Robinson projection
land_sf <- st_as_sf(land)
world_prj <- land_sf %>% st_transform(proj)
b_dens_proj <- projectRaster(b_sum1, crs = proj)

# convert raster to dataframe for ggplot
bat_df <- rasterToPoints(b_dens_proj) %>% as_tibble()

# if you can simply mask it then create a polygon that is a square larger than the map area
# then cookie cut out the map region

# define a box to clip shapefile to - make slightly bigger than required to avoid edge clip effects
CP <- sf::st_bbox(c(xmin = -180, xmax = 180, ymin = -90, ymax = 90)) %>%
  sf::st_as_sfc() %>%
  sf::st_segmentize(1) %>% st_set_crs(4326)
CP_prj <- CP %>% st_transform(crs = proj)

# get the bounding box in transformed coordinates and expand by 10%
xlim <- st_bbox(CP_prj)[c("xmin", "xmax")]*1.2
ylim <- st_bbox(CP_prj)[c("ymin", "ymax")]*1.2

# turn into enclosing rectangle
encl_rect <- 
  list(
    cbind(
      c(xlim[1], xlim[2], xlim[2], xlim[1], xlim[1]), 
      c(ylim[1], ylim[1], ylim[2], ylim[2], ylim[1])
    )
  ) %>%
  st_polygon() %>%
  st_sfc(crs = proj)

# calculate the area outside the earth outline as the difference
# between the enclosing rectangle and the earth outline
cookie <- st_difference(encl_rect, CP_prj)

#plot species richness ####
colonies <- read.csv("outputs/02_colony_locations.csv")
colonies <- subset(colonies,!is.na(colonies$lat_colony))

sp::coordinates(colonies) <- ~lon_colony+lat_colony
proj4string(colonies) <- proj4string(land)

colonies_prj <- spTransform(colonies,proj)
colonies_df <- as.data.frame(colonies_prj)

sp_rich <- raster("outputs/08_species_richness.tif")
cols_sp_rich <- c("white",
                  colorRampPalette(yelblus)(sp_rich@data@max))
# convert raster to dataframe for ggplot
sp_rich_prj <- projectRaster(sp_rich, crs = proj)
r_df <- rasterToPoints(sp_rich_prj) %>% as_tibble()

p1 <- ggplot() +
  theme_minimal() +
  scale_fill_gradientn(colors = cols_sp_rich)+
  theme(legend.position = "none")+
  geom_tile(aes(x = x, y = y, fill = X08_species_richness),
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
  geom_tile(aes(x = x, y = y, fill = X08_all_species_distribution),
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
p_max <- (max(getValues(plastics),na.rm = T))/10
p_cap[p_cap > p_max] <- p_max
p_dens_proj <- projectRaster(p_cap, crs = proj)

# convert raster to dataframe for ggplot
p_df <- rasterToPoints(p_dens_proj) %>% as_tibble()

p3 <- ggplot() +
  theme_minimal() +
  scale_fill_viridis(option="inferno",direction=-1)+
  theme(legend.position = "none")+
  geom_tile(aes(x = x, y = y, fill = X00_PlasticsRaster),
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
e_max <- (max(getValues(exposure),na.rm = T))/100
e_cap[e_cap > e_max] <- e_max
exposure_proj <- projectRaster(e_cap, crs = proj)

# convert raster to dataframe for ggplot
exposure_df <- rasterToPoints(exposure_proj) %>% as_tibble()
p4 <- ggplot() +
  theme_minimal() +
  scale_fill_viridis(option="inferno",direction=-1)+
  theme(legend.position = "none")+
  geom_tile(aes(x = x, y = y, fill = layer),
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
eez <- readOGR(dsn="input_data/EEZ_land_union_v3_202003", layer = "EEZ_Land_v3_202030") 

eez_sf <- st_as_sf(eez)
eez_prj <- eez_sf %>% st_transform(proj)
p5 <- ggplot() +
  theme_minimal() +
  scale_fill_viridis(option="inferno",direction=-1)+
  theme(legend.position = "none")+
  geom_tile(aes(x = x, y = y, fill = layer),
            data = exposure_df) +
  geom_sf(aes(), fill = NA, colour = "#2684ff", data = eez_prj) +
  geom_sf(aes(), colour = NA, fill = "grey75", data = world_prj) +
  geom_sf(aes(), fill = "white", color = "black", data = cookie, size = .5) +
  xlab("") + ylab("");p5

png("outputs/09_exposure_capped1percent_eez.png", width=2000,height=1125)
p5
dev.off()
dev.off()

