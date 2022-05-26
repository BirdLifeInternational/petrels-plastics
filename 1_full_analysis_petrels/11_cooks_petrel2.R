#Case study figures for plastics project

################ LOADING PACKAGES ###################
rm(list=ls()) 

library(ggthemes)
library(sp) #1.3-2
library(raster) #3.1-5
library(rgdal) #1.4-8
library(cowplot) #1.0.0
library(viridis)#0.5.1
library(stringr)#1.4.0
library(RColorBrewer)#1.1-2
library(geomerge)#0.3.2
library(maptools)#1.0-1
library(gridExtra)#2.3
library(sf)#0.9-4
library(tidyverse)#1.3.0
library(rgeos)#0.5-3
library(adehabitatHR)

######### GENERAL DIRECTIONS AND FILES ##############

## GENERAL DIR
dir <- paste0("C:/Users/bethany.clark/OneDrive - BirdLife International/",
              "Methods/pacific_centre_robinson") ## copy and paste here your working directory

land <- readOGR(dsn=paste0(dir,"/baselayer"), layer = "world-dissolved") #Changed - BC 

#for colony locations
colonies <- read.csv(paste0(dir,"/colony_locations.csv"))
colony <- colonies[colonies$scientific_name == "Pterodroma cookii" &
                 colonies$colony_name == "Codfish Island",]
sp::coordinates(colony) <- ~lon_colony+lat_colony
proj4string(colony) <- proj4string(land)

#colour palette
yelblus <- c(brewer.pal(n = 9, name = "YlGnBu"),"#00172e")
cols_sp <- colorRampPalette(yelblus)(255)

#plot nbr maps for Pterodroma cookii
pop <- raster(paste0(dir,"/Pterodroma cookii_Codfish Island_sum_nonbr.tif"))
plot(pop)

#Normal Robinson projection ####
# define Robinson projection
proj <- "+proj=robin"

# project shapefile and raster to Robinson projection
land_sf <- st_as_sf(land)
world_prj <- land_sf %>% st_transform(proj)
b_dens_proj <- projectRaster(pop, crs = proj)

cols_prj <- spTransform(colony,proj)
cols_prj_df <- as.data.frame(cols_prj)

# convert raster to dataframe for ggplot
bird_df <- rasterToPoints(b_dens_proj) %>% as_tibble()
names(bird_df)[3] <- "layer"

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

# cookie cut/ mask plot
p2 <- ggplot() +
  theme_minimal() +
  scale_fill_gradientn(colors = col_birds)+
  theme(legend.position = "none")+
  geom_raster(aes(x = x, y = y, fill = layer),
              data = bird_df) +
  geom_sf(aes(), colour = NA, fill = "grey75", data = world_prj) +
  geom_sf(aes(), fill = "white", color = "black", data = cookie, size = .5) +
  geom_point(aes(x=lon_colony,y=lat_colony),
             data=cols_prj_df,colour="red",pch=18,size=6)+
  xlab("") + ylab("")
p2

#Pacific centred (180* lon or similar)
pacific_proj <- "+proj=robin +lon_0=180 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs"

land_proj <-  spTransform(land,pacific_proj)
plot(land_proj)
#odd lines joining the edges

cols_prj <- spTransform(colony,pacific_proj)
cols_prj_df <- as.data.frame(cols_prj)

plot(cols_prj,add=T,col="red",pch=18)
#correct location

dens_proj <- projectRaster(pop, crs = pacific_proj)
plot(dens_proj)
#not working



