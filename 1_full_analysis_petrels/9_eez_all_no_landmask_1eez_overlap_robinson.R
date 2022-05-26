## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Mapping the global distribution of seabird populations
## R script to aggregate results into a 5x5 degree grid
## Ana Carneiro and Anne-Sophie Bonnet-Lebrun
## July 2018
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#Adapted by Beth Clark Mar 2020 for overlap with 1x1 degree plastics data
rm(list=ls()) 

################ LOADING PACKAGES ###################
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

#the above versions were used, but to reproject to the new equal earth project,
#the rgdal package needed to be updated. this might mess up previous processes
#that use custom equal area projections centred on the geomean of the data
#and so we might need to return to previous versions.

#install.packages("proj4")
#library(proj4)#1.0-10.1

######### GENERAL DIRECTIONS AND FILES ##############

## GENERAL DIR
dir <- paste0("C:/Users/bethany.clark/OneDrive - BirdLife International/",
              "Methods") ## copy and paste here your working directory

## DIRECTION TO YOUR RASTERS (ALL DEM CLASSES COMBINED AND BY YEAR QUARTER)
#dir_demClasses <- paste0(dir,"/scripts_results/09_sum_demClasses")
land <- readOGR(dsn=paste0(dir,"/baselayer"), layer = "world-dissolved") #Changed - BC  

pops <- read.csv(paste0(dir,"/scripts_results/05_phenology/pops.csv"))

#eez ####
#Flanders Marine Institute (2020). Union of the ESRI Country shapefile and the Exclusive Economic Zones (version 3). Available online at https://www.marineregions.org/. https://doi.org/10.14284/403. Consulted on 2021-03-04.
eez_file <- readOGR(dsn=paste0(dir,"/EEZ_land_union_v3_202003"), layer = "EEZ_Land_v3_202030") 
#martin did not use a polygon for highseas, but coded NAs as high seas
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
plot(eez_file, add=T)

plot(eez_file)

eez <- bind(eez_file,highseas)
#to check
plot(highseas,col="green")
plot(eez[22,],add=T,col="red",border="orange");eez[22,]

#rm(eez_file)

eez@data$UNION[nrow(eez@data)] <- "High Seas"
eez@data$TERRITORY1[nrow(eez@data)] <- "High Seas"
eez@data$SOVEREIGN1[nrow(eez@data)] <- "High Seas"
eez@data$POL_TYPE[nrow(eez@data)] <- "High Seas"
eez@data$AREA_KM2[nrow(eez@data)] <- 219116810
tail(eez@data)

plot(eez,col="blue")

eez_proj <- eez

sp_files <- list.files(paste0(dir,"/scripts_results/06_species/"))

bird_dist <- raster("C:/Users/bethany.clark/OneDrive - BirdLife International/Methods/scripts_results/all_birds_distribution.tif")
plot(bird_dist)
b <- bird_dist
b[is.na(b)] <- 0 
sum(getValues(b))
b_sum1 <- b/sum(getValues(b))
b_sum1[is.na(bird_dist)] <- NA

m <- raster("C:/Users/bethany.clark/OneDrive - BirdLife International/Data/AverageForBeth2.tif")

## rescale to 1
m2 <- m
m2[is.na(m2)] <- 0 
p_sum1    <- m2/sum(getValues(m2))
p_sum1[is.na(m)] <- NA

#plot(p_sum1, main = "million plastic piece per 10km2")

a <- b_sum1 * p_sum1


yelblus <- c(brewer.pal(n = 9, name = "YlGnBu"),"#00172e")

col_birds <- c(colorRampPalette(yelblus)(1000))
b_sum1[b_sum1 == 0] <- NA

# define Robinson projection
proj <- "+proj=robin"

# load an example land shapefile from rnaturalearth dataset
#world <- ne_countries(scale = "medium", returnclass = "sf")

# project shapefile and raster to Robinson projection
land_sf <- st_as_sf(land)
world_prj <- land_sf %>% st_transform(proj)
b_dens_proj <- projectRaster(b_sum1, crs = proj)

# convert raster to dataframe for ggplot
bat_df <- rasterToPoints(b_dens_proj) %>% as_tibble()

# generate quick ggplot figure
p1 <- ggplot() +
  geom_raster(aes(x = x, y = y, fill = all_birds_distribution), 
              data = bat_df) +
  geom_sf(aes(), colour = "grey50", fill = "grey50", 
          data = world_prj);p1

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
  geom_raster(aes(x = x, y = y, fill = all_birds_distribution),
              data = bat_df) +
  geom_sf(aes(), colour = NA, fill = "grey75", data = world_prj) +
  geom_sf(aes(), fill = "white", color = "black", data = cookie, size = .5) +
  xlab("") + ylab("");p2
# I've coloured the line black so you can see it - make this white in the final figure

#plot bird density ####

png(paste0(dir,"/scripts_results/density_weight_pop_season_rob.png"),
    width=2000,height=1125)
p2
dev.off()
dev.off()

#plot species richness ####

sp_rich <- raster(paste0(dir,"/all_birds_sp_richness.tif"))
cols_sp <- c("white",
             colorRampPalette(yelblus)(sp_rich@data@max))

all_pops <- read.csv(paste0(dir,"/scripts_results/all_locations.csv"))
cols <- all_pops[!duplicated(all_pops[
  c("lon_colony","lat_colony","scientific_name")]),]
cols <- subset(cols,!is.na(cols$lat_colony))

sp::coordinates(cols) <- ~lon_colony+lat_colony
proj4string(cols) <- proj4string(land)

cols_prj <- spTransform(cols,proj)
cols_df <- as.data.frame(cols_prj)

# convert raster to dataframe for ggplot
sp_rich_prj <- projectRaster(sp_rich, crs = proj)
r_df <- rasterToPoints(sp_rich_prj) %>% as_tibble()

p1 <- ggplot() +
  theme_minimal() +
  scale_fill_gradientn(colors = cols_sp)+
  theme(legend.position = "none")+
  geom_raster(aes(x = x, y = y, fill = all_birds_sp_richness),
              data = r_df) +
  geom_sf(aes(), colour = NA, fill = "grey75", 
          data = world_prj) +
  geom_point(aes(x=lon_colony,y=lat_colony),
             data=cols_df,colour="red",pch=18,size=6)+
  geom_sf(aes(), fill = "white", color = "black", 
          data = cookie, size = .5) +
  xlab("") + ylab("");p1

png(paste0(dir,"/scripts_results/sp_rich_rob.png"), 
    width=2000,height=1125)
par(mfrow=c(1,1))
p1
dev.off()
dev.off()

#plot plastic ####
p_cap <- m
p_cap[p_cap > 2422700.2] <- 2422700.2
p_dens_proj <- projectRaster(p_cap, crs = proj)

# convert raster to dataframe for ggplot
p_df <- rasterToPoints(p_dens_proj) %>% as_tibble()

cols <- rev(inferno(3000))
p3 <- ggplot() +
  theme_minimal() +
  scale_fill_gradientn(colors = cols)+
  #scale_fill_viridis(option="inferno",direction=-1)+
  theme(legend.position = "none")+
  geom_raster(aes(x = x, y = y, fill = AverageForBeth2),
              data = p_df) +
  geom_sf(aes(), colour = NA, fill = "grey75", 
          data = world_prj) +
  geom_sf(aes(), fill = "white", color = "black", 
          data = cookie, size = .5) +
  xlab("") + ylab("");p3

png(paste0(dir,"/scripts_results/plastic_cap10_rob.png"), 
    width=2000,height=1125)
par(mfrow=c(1,1))
p3
dev.off()
dev.off()

a[a == 0] <- NA
a_cap <- a
a_cap[a_cap > 1.483366e-08] <- 1.483366e-08

#plot exposure ####
o_dens_proj <- projectRaster(a_cap, crs = proj)

# convert raster to dataframe for ggplot
o_df <- rasterToPoints(o_dens_proj) %>% as_tibble()

#cols <- rev(inferno(5000))
p4 <- ggplot() +
  theme_minimal() +
  scale_fill_viridis(option="inferno",direction=-1)+
  #scale_fill_gradientn(colors = cols)+
  theme(legend.position = "none")+
  geom_raster(aes(x = x, y = y, fill = layer),
              data = o_df) +
  geom_sf(aes(), colour = NA, fill = "grey75", data = world_prj) +
  geom_sf(aes(), fill = "white", color = "black", 
          data = cookie, size = .5) +
  xlab("") + ylab("");p4

png(paste0(dir,"/scripts_results/overlap_cap_rob.png"), 
    width=2000,height=1125)
p4
dev.off()
dev.off()

#plot overlap + eez ####
eez_sf <- st_as_sf(eez_file)
eez_prj <- eez_sf %>% st_transform(proj)
p5 <- ggplot() +
  theme_minimal() +
  scale_fill_viridis(option="inferno",direction=-1)+
  theme(legend.position = "none")+
  geom_raster(aes(x = x, y = y, fill = layer),
              data = o_df) +
  geom_sf(aes(), fill = NA, colour = "#2684ff", data = eez_prj) +
  geom_sf(aes(), colour = NA, fill = "grey75", data = world_prj) +
  geom_sf(aes(), fill = "white", color = "black", data = cookie, size = .5) +
  xlab("") + ylab("");p5

png(paste0(dir,"/scripts_results/overlap_cap_eez_rob.png"), width=2000,height=1125)
p5
dev.off()
dev.off()


#overlap
eez_over <- raster::extract(a,eez_proj,fun=sum,na.rm = T)
eez_proj$over <- eez_over

rank_eezs <- as.data.frame(eez_proj@data)

rank_eezs <- rank_eezs[order(-rank_eezs$over),]
head(rank_eezs)

rank_used <- subset(rank_eezs,over != 0)
rank_used$prop <- rank_used$over/sum(rank_used$over)

write.csv(rank_used,paste0(dir,"/eezs_used_allbirds.csv"),row.names = F)


#Use eez results and create summary statistics ####

#need to combine the species in the same way as in the density section for this to work.
#otherwise biased towards countries with many populations.
eezs_used <- rank_used
eezs_used$MRGID_EEZ <- NULL; eezs_used$MRGID_TER1 <- NULL
eezs_used$MRGID_SOV1 <- NULL; eezs_used$MRGID_TER2 <- NULL
eezs_used$MRGID_SOV2 <- NULL; eezs_used$MRGID_TER3 <- NULL
eezs_used$MRGID_SOV3 <- NULL; eezs_used$ISO_TER1 <- NULL
eezs_used$ISO_SOV1 <- NULL; eezs_used$ISO_TER2 <- NULL
eezs_used$ISO_SOV2 <- NULL; eezs_used$ISO_TER3 <- NULL
eezs_used$ISO_SOV3 <- NULL; eezs_used$UN_TER1 <- NULL
eezs_used$UN_TER2 <- NULL; eezs_used$UN_TER3 <- NULL
eezs_used$UN_SOV1 <- NULL; eezs_used$UN_SOV2 <- NULL
eezs_used$UN_SOV3 <- NULL; eezs_used$Y_1 <- NULL
eezs_used$x_1 <- NULL; eezs_used$id <- NULL
head(eezs_used)

summary(eezs_used$over)

#figure out what to do with multiple sovereign cases
unique(eezs_used$POL_TYPE)


table(eezs_used$POL_TYPE)

pol_type <- eezs_used %>%
  group_by(POL_TYPE) %>% 
  summarise(total = sum(over),
            prop = sum(prop),
            label = "total") %>%
  data.frame();pol_type

ggplot(pol_type,aes(x=label,y=total,fill=POL_TYPE))+
  geom_bar(stat="identity",position="stack")

#all joint regimes have only 2 soveriegns.
#divide the total in half and add to the totals of each country
joint_regime <- subset(eezs_used,POL_TYPE == "Joint regime (EEZ)")

joint_regime$over <- joint_regime$over/2
joint_regime$prop <- joint_regime$prop/2

joint_regime2 <- joint_regime

joint_regime$TERRITORY2 <- NA
joint_regime$SOVEREIGN2 <- NA

joint_regime2$TERRITORY1 <- joint_regime2$TERRITORY2
joint_regime2$SOVEREIGN1 <- joint_regime2$SOVEREIGN2

joint_regime2$TERRITORY2 <- NA
joint_regime2$SOVEREIGN2 <- NA

eezs_used_notjoint <- subset(eezs_used,POL_TYPE != "Joint regime (EEZ)")

eezs_used2 <- rbind(eezs_used_notjoint,joint_regime,joint_regime2)
eezs_used <- eezs_used2

#for landlocked countries, add to nearest country
#Vatican + San Marino = Itlay

italy <- c("Vatican City","San Marino")

eezs_used$UNION <- ifelse(eezs_used$UNION %in% italy, "Italy",eezs_used$UNION)
eezs_used$TERRITORY1 <- ifelse(eezs_used$TERRITORY1 %in% italy, "Italy",eezs_used$TERRITORY1)
eezs_used$SOVEREIGN1 <- ifelse(eezs_used$SOVEREIGN1 %in% italy, "Italy",eezs_used$SOVEREIGN1)

#Bouvet, can be added to the high seas.
eezs_used$TERRITORY1 <- ifelse(eezs_used$UNION == "Bouvet", "High Seas",eezs_used$TERRITORY1)
eezs_used$SOVEREIGN1 <- ifelse(eezs_used$UNION == "Bouvet", "High Seas",eezs_used$SOVEREIGN1)
eezs_used$UNION <- ifelse(eezs_used$UNION == "Bouvet", "High Seas",eezs_used$UNION)


#for overlapping claims, make 1 combined value in the summary

#eezs_used$cat <- ifelse(eezs_used$POL_TYPE == "Overlapping claim",
#                        "Overlapping claim",eezs_used$TERRITORY1)

eezs_used$cat <- ifelse(eezs_used$POL_TYPE == "Overlapping claim",
                                      paste(eezs_used$SOVEREIGN1,eezs_used$SOVEREIGN2,eezs_used$SOVEREIGN3,sep="_")
                                      ,eezs_used$SOVEREIGN1)

table(eezs_used$overlapping_claim)



eezs_country <- eezs_used %>%
  group_by(cat) %>% 
  summarise(total = sum(over),
            prop = sum(prop),
            label = "total") %>%
  data.frame()


eezs_country
eezs_country$cat2 <- ifelse(eezs_country$total < 2.575042e-07,
                            "low impact countries",eezs_country$cat)

eezs_country

eezs_country_lowimpact <- eezs_country[eezs_country$cat2 == "low impact countries",]

eezs_country_highimpact <- eezs_country[eezs_country$cat2 != "low impact countries",]

eezs_country_lowimpact_summary <- c("lowimp",sum(eezs_country_lowimpact$total),
                                    mean(eezs_country_lowimpact$prop),
                                    "total","low impact countries")


eezs_country_highimpact[nrow(eezs_country_highimpact)+1,] <- eezs_country_lowimpact_summary
eezs_country_highimpact$total <- as.numeric(eezs_country_highimpact$total)

eezs_country_highimpact <- eezs_country_highimpact[order(-eezs_country_highimpact$total),]

rank_list <- eezs_country_highimpact$cat2[order(-eezs_country_highimpact$total)]

rank_list <- rank_list[rank_list != "low impact countries"]
rank_list <- rank_list[rank_list != "Overlapping claim"]

level_list <- c(rank_list,"low impact countries","Overlapping claim")


eezs_country_highimpact$cat2 <- as.factor(eezs_country_highimpact$cat2)
eezs_country_highimpact$cat2 <- factor(eezs_country_highimpact$cat2, 
                                       levels = as.factor(level_list))

eezs_country_highimpact$percent <- (eezs_country_highimpact$total/
                                      sum(eezs_country_highimpact$total))*100


  

plot1 <- ggplot(eezs_country_highimpact,aes(x=label,y=total,fill=cat2))+
  geom_bar(stat="identity",position="fill",colour="white") +
  scale_fill_viridis(option="inferno",discrete = T) +
  theme_bw()



write.csv(eezs_country,
          paste0(dir,"/scripts_results/eezs_by_country.csv"),
                 row.names=F)


