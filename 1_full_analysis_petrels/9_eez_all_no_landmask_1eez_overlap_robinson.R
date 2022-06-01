## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Calculate proportions of exposure among marine
## political regions (EEZs and the high seas)
## 
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

library(cowplot) #1.0.0
library(stringr)#1.4.0
library(geomerge)#0.3.2
library(maptools)#1.0-1
library(gridExtra)#2.3
library(adehabitatHR)

######### GENERAL DIRECTIONS AND FILES ##############

## paste home directory here
dir <- "C:/Users/bethany.clark/OneDrive - BirdLife International/Methods"

land <- readOGR(dsn=paste0(dir,"/input_data/baselayer"), layer = "world-dissolved") 

pops <- read.csv(paste0(dir,"/outputs/06_phenology.csv"))

#EEZ ####
#Flanders Marine Institute (2020). Union of the ESRI Country shapefile and the Exclusive Economic Zones (version 3). Available online at https://www.marineregions.org/. https://doi.org/10.14284/403. Consulted on 2021-03-04.
eez_file <- readOGR(dsn=paste0(dir,"/input_data/EEZ_land_union_v3_202003"), layer = "EEZ_Land_v3_202030") 

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
plot(eez[1,],add=T,col="red",border="orange");eez[1,]

rm(eez_file)

eez@data$UNION[nrow(eez@data)] <- "High Seas"
eez@data$TERRITORY1[nrow(eez@data)] <- "High Seas"
eez@data$SOVEREIGN1[nrow(eez@data)] <- "High Seas"
eez@data$POL_TYPE[nrow(eez@data)] <- "High Seas"
eez@data$AREA_KM2[nrow(eez@data)] <- 219116810
tail(eez@data)

plot(eez,col="blue")

sp_files <- list.files(paste0(dir,"/outputs/06_species/"))

bird_dist <- raster(paste0(dir,"/outputs/08_all_species_distribution.tif"))
b <- bird_dist
b[is.na(b)] <- 0 
b_sum1 <- b/sum(getValues(b))
b_sum1[is.na(bird_dist)] <- NA
b_sum1[b_sum1 == 0] <- NA

#read in plastics data
plastics <- raster(paste0(dir,"/outputs/00_PlasticsRaster.tif"))

## rescale to 1
plastics2 <- plastics
plastics2[is.na(plastics2)] <- 0 
p_sum1    <- plastics2/sum(getValues(plastics2))
p_sum1[is.na(plastics)] <- NA

# exposure ####
exposure <- b_sum1 * p_sum1
exposure[exposure == 0] <- NA

#overlap with eez ####
eez_over <- raster::extract(exposure,eez,fun=sum,na.rm = T)
eez$over <- eez_over

rank_eezs <- as.data.frame(eez@data)

rank_eezs <- rank_eezs[order(-rank_eezs$over),]
head(rank_eezs)

rank_used <- subset(rank_eezs,over != 0)
rank_used$prop <- rank_used$over/sum(rank_used$over)

names(rank_used)

write.csv(rank_used,paste0(dir,"/11_eezs_used_all_species.csv"),
          row.names = F)


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
  theme_bw(); plot1



write.csv(eezs_country,
          paste0(dir,"/outputs/11_eezs_by_country.csv"),
                 row.names=F)


