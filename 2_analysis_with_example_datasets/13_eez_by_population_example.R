## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Use eez results and create summary statistics
## Beth Clark 2022
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

rm(list=ls()) 

library(sp)
library(raster)
library(rgdal)
library(cowplot)
library(viridis)
library(stringr)
library(RColorBrewer)
library(geomerge)
library(maptools)
library(gridExtra)
library(sf)
library(tidyverse)
library(rgeos)


## DIRECTION TO YOUR RASTERS (ALL DEM CLASSES COMBINED AND BY YEAR QUARTER)

land <- readOGR(dsn="input_data/baselayer", layer = "world-dissolved") #Changed - BC  

pops <- read.csv("outputs/06_phenology.csv")

species_weights <- read.csv("outputs/08_exposure_scores_by_species.csv")

#eez ####
#Flanders Marine Institute (2020). Union of the ESRI Country shapefile and the Exclusive Economic Zones (version 3). Available online at https://www.marineregions.org/. https://doi.org/10.14284/403. Consulted on 2021-03-04.
eez_file <- readOGR(dsn="input_data/EEZ_land_union_v3_202003", layer = "EEZ_Land_v3_202030") 
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

eez_file$id <-  as.character(1:323)
id <- as.character(1:323)

plot(eez_file)

eez <- bind(eez_file,highseas)
#to check
plot(highseas,col="green")
plot(eez[37,],add=T,col="red");eez[37,]

rm(eez_file)

eez_proj <- eez

sp_files <- list.files("outputs/12_breeding_countries/", pattern = ".*\\.tif$");sp_files

#eezs table
eezs <- as.data.frame(sp_files)
eezs$n_used <- NA
eezs$eezs_used <- NA
eezs$eezs_sovereigns_used <- NA

# i <- 1
for (i in 1:length(sp_files)){
  
  a <- raster(paste0("outputs/12_breeding_countries/",sp_files[i]))
  
  b <- a*species_weights$seasons[i]
  
  #overlap
  eez_over <- raster::extract(b,eez_proj)
  eez_proj$over <- unlist(lapply(eez_over,mean,na.rm = T))
  
  rank_eezs <- as.data.frame(eez_proj@data)
  
  rank_eezs <- rank_eezs[order(-rank_eezs$over),]
  head(rank_eezs)
  
  rank_used <- subset(rank_eezs,over != 0)
  rank_used$prop <- rank_used$over/sum(rank_used$over)
  
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

eezs_used$UNION <- ifelse(is.na(eezs_used$UNION),"High Seas",eezs_used$UNION)
eezs_used$TERRITORY1 <- ifelse(is.na(eezs_used$TERRITORY1),"High Seas",eezs_used$TERRITORY1)
eezs_used$SOVEREIGN1 <- ifelse(is.na(eezs_used$SOVEREIGN1),"High Seas",eezs_used$SOVEREIGN1)

write.csv(eezs_used,"outputs/13_eezs_used_per_species.csv",row.names = F)
write.csv(eezs,"outputs/13_eezs_per_species.csv",row.names = F)


eezs_used <- read.csv("outputs/13_eezs_used_per_species.csv")
dat <- read.csv("outputs/12_species_country_scores.csv")

#figure out what to do with multiple sovereign cases
unique(eezs_used$POL_TYPE)

eezs_used$POL_TYPE <- ifelse(is.na(eezs_used$POL_TYPE),"High Seas",eezs_used$POL_TYPE)
eezs_used$AREA_KM2 <- ifelse(eezs_used$POL_TYPE == "High Seas",219116810,eezs_used$AREA_KM2)

table(eezs_used$POL_TYPE)

#all joint regimes have only 2 sovereigns.
#divide the total in half and add to the totals of each country

joint_regime <- subset(eezs_used,POL_TYPE == "Joint regime (EEZ)")

joint_regime$over <- joint_regime$over/2
joint_regime$prop <- joint_regime$prop/2
joint_regime$AREA_KM2 <- joint_regime$AREA_KM2/2

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
#Vatican + San Marino = Italy
#San Marino and The Vatican do not have an EEZ
#they are merged with Italy to avoid having NAs in the dataset in later analyses.

italy <- c("Vatican City","San Marino")

eezs_used$UNION <- ifelse(eezs_used$UNION %in% italy, "Italy",eezs_used$UNION)
eezs_used$TERRITORY1 <- ifelse(eezs_used$TERRITORY1 %in% italy, "Italy",eezs_used$TERRITORY1)
eezs_used$SOVEREIGN1 <- ifelse(eezs_used$SOVEREIGN1 %in% italy, "Italy",eezs_used$SOVEREIGN1)

#Bouvet, can be added to the high seas.
eezs_used$TERRITORY1 <- ifelse(eezs_used$UNION == "Bouvet", "High Seas",eezs_used$TERRITORY1)
eezs_used$SOVEREIGN1 <- ifelse(eezs_used$UNION == "Bouvet", "High Seas",eezs_used$SOVEREIGN1)
eezs_used$UNION <- ifelse(eezs_used$UNION == "Bouvet", "High Seas",eezs_used$UNION)


#too many overlapping claims to ignore
#for overlapping claims, 

eezs_used$group <- ifelse(eezs_used$POL_TYPE == "Overlapping claim",
                          paste(eezs_used$SOVEREIGN1,
                                eezs_used$SOVEREIGN2,
                                eezs_used$SOVEREIGN3,sep="/")
                          ,eezs_used$SOVEREIGN1)
eezs_used$group <- ifelse(eezs_used$group =="China/NA/NA",
                          "South China Sea",eezs_used$group)
eezs_used$group <- str_remove(eezs_used$group,"/NA")


eezs_used2 <- eezs_used %>% 
  group_by(group,breeding_country) %>%
  summarise( 
    prop = sum(prop),
    species = species[1],
    breeding_country = breeding_country[1]) %>%
  data.frame; eezs_used2

eezs_used <- eezs_used2

unique(eezs_used$group)

length(unique(eezs_used$group))

head(eezs_used)

#replace sci names
#comma instead of _ for label
#make own country different
#add IUCN category as label for plot 2
names <- read.csv("input_data/Species_list_IUCN.csv")
head(names)

dat$common_name <- names$common_name[match(dat$species,names$scientific_name )]
dat$iucn <- names$IUCN[match(dat$species,names$scientific_name )]
head(dat)

dat$sci_name_pop <- dat$sp_country
dat$country <- str_split_fixed(dat$sp_country,"_",n=2)[,2]
dat$common_country <- paste0(dat$common_name,", ",dat$country)

head(dat)
dat$common_country_iucn <- paste0(dat$common_country," ",dat$iucn)

eezs_used <- eezs_used[order(-eezs_used$prop),]

dat$common_country <- as.factor(dat$common_country)

eezs_used$sp_country <- paste(eezs_used$species,eezs_used$breeding_country,sep="_")
eezs_used$common_country <- dat$common_country[match(eezs_used$sp_country,dat$sp_country)]

eezs <- ggplot(eezs_used,aes(x=prop,y=common_country,
                             fill=prop,label = group))+
  geom_col(color="white") +
  scale_fill_viridis(option="inferno",direction=-1) +
  geom_text(position = position_stack(vjust = 0.5),
            col="white",size=8) +
  ylab("") +  
  xlab("Proportion of Plastic Encounter Risk")+
  theme_bw()+
  scale_x_continuous(expand = c(0, 0))+
  theme(legend.position = "position.none",
        text = element_text(size=rel(6)),
        axis.title = element_text(size=30),
        axis.text = element_text(colour = "black"));eezs

png(paste0("outputs/13_eez_proportions.png"), width=2000,height=1350)
par(mfrow=c(1,1))
eezs
dev.off()
dev.off()


#Table for supplementary ####
#group eezs with less than a threshold proportion of score 
score_threshold <- 0.05

# i<-1
for(i in 1:nrow(dat)){
  species_df <- eezs_used[eezs_used$sp_country == dat$sp_country[i],]
  
  if(nrow(species_df) > 1){
    species_df_crop <- species_df[species_df$prop >= score_threshold,]
    species_df_crop_low <- species_df[species_df$prop < score_threshold,]
    
    species_df_crop <- species_df_crop[order(-species_df_crop$prop),]
    
    species_df_crop[nrow(species_df_crop)+1,] <- species_df_crop[nrow(species_df_crop),]
    species_df_crop[nrow(species_df_crop),]$prop <- sum(species_df_crop_low$prop)
    species_df_crop[nrow(species_df_crop),]$group <- nrow(species_df_crop_low)
    
    print(sum(species_df_crop$prop))
  } else {
    species_df_crop <- species_df
  }
  
  if(i == 1){
    dat_sup <- species_df_crop
  } else {
    dat_sup <- rbind(dat_sup,species_df_crop)
  }
  
}
nrow(dat_sup)


pops <- unique(dat_sup$sp_country)
for(i in 1:length(pops)){
  pop <- subset(dat_sup,sp_country == pops[i])
  pop$Label <- 1:nrow(pop)
  if(i == 1){
    all_pops <- pop
  } else {
    all_pops <- rbind(all_pops,pop)
  }
}

head(all_pops,20)

all_pops$Percent <- all_pops$prop*100
all_pops$prop <- NULL

pops_table <- all_pops %>% 
  pivot_wider(names_from = Label,values_from = c(group,Percent)) %>%
  dplyr::select(c(species,breeding_country,group_1,Percent_1,
                  group_2,Percent_2,group_3,Percent_3,group_4,Percent_4)) %>%
  data.frame()
head(pops_table)

write.csv(pops_table,"outputs/13_eezs_percent_allpops.csv",
          row.names = F)


