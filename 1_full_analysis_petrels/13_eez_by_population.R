## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Use EEZ results and create summary statistics
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
library(svglite)

######### GENERAL DIRECTIONS AND FILES ##############

## GENERAL DIR
#dir <- paste0("C:/Users/bethany.clark/OneDrive - BirdLife International/",
#              "Methods") ## copy and paste here your working directory

## DIRECTION TO YOUR RASTERS (ALL DEM CLASSES COMBINED AND BY YEAR QUARTER)

#Read in land file for visualisation:
#Natural Earth land 1:10m polygons version 5.1.1 
#downloaded from www.naturalearthdata.com/
land <- rgdal::readOGR(dsn = "input_data/baselayer", layer = "ne_10m_land")

pops <- read.csv("outputs/06_phenology.csv")

species_weights <- read.csv("outputs/08_exposure_scores_by_species.csv")

#Read in and tidy up eez data ####
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

plot(eez,col="blue")

eez_proj <- eez

sp_files <- list.files("outputs/12_breeding_countries/", pattern = ".*\\.tif$");sp_files
#sp_files <- list.files("outputs/12_breeding_countries/", pattern = ".*\\.tif$");sp_files


#Create eezs table ####
eezs <- as.data.frame(sp_files)
eezs$n_used <- NA
eezs$eezs_used <- NA
eezs$eezs_sovereigns_used <- NA

i <- 1
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

write.csv(eezs_used,"outputs/13_eezs_used_per_species2.csv",row.names = F)
write.csv(eezs,"outputs/13_eezs_per_species2.csv",row.names = F)


eezs_used <- read.csv("outputs/13_eezs_used_per_species.csv")
dat <- read.csv("outputs/12_species_country_scores.csv")


#Investigate results ####
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
eezs_used$group <- ifelse(is.na(eezs_used$group),"High Seas",eezs_used$group)

unique(eezs_used$group)

length(unique(eezs_used$group))

# too much to plot, need to simplify for visualisation
# order from highest to lowest
# group countries below the mean into low impact group

eezs_group <- eezs_used %>%
  group_by(group) %>% 
  summarise(prop = sum(prop),
            nspp = length(unique(species))) %>%
  data.frame();eezs_group

#loop through each species_country and lump together everything that falls 
#below threshold of proportion of score

score_threshold <- 0.05

#Create df to plot top scoring populations ####
#(above 15.3, the value if plastic was evenly distributed globally)
dat <- dat[order(-dat$score),]
top_scoring <- subset(dat, score > 15.3)

i<-1
for(i in 1:nrow(top_scoring)){
  species_df <- eezs_used[eezs_used$sp_country == top_scoring$sp_country[i],]
  
  if(nrow(species_df) > 1){
    species_df_crop <- species_df[species_df$prop >= score_threshold,]
    species_df_crop_low <- species_df[species_df$prop < score_threshold,]
    
    species_df_crop <- species_df_crop[order(-species_df_crop$prop),]
    
    species_df_crop[nrow(species_df_crop)+1,] <- species_df_crop[nrow(species_df_crop),]
    species_df_crop[nrow(species_df_crop),]$prop <- sum(species_df_crop_low$prop)
    species_df_crop[nrow(species_df_crop),]$group <- nrow(species_df_crop_low)
    
    print(sum(species_df_crop$prop)) #should all = 1
  } else {
    species_df_crop <- species_df
  }
  
  if(i == 1){
    all_sp <- species_df_crop
  } else {
    all_sp <- rbind(all_sp,species_df_crop)
  }
  
}
nrow(all_sp)

#change some labels
unique(all_sp$group)
all_sp$group <- ifelse(all_sp$group == "United States","USA",all_sp$group)
all_sp$group <- ifelse(all_sp$group == "United Kingdom","UK",all_sp$group)
all_sp$group <- ifelse(all_sp$group == "Republic of Mauritius","Mauritius",all_sp$group)
all_sp$group <- ifelse(all_sp$group == "Democratic Republic of the Congo","DRC",all_sp$group)
all_sp$group <- ifelse(all_sp$group == "United States/Dominican Republic" ,"USA/Dominican Republic",all_sp$group)
all_sp$group <- ifelse(all_sp$group == "Haiti/United States/Jamaica" ,"Haiti/USA/Jamaica" ,all_sp$group)
all_sp$group <- ifelse(all_sp$group == "France/Madagascar/Republic of Mauritius" ,"FR/MG/MU",all_sp$group)
all_sp$group <- ifelse(all_sp$group == "Madagascar" & 
                         all_sp$species == "Pterodroma baraui","MDG",all_sp$group)
all_sp$group <- ifelse(all_sp$group == "0" ," ",all_sp$group)
all_sp$group <- ifelse(all_sp$group == "1" ,"1   ",all_sp$group)
all_sp$group <- ifelse(all_sp$group == "North Korea" ,"N Korea",all_sp$group)
all_sp$group <- ifelse(all_sp$group == "South Korea" & 
                         all_sp$breeding_country == "South Korea" ,"S Korea*",all_sp$group)
all_sp$group <- ifelse(all_sp$group == "South Africa" ,"S Africa",all_sp$group)
all_sp$group <- ifelse(all_sp$group == "Western Sahara" ,"W Sahara",all_sp$group)
all_sp$group <- ifelse(all_sp$group == "Japan/South Korea" ,"Japan/S Korea",all_sp$group)
all_sp$group <- ifelse(all_sp$group == "Spain/Morocco" ,"Spain*/Morocco",all_sp$group)
all_sp$group <- ifelse(all_sp$group == "Japan/Russia" & 
                         all_sp$breeding_country == "Japan" ,"J*/Russia",all_sp$group)

#all_sp$pop <- as.factor(all_sp$pop)

#all_sp$pop <- factor(all_sp$pop, 
#                           level = top_scoring$sp_country)

#replace sci names
#comma instead of _ for label
#make own country different
#add IUCN category as label for plot 2
names <- read.csv("input_data/Species_list_IUCN.csv")
head(names)

top_scoring$common_name <- names$common_name[match(top_scoring$species,names$scientific_name)]
top_scoring$iucn <- names$IUCN[match(top_scoring$species,names$scientific_name)]
head(top_scoring)

dat$sci_name_pop <- dat$sp_country 
top_scoring$country <- str_split_fixed(top_scoring$sp_country,"_",n=2)[,2]
top_scoring$common_country <- paste0(top_scoring$common_name,", ",top_scoring$country)

head(top_scoring)
all_sp$common_name <- names$common_name[match(all_sp$species,names$scientific_name)]
all_sp$common_country <- paste0(all_sp$common_name,", ",all_sp$breeding_country)
all_sp$iucn <- names$IUCN[match(all_sp$species,names$scientific_name)]
all_sp$common_country_iucn <- paste0(all_sp$common_country," ",all_sp$iucn)
all_sp$pop_score <- dat$score[match(all_sp$sp_country,dat$sp_country)]

all_sp <- all_sp[order(all_sp$pop_score),]

all_sp$common_country <- as.factor(all_sp$common_country)
all_sp$common_country <- factor(all_sp$common_country, 
                                levels = unique(all_sp$common_country))

all_sp$group_label <- ifelse(all_sp$group == all_sp$breeding_country,
                             paste0(all_sp$group,"*"),
                             all_sp$group)

#Plot results ####

#set greyscale matching values in Figure 4b
greys <- c(brewer.pal(n = 9, name = "Greys"))
greys_eez <- c(rev(colorRampPalette(greys)(15)))[1:14]

all_sp$fill_col <- greys_eez[14]
all_sp$fill_col <- ifelse(all_sp$group == "High Seas",greys_eez[1],all_sp$fill_col)
all_sp$fill_col <- ifelse(all_sp$group == "Spain",greys_eez[2],all_sp$fill_col)
all_sp$fill_col <- ifelse(all_sp$group == "Tunisia",greys_eez[3],all_sp$fill_col)
all_sp$fill_col <- ifelse(all_sp$group == "Italy",greys_eez[4],all_sp$fill_col)
all_sp$fill_col <- ifelse(all_sp$group == "Libya",greys_eez[5],all_sp$fill_col)
all_sp$fill_col <- ifelse(all_sp$group == "Turkey",greys_eez[6],all_sp$fill_col)
all_sp$fill_col <- ifelse(all_sp$group == "Algeria",greys_eez[7],all_sp$fill_col)
all_sp$fill_col <- ifelse(all_sp$group == "Malta",greys_eez[8],all_sp$fill_col)
all_sp$fill_col <- ifelse(all_sp$group == "Greece",greys_eez[9],all_sp$fill_col)
all_sp$fill_col <- ifelse(all_sp$group == "USA",greys_eez[10],all_sp$fill_col)
all_sp$fill_col <- ifelse(all_sp$group == "Japan",greys_eez[11],all_sp$fill_col)
all_sp$fill_col <- ifelse(all_sp$group == "UK",greys_eez[12],all_sp$fill_col)
all_sp$fill_col <- ifelse(all_sp$group == "New Zealand",greys_eez[13],all_sp$fill_col)

all_sp$text_col <- ifelse(all_sp$group %in% c("High Seas", "Spain", "Tunisia", "Italy", "Libya",
                                              "Turkey", "Algeria", "Malta", "Greece", "USA", "Japan", 
                                              "UK", "New Zealand"), "white", "black")

eezs_grey <- ggplot(all_sp,aes(x=prop,y=common_country,
                               label = group_label))+
  geom_col(color="white",fill=all_sp$fill_col) +
  #scale_fill_viridis(option="inferno",direction=-1,discrete=T) +
  geom_text(position = position_stack(vjust = 0.5),
            col=all_sp$text_col,size=8) +
  ylab("") +  
  xlab("Proportion of Plastic Encounter Risk")+
  theme_bw()+
  scale_x_continuous(expand = c(0, 0))+
  theme(legend.position = "position.none",
        text = element_text(size=rel(6)),
        axis.title = element_text(size=30),
        axis.text = element_text(colour = "black"));eezs_grey

#save plot as png
png(paste0("outputs/13_top_scoring_eezs",score_threshold,"grey.png"), width=2000,height=1350)
par(mfrow=c(1,1))
eezs_grey <- ggplot(all_sp,aes(x=prop,y=common_country,
                               label = group_label))+
  geom_col(color="white",fill=all_sp$fill_col) +
  #scale_fill_viridis(option="inferno",direction=-1,discrete=T) +
  geom_text(position = position_stack(vjust = 0.5),
            col=all_sp$text_col,size=8) +
  ylab("") +  
  xlab("Proportion of Plastic Encounter Risk")+
  theme_bw()+
  scale_x_continuous(expand = c(0, 0))+
  theme(legend.position = "position.none",
        text = element_text(size=rel(6)),
        axis.title = element_text(size=30),
        axis.text = element_text(colour = "black"));eezs_grey
dev.off()
dev.off()

#reformat to save plot as svg for journal plot requirements
eezs_grey_svg <- ggplot(all_sp,aes(x=prop,y=common_country,
                                   label = group_label))+
  geom_col(color="white",fill=all_sp$fill_col) +
  #scale_fill_viridis(option="inferno",direction=-1,discrete=T) +
  geom_text(position = position_stack(vjust = 0.5),
            col=all_sp$text_col,size=4) +
  ylab("") +  
  xlab("\nPercentage of Plastic Encounter Risk")+
  theme_bw()+
  scale_x_continuous(expand = c(0, 0),labels = scales::percent)+
  theme(legend.position = "position.none",
        text = element_text(size=rel(6)),
        axis.title = element_text(size=15),
        axis.text = element_text(colour = "black",size = 12));eezs_grey_svg

#save as svg
ggsave(filename = "outputs/13_top_scoring_eezs.svg",
       plot = eezs_grey_svg, 
       width = 4000, height = 2700, unit = "px") 

#Create table for supplementary ####
i<-1
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
dat_sup

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

max(all_pops$Label)

pops_table <- all_pops %>% 
  pivot_wider(names_from = Label,values_from = c(group,Percent)) %>%
  select(c(species,breeding_country,group_1,Percent_1,
           group_2,Percent_2,group_3,Percent_3,group_4,Percent_4,
           group_5,Percent_5,group_6,Percent_6,group_7,Percent_7,
           group_8,Percent_8,group_9,Percent_9)) %>%
  data.frame()
head(pops_table)

write.csv(pops_table,"outputs/13_eezs_percent_allpops.csv",
          row.names = F)

