## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Calculate proportions of exposure among marine
## political regions (EEZs and the high seas)
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
library(svglite)

sessionInfo()
#R version 4.1.2 (2021-11-01)
#Platform: x86_64-w64-mingw32/x64 (64-bit)
#Running under: Windows 10 x64 (build 19045)

#Matrix products: default

#locale:
#[1] LC_COLLATE=English_United Kingdom.1252 
#[2] LC_CTYPE=English_United Kingdom.1252   
#[3] LC_MONETARY=English_United Kingdom.1252
#[4] LC_NUMERIC=C                           
#[5] LC_TIME=English_United Kingdom.1252    

#attached base packages:
#[1] stats     graphics  grDevices utils     datasets  methods  
#[7] base     

#other attached packages:
#[1] svglite_2.1.1      viridis_0.6.2      viridisLite_0.4.0 
#[4] forcats_0.5.1      stringr_1.4.0      dplyr_1.0.8       
#[7] purrr_0.3.4        readr_2.1.2        tidyr_1.2.0       
#[10] tibble_3.1.6       ggplot2_3.3.5      tidyverse_1.3.2   
#[13] sf_1.0-7           RColorBrewer_1.1-2 rgeos_0.5-9       
#[16] rgdal_1.4-8        raster_3.1-5       sp_1.5-0          

#loaded via a namespace (and not attached):
#[1] Rcpp_1.0.8          lubridate_1.8.0     lattice_0.20-45    
#[4] class_7.3-19        assertthat_0.2.1    utf8_1.2.2         
#[7] R6_2.5.1            cellranger_1.1.0    backports_1.4.1    
#[10] reprex_2.0.1        e1071_1.7-9         httr_1.4.2         
#[13] pillar_1.7.0        rlang_1.0.6         googlesheets4_1.0.0
#[16] readxl_1.3.1        rstudioapi_0.13     googledrive_2.0.0  
#[19] munsell_0.5.0       proxy_0.4-26        broom_0.7.12       
#[22] compiler_4.1.2      modelr_0.1.8        systemfonts_1.0.4  
#[25] pkgconfig_2.0.3     tidyselect_1.1.2    gridExtra_2.3      
#[28] codetools_0.2-18    fansi_1.0.2         crayon_1.5.0       
#[31] tzdb_0.2.0          dbplyr_2.1.1        withr_2.5.0        
#[34] grid_4.1.2          jsonlite_1.8.0      gtable_0.3.0       
#[37] lifecycle_1.0.3     DBI_1.1.2           magrittr_2.0.2     
#[40] units_0.8-0         scales_1.2.1        KernSmooth_2.23-20 
#[43] cli_3.3.0           stringi_1.7.6       fs_1.5.2           
#[46] xml2_1.3.3          ellipsis_0.3.2      generics_0.1.2     
#[49] vctrs_0.3.8         tools_4.1.2         glue_1.6.2         
#[52] hms_1.1.1           colorspace_2.0-3    gargle_1.2.0       
#[55] classInt_0.4-3      rvest_1.0.2         haven_2.4.3

######### GENERAL DIRECTIONS AND FILES ##############

## check we're still in rproj home directory "1_full_analysis_petrels"
getwd()

#Read in land file for visualisation:
#Natural Earth land 1:10m polygons version 5.1.1 
#downloaded from www.naturalearthdata.com/
land <- rgdal::readOGR(dsn = "input_data/baselayer", layer = "ne_10m_land")

pops <- read.csv("outputs/06_phenology.csv")

#EEZ ####
#Flanders Marine Institute (2020). Union of the ESRI Country shapefile and the Exclusive Economic Zones (version 3). Available online at https://www.marineregions.org/. https://doi.org/10.14284/403. Consulted on 2021-03-04.
eez_file <- rgdal::readOGR(dsn = "input_data/EEZ_land_union_v3_202003", layer = "EEZ_Land_v3_202030") 

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

sp_files <- list.files("outputs/06_species/")

bird_dist <- raster::raster("outputs/08_all_species_distribution.tif")
b <- bird_dist
b[is.na(b)] <- 0 
b_sum1 <- b/sum(raster::getValues(b))
b_sum1[is.na(bird_dist)] <- NA
b_sum1[b_sum1 == 0] <- NA

#read in plastics data
plastics <- raster::raster("outputs/00_PlasticsRaster.tif")

## rescale to 1
plastics2 <- plastics
plastics2[is.na(plastics2)] <- 0 
p_sum1    <- plastics2/sum(raster::getValues(plastics2))
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

eezs_used <- subset(rank_eezs,over != 0)
eezs_used$prop <- eezs_used$over/sum(eezs_used$over)

head(eezs_used)


#Use eez results and create summary statistics ####

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
#Vatican + San Marino = Italy

italy <- c("Vatican City","San Marino")

eezs_used$UNION <- ifelse(eezs_used$UNION %in% italy, "Italy",eezs_used$UNION)
eezs_used$TERRITORY1 <- ifelse(eezs_used$TERRITORY1 %in% italy, "Italy",eezs_used$TERRITORY1)
eezs_used$SOVEREIGN1 <- ifelse(eezs_used$SOVEREIGN1 %in% italy, "Italy",eezs_used$SOVEREIGN1)

#Bouvet, can be added to the high seas.
eezs_used$TERRITORY1 <- ifelse(eezs_used$UNION == "Bouvet", "High Seas",eezs_used$TERRITORY1)
eezs_used$SOVEREIGN1 <- ifelse(eezs_used$UNION == "Bouvet", "High Seas",eezs_used$SOVEREIGN1)
eezs_used$UNION <- ifelse(eezs_used$UNION == "Bouvet", "High Seas",eezs_used$UNION)


#for overlapping claims, make 1 combined value in the summary

eezs_used$sovereigns <- ifelse(eezs_used$POL_TYPE == "Overlapping claim",
                                      paste(eezs_used$SOVEREIGN1,eezs_used$SOVEREIGN2,eezs_used$SOVEREIGN3,sep="_")
                                      ,eezs_used$SOVEREIGN1)
#count number of sovereigns used
all_sovereigns <- c(eezs_used$SOVEREIGN1,eezs_used$SOVEREIGN2,eezs_used$SOVEREIGN3)
unique(all_sovereigns) 
length(unique(all_sovereigns)) - 2 #high seas and NA are not sovereigns

write.csv(eezs_used, "outputs/11_eezs_used_all_species.csv",
          row.names = F)


eezs_country <- eezs_used %>%
  group_by(sovereigns) %>% 
  summarise(total = sum(over),
            prop = sum(prop),
            label = "total") %>%
  data.frame()

#For visualisation, combine the scores for all countries scoring below 
eezs_country
eezs_country$category <- ifelse(eezs_country$total < 2.575042e-07,
                            "low exposure countries",eezs_country$sovereigns)

eezs_country

eezs_country_lowexposure <- eezs_country[eezs_country$category == "low exposure countries",]

eezs_country_highexposure <- eezs_country[eezs_country$category != "low exposure countries",]

eezs_country_lowexposure_summary <- c("lowimp",sum(eezs_country_lowexposure$total),
                                    mean(eezs_country_lowexposure$prop),
                                    "total","low exposure countries")


eezs_country_highexposure[nrow(eezs_country_highexposure)+1,] <- eezs_country_lowexposure_summary
eezs_country_highexposure$total <- as.numeric(eezs_country_highexposure$total)

eezs_country_highexposure <- eezs_country_highexposure[order(-eezs_country_highexposure$total),]

rank_list <- eezs_country_highexposure$category[order(-eezs_country_highexposure$total)]

rank_list <- rank_list[rank_list != "low exposure countries"]
rank_list <- rank_list[rank_list != "Overlapping claim"]

level_list <- c(rank_list,"low exposure countries","Overlapping claim")


eezs_country_highexposure$category <- as.factor(eezs_country_highexposure$category)
eezs_country_highexposure$category <- factor(eezs_country_highexposure$category, 
                                       levels = as.factor(level_list))

eezs_country_highexposure$percent <- (eezs_country_highexposure$total/
                                      sum(eezs_country_highexposure$total))*100

#plot a stacked bar of all species combined
plot1 <- ggplot(eezs_country_highexposure,aes(
                x=label,y=total,fill=category))+
  geom_bar(stat="identity",position="fill",colour="white") +
  scale_fill_viridis(option="inferno",discrete = T) +
  theme_bw(); plot1

png("outputs/11_eez_allspecies.png", 
    width=700,height=2000)
plot1
dev.off()
dev.off()

#produce a greyscale version of the previous plot
greys <- c(brewer.pal(n = 9, name = "Greys"))
col_eez <- c(rev(colorRampPalette(greys)(15)))[1:14]

eezs_greyscale <- ggplot(eezs_country_highexposure,aes(
  x=label,y=total,fill=category))+
  geom_bar(stat="identity",position="fill",colour="white") +
  scale_fill_manual(values = col_eez)+
  theme_bw(); eezs_greyscale

#save as svg for journal plot requirements
ggsave(filename = "outputs/11_eez_allspecies_grey.svg",
       plot = eezs_greyscale, 
       width = 1000, height = 2000, unit = "px") 

#save plot source data
write.csv(eezs_country,
          "outputs/11_eezs_by_country.csv",
                 row.names=F)

write.csv(eezs_country_highexposure,
          "outputs/11_eezs_by_country_plot.csv",
          row.names=F)


