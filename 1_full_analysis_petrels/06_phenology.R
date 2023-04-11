## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Labelling breeding and non-breeding months based on distance to the colony
## and comparison to breeding schedules from the literature
## Beth Clark Mar 2021
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

################ LOADING PACKAGES ###################
#require(devtools)
#install_version("sp", version = "1.3-2", repos = "http://cran.us.r-project.org")
#install_version("rgdal", version = "1.4-8", repos = "http://cran.us.r-project.org")
#install_version("raster", version = "3.1-5", repos = "http://cran.us.r-project.org")
#newer versions of sp and rgdal don't work with the custom projection see:
#https://github.com/gdauby/ConR/issues/5
#https://github.com/BirdLifeInternational/track2kba/issues/29

rm(list=ls())

lu=function (x=x) length(unique(x))
library(rgeos)
library(rgdal)
library(sp)
library(geosphere)
library(adehabitatHR)
library(raster)
library(tidyverse)
library(RColorBrewer)

######### GENERAL DIRECTIONS AND FILES ##############

## PROJECTIONS
#Read in land file for visualisation:
#Natural Earth land 1:10m polygons version 5.1.1 
#downloaded from www.naturalearthdata.com/
land <- rgdal::readOGR(dsn = "input_data/baselayer", layer = "ne_10m_land")
proj_wgs84 <- sp::CRS(sp::proj4string(land))

## TO SAVE RESULTS
dir.create("outputs/06_phenology/")
dir_phen <- "outputs/06_phenology/"

################# LOADING SPP DATA ##################
data_std <- "outputs/02_pops/"
files <- list.files(data_std);files

pops <- as.data.frame(files)
names(pops) <- "species_pop"
pops$species_pop <- stringr::str_remove(pops$species_pop,".csv")

head(pops)
pops$breeding <- NA
pops$nonbreeding <- NA

#plot the annual cycle
par(mfrow=c(2,1))

for(dataset_number in c(1:length(files))){ #
  print(files[dataset_number])
  name_png <- stringr::str_remove(files[dataset_number],".csv")
  df <- read.csv(paste0(data_std,files[dataset_number]))  
  
  if(files[dataset_number] == "Procellaria aequinoctialis_Crozet.csv"){
    df$lat_colony <- -46.41
    df$lon_colony <- 51.77
  }
  if(files[dataset_number] == "Procellaria conspicillata_Tristan da Cunha.csv"){
    df$lat_colony <- -37.31
    df$lon_colony <- -12.73
  }
  
  head(df)
  df <- subset(df, latitude < 90)
  df <- df[!is.na(df$dtime), ]
  df$month <- as.factor(as.character(substr(df$dtime,6,7)))
  summary(df$month)
  
  months <- sort(unique(df$month))
  #
  #calculate the distance from each point to the colony
  df$coldist <- raster::pointDistance(matrix(c(df$longitude, df$latitude),  ncol=2),
                                      matrix(c(df$lon_colony,df$lat_colony),ncol=2),longlat=TRUE)/1000
  #make a column to combine dates into a single "year
  df$time_of_year <- paste0("1000",substr(df$dtime,5,nchar(df$dtime[1])))
  
  if(nchar(df$dtime[1])==10){
    df$time_of_year <- as.POSIXct(strptime(df$time_of_year,format="%Y-%m-%d"),"GMT")
  } else {
    df$time_of_year <- as.POSIXct(strptime(df$time_of_year,format="%Y-%m-%d %H:%M:%S"),"GMT") 
  }
  
  #summary for each month
  months_sum <- df %>% group_by(month) %>%
    summarise(mean_coldist = mean(coldist),
              min_coldist = min(coldist)) %>%
    data.frame; months_sum
  
  summary(df$coldist)
  
  df$mean_coldist <- months_sum$mean_coldist[match(df$month,months_sum$month)]
  
  
  half1 <- median(months_sum$mean_coldist)
  half2 <- median(df$coldist)
  mean1 <- mean(months_sum$mean_coldist)
  mean2 <- mean(df$coldist)
  
  devices <- unique(df$device)
  months <- unique(df$month)
  
  head(df)
  if(("PTT" %in% devices | "GLS" %in% devices | nlevels(months) >= 10) & nlevels(months) >= 6){
    
    months_sum$br <- NA
    months_sum$br <- ifelse(months_sum$mean_coldist <= mean(months_sum$mean_coldist),"br","nonbr")
    months_sum$br <- ifelse(months_sum$min_coldist > 200,"nonbr",months_sum$br)

      
    if(nlevels(months) >= 9) {
      for(i in 2:(nrow(months_sum)-1)){
        if(months_sum$br[i+1] == "br" & months_sum$br[i-1] == "br"){
          months_sum$br[i] <- "br"
       }
        if(months_sum$br[i+1] == "nonbr" & months_sum$br[i-1] == "nonbr"){
          months_sum$br[i] <- "nonbr"
        }
      }
    
      if(months_sum$br[2] == "br" & months_sum$br[nrow(months_sum)] == "br"){ months_sum$br[1] <- "br"}
      if(months_sum$br[2] == "nonbr" & months_sum$br[nrow(months_sum)] == "nonbr"){ months_sum$br[1] <- "nonbr"}
      if(months_sum$br[1] == "br" & months_sum$br[nrow(months_sum)-1] == "br"){ months_sum$br[nrow(months_sum)] <- "br"}
      if(months_sum$br[1] == "nonbr" & months_sum$br[nrow(months_sum)-1] == "nonbr"){ months_sum$br[nrow(months_sum)] <- "nonbr"}
    }
    
    br_months <- months_sum$month[months_sum$br == "br"]
    nonbr_months <- months_sum$month[months_sum$br == "nonbr"]
    pops$breeding[dataset_number] <- paste(br_months, collapse = "_")
    pops$nonbreeding[dataset_number] <- paste(nonbr_months, collapse = "_")
    df$br <- months_sum$br[match(df$month,months_sum$month)]
    palette(brewer.pal(n = 12, name = "Paired"))
    
    png(filename = paste0(dir_phen, name_png, ".png"))
    par(mfrow=c(2,1),mar = c(2.2, 4, 0.4, 0.4))
    plot(latitude~longitude, data=df, type="n", asp=1, 
         xlim=c(min(df$longitude)+0.2,max(df$longitude)-0.2), 
         ylim=c(min(df$latitude)+0.5, max(df$latitude)-0.5), 
         main="", frame = T, xlab="", xaxt="n",
         ylab=pops$species_pop[dataset_number])
    plot(land, col='lightgrey', add=T)
    points(latitude~longitude, data=df, pch=16, cex=0.3, col=factor(df$month))
    points(lat_colony~lon_colony, data=df, pch=18, cex=2, col="red")
    
    plot(df$time_of_year,df$coldist,type="n",ylab="Distance to the colony (km)")
    points(df$time_of_year,df$coldist,cex=0.4,
           col=factor(df$month),pch=16)
    #abline(h=half1,col="darkgrey")
    #abline(h=half2,col="black")
    abline(h=mean1,col="grey")
    #abline(h=mean2,col="blue")
    #abline(h=third2,col="darkgrey")  
    palette(c("blue","black"))
    points(df$time_of_year,df$mean_coldist,
           col=factor(df$br),pch=16, cex=0.4)
    
    dev.off()
    
  } else {
    df$br <- "br" 
    pops$breeding[dataset_number] <- paste(unique(months),collapse = "_")
    pops$nonbreeding[dataset_number] <- NA
    
    palette(brewer.pal(n = 12, name = "Paired"))
    
    png(filename = paste0(dir_phen, name_png, ".png"))
    par(mfrow=c(2,1),mar = c(2.2, 4, 0.4, 0.4))
    plot(latitude~longitude, data=df, type="n", asp=1, 
         xlim=c(min(df$longitude)+0.2,max(df$longitude)-0.2), 
         ylim=c(min(df$latitude)+0.5, max(df$latitude)-0.5), 
         main="", frame = T, xlab="", xaxt="n",
         ylab=pops$species_pop[dataset_number])
    plot(land, col='lightgrey', add=T)
    points(latitude~longitude, data=df, pch=16, cex=0.3, col=factor(df$month))
    points(lat_colony~lon_colony, data=df, pch=18, cex=2, col="red")
    
    plot(df$time_of_year,df$coldist,type="n",ylab="Distance to the colony (km)")
    points(df$time_of_year,df$coldist,cex=0.4,
           col=factor(df$month),pch=16)
    points(df$time_of_year,df$mean_coldist,
           col="blue",pch=16, cex=0.4)
    dev.off()
  }
}


pops <- read.csv("input_data/breeding_months.csv")

head(pops)
pops$tracks_breeding <- as.character(pops$tracks_breeding)
pops$nonbreeding <- as.character(pops$nonbreeding)
pops$ref_breeding <- as.character(pops$ref_breeding)
pops$ref_nonbreeding <- as.character(pops$ref_nonbreeding)
# pops$com_breeding <- as.character(pops$com_breeding)  # these fields aren't in the dataframe
# pops$com_nonbreeding <- as.character(pops$com_nonbreeding)

pops$tracks_breeding <- ifelse(nchar(pops$tracks_breeding) == 1,paste0("0",pops$tracks_breeding),pops$tracks_breeding)
pops$nonbreeding <- ifelse(nchar(pops$nonbreeding) == 1,paste0("0",pops$nonbreeding),pops$nonbreeding)
pops$ref_breeding <- ifelse(nchar(pops$ref_breeding) == 1,paste0("0",pops$ref_breeding),pops$ref_breeding)
pops$ref_nonbreeding <- ifelse(nchar(pops$ref_nonbreeding) == 1,paste0("0",pops$ref_nonbreeding),pops$ref_nonbreeding)
# pops$com_breeding <- ifelse(nchar(pops$com_breeding) == 1,paste0("0",pops$com_breeding),pops$com_breeding)  # these fields aren't in the dataframe
# pops$com_nonbreeding <- ifelse(nchar(pops$com_nonbreeding) == 1,paste0("0",pops$com_nonbreeding),pops$com_nonbreeding)

pops$over_val_br <- NA
pops$over_val_nonbr <- NA
pops$pop_exposure <- NA



write.csv(pops, "outputs/06_phenology.csv",
          row.names = F)




