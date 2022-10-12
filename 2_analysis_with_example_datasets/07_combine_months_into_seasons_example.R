## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Combining rasters and scores for each month into averages per season per population
## Beth Clark 2022
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

rm(list=ls()) 

################ LOADING PACKAGES ###################

library(raster)
library(rgdal)
library(cowplot)
library(viridis)
library(stringr)
library(RColorBrewer)

######### GENERAL DIRECTIONS AND FILES ##############

land <- readOGR(dsn="input_data/baselayer", layer = "world-dissolved") 

## DIRECTION TO YOUR RASTERS 
dir_1by1 <- "outputs/04_aggregate_1by1_grid"
files <- list.files(dir_1by1, full.names = TRUE,pattern="tif"); head(files)

dir_seasons <- "outputs/07_seasons"
dir.create(dir_seasons)
dir.create(paste0(dir_seasons,"/maps_pdf/")) 

#read in exposure scores
dat <- read.csv("outputs/04_exposure_scores_by_month.csv") 
head(dat)
dat$month <- substr(dat$sp_pop_month,nchar(dat$sp_pop_month)-1,nchar(dat$sp_pop_month))

#read in phenology data
#This can be directly from the estimations from the tracking data
#pops <- read.csv(paste0(dir,"/outputs/06_phenology.csv"))
#However in this study, we also recorded published breeding schedules
#and then use a final dataset that a combination of both sources
pops <- read.csv("input_data/breeding_months.csv")
pops[,8:14] <- NULL
head(pops)

pops$breeding <- as.character(pops$breeding)
pops$nonbreeding <- as.character(pops$nonbreeding)
pops$breeding <- ifelse(nchar(pops$breeding) == 1,paste0("0",pops$breeding),pops$breeding)
pops$nonbreeding <- ifelse(nchar(pops$nonbreeding) == 1,paste0("0",pops$nonbreeding),pops$nonbreeding)

pops$br_exposure <- NA
pops$nonbr_exposure <- NA

pops$br_n <- NA
pops$nonbr_n <- NA

#set cols for plots
cols_inferno <- rev(inferno(20))
cols_inf <- colorRampPalette(c(cols_inferno))(255)
yelblus <- c(brewer.pal(n = 9, name = "YlGnBu"),"#00172e")
cols <- colorRampPalette(yelblus)(255)
colsviri <- cols[20:255]

#read in plastics data
plastics <- raster("outputs/00_PlasticsRaster.tif")

## rescale to 1
plastics2 <- plastics
plastics2[is.na(plastics2)] <- 0 
p_sum1    <- plastics2/sum(getValues(plastics2))
p_sum1[is.na(plastics)] <- NA

#


collocs <- read.csv("outputs/02_colony_locations.csv")
collocs$sp_pop <- paste(collocs$species,collocs$population,sep="_")

for (i in 1:nrow(pops)){
  
  pop <- pops$species_pop[i];pop
  
  br <- strsplit(pops$breeding[i],"_")[[1]]
  nonbr <- strsplit(pops$nonbreeding[i],"_")[[1]]
  
  #exposure scores
  br_exp <- subset(dat, sp_pop == pop & month %in% br)
  nonbr_exp <- subset(dat, sp_pop == pop & month %in% nonbr)
  
  pops$br_exposure[i] <- mean(br_exp$exposure_score)
  pops$nonbr_exposure[i] <- mean(nonbr_exp$exposure_score)
  
  pops$br_n[i] <- length(br)
  pops$nonbr_n[i] <- length(nonbr)
  
  br_rasters_possible <- paste0(dir_1by1,"/",pop,"_",br,".tif")
  nonbr_rasters_possible <- paste0(dir_1by1,"/",pop,"_",nonbr,".tif")
  
  br_rasters <- br_rasters_possible[br_rasters_possible %in% files]
  nonbr_rasters <- nonbr_rasters_possible[nonbr_rasters_possible %in% files]
  
  #breeding ####
  if(is.na(pops$breeding[i])){
    print("no br")
  } else {
    for(j in 1:length(br_rasters)){
      if(br_rasters[j] %in% files){
        a <- raster(br_rasters[j])
        if(j==1){         
          br_rast_sum <- a
        } else {
          br_rast_sum <- a + br_rast_sum
        }
        
      }
    }  
    
    br_rast_mean <- br_rast_sum / length(br)
    writeRaster(br_rast_mean, filename=paste0(dir_seasons,"/" ,pop,"_br.tif"), format="GTiff", overwrite=TRUE)
    
    #plot results
    br_exposure <- br_rast_sum * p_sum1
    colloc <- subset(collocs, sp_pop == pop)
    
    pdf(paste0(dir_seasons,"/maps_pdf/",pop,"_br.pdf"), paper = "a4r",width = 0, height = 0)
    #png(paste0(dir_seasons,"/maps2/",pop,"_br_sum.png"), width=1400,height=760)  #, width=1399,height=455)
    plot(br_exposure,main=paste0(pop,"\nBreeding season plastic exposure score = ",mean(br_exp$exposure_score)),
         col=cols_inf,legend=F)
    plot(land,col="grey75", 
         border = NA, add=T)
    points(colloc$lon_colony,colloc$lat_colony,pch=18,
           cex=2,col="#004fd9")
    dev.off()
  }
  #nonbreeding if applicable ####
  if(is.na(pops$nonbreeding[i])){
    print("no nonbr")
  } else {
    for(j in 1:length(nonbr_rasters)){
      if(nonbr_rasters[j] %in% files){
        a <- raster(nonbr_rasters[j])
        if(j == 1){
          nonbr_rast_sum <- a
        } else {
          nonbr_rast_sum <- a + nonbr_rast_sum
        }
      }
    }
    
    
    nonbr_rast_mean <- nonbr_rast_sum / length(nonbr)
    writeRaster(nonbr_rast_mean, filename=paste0(dir_seasons,"/" ,pop,"_nonbr.tif"), format="GTiff", overwrite=TRUE)
    
    nonbr_exposure <- nonbr_rast_sum * p_sum1
    
    pdf(paste0(dir_seasons,"/maps_pdf/",pop,"_nonbr.pdf"), paper = "a4r",width = 0, height = 0)
    #png(paste0(dir_seasons,"/maps2/",pop,"_nonbr_sum.png"), width=1400,height=760)  #, width=1399,height=455)
    plot(nonbr_exposure,main=paste0(pop,"\nNonbreeding season plastic exposure score = ",mean(nonbr_exp$exposure_score)),
         col=cols_inf,legend=F)
    plot(land,col="grey75", 
         border = NA, add=T)
    points(colloc$lon_colony,colloc$lat_colony,pch=18,
           cex=2,col="#004fd9")
    dev.off()
    
  }
  
  print(pops$species_pop[i])
  print(i)
}


write.csv(pops,"outputs/07_exposure_scores_by_season.csv",
          row.names = F)  

head(pops)

#Test differences between using published breeding schedules and those estimated from the tracking data ####
pops$tracks_breeding  <- as.character(pops$tracks_breeding)
pops$tracks_nonbreeding <- as.character(pops$tracks_nonbreeding)
pops$ref_breeding <- as.character(pops$ref_breeding)
pops$ref_nonbreeding <- as.character(pops$ref_nonbreeding)

pops$ref_breeding <- ifelse(nchar(pops$ref_breeding) == 1,
                            paste0("0",pops$ref_breeding),pops$ref_breeding)
pops$ref_nonbreeding <- ifelse(nchar(pops$ref_nonbreeding) == 1,
                               paste0("0",pops$ref_nonbreeding),pops$ref_nonbreeding)
pops$tracks_breeding  <- ifelse(nchar(pops$tracks_breeding ) == 1,
                                paste0("0",pops$tracks_breeding ),pops$tracks_breeding )
pops$tracks_nonbreeding <- ifelse(nchar(pops$tracks_nonbreeding) == 1,
                                  paste0("0",pops$tracks_nonbreeding),pops$tracks_nonbreeding)

pops$br_exposure_tracks <- NA
pops$nonbr_exposure_tracks <- NA
pops$br_exposure_refs <- NA
pops$nonbr_exposure_refs <- NA

for (i in 1:nrow(pops)){
  
  br <- strsplit(pops$tracks_breeding[i],"_")[[1]]
  nonbr <- strsplit(pops$tracks_nonbreeding[i],"_")[[1]]
  
  #exposure scores tracks
  br_exp <- subset(dat, sp_pop == pops$species_pop[i] & month %in% br)
  nonbr_exp <- subset(dat, sp_pop == pops$species_pop[i] & month %in% nonbr)
  
  pops$br_exposure_tracks[i] <- mean(br_exp$exposure_score)
  pops$nonbr_exposure_tracks[i] <- mean(nonbr_exp$exposure_score)
  
  br <- strsplit(pops$ref_breeding[i],"_")[[1]]
  nonbr <- strsplit(pops$ref_nonbreeding[i],"_")[[1]]
  
  #exposure scores ref
  br_exp <- subset(dat, sp_pop == pops$species_pop[i] & month %in% br)
  nonbr_exp <- subset(dat, sp_pop == pops$species_pop[i] & month %in% nonbr)
  
  pops$br_exposure_ref[i] <- mean(br_exp$exposure_score)
  pops$nonbr_exposure_ref[i] <- mean(nonbr_exp$exposure_score)
  
}


plot(pops$br_exposure_ref,pops$br_exposure_tracks)
plot(pops$nonbr_exposure_ref,pops$nonbr_exposure_tracks)

cor.test(pops$br_exposure_ref,pops$br_exposure_tracks, method = "kendall")
cor.test(pops$nonbr_exposure_ref,pops$nonbr_exposure_tracks, method = "kendall")
