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
#[1] RColorBrewer_1.1-2 stringr_1.4.0      viridis_0.6.2     
#[4] viridisLite_0.4.0  cowplot_1.1.1      rgdal_1.4-8       
#[7] raster_3.1-5       sp_1.5-0          

#loaded via a namespace (and not attached):
#[1] Rcpp_1.0.8       pillar_1.7.0     compiler_4.1.2   tools_4.1.2     
#[5] lifecycle_1.0.3  tibble_3.1.6     gtable_0.3.0     lattice_0.20-45 
#[9] pkgconfig_2.0.3  rlang_1.0.6      cli_3.3.0        DBI_1.1.2       
#[13] rstudioapi_0.13  gridExtra_2.3    dplyr_1.0.8      generics_0.1.2  
#[17] vctrs_0.3.8      grid_4.1.2       tidyselect_1.1.2 glue_1.6.2      
#[21] R6_2.5.1         fansi_1.0.2      ggplot2_3.3.5    purrr_0.3.4     
#[25] magrittr_2.0.2   scales_1.2.1     codetools_0.2-18 ellipsis_0.3.2  
#[29] assertthat_0.2.1 colorspace_2.0-3 utf8_1.2.2       stringi_1.7.6   
#[33] munsell_0.5.0    crayon_1.5.0  

######### GENERAL DIRECTIONS AND FILES ##############

## check we're still in rproj home directory "1_full_analysis_petrels"
getwd()

#Read in land file for visualisation:
#Natural Earth land 1:10m polygons version 5.1.1 
#downloaded from www.naturalearthdata.com/
land <- rgdal::readOGR(dsn = "input_data/baselayer", layer = "ne_10m_land")

## DIRECTION TO YOUR RASTERS 
dir_1by1 <- "outputs/04_aggregate_1by1_grid"
files <- list.files(dir_1by1, full.names = TRUE,pattern=".*\\.tif$"); head(files)

dir_seasons <- "outputs/07_seasons"
dir.create(dir_seasons)
dir.create(paste0(dir_seasons,"/maps/")) 
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
p_sum1    <- plastics2/sum(raster::getValues(plastics2))
p_sum1[is.na(plastics)] <- NA

collocs <- read.csv("outputs/02_colony_locations.csv")
collocs$sp_pop <- paste(collocs$species,collocs$population,sep="_")

#42 failed "Fulmarus glacialis_Bj?rn?ya"
pops$species_pop <- ifelse(pops$species_pop == "Fulmarus glacialis_Bj?rn?ya",
                           "Fulmarus glacialis_Bjornoya",pops$species_pop)
head(dat)
dat$population <- ifelse(dat$population == "Bj?rn?ya","Bjornoya",dat$population)
dat$sp_pop <- ifelse(dat$sp_pop == "Fulmarus glacialis_Bj?rn?ya",
                           "Fulmarus glacialis_Bjornoya",dat$sp_pop)

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
        a <- raster::raster(br_rasters[j])
        if(j==1){         
          br_rast_sum <- a
        } else {
          br_rast_sum <- a + br_rast_sum
        }
        
      }
    }  
    
    br_rast_mean <- br_rast_sum / length(br)
    raster::writeRaster(br_rast_mean, filename=paste0(dir_seasons,"/" ,pop,"_br.tif"), format="GTiff", overwrite=TRUE)
    
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


write.csv(pops, "outputs/07_exposure_scores_by_season.csv",
          row.names = F)  

head(pops)

#Test differences between using published breeding schedules and those estimated from the tracking data ####

pops <- read.csv("outputs/07_exposure_scores_by_season.csv")

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

hist(pops$br_exposure_ref)
hist(pops$nonbr_exposure_ref)

cor.test(pops$br_exposure_ref,pops$br_exposure_tracks, method = "kendall")
cor.test(pops$nonbr_exposure_ref,pops$nonbr_exposure_tracks, method = "kendall")
