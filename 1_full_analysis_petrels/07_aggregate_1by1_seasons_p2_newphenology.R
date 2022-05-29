## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Combining rasters and scores for each month into averages by season
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

## paste home directory here
dir <- paste0("C:/Users/bethany.clark/OneDrive - BirdLife International/",
              "Methods") 

land <- readOGR(dsn=paste0(dir,"/input_data/baselayer"), layer = "world-dissolved") 

## DIRECTION TO YOUR RASTERS 
dir_1by1 <- paste0(dir,"/outputs/04_aggregate_1by1_grid")

dat <- read.csv(paste0(dir,"/outputs/05_exposure_scores_by_month.csv"))  
head(dat)

## DIRECTION TO YOUR RESULTS
dir_seasons <- paste0(dir,"/outputs/07_combine_by_season")

dir.create(dir_seasons)
dir.create(paste0(dir_seasons,"/maps/")) 
dir.create(paste0(dir_seasons,"/maps_pdf/")) 

pops <- read.csv(paste0(dir,"/outputs/06_phenology.csv"))

pops$over_val_br <- NA
pops$over_val_nonbr <- NA

files <- list.files(dir_demClasses, full.names = TRUE,pattern="tif"); head(files)

pops$br_n <- NA
pops$nonbr_n <- NA

#add common name & IUCN & pop size weighting
pop_names <- unlist(str_split(pops$species_pop,"_"))
pops$species <- pop_names[seq(1,length(pop_names),by=2)]
pops$pop <- pop_names[seq(2,length(pop_names),by=2)]
head(pops)

names <- read.csv(paste0(dir,"/input_data/Species_list_IUCN.csv"))
head(names)

pops$common_name <- names$common_name[match(pops$species,names$scientific_name)]
head(pops)
cols_inferno <- rev(inferno(20))
cols_inf <- colorRampPalette(c(cols_inferno))(255)

all_pops <- read.csv(paste0(dir,"/outputs/02_all_locations.csv"))
all_pops$pop_loc <- paste(all_pops$pop,all_pops$lon_colony,all_pops$lat_colony)

#add latlon of colony to pops datasheets

head(all_pops)


for (i in c(1:nrow(pops))){#
  
  colloc <- all_pops[all_pops$sp_pop==pops$species_pop[i],]
  if(pops$species_pop[i] == "Ardenna bulleri_Aorangi Island"){
    colloc$lat_colony <- -35.50
    colloc$lon_colony <- 174.75
  }
  
  pop <- pops$species_pop[i];pop
  
  br <- strsplit(pops$ref_breeding[i],"_")[[1]]
  nonbr <- strsplit(pops$ref_nonbreeding[i],"_")[[1]]
  
  br_rasters_possible <- paste0(dir_demClasses,"/",pop,"_",br,".tif")
  nonbr_rasters_possible <- paste0(dir_demClasses,"/",pop,"_",nonbr,".tif")
  
  br_rasters <- br_rasters_possible[br_rasters_possible %in% files]
  nonbr_rasters <- nonbr_rasters_possible[nonbr_rasters_possible %in% files]

  for(j in 1:length(br_rasters)){
    if(br_rasters[j] %in% files){
      a <- raster(br_rasters[j])
      a[is.na(a)] <- 0 
      b <- sum(getValues(a)) # total number of birds in the 10km x 10km raster
      
      ## reprojecting and rescaling
      a_proj <- projectRaster(a, m, method = "bilinear")
      a_proj2 <- a_proj * r_area / 100000000 # rescaling the values in each cell
      a_proj2[is.na(a_proj2)] <- 0 
      a_proj2[is.na(m)] <- NA 
      if(j==1){         #
        br_rast_sum <- a_proj2
        count <- 1
      } else {
        br_rast_sum <- a_proj2 + br_rast_sum
        count <- count + 1
      }
      
    }
  }  
  pops$br_n[i] <- count
  br_rast_mean <- br_rast_sum / count
  count <- 0
  
  if(is.na(pops$ref_nonbreeding[i])){
    print("no nonbr")
  } else {
    for(j in 1:length(nonbr_rasters)){
      if(nonbr_rasters[j] %in% files){
        a <- raster(nonbr_rasters[j])
        a[is.na(a)] <- 0 
        b <- sum(getValues(a)) # total number of birds in the 10km x 10km raster
        
        ## reprojecting and rescaling
        a_proj <- projectRaster(a, m, method = "bilinear")
        a_proj2 <- a_proj * r_area / 100000000 # rescaling the values in each cell
        a_proj2[is.na(a_proj2)] <- 0 
        a_proj2[is.na(m)] <- NA
        if(j == 1){
          nonbr_rast_sum <- a_proj2
          count <- 1
        } else {
          nonbr_rast_sum <- a_proj2 + nonbr_rast_sum
          count <- count + 1
        }
      }
    }
  pops$nonbr_n[i] <- count
  nonbr_rast_mean <- nonbr_rast_sum / count
  count <- 0  
  #writeRaster(nonbr_rast_sum, filename=paste0(dir_1by1,"/" ,pop,"_sum_nonbr.tif"), format="GTiff", overwrite=TRUE)
     
  }

  
  ## exporting results
  
  # write raster temp comment out####
  #writeRaster(br_rast_sum, filename=paste0(dir_1by1,"/" ,pop,"_sum_br.tif"), format="GTiff", overwrite=TRUE)
  
  #writeRaster(br_rast_mean, filename=paste0(dir_1by1m,"/" ,pop,"_mean_br.tif"), format="GTiff", overwrite=TRUE)
  #writeRaster(nonbr_rast_mean, filename=paste0(dir_1by1m,"/" ,pop,"_mean_nonbr.tif"), format="GTiff", overwrite=TRUE)
  
  #breeding season overlap
  over <- br_rast_sum * p_sum1
  over_score <- over
  summary(over_score@data@values)
  over_score[is.na(over_score)] <- 0
  over_val_br <- round(sum(getValues(over_score))*1000000,4)

  rast <- br_rast_sum
  rast[is.na(rast)] <- 0
  sum(getValues(rast))
  
  #png(paste0(dir_1by1,"/maps/",pop,"_br_sum.png"), width=1399,height=455)
  #par(mfrow=c(1,2))
  #plot(br_rast_sum,main=paste(pop,"br"),col=colsviri,legend=F)
  #plot(over,main=paste0("pieces per km2 * seabird = ",over_val_br),
  #     col=colsviri,legend=F)
  #dev.off()
  
  #plot png temp comment out ####
  #pdf(paste0(dir_1by1,"/maps_pdf/",pop,"_br.pdf"),
  #    paper = "a4r",width = 0, height = 0)
  #png(paste0(dir_1by1,"/maps2/",pop,"_br_sum.png"), width=1400,height=760)
  #par(mfrow=c(1,1))
  #plot(over,main=paste0(pops$common_name[i]," ",pops$species[i],", ",pops$pop[i],
  #                      "\nBreeding season plastic exposure score = ",
  #                      round(over_val_br,2)),
  #     cex.main=2,
  #     col=cols_inf,legend=F)
  #plot(land,col="grey75", 
  #     border = NA, add=T)
  #points(colloc$lon_colony,colloc$lat_colony,pch=18,
  #       cex=2.8,col="#004fd9")
  #dev.off()

  
  pops$over_val_br[i] <- over_val_br
  
  #nonbreeding season overlap
  if(is.na(pops$ref_nonbreeding[i])){
    print(pop)}else{
      over <- nonbr_rast_sum * p_sum1
      over_score <- over
      summary(over_score@data@values)
      over_score[is.na(over_score)] <- 0
      over_val_nonbr <- round(sum(getValues(over_score))*1000000,4)
      
      #png(paste0(dir_1by1,"/maps/",pop,"_nonbr_sum.png"), width=1399,height=455)
      #par(mfrow=c(1,2))
      #plot(nonbr_rast_sum,main=paste(pop,"nonbr"),col=colsviri,legend=F)
      #plot(over,main=paste0("pieces per km2 * seabird = ",over_val_nonbr),
      #     col=colsviri,legend=F)
      #dev.off()
      
      #plot png temp comment out ####
      #png(paste0(dir_1by1,"/maps2/",pop,"_nonbr_sum.png"), width=1400,height=760)
      #pdf(paste0(dir_1by1,"/maps_pdf/",pop,"_nonbr.pdf"),
      #    paper = "a4r",width = 0, height = 0)
      #par(mfrow=c(1,1))
      #plot(over,main=paste0(pops$common_name[i]," ",pops$species[i],", ",pops$pop[i],
      #                      "\nNon-breeding season plastic exposure score = ",
      #                      round(over_val_br,2)),
      #     cex.main=2,
      #     col=cols_inf,legend=F)
      #plot(land,col="grey75", 
      #     border = NA, add=T)
      #points(colloc$lon_colony,colloc$lat_colony,pch=18,
      #       cex=2.5,col="#004fd9")
      #dev.off()
      
      pops$over_val_nonbr[i] <- over_val_nonbr
      
      

    }
  
  print(i)

}

pops$mean_br <- pops$over_val_br / pops$br_n
pops$mean_nonr <- pops$over_val_nonbr / pops$nonbr_n



write.csv(pops, paste0(dir_1by1, "/results_rasters_br_p2_v2_ref_phen.csv"),
          row.names = F)  

pops_ref <- pops

#run with com instead
write.csv(pops, paste0(dir_1by1, "/results_rasters_br_p2_v2_com_phen.csv"),
          row.names = F)  


pops_orig <- read.csv(paste0(dir,"/outputs/04_aggregate_1by1_grid_br_p2",
                             "/results_rasters_br_p2_v2.csv")) 

pops$mean_br_orig <- pops_orig

plot(pops$mean_br,pops_orig$mean_br)
plot(pops$mean_nonr,pops_orig$mean_nonr)

cor.test(pops$mean_br,pops_orig$mean_br)

plot(pops$mean_br,pops_orig$mean_br)

cor.test(pops$mean_br,pops_orig$mean_br)

cor.test(pops$mean_nonr,pops_orig$mean_nonr)


plot(c(pops$mean_br,pops$mean_nonr),
         c(pops_orig$mean_br,pops_orig$mean_nonr))


pops$mean_br_orig <- pops_orig$mean_br
pops$mean_nonr_orig <- pops_orig$mean_nonr
pops$diffs_br <- pops$mean_br_orig - pops$mean_br
pops$diffs_nonr <- abs(pops$mean_nonr_orig - pops$mean_nonr)

hist(pops$diffs_br)
hist(pops$diffs_nonr)

pops$diff_percent_br <- pops$diffs_br/pops$mean_br_orig*100
mean(pops$diff_percent_br, na.rm=T)


pops$br_n_com <- NA
pops$nonbr_n_com <- NA

for (i in c(1:nrow(pops))){#
  
  colloc <- all_pops[all_pops$sp_pop==pops$species_pop[i],]
  if(pops$species_pop[i] == "Ardenna bulleri_Aorangi Island"){
    colloc$lat_colony <- -35.50
    colloc$lon_colony <- 174.75
  }
  
  pop <- pops$species_pop[i];pop
  
  br <- strsplit(pops$com_breeding[i],"_")[[1]]
  nonbr <- strsplit(pops$com_nonbreeding[i],"_")[[1]]
  
  br_rasters_possible <- paste0(dir_demClasses,"/",pop,"_",br,".tif")
  nonbr_rasters_possible <- paste0(dir_demClasses,"/",pop,"_",nonbr,".tif")
  
  br_rasters <- br_rasters_possible[br_rasters_possible %in% files]
  nonbr_rasters <- nonbr_rasters_possible[nonbr_rasters_possible %in% files]
  
  for(j in 1:length(br_rasters)){
    if(br_rasters[j] %in% files){
      a <- raster(br_rasters[j])
      a[is.na(a)] <- 0 
      b <- sum(getValues(a)) # total number of birds in the 10km x 10km raster
      
      ## reprojecting and rescaling
      a_proj <- projectRaster(a, m, method = "bilinear")
      a_proj2 <- a_proj * r_area / 100000000 # rescaling the values in each cell
      a_proj2[is.na(a_proj2)] <- 0 
      a_proj2[is.na(m)] <- NA 
      if(j==1){         #
        br_rast_sum <- a_proj2
        count <- 1
      } else {
        br_rast_sum <- a_proj2 + br_rast_sum
        count <- count + 1
      }
      
    }
  }  
  pops$br_n[i] <- count
  br_rast_mean <- br_rast_sum / count
  count <- 0
  
  if(is.na(pops$com_nonbreeding[i])){
    print("no nonbr")
  } else {
    for(j in 1:length(nonbr_rasters)){
      if(nonbr_rasters[j] %in% files){
        a <- raster(nonbr_rasters[j])
        a[is.na(a)] <- 0 
        b <- sum(getValues(a)) # total number of birds in the 10km x 10km raster
        
        ## reprojecting and rescaling
        a_proj <- projectRaster(a, m, method = "bilinear")
        a_proj2 <- a_proj * r_area / 100000000 # rescaling the values in each cell
        a_proj2[is.na(a_proj2)] <- 0 
        a_proj2[is.na(m)] <- NA
        if(j == 1){
          nonbr_rast_sum <- a_proj2
          count <- 1
        } else {
          nonbr_rast_sum <- a_proj2 + nonbr_rast_sum
          count <- count + 1
        }
      }
    }
    pops$nonbr_n[i] <- count
    nonbr_rast_mean <- nonbr_rast_sum / count
    count <- 0  
    #writeRaster(nonbr_rast_sum, filename=paste0(dir_1by1,"/" ,pop,"_sum_nonbr.tif"), format="GTiff", overwrite=TRUE)
  }
  
  ## exporting results
  
  #write raster temp comment out####
  #writeRaster(br_rast_sum, filename=paste0(dir_1by1,"/" ,pop,"_sum_br.tif"), format="GTiff", overwrite=TRUE)
  
  #writeRaster(br_rast_mean, filename=paste0(dir_1by1m,"/" ,pop,"_mean_br.tif"), format="GTiff", overwrite=TRUE)
  #writeRaster(nonbr_rast_mean, filename=paste0(dir_1by1m,"/" ,pop,"_mean_nonbr.tif"), format="GTiff", overwrite=TRUE)
  
  #breeding season overlap
  over <- br_rast_sum * p_sum1
  over_score <- over
  summary(over_score@data@values)
  over_score[is.na(over_score)] <- 0
  over_val_br <- round(sum(getValues(over_score))*1000000,4)
  
  rast <- br_rast_sum
  rast[is.na(rast)] <- 0
  sum(getValues(rast))
  
  #png(paste0(dir_1by1,"/maps/",pop,"_br_sum.png"), width=1399,height=455)
  #par(mfrow=c(1,2))
  #plot(br_rast_sum,main=paste(pop,"br"),col=colsviri,legend=F)
  #plot(over,main=paste0("pieces per km2 * seabird = ",over_val_br),
  #     col=colsviri,legend=F)
  #dev.off()
  
  #plot png temp comment out ####
  #pdf(paste0(dir_1by1,"/maps_pdf/",pop,"_br.pdf"),
  #    paper = "a4r",width = 0, height = 0)
  #png(paste0(dir_1by1,"/maps2/",pop,"_br_sum.png"), width=1400,height=760)
  #par(mfrow=c(1,1))
  #plot(over,main=paste0(pops$common_name[i]," ",pops$species[i],", ",pops$pop[i],
  #                      "\nBreeding season plastic exposure score = ",
  #                      round(over_val_br,2)),
  #     cex.main=2,
  #     col=cols_inf,legend=F)
  #plot(land,col="grey75", 
  #     border = NA, add=T)
  #points(colloc$lon_colony,colloc$lat_colony,pch=18,
  #       cex=2.8,col="#004fd9")
  #dev.off()
  
  pops$over_val_br[i] <- over_val_br
  
  #nonbreeding season overlap
  if(is.na(pops$com_nonbreeding[i])){
    print(pop)}else{
      over <- nonbr_rast_sum * p_sum1
      over_score <- over
      summary(over_score@data@values)
      over_score[is.na(over_score)] <- 0
      over_val_nonbr <- round(sum(getValues(over_score))*1000000,4)
      
      #png(paste0(dir_1by1,"/maps/",pop,"_nonbr_sum.png"), width=1399,height=455)
      #par(mfrow=c(1,2))
      #plot(nonbr_rast_sum,main=paste(pop,"nonbr"),col=colsviri,legend=F)
      #plot(over,main=paste0("pieces per km2 * seabird = ",over_val_nonbr),
      #     col=colsviri,legend=F)
      #dev.off()
      
      #plot png temp comment out ####
      #png(paste0(dir_1by1,"/maps2/",pop,"_nonbr_sum.png"), width=1400,height=760)
      #pdf(paste0(dir_1by1,"/maps_pdf/",pop,"_nonbr.pdf"),
      #    paper = "a4r",width = 0, height = 0)
      #par(mfrow=c(1,1))
      #plot(over,main=paste0(pops$common_name[i]," ",pops$species[i],", ",pops$pop[i],
      #                      "\nNon-breeding season plastic exposure score = ",
      #                      round(over_val_br,2)),
      #     cex.main=2,
      #     col=cols_inf,legend=F)
      #plot(land,col="grey75", 
      #     border = NA, add=T)
      #points(colloc$lon_colony,colloc$lat_colony,pch=18,
      #       cex=2.5,col="#004fd9")
      #dev.off()
      pops$over_val_nonbr[i] <- over_val_nonbr
    }
  print(i)
}

pops$mean_br_com <- pops$over_val_br / pops$br_n
pops$mean_nonr_com <- pops$over_val_nonbr / pops$nonbr_n

write.csv(pops, paste0(dir_1by1, "/)7_exposure_by_season.csv"),
          row.names = F)  

pops <- read.csv(paste0(dir,"/outputs/05_phenology/pops_literature_test.csv"))

head(pops)
pops$breeding <- as.character(pops$breeding)
pops$nonbreeding <- as.character(pops$nonbreeding)
pops$ref_breeding <- as.character(pops$ref_breeding)
pops$ref_nonbreeding <- as.character(pops$ref_nonbreeding)
pops$com_breeding <- as.character(pops$com_breeding)
pops$com_nonbreeding <- as.character(pops$com_nonbreeding)

pops$breeding <- ifelse(nchar(pops$breeding) == 1,paste0("0",pops$breeding),pops$breeding)
pops$nonbreeding <- ifelse(nchar(pops$nonbreeding) == 1,paste0("0",pops$nonbreeding),pops$nonbreeding)
pops$ref_breeding <- ifelse(nchar(pops$ref_breeding) == 1,paste0("0",pops$ref_breeding),pops$ref_breeding)
pops$ref_nonbreeding <- ifelse(nchar(pops$ref_nonbreeding) == 1,paste0("0",pops$ref_nonbreeding),pops$ref_nonbreeding)
pops$com_breeding <- ifelse(nchar(pops$com_breeding) == 1,paste0("0",pops$com_breeding),pops$com_breeding)
pops$com_nonbreeding <- ifelse(nchar(pops$com_nonbreeding) == 1,paste0("0",pops$com_nonbreeding),pops$com_nonbreeding)

