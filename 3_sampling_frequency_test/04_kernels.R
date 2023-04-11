## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Mapping the global distribution of seabird populations
## R script to run kernel analysis (per individual and then merged) 
## Ana Carneiro May 2018 + Beth Clark Mar 2020-May2022
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
library(rgeos) 
library(stringr)
library(trip)

######### GENERAL DIRECTIONS AND FILES ##############

## PROJECTIONS

#Read in land file for visualisation:
#Natural Earth land 1:10m polygons version 5.1.1 
#downloaded from www.naturalearthdata.com/
land <- rgdal::readOGR(dsn = "input_data/baselayer", layer = "ne_10m_land")
proj_wgs84 <- sp::CRS(sp::proj4string(land))

## TO SAVE KERNEL RESULTS
## Create a folder in your computer to save kernel results 
dir.create("outputs/04_kernels")
dir.create("outputs/04_kernels/locations_plots")
dir.create("outputs/04_kernels/kde_plots")
dir_kernels <- "outputs/04_kernels/"

################# LOADING SPP DATA ##################
data_std <- "outputs/02_pops/"
files <- list.files(data_std);files

sampling_freq_record <- read.csv("outputs/03_sampling_freq_record.csv")
head(sampling_freq_record)
sampling_freq_record$n <- 1:nrow(sampling_freq_record)

not_GLS <- subset(sampling_freq_record, freq != "GLS")
not_GLS$freq <- as.numeric(not_GLS$freq)

#freqs less than 6 hour intervals
kernels_to_run <- subset(not_GLS, freq < (6*60*60))

summary(kernels_to_run$freq)/60
max(kernels_to_run$freq)/(60*60)

dataset_number <- 1

for(dataset_number in kernels_to_run$n){ 
  print(dataset_number)
  print(files[dataset_number])
  name_png <- stringr::str_remove(files[dataset_number],".csv")
  df <- read.csv(paste0(data_std,files[dataset_number]))  
  
  head(df)
  
  # Regularisation using linear interpolation for PTT and GPS data #################  
  devices <- unique(df$device);devices
  
  if(!("GLS" %in% devices)){
    
    #figure out sampling frequency
    df$data_bird_track <- paste(df$dataset_id, df$bird_track,sep="_")
    bird_tracks <- unique(df$data_bird_track)

    #set regularisation to the max median sampling frequency
    regularisation_freq <- as.numeric(sampling_freq_record$freq[dataset_number])
    
    x4 <- df
    
    sp::coordinates(x4) <- ~longitude+latitude
    sp::proj4string(x4) <- sp::CRS(sp::proj4string(land))
    
    ## INTERPOLATION
    x5 <- data.frame()
    x6 <- data.frame()
    dset_id <- unique(as.factor(x4$dataset_id))
    x4$id_stage <- paste0(x4$bird_id, "_", x4$track_id)   
    
    #for each dataset id
    for(i in 1:nlevels(dset_id)){
      print(dset_id[i])
      tracks <- x4[x4$dataset_id==dset_id[i],]
      tracks$device <- factor(tracks$device)
      if(tracks$device[1]!="GLS"){
        tracks$id_stage <- factor(tracks$id_stage)
        tracks$dtime <- as.POSIXct(tracks$dtime, 
                                   format = "%Y-%m-%d %H:%M:%S",
                                   tz = "GMT") 
        tracks$track_time <- as.double(tracks$dtime)            
        
        tracks$birdid_time <- paste0(tracks$bird_id,tracks$track_time)
        
        #remove duplicates and NAs
        tracks <- tracks[!duplicated(tracks$birdid_time),]
        
        tracks <- tracks[!is.na(tracks$track_time),]
        
        tracks$id_stage <- as.factor(as.character(tracks$id_stage))
        
        traj <- adehabitatLT::as.ltraj(xy=data.frame(tracks$longitude, tracks$latitude), 
                                       date=as.POSIXct(tracks$track_time, origin="1970/01/01", tz="GMT"), 
                                       id=tracks$id_stage, typeII = TRUE)
        
        ## Rediscretization according to sampling frequency
        tr <- adehabitatLT::redisltraj(traj, regularisation_freq, type="time")
        ## Convert output into a data frame
        tracks.intpol <- data.frame()
        for (l in 1:length(unique(tracks$id_stage))){
          # print(tracks$id_stage[l])
          out <- tr[[l]]
          out$ID <- as.character(attributes(tr[[l]])[4])				
          tracks.intpol <- rbind(tracks.intpol, out)
        } 
        ### re-insert into the dataframe
        tracks.intpol$dataset_id <- tracks$dataset_id[match(tracks.intpol$ID,tracks$id_stage)]
        tracks.intpol$scientific_name <- tracks$scientific_name[match(tracks.intpol$ID,tracks$id_stage)]
        tracks.intpol$common_name <- tracks$common_name[match(tracks.intpol$ID,tracks$id_stage)]
        tracks.intpol$site_name <- tracks$site_name[match(tracks.intpol$ID,tracks$id_stage)]
        tracks.intpol$colony_name <- tracks$colony_name[match(tracks.intpol$ID,tracks$id_stage)]
        tracks.intpol$lat_colony <- tracks$lat_colony[match(tracks.intpol$ID,tracks$id_stage)]
        tracks.intpol$lon_colony <- tracks$lon_colony[match(tracks.intpol$ID,tracks$id_stage)]
        tracks.intpol$device <- tracks$device[match(tracks.intpol$ID,tracks$id_stage)]
        tracks.intpol$bird_id <- tracks$bird_id[match(tracks.intpol$ID,tracks$id_stage)]
        tracks.intpol$track_id <- tracks$track_id[match(tracks.intpol$ID,tracks$id_stage)]
        tracks.intpol$original_track_id <- tracks$original_track_id[match(tracks.intpol$ID,tracks$id_stage)]
        tracks.intpol$age<-tracks$age[match(tracks.intpol$ID,tracks$id_stage)]
        tracks.intpol$sex<-tracks$sex[match(tracks.intpol$ID,tracks$id_stage)]
        tracks.intpol$breed_stage <- tracks$breed_stage[match(tracks.intpol$ID,tracks$id_stage)]
        tracks.intpol$breed_status <- tracks$breed_status[match(tracks.intpol$ID,tracks$id_stage)]
        tracks.intpol$track_time <- as.double(tracks.intpol$date)
        tracks.intpol <- tracks.intpol[order(tracks.intpol$ID, tracks.intpol$track_time),]
        tracks.intpol$track_time <- adjust.duplicateTimes(tracks.intpol$track_time, tracks.intpol$ID)
        #### combines all data
        x5 <- rbind(x5, as.data.frame(tracks.intpol))
      } else {
        x6 <- rbind(x6, as.data.frame(tracks))}
    }
    ## renaming x5 and combining x5 and x6
    head(x5)
    head(x6) 
    names(x5)[names(x5) == "date"] <- "dtime"
    names(x5)[names(x5) == "x"] <- "longitude"
    names(x5)[names(x5) == "y"] <- "latitude"
    
    #remove columns that are no longer needed
    x5$ID <- NULL
    x5$dx <- NULL
    x5$dy <- NULL
    x5$dist <- NULL
    x5$dt <- NULL
    x5$R2n <- NULL
    x5$abs.angle <- NULL
    x5$rel.angle <- NULL
    x6$argos_quality <- NULL
    x6$date_gmt <- NULL
    x6$time_gmt <- NULL
    x6$track_time <- as.double(x6$dtime)
    x6$id_stage <- NULL
    x6$equinox <- NULL
    df <- rbind(x5,x6)
    df$equinox <- NA
    
  } else {
    sampling_freq_record$freq[dataset_number] <- "GLS"
  }
  
  df <- subset(df, latitude < 90)
  
  png(filename = paste0(dir_kernels, "locations_plots/", name_png, ".png"))
  plot(latitude~longitude, data=df, type="n", asp=1, 
       xlim=c(min(df$longitude)+0.2,max(df$longitude)-0.2), 
       ylim=c(min(df$latitude)+0.5, max(df$latitude)-0.5), 
       main="", frame = T, xlab="", ylab="")
  plot(land, col='lightgrey', add=T)
  points(latitude~longitude, data=df, pch=16, cex=0.5, col="blue")
  points(lat_colony~lon_colony, data=df, pch=18, cex=2, col="red")
  dev.off()
  
  ################## KERNEL ANALYSIS ####################
  
  all_data <- df[!is.na(df$dtime), ]
  
  all_data$month <- as.factor(as.character(substr(all_data$dtime,6,7)))
  
  months <- sort(unique(all_data$month))
  
  print(summary(all_data$month))
  
  for (month_number in 1:length(months)){ #1:length(months)
    
    tracks_wgs <- all_data[all_data$month == months[month_number],]
    
    #plot one month on top of the rest
    plot(latitude~longitude, data=df, type="n", asp=1, 
         xlim=c(min(df$longitude)+0.2,max(df$longitude)-0.2), 
         ylim=c(min(df$latitude)+0.5, max(df$latitude)-0.5), 
         main="", frame = T, xlab="", ylab="")
    plot(land, col='lightgrey', add=T)
    points(latitude~longitude, data=df, pch=16, cex=0.5, col="blue")
    points(latitude~longitude, data=tracks_wgs, pch=16, cex=0.5, col="red")
    
    
    if(nrow(tracks_wgs) > 4){
      
      ##### CREATING A NULL GRID  ######
      
      if(min(tracks_wgs$longitude) <= -179 ){ lon_min <- -180
      } else {lon_min <- floor(min(tracks_wgs$longitude))-1 }
      
      if(max(tracks_wgs$longitude) >= 179){ lon_max <- 180
      } else { lon_max <- ceiling(max(tracks_wgs$longitude))+1 }
      
      if(min(tracks_wgs$latitude) <= -89 ){ lat_min <- -90 
      } else { lat_min <- floor(min(tracks_wgs$latitude))-1 }
      
      if(max(tracks_wgs$latitude) >= 89){ lat_max <- 90
      } else { lat_max <- ceiling(max(tracks_wgs$latitude))+1 }
      
      so.grid <- expand.grid(LON = seq(lon_min, lon_max, by=1), 
                             LAT = seq(lat_min, lat_max, by=1))
      
      sp::coordinates(so.grid) <- ~LON+LAT
      sp::proj4string(so.grid) <- sp::proj4string(land)
      
      mean_loc <- geosphere::geomean(cbind(tracks_wgs$longitude,tracks_wgs$latitude))
      DgProj <- sp::CRS(paste0("+proj=laea +lon_0=",mean_loc[1],
                               " +lat_0=",mean_loc[2])) 
      
      so.grid.proj <- sp::spTransform(so.grid, CRS=DgProj)
      coords <- so.grid.proj@coords
      
      c <- min(coords[,1])-1000000   ## to check my min lon
      d <- max(coords[,1])+1000000   ## to check my max lon
      
      e <- min(coords[,2])-1000000   ## to check my min lat
      f <- max(coords[,2])+1000000   ## to check my max lat
      
      a <- seq(c, d, by=10000)
      b <- seq(e, f, by=10000)
      null.grid <- expand.grid(x=a,y=b)
      sp::coordinates(null.grid) <- ~x+y
      sp::gridded(null.grid) <- TRUE
      class(null.grid)
      
      sp::coordinates(tracks_wgs) <- ~longitude+latitude
      sp::proj4string(tracks_wgs) <- sp::proj4string(land)
      tracks <- sp::spTransform(tracks_wgs, CRS=DgProj)
      
      tracks$month <- factor(tracks@data$month)
      KDE_ref <- paste0(str_remove(files[dataset_number],".csv"), "_", 
                        months[month_number])
      
        kudl <- adehabitatHR::kernelUD(tracks[,"month"], 
                                       grid = null.grid, h = 50000) 

      ## smoothing factor equals 200 km due to error on GLS data
      # 50 km for GPS and PTT data (See Carneiro)
      
      #image(kudl)
      vud <- adehabitatHR::getvolumeUD(kudl)
      ## store the volume under the UD (as computed by getvolumeUD)
      ## of the first animal in fud
      fud <- vud[[1]]
      ## store the value of the volume under UD in a vector hr95
      hr95 <- as.data.frame(fud)[,1]
      ## if hr95 is <= 95 then the pixel belongs to the home range
      ## (takes the value 1, 0 otherwise)
      hr95 <- as.numeric(hr95 <= 95)
      ## Converts into a data frame
      hr95 <- data.frame(hr95)
      ## Converts to a SpatialPixelsDataFrame
      coordinates(hr95) <- coordinates(fud)
      sp::gridded(hr95) <- TRUE
      
      ## display the results
      kde_spixdf <- adehabitatHR::estUDm2spixdf(kudl)
      kern95 <- kde_spixdf
      
      stk_100 <- raster::stack(kern95)
      stk_95 <- raster::stack(hr95)
      
      sum_all_100 <- stk_100[[1]]
      sum_all_95 <- stk_95[[1]]
      
      sum_all_raw <- sum_all_100*sum_all_95
      
      rast <- sum_all_raw/sum(raster::getValues(sum_all_raw))
      rast[rast == 0] <- NA
      # plot(rast)
      
      KDERasName_sum <- paste0(dir_kernels, KDE_ref, ".tif")
      
      x.matrix <- is.na(as.matrix(rast))
      colNotNA <- which(colSums(x.matrix) != nrow(rast))
      rowNotNA <- which(rowSums(x.matrix) != ncol(rast))
      
      croppedExtent <- raster::extent(rast, 
                                      r1 = rowNotNA[1]-2, 
                                      r2 = rowNotNA[length(rowNotNA)]+2,
                                      c1 = colNotNA[1]-2, 
                                      c2 = colNotNA[length(colNotNA)]+2)
      
      cropped <- raster::crop(rast, croppedExtent)
      cropped[is.na(cropped)] <- 0
      #plot(cropped)
      
      #Land mask
      mask_proj <- sp::spTransform(land, DgProj)   ## changing projection
      mask_proj_pol <- as(mask_proj, "SpatialPolygons")   ## converting SpatialPolygonsDataFrame to SpatialPolygons
      
      ## set to NA cells that overlap mask (land)
      rast_mask_na <- raster::mask(cropped, mask_proj_pol, inverse = TRUE)
      rast_mask <- rast_mask_na
      rast_mask[is.na(rast_mask)] <- 0
      rast_mask_sum1 <- rast_mask/sum(raster::getValues(rast_mask))
      #rast_mask[rast_mask == 0] <- NA
      rast_mask_final <- raster::mask(rast_mask_sum1, mask_proj_pol, inverse = TRUE)
      rast_mask_final2 <- rast_mask_final 
      
      #one dataset had too big a range, so a small amount was cropped ####
      if(files[dataset_number] == "Ardenna grisea_Codfish Island.csv"){
        if(month_number == 5){
          rast_mask_final2 <- raster::crop(rast_mask_final, 
                                           extent(extent(rast_mask_final)[1],
                                                  extent(rast_mask_final)[2]-30000,
                                                  extent(rast_mask_final)[3]+20000,
                                                  extent(rast_mask_final)[4])) 
        }
      }
      
      #PLOT & SAVE ####
      mask_wgs84 <- raster::projectRaster(rast_mask_final2, crs=proj_wgs84, over = F)
      
      raster::writeRaster(mask_wgs84, filename = KDERasName_sum, 
                          format = "GTiff", overwrite = TRUE)
      
      ## Plot
      png(filename = paste0(dir_kernels, "kde_plots/", KDE_ref, ".png"))
      plot(mask_wgs84, main = KDE_ref)
      plot(land, add=T, col = "#66000000")
      dev.off()
      
    }
  }

}

#error for 106 "Pterodroma cervicalis_Phillip Island.csv"
