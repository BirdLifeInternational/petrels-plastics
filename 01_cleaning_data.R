## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Mapping the global distribution of seabird populations
## R script to clean and prepare files (STD) prior to kernel analysis 
## Ana Carneiro
## May 2018
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#Adapted without age classes Beth Clark Mar 2020 

################ LOADING PACKAGES ###################

rm(list=ls())

#install.packages("maps"); install.packages("geosphere"); install.packages("trip"); install.packages("adehabitatHR"); install.packages("maptools"); 
#install.packages("rgdal"); install.packages("sp"); install.packages("rgeos"); install.packages("geosphere"); install.packages("mapdata"); install.packages("rworldmap")

library(maps) #3.3.0
library(geosphere) #1.5-10
library(trip) #1.7.1
library(sp) #1.3-2
library(adehabitatHR) #
library(maptools) #
library(rgdal)  #1.4-8
library(rgeos) #
library(geosphere) #
library(mapdata) #
library(circular) #
library(rworldmap) #
library(tidyverse) #
data(countriesLow)
lu=function (x=x) length(unique(x))

######### GENERAL DIRECTIONS AND FILES ##############

## GENERAL DIR
homedir <- paste0("C:/Users/bethany.clark/OneDrive - BirdLife International/",
                  "Methods") ## copy and paste here your working directory
#dir.create(paste0(homedir,"/scripts_results/")) 
dir <- paste0(homedir,"/scripts_results/01_cleaning_data/")
#dir.create(dir) 
dir_eq <- paste0(homedir,"/scripts_results/01_cleaning_data/equinox_new_oct20/")
#dir.create(dir_eq) 
#dir.create(paste0(dir,"maps"))


## PROJECTIONS
land <- readOGR(dsn=paste0(homedir,"/baselayer"), layer = "world-dissolved")  ## changed - BC

species_list <- read.csv("C:/Users/bethany.clark/OneDrive - BirdLife International/Requests/Species_list_petrels2.csv")

equinoxes <- read.csv("C:/Users/bethany.clark/OneDrive - BirdLife International/Data/equinoxes.csv")
head(equinoxes)
equinoxes$mar <- as.POSIXct(equinoxes$mar, format = "%d/%m/%Y %H:%M:%S", tz = "GMT")
equinoxes$sep <- as.POSIXct(equinoxes$sep, format = "%d/%m/%Y %H:%M:%S", tz = "GMT")

#filter
equinoxes$mar_start <- equinoxes$mar - (21*24*60*60)
equinoxes$mar_end <- equinoxes$mar + (7*24*60*60)
equinoxes$sep_start <- equinoxes$sep - (7*24*60*60)
equinoxes$sep_end <- equinoxes$sep + (21*24*60*60)

## DIRECTION OF THE ORIGINAL SPECIES FILES (AS DOWNLOADED FROM THE SEABIRD TRACKING DATABASE)
data_std <- "C:/Users/bethany.clark/OneDrive - BirdLife International/Data/PlasticsTrackingData/data_from_Martin/"#7 not working - not times
#data_std <- "C:/Users/bethany.clark/OneDrive - BirdLife International/Data/PlasticsTrackingData/STD_onedrive/"
#data_std <- "C:/Users/bethany.clark/OneDrive - BirdLife International/Data/PlasticsTrackingData/STD_onedrive_new/"
#data_std <- "C:/Users/bethany.clark/OneDrive - BirdLife International/Data/PlasticsTrackingData/Seatrack/"
#data_std <- "C:/Users/bethany.clark/OneDrive - BirdLife International/Data/PlasticsTrackingData/STD/"
#data_std <- "C:/Users/bethany.clark/OneDrive - BirdLife International/Data/PlasticsTrackingData/A_Jacob_G-S_datasets/"
#data_std <- "C:/Users/bethany.clark/OneDrive - BirdLife International/Data/PlasticsTrackingData/zoatrack/"
#data_std <- "C:/Users/bethany.clark/OneDrive - BirdLife International/Data/PlasticsTrackingData/data_from_Tommy/"
data_std <- "C:/Users/bethany.clark/OneDrive - BirdLife International/Data/PlasticsTrackingData/other_sources/"
#data_std <- "C:/Users/bethany.clark/OneDrive - BirdLife International/Data/PlasticsTrackingData/issue/"
#data_std <- "C:/Users/bethany.clark/OneDrive - BirdLife International/Data/PlasticsTrackingData/ARaine/"


files <- list.files(data_std, pattern="csv");files


#these are excluded from equinox filterting based on maps in 
#"/scripts_results/01_cleaning_data/equinox/"
no_filter <- list.files("C:/Users/bethany.clark/OneDrive - BirdLife International/Methods/scripts_results/01_cleaning_data/equinox_new/no filter/",
                   pattern = ".png")
prob_no_filter <- list.files("C:/Users/bethany.clark/OneDrive - BirdLife International/Methods/scripts_results/01_cleaning_data/equinox_new/probably no filter/",
                               pattern = ".png")
no_equinox <- list.files("C:/Users/bethany.clark/OneDrive - BirdLife International/Methods/scripts_results/01_cleaning_data/no_equinox/",
                             pattern = ".png")

equinox_checked <- c(no_filter,prob_no_filter,no_equinox)                
equinox_checked <- str_replace(equinox_checked,".png",".csv")                  

#equinox_checked <- c("done")

skipped_age <- data.frame()
skipped_species <- data.frame()

################# LOADING SPP DATA ##################

#for(dataset_number in 1:length(files)){ #length(files)
#  print(paste(dataset_number,files[dataset_number]))
#  df <- read.csv(paste0(data_std,files[dataset_number])) 
#  sci_name <- df$scientific_name[1];sci_name
#  print(nrow(df))

#for(dataset_number in 1:length(files)){ #length(files)
#  print(paste(dataset_number,files[dataset_number]))
#  df <- read.csv(paste0(data_std,files[dataset_number])) 
#  sci_name <- df$scientific_name[1];print(sci_name)
#  print(df$site_name[1])
#  print(df$colony_name[1])
#}
dataset_number <- 19
for(dataset_number in c(15)){ #length(files)
  print(paste(dataset_number,files[dataset_number]))
  df <- read.csv(paste0(data_std,files[dataset_number])) 
  sci_name <- df$scientific_name[1];sci_name
  head(df)
  df <- df[!is.na(df$latitude),]
  df <- df[!is.na(df$longitude),]
  summary(df)
  #remove bad latitudes at north/south pole
  df <- subset(df, latitude < 90 & latitude > -90)
  df <- subset(df, longitude < 180 & longitude > -180)
  
  #dataset specific corrections
  if(files[dataset_number] == "Dataset_973_2019-05-30.csv"){
    df <- subset(df,  longitude > -50)
  }#this is well outside the range of the data but not in equinox period
  if(files[dataset_number] == "Procellaria parkinsoni_New Zealand_GLS_2005-2006.csv"){
    df <- subset(df, date_gmt != "2006-12-01" & date_gmt != "2006-01-20" & date_gmt != "2006-01-30")
  } #these 3 dates were outside the range of the rest of the data and had the same lat/lon for different birds
  if(files[dataset_number] == "Dataset_1585_2020-07-16.csv"){
    df <- subset(df,  longitude > -2 & latitude > 26)
  }#this is well outside the range of the data but not in equinox period

  
  if(data_std == "C:/Users/bethany.clark/OneDrive - BirdLife International/Data/PlasticsTrackingData/STD/" |
     data_std == "C:/Users/bethany.clark/OneDrive - BirdLife International/Data/PlasticsTrackingData/STD2/"){
    df$bird_id <- "missing"
  }
  
  if(df$scientific_name[1] %in% species_list$Scientific_name){
    
    mean_loc <- geomean(cbind(df$longitude,df$latitude))
    
    DgProj <- CRS(paste0("+proj=laea +lon_0=",mean_loc[1],
                         "+lat_0=",mean_loc[2])) 
    #remove non-adults
    df <- subset(df,age != "immature")
    print(table(df$age))
    
    if(nrow(df) != 0){
      ###### CHECKING DATA AND CREATING DTIME COLUMN ######
      #df$bird_id <- factor(df$bird_id)
      df$track_id <- factor(df$track_id)
      df$time_gmt <- ifelse(is.na(df$time_gmt),"00:00:00",df$time_gmt)
      df$dtime <- as.POSIXct(strptime(paste(df$date_gmt, df$time_gmt, sep=" "), 
                                      "%Y-%m-%d %H:%M:%S "),"GMT") 
      df <- df[!is.na(df$dtime),]
      ## PLOTTING TO CHECK RESULTS 
      par(mfrow=c(1,1))
      plot(latitude~longitude, data=df, type="n", asp=1, main="", frame = T, xlab="", ylab="")
      plot(countriesLow, col='lightgrey', add=T)
      points(latitude~longitude, data=df, pch=16, cex=0.5, col="blue")
      points(lat_colony~lon_colony, data=df, pch=18, cex=2, col="red")
      
      ############# DATA CLEANING: SPEED FILTER FOR PTT DATA #############
      
      devices <- unique(df$device)
      devices
      
      if("PTT" %in% devices | "GPS" %in% devices){
        if("PTT" %in% devices){
          ## REMOVE LOCATIONS WITH > 90 KM/H
          x2 <- data.frame()
          x3 <- data.frame()
          id <- unique(as.factor(df$track_id))
          for(i in 1:nlevels(id)){
            a <- subset(df, track_id == id[i])
            if(a$device[1]=="PTT" & nrow(a) > 5){
              # Order the rows by ID, then by time
              b <- a[order(a$dtime), ]
              # Remove completely-duplicated rows
              b <- b[!duplicated(b),]
              b$dtime <- adjust.duplicateTimes(b$dtime, b$track_id)
              # Change times to hours since first fix
              b$hours <- as.numeric(difftime(b$dtime, min(b$dtime),units = "hours"))
              x2 <- rbind(x2, as.data.frame(b))} else {
                x3 <- rbind(x3, as.data.frame(a))
              }
          }
          x1 <- x2
          ## APPLY MCCONNELL SPEED FILTER IN TRIP PACKAGE TO REMOVE ERRONEOUS FIXES
          x2 <- data.frame(lat = x1$latitude, lon = x1$longitude, DateTime = x1$dtime, id = x1$track_id)
          ## CREATE COORDINATE VARIABLE
          coordinates(x2) <- c("lon","lat")
          ## CREATE TRIP OBJECT
          tr <- trip(x2, c("DateTime","id"))
          ## MCCONNELL SPEED FILTER; ignore coordinates warning as data are lonlat
          x1$Filter <- speedfilter(tr, max.speed = 90)
          ## REMOVE FILTERED COORDINATES
          x1 <- subset(x1, x1$Filter==TRUE)
          x1$hours <- NULL
          x1$Filter <- NULL
          
          ## COMBINE PTT FILTERED FILE BACK INTO DF FILE (with GLS and GPS data)
          df <- rbind(x1,x3)
          ## PLOT DATA TO CHECK IT LOOK OK
          #xlow<-min(df$longitude)+0.2
          #xup<-max(df$longitude)-0.2
          #yup<-max(df$latitude)-0.5
          #ylow<-min(df$latitude)+0.5
          #plot(latitude~longitude, data=df, pch=16, cex=0.3, col=factor(dataset_id), asp=1, xlim=c(xlow,xup), ylim=c(ylow,yup), main="", frame=F, axes=F, xlab="", ylab="")
          #plot(countriesLow, col='darkgrey', add=T)
          #legend("topright", legend=levels(factor(df$dataset_id)), pch=16, col=unique(factor(df$dataset_id)))
        }
        ################## LINEAR INTERPOLATION PTT AND GPS DATA #################
        x4 <- df
        
        coordinates(x4) <- ~longitude+latitude
        proj4string(x4) <- CRS(proj4string(land))
        ## changing projection
        x4_laea <- spTransform(x4, DgProj)
        x4 <- as.data.frame(x4_laea)
        ## INTERPOLATION
        x5 <- data.frame()
        x6 <- data.frame()
        dset_id <- unique(as.factor(x4$dataset_id))
        x4$id_stage <- paste0(x4$bird_id, "_", x4$track_id)   ## had to create that to run the analysis because some tracks start as breeding and end up as non-breeding

        for(i in 1:nlevels(dset_id)){
          print(dset_id[i])
          tracks <- x4[x4$dataset_id==dset_id[i],]
          tracks$device <- factor(tracks$device)
          if(tracks$device[1]!="GLS"){
            tracks$id_stage <- factor(tracks$id_stage)
            tracks$track_time <- as.double(tracks$dtime)            
            
            
            if(tracks$bird_id[1] != "missing"|is.na(tracks$bird_id[1])){
              tracks$birdid_time <- paste0(tracks$bird_id,tracks$track_time)
            } else {
              tracks$birdid_time <- paste0(tracks$track_id,tracks$track_time)
            }
            
            #tracks$track_time <- adjust.duplicateTimes(tracks$track_time, tracks$id_stage) #this crashed some of Jacob's datasets
            
            tracks <- tracks[!duplicated(tracks$birdid_time),]
            
            tracks <- tracks[!is.na(tracks$track_time),]
            
            tracks$id_stage <- as.factor(as.character(tracks$id_stage))

            #sort by bird then time
            #tracks <- dplyr::arrange(tracks, bird_id, track_time)
            
            traj <- as.ltraj(xy=data.frame(tracks$longitude, tracks$latitude), 
                             date=as.POSIXct(tracks$track_time, origin="1970/01/01", tz="GMT"), 
                             id=tracks$id_stage, typeII = TRUE)
            ## Rediscretization every 12 hours
            tr <- adehabitatLT::redisltraj(traj, 43200, type="time")
            ## Convert output into a data frame
            tracks.intpol <- data.frame()
            for (l in 1:length(unique(tracks$id_stage))){
              # print(tracks$id_stage[l])
              out <- tr[[l]]
              out$ID <- as.character(attributes(tr[[l]])[4])				
              tracks.intpol <- rbind(tracks.intpol, out)
            } 
            ### re-insert
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
        
      } 
      
      if(devices == "GLS"){  
        #equinox filter if needed  ####
        print(table(as.factor(df$equinox)))
        
        if(files[dataset_number] %in% equinox_checked) {
          print("equinox_checked")
        } else {
          df$year <- substr(df$dtime,1,4)
          years <- unique(df$year)
          for(i in 1:length(years)){
            
            yr <- subset(df, year == years[i])
            
            df_mar <- subset(yr, dtime < equinoxes$mar_start[equinoxes$year == years[i]] |
                               dtime > equinoxes$mar_end[equinoxes$year == years[i]] ) 
            df_sep <- subset(df_mar, dtime < equinoxes$sep_start[equinoxes$year == years[i]] |
                               dtime > equinoxes$sep_end[equinoxes$year == years[i]] ) 
            if(i == 1){
              df_allyrs <- df_sep
            } else {
              df_allyrs <- rbind(df_allyrs,df_sep)
            }
          }  
          
          
          png(filename = paste0("C:/Users/bethany.clark/OneDrive - BirdLife International/Methods/scripts_results/01_cleaning_data/equinox_filtered/", str_remove(files[dataset_number],".csv"),".png"))
          par(mfrow=c(2,1),mar = c(3, 4, 1, 1))
          plot(latitude~longitude, data=df, type="n", asp=1, main="", 
               frame = T, xlab="", ylab=paste(df$scientific_name[1]))
          plot(countriesLow, col='lightgrey', add=T)
          points(latitude~longitude, data=df, pch=16, cex=0.5, col="blue")
          points(latitude~longitude, data=df_allyrs, pch=16, cex=0.5, col="green")
          points(lat_colony~lon_colony, data=df, pch=18, cex=2, col="red")
          
          plot(df$dtime,df$latitude,col="blue")
          points(df_allyrs$dtime,df_allyrs$latitude,col="green")
          abline(v=equinoxes$mar,col="red")
          abline(v=equinoxes$sep,col="red")
          
          abline(v=equinoxes$mar_start,col="orange")
          abline(v=equinoxes$sep_start,col="orange")
          abline(v=equinoxes$mar_end,col="orange")
          abline(v=equinoxes$sep_end,col="orange")
          
          dev.off()
          df <- df_allyrs
          df$year <- NULL
        }
        
        coordinates(df) <- ~longitude+latitude
        proj4string(df) <- CRS(proj4string(land))
        ## changing projection
        x4_laea <- spTransform(df, DgProj)
        df <- as.data.frame(x4_laea)
      }
      ## PLOTTING TO CHECK RESULTS
      #xlow<-min(df$longitude)+0.2
      #xup<-max(df$longitude)-0.2
      #yup<-max(df$latitude)-0.5
      #ylow<-min(df$latitude)+0.5
      #plot(latitude~longitude, data=df, pch=16, cex=0.3, col=factor(dataset_id), asp=1, xlim=c(xlow,xup), ylim=c(ylow,yup), main="", frame=F, axes=F, xlab="", ylab="")
      #legend("topright", legend=levels(factor(df$dataset_id)), pch=16, col=unique(factor(df$dataset_id)))
      
      ############### REMOVE INLAND POSITION ####
      
      ## Exclude all locations within 5 km (GPS) or 15 km (PTT) of the colony, 
      #and retain all the rest. Colony here is a single lat/lon position.
      x7 <- df
      
      ## Creating a buffer around colonies
      gps_df <- data.frame()
      ptt_df <- data.frame()
      gls_df <- data.frame()
      
      #use site name instead
      x7$site_name <- as.character(x7$site_name)
      x7$colony_name <- as.character(x7$colony_name)
      x7$colony_name <- ifelse(is.na(x7$colony_name), x7$site_name, x7$colony_name)
      x7$colony_name <- as.factor(x7$colony_name)
      
      col <- unique(x7$colony_name);col
      
      #for Bulwer's petrel, this loop returned only 1 lat/lon
      
      for(i in 1:nlevels(col)){
        
        print(col[i])
        sub_col <- x7[x7$colony_name==col[i],]
        coordinates(sub_col) <- ~longitude+latitude
        proj4string(sub_col) <- DgProj
        
        ## removing land overlap
        sub_col@data$device <- factor(sub_col@data$device)
        dev <- unique(sub_col@data$device)
        
        for (j in 1:nlevels(dev)){
          print(dev[j])
          a <- sub_col[sub_col$device==dev[j],]
          
          if (a$device[1]=="GPS"){
            
            if(col[i] != "At-Sea"){
              df_col <- data.frame(cbind(lon=sub_col$lon_colony[1], lat=sub_col$lat_colony[1]))
              coordinates(df_col) <- ~lon+lat
              ## assigning projection
              proj4string(df_col) <- CRS(proj4string(land))
              ## changing projection
              col_laea <- spTransform(df_col, DgProj)
              ## buffers
              buf_small <- gBuffer(col_laea, width = 5000)
              gps <- a[is.na(over(a, buf_small)),]
            } else {
              gps <- a[is.na(a),]
            }
            col_df <- as.data.frame(gps)
            col_df$argos_quality <- NA
            col_df$equinox <- NA
            gps_df <- rbind(gps_df, col_df)
            
            
          } else if (a$device[1]=="PTT"){
            
            if(col[i] != "At-Sea"){
              df_col <- data.frame(cbind(lon=sub_col$lon_colony[1], lat=sub_col$lat_colony[1]))
              coordinates(df_col) <- ~lon+lat
              ## assigning projection
              proj4string(df_col) <- CRS(proj4string(land))
              ## changing projection
              col_laea <- spTransform(df_col, DgProj)
              ## buffers
              buf_large <- gBuffer(col_laea, width = 15000)
              ptt <- a[is.na(over(a, buf_large)),]
            } else {
              ptt <- a
            }
            col_df <- as.data.frame(ptt)
            col_df$argos_quality <- NA
            col_df$equinox <- NA
            ptt_df <- rbind(ptt_df, col_df)
            
            
          } else {
            col_df <- as.data.frame(a)
            col_df$track_time <- NA
            col_df$date_gmt <- NULL
            col_df$time_gmt <- NULL
            
            head(col_df)
            years <- unique(substr(col_df$dtime,1,4))
            
            
            gls_df <- rbind(gls_df, col_df)
            
            
          }
        }
      } 
      
      df_all <-  rbind(gps_df, ptt_df, gls_df)
      coordinates(df_all) <- ~longitude+latitude
      proj4string(df_all) <- DgProj
      ## changing projection
      df_all_wgs <- spTransform(df_all, (proj4string(land)))
      df_all_wgs <- as.data.frame(df_all_wgs)
      df <- df_all_wgs
      df$original_track_id <- NULL
      df$equinox <- NA
      
      ## PLOTTING TO CHECK RESULTS
      #plot(latitude~longitude, data=df, type="n", asp=1, 
      #     xlim=c(-180,180), ylim=c(-90,90), main="", frame = T, xlab="", ylab="")
      #plot(countriesLow, col='lightgrey', add=T)
      #points(latitude~longitude, data=df, pch=16, cex=0.5, col="green")
      #points(lat_colony~lon_colony, data=df, pch=18, cex=2, col="purple")
      
      # Removed YEAR QUARTERS - BC
      
      ############# SPECIES METADATA FOR KERNELS ###########
      
      ## Create a folder in your computer to export the metadata to run kernels
      
      
      
      ######## REMOVING GROUPS WITH LESS THAN 5 ROWS ########
      ## THIS IS A REQUIREMENT TO RUN KERNELS IN THE ADEHABITATHR PACKAGE
      
      
      ## REMOVING GROUPS WITH LESS THAN 5 ROWS
      
      ########### REORDER COLUMNS (ALL SAME FORMAT) - removed BC
      
      ############ EXPORT RESULTS READY TO KERNELS #########
      
      ## Create a folder in your computer to export tracking data already cleaned and formatted
      
      site <- str_remove(df$site_name[1],"/")
      colony <- str_remove(df$colony_name[1],"/")
      
      if(data_std == "C:/Users/bethany.clark/OneDrive - BirdLife International/Data/PlasticsTrackingData/data_from_Martin/"){
        write.csv(df, paste0(dir, 
                             df$scientific_name[1],"_",site,"_",colony,"_Martin",dataset_number,".csv"), 
                  row.names = FALSE)
        png(filename = paste0(dir, "maps/", 
                              df$scientific_name[1],"_",site,"_",colony,"_Martin",dataset_number,".png"))
      } else {
        
        write.csv(df, paste0(dir,  
                             df$scientific_name[1],"_",site,"_",colony,"_",
                             df$dataset_id[1],".csv"), 
                  row.names = FALSE)
        png(filename = paste0(dir, "maps/", 
                              df$scientific_name[1],"_",site,"_",colony,"_",
                              df$dataset_id[1],".png"))
      }
      
      ## PLOTTING TO CHECK RESULTS

      
      plot(latitude~longitude, data=df, type="n", asp=1, 
           xlim=c(-180,180), ylim=c(-90,90), 
           main="", frame = T, xlab="", ylab="")
      plot(countriesLow, col='lightgrey', add=T)
      points(latitude~longitude, data=df, pch=16, cex=0.5, col="green")
      points(lat_colony~lon_colony, data=df, pch=18, cex=2, col="purple")
      dev.off()
      
      print(df$scientific_name[1])
      print(df$colony_name[1])
      
    } else {
      name <- paste0(sci_name," dataset_number = ",dataset_number)
      skipped_age <- rbind(skipped_age, as.data.frame(name))
    }
  } else {
    name <- paste0(sci_name," dataset_number = ",dataset_number)
    skipped_species <- rbind(skipped_species, as.data.frame(name))
  }
  
}

#check skipped files
skipped_age
skipped_species
