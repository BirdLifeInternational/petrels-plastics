## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Mapping the global distribution of seabird populations
## R script to clean and prepare files (Seabird Tracking Database format) prior to kernel analysis 
## Ana Carneiro May 2018, adapted by Beth Clark Mar 2020 
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

################ LOADING PACKAGES ###################

rm(list=ls())

#If there are errors, can try installing these package versions:
#install.packages("devtools")
#require(devtools)
#install_version("sp", version = "1.3-2", repos = "http://cran.us.r-project.org")
#install_version("rgdal", version = "1.4-8", repos = "http://cran.us.r-project.org")
#newer versions of sp and rgdal don't work with the custom projection see:
#https://github.com/gdauby/ConR/issues/5
#https://github.com/BirdLifeInternational/track2kba/issues/29

#install.packages("geosphere"); install.packages("adehabitatHR"); 
#install.packages("rgeos"); install.packages("stringr"); nstall.packages("trip");
library(sp) #1.3-2
library(rgdal)  #1.4-8
library(geosphere) #1.5-10
library(adehabitatHR) 
library(rgeos) 
library(stringr)
library(trip)
lu=function (x=x) length(unique(x))

######### GENERAL DIRECTORIES AND FILES ##############

## set up input and output folders
dir <- "outputs/01_cleaning_data/"

#if running for the first time, create directories for outputs
dir.create(dir) 
dir.create(paste0(dir,"equinox_filtered/"))
dir.create(paste0(dir,"equinox_filtered/remove_equinox_filter"))
dir.create(paste0(dir,"maps/"))

## PROJECTIONS
land <- readOGR(dsn = "input_data/baselayer", layer = "world-dissolved")  

#Read in a list of the names of the target species for the study
species_list <- read.csv("input_data/Species_list_IUCN.csv")

equinoxes <- read.csv("input_data/equinoxes.csv")
head(equinoxes)
equinoxes$mar <- as.POSIXct(equinoxes$mar, format = "%d/%m/%Y %H:%M:%S", tz = "GMT")
equinoxes$sep <- as.POSIXct(equinoxes$sep, format = "%d/%m/%Y %H:%M:%S", tz = "GMT")

#mark the start and end of the periods to filter out (it is asymmetrical)
equinoxes$mar_start <- equinoxes$mar - (21*24*60*60) #-21 days
equinoxes$mar_end <- equinoxes$mar + (7*24*60*60)    #+7 days
equinoxes$sep_start <- equinoxes$sep - (7*24*60*60)
equinoxes$sep_end <- equinoxes$sep + (21*24*60*60)

## DIRECTION OF THE ORIGINAL SPECIES FILES (AS DOWNLOADED FROM THE SEABIRD TRACKING DATABASE)
datasets <- "input_data/tracking_data/"

files <- list.files(datasets, pattern = "csv");files

skipped_age <- data.frame()
skipped_species <- data.frame()

################# LOADING SPP DATA ##################

#loop through tracking data files
dataset_number <- 1
for(dataset_number in 1:length(files)){ 
  print(paste(dataset_number,files[dataset_number]))
  
  #read in the dataset
  df <- read.csv(paste0(datasets,files[dataset_number])) 
  sci_name <- df$scientific_name[1];sci_name
  head(df)
  
  site <- str_remove(df$site_name[1],"/")
  colony <- str_remove(df$colony_name[1],"/")
  
  #remove rows with NA for lat or lon
  df <- df[!is.na(df$latitude),]
  df <- df[!is.na(df$longitude),]
  
  #remove data with impossible lat/lons
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
  
  #Check the scientific name is in the list of chosen species
  if(df$scientific_name[1] %in% species_list$scientific_name){
    
    #remove non-adults
    df <- subset(df, age != "immature" & age != "juvenile")
    print(table(df$age))
    
    if(nrow(df) != 0){
      
      ###### CHECKING DATA AND CREATING DTIME COLUMN in timestamp format ######
      df$bird_id <- factor(df$bird_id)
      df$track_id <- factor(df$track_id)
      df$time_gmt <- ifelse(is.na(df$time_gmt),"00:00:00",df$time_gmt)
      df$dtime <- as.POSIXct(strptime(paste(df$date_gmt, df$time_gmt, sep=" "), 
                                      "%Y-%m-%d %H:%M:%S "),"GMT") 
      df <- df[!is.na(df$dtime),]
      ## PLOTTING TO CHECK RESULTS 
      par(mfrow=c(1,1))
      plot(latitude~longitude, data=df, type="n", asp=1, main="", frame = T, xlab="", ylab="")
      plot(land, col='lightgrey', add=T)
      points(latitude~longitude, data=df, pch=16, cex=0.5, col="blue")
      points(lat_colony~lon_colony, data=df, pch=18, cex=2, col="red")
      
      ############# DATA CLEANING: SPEED FILTER FOR PTT DATA #############
      
      devices <- unique(df$device)
      devices
      
      #If the tag is a PTT, apply speed filter
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
            b$dtime <- trip::adjust.duplicateTimes(b$dtime, b$track_id)
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
        sp::coordinates(x2) <- c("lon","lat")
        ## CREATE TRIP OBJECT
        tr <- trip::trip(x2, c("DateTime","id"))
        ## MCCONNELL SPEED FILTER; ignore coordinates warning as data are lonlat
        x1$Filter <- trip::speedfilter(tr, max.speed = 90)
        ## REMOVE FILTERED COORDINATES
        x1 <- subset(x1, x1$Filter==TRUE)
        x1$hours <- NULL
        x1$Filter <- NULL
        
        ## COMBINE PTT FILTERED FILE BACK INTO DF FILE (with GLS and GPS data)
        df <- rbind(x1,x3)
        
      }
      
      #if the device is GLS, filter the equinox if needed
      if("GLS" %in% devices){  
        
        df$year <- substr(df$dtime,1,4)
        years <- unique(df$year)
        
        i <- 1
        for(i in 1:length(years)){
          
          yr <- subset(df, df$year == years[i])
          
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
        #export plots
        
        png(filename = paste0("outputs/01_cleaning_data/equinox_filtered/", df$scientific_name[1],"_",site,
                              "_",colony,"_",dataset_number,".png"))
        par(mfrow=c(2,1),mar = c(3, 4, 1, 1))
        plot(latitude~longitude, data=df, type="n", asp=1, main="", 
             frame = T, xlab="", ylab=paste(df$scientific_name[1]))
        plot(land, col='lightgrey', add=T)
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
      
      ############### REMOVE COLONY POSITIONs ####
      
      ## Exclude all locations within 5 km (GPS) or 15 km (PTT) of the colony, 
      #and retain all the rest. Colony here is a single lat/lon position.
      
      #create a bespoke equal areas projections
      mean_loc <- geosphere::geomean(cbind(df$longitude,df$latitude))
      DgProj <- sp::CRS(paste0("+proj=laea +lon_0=",mean_loc[1],
                               " +lat_0=",mean_loc[2]))
      
      sp::coordinates(df) <- ~longitude+latitude
      sp::proj4string(df) <- sp::CRS(sp::proj4string(land))
      x7 <- sp::spTransform(df, DgProj)
      df <- as.data.frame(x7)
      
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
      
      i <- 1
      for(i in 1:nlevels(col)){
        
        print(col[i])
        sub_col <- x7[x7$colony_name==col[i],]
        
        ## removing land overlap at colony
        sub_col@data$device <- factor(sub_col@data$device)
        dev <- unique(sub_col@data$device)
        
        for (j in 1:nlevels(dev)){
          print(dev[j])
          a <- sub_col[sub_col$device==dev[j],]
          
          if (a$device[1]=="GPS"){
            
            if(col[i] != "At-Sea"){
              df_col <- data.frame(cbind(lon=sub_col$lon_colony[1], lat=sub_col$lat_colony[1]))
              sp::coordinates(df_col) <- ~lon+lat
              ## assigning projection
              sp::proj4string(df_col) <- sp::CRS(sp::proj4string(land))
              ## changing projection
              col_laea <- sp::spTransform(df_col, DgProj)
              ## buffers
              buf_small <- rgeos::gBuffer(col_laea, width = 5000)
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
              sp::coordinates(df_col) <- ~lon+lat
              ## assigning projection
              sp::proj4string(df_col) <- sp::CRS(sp::proj4string(land))
              ## changing projection
              col_laea <- sp::spTransform(df_col, DgProj)
              ## buffers
              buf_large <- rgeos::gBuffer(col_laea, width = 15000)
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
      sp::coordinates(df_all) <- ~longitude+latitude
      sp::proj4string(df_all) <- DgProj
      ## changing projection
      df_all_wgs <- sp::spTransform(df_all, (sp::proj4string(land)))
      df_all_wgs <- as.data.frame(df_all_wgs)
      df <- df_all_wgs
      df$original_track_id <- NULL
      df$equinox <- NA
      
      ############ EXPORT RESULTS #########
      
      df$date_gmt <- NULL
      df$time_gmt <- NULL
      df$incomplete <- NULL
      df$track_time <- NULL
      
      write.csv(df, paste0(dir,  
                           df$scientific_name[1],"_",site,"_",colony,"_",
                           dataset_number,".csv"), 
                row.names = FALSE)
      
      ## PLOTTING TO CHECK RESULTS
      png(filename = paste0(dir, "maps/", df$scientific_name[1],"_",site,
                            "_",colony,"_",dataset_number,".png"))
      plot(latitude~longitude, data=df, type="n", asp=1, 
           xlim=c(-180,180), ylim=c(-90,90), 
           main="", frame = T, xlab="", ylab="")
      plot(land, col='lightgrey', add=T)
      points(latitude~longitude, data=df, pch=16, cex=0.5, col="green")
      points(lat_colony~lon_colony, data=df, pch=18, cex=1, col="purple")
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
  
  #check skipped files
  print(skipped_age)
  print(skipped_species)
}

#check skipped files
skipped_age
skipped_species

