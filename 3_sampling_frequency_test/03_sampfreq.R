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

######### GENERAL DIRECTIONS AND FILES ##############

################# LOADING SPP DATA ##################
data_std <- "outputs/02_pops/"
files <- list.files(data_std);files

sampling_freq_record <- as.data.frame(files)
sampling_freq_record$freq <- NA

sampling_freq_record$devices <- NA

#issues with: 
#Linosa Scopoli's 30, 530039
#Calonectris leucomelas_Korea.csv 37, 70517.5
#Hydrobates pelagicus_Shetland.csv 67, 57630
#Puffinus mauretanicus_Balearic Archipelago.csv 139, 173116

dataset_number <- 15
for(dataset_number in 1:length(files)){ #
  print(dataset_number)
  print(files[dataset_number])
  name_png <- stringr::str_remove(files[dataset_number],".csv")
  df <- read.csv(paste0(data_std,files[dataset_number]))  
  
  head(df)
  
  # Regularisation using linear interpolation for PTT and GPS data #################  
  devices <- unique(df$device);devices
  sampling_freq_record$devices[dataset_number] <- paste(devices,collapse = "_")
  
  if(!("GLS" %in% devices)){
    
    #figure out sampling frequency
    df$data_bird_track <- paste(df$dataset_id, df$bird_track,sep="_")
    bird_tracks <- unique(df$data_bird_track)
    samp_freq <- as.data.frame(bird_tracks)
    samp_freq$samp_freq <- NA
    
    j <- 1
    for(j in 1:length(bird_tracks)){
      bird_track <- subset(df,data_bird_track == bird_tracks[j])
      head(bird_track)
      
      bird_track$time_stamp <- as.POSIXct(bird_track$dtime,
                                          format = "%Y-%m-%d %H:%M:%S",
                                          tz = "GMT")
      
      bird_track <- bird_track[order(bird_track$time_stamp), ]
      
      if(nrow(bird_track) > 1){
      bird_track$time_stamp2 <- c(bird_track$time_stamp[2:nrow(bird_track)], NA)
      bird_track$difftime <- as.numeric(difftime(bird_track$time_stamp2,bird_track$time_stamp, units = "secs"))
      samp_freq$samp_freq[j] <- median(bird_track$difftime,na.rm =T)
      } 

    } 
    
    #set regularisation to the max median sampling frequency
    regularisation_freq <- max(samp_freq$samp_freq, na.rm = T)
    sampling_freq_record$freq[dataset_number] <- regularisation_freq
  
    
  } else {
    sampling_freq_record$freq[dataset_number] <- "GLS"
  }
 print(dataset_number)
}


write.csv(sampling_freq_record,"outputs/03_sampling_freq_record.csv",
          row.names = F)



