#Import plastics data, average between three models
#Convert into an Atlantic-centred plastic density raster
#Win Cowger & Beth Clark 2021
rm(list=ls()) 

# Load packages and data ####
library(raster)
library(rgdal)
library(dplyr)

## paste home directory here
dir <- "C:/Users/bethany.clark/OneDrive - BirdLife International/Methods"
dir_input <- paste0(dir,"/input_data/plastics_data/")
dir_output <- paste0(dir,"/outputs/")

#Data to read in ----
Lebreton <- as.matrix(read.csv(paste0(dir_input,"lebretonmodel_abundance.csv"), header = F))
Maximenko <- as.matrix(read.csv(paste0(dir_input,"maximenkomodel_abundance.csv"), header = F))
VanSeb <- as.matrix(read.csv(paste0(dir_input,"vansebillemodel_abundance.csv"), header = F))

#Data Cleanup ----
df <- data.frame(van = as.vector(VanSeb), max = as.vector(Maximenko), leb = as.vector(Lebreton))

#Geomean
Average <- 10^rowMeans(mutate_all(df, function(x) log10(x+1)) ,na.rm = T)

dim(Average) <- c(181, 361)

Ave <- raster(Average, xmn = 1, xmx= 361, ymn=-90, ymx=90, 
              crs="+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")

AveRaster <- shift(rotate(raster(Average, 
                                 xmn = 0, xmx= 360, ymn=-90, ymx=90, 
                                 crs="+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")), dx=0.5)

plot(log(AveRaster))
#After rotating to atlantic-centred, there is 
#a column in the centre with incorrect values
#corrected below

plastics <- AveRaster

r178 <- plastics[cellFromCol(plastics,178)]
cols <- as.data.frame(r178)
cols$r179 <- plastics[cellFromCol(plastics,179)]
cols$r180 <- plastics[cellFromCol(plastics,180)]

cols$r182 <- plastics[cellFromCol(plastics,182)]
cols$r183 <- plastics[cellFromCol(plastics,183)]
cols$r184 <- plastics[cellFromCol(plastics,184)]

cols$mean <- rowMeans(cols,na.rm = T)

cols$mean <- ifelse(cols$mean == "NaN",NA,cols$mean)
cols$mean <- ifelse(is.na(cols$r180) & is.na(cols$r182),NA,cols$mean)

plastics[cellFromCol(plastics,181)] <- cols$mean

plot(log(plastics))

#save the raster
raster_name <- paste0(dir_output,"00_PlasticsRaster.tif")
writeRaster(plastics, filename = raster_name,
            format="GTiff", overwrite=TRUE)
