#Import plaster raster and convert into an Atlantic-centred plastic density raster
#Beth Clark 18/3/2021

rm(list=ls()) 

# Load packages and data ####

library(raster)
library(rgdal)

## paste home directory here
dir <- paste0("C:/Users/bethany.clark/OneDrive - BirdLife International/",
              "Methods") 

#read in plastics data
plastics_raw <- raster(paste0(dir,"/input_data/PlasticsRaster.tif"))

plot((plastics_raw))

plastics <- plastics_raw

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

plot(plastics)

#save the raster
writeRaster(plastics, filename="C:/Users/bethany.clark/OneDrive - BirdLife International/Data/PlasticRasterAltantic.tif",
            format="GTiff", overwrite=TRUE)
