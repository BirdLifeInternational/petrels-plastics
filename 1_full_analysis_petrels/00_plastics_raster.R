#Import plastics data, average between three models
#Convert into an Atlantic-centred plastic density raster
#Win Cowger & Beth Clark 2021
rm(list=ls()) 

# Load packages and data ####
library(raster)
library(rgdal)
library(dplyr)
library(RColorBrewer)

## specify/create directories
dir.create("outputs/")

#Data to read in ----
Lebreton <- as.matrix(read.csv("input_data/plastics_data/lebretonmodel_abundance.csv", header = F))
Maximenko <- as.matrix(read.csv("input_data/plastics_data/maximenkomodel_abundance.csv", header = F))
VanSeb <- as.matrix(read.csv("input_data/plastics_data/vansebillemodel_abundance.csv", header = F))


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
plot(log(Ave))
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
raster_name <- "outputs/00_PlasticsRaster.tif"
writeRaster(plastics, filename = raster_name,
            format="GTiff", overwrite=TRUE)

#Plot difference in coverage between the three models ####
land <- rgdal::readOGR(dsn = "input_data/baselayer", layer = "world-dissolved") 

VanSeb_01 <- ifelse(VanSeb>0,1,0)
VanSeb_01 <- ifelse(is.na(VanSeb_01),0,VanSeb_01)

Lebreton_01 <- ifelse(Lebreton>0,1,0)
Lebreton_01 <- ifelse(is.na(Lebreton_01),0,Lebreton_01)

Maximenko_01 <- ifelse(Maximenko>0,1,0)
Maximenko_01 <- ifelse(is.na(Maximenko_01),0,Maximenko_01)

sum01 <- VanSeb_01+Lebreton_01+Maximenko_01
sum01_r <- shift(rotate(raster(sum01, 
                                xmn = 0, xmx= 360, ymn=-90, ymx=90, 
                                crs="+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")), 
                                dx=0.5)

yelblus <- c(brewer.pal(n = 5, name = "YlGnBu"),"#00172e")

plot(sum01_r,col=c("white",yelblus[3:5]),breaks = c(-1:3))
plot(land, col="grey75", add=T)

png("outputs/00_plastics_model_coverage.png", 
    width=1379,height=750)
plot(sum01_r,col=c("white",yelblus[3:5]),breaks = c(-1:3))
plot(land, col="grey75", add=T)
dev.off()


