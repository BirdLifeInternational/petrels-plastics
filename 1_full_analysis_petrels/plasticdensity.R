setwd("G:/My Drive/GrayLab/Projects/Plastics/ActiveProjects/OceanModeling/OceanTimeTrend/Data/Processed Data")
library(sp)  # classes for spatial data
library(raster)  # grids, rasters
library(rasterVis)  # raster visualisation
library(maptools)
library(rgeos)
library(dismo)
library(rgdal)
library(mgcv)
library(ggplot2)
library(dplyr)
library(leaflet)
library(viridis)
library(gridExtra)
library(purrr)
library(tidyr)
library(magick)
library(plotly)

#Functions----

BootMedian <- function(data) {
  B <- 10000
  ave <- numeric(B)
  n = length(data)
  
  set.seed(34345)
  for (i in 1:B) {
    boot <- sample(1:n, size=n, replace = TRUE)
    ave[i] <- mean(data[boot])
  }
  return(quantile(ave, c(0.025, 0.5, 0.975), na.rm = T))
}

getmode <- function(v) {
  uniqv <- unique(v[!is.nan(v)])
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

getmean <- function(v) {
  mean(v, na.rm = T)
}

getgeomean <- function(x) {
  10^mean(log10(x + 1))
}


#Data to read in ----
Lebreton <- as.matrix(read.csv("ModelGrid/lebretonmodel_abundance.csv", header = F))
Maximenko <- as.matrix(read.csv("ModelGrid/maximenkomodel_abundance.csv", header = F))
VanSeb <- as.matrix(read.csv("ModelGrid/vansebillemodel_abundance.csv", header = F))

#Data Cleanup ----
df <- data.frame(van = as.vector(VanSeb), max = as.vector(Maximenko), leb = as.vector(Lebreton))
Average <- 10^rowMeans(mutate_all(df, function(x) log10(x+1)) ,na.rm = T)

dim(Average) <- c(181, 361)

Ave <- raster(Average, xmn = 1, xmx= 361, ymn=-90, ymx=90, crs="+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
Van <- raster(VanSeb, xmn = 1, xmx= 361, ymn=-90, ymx=90, crs="+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")

Avebeth <- shift(rotate(raster(Average, xmn = 0, xmx= 360, ymn=-90, ymx=90, crs="+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")), dx=0.5)
Vanbeth <- shift(rotate(raster(VanSeb, xmn = 0, xmx= 360, ymn=-90, ymx=90, crs="+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")), dx = 0.5)

plot(Avebeth)
extent(Avebeth)

writeRaster(Avebeth, "AverageForBeth.tif", overwrite = T)
writeRaster(Vanbeth, "VanSebForBeth.tif", overwrite = T)