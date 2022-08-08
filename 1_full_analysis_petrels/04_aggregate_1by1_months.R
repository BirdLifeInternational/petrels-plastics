## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Mapping the global distribution of seabird populations
## R script to aggregate results into a 5x5 degree grid
## Ana Carneiro and Anne-Sophie Bonnet-Lebrun
## July 2018
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#Adapted by Beth Clark Mar 2020 for overlap with 1x1 degree plastics data


# This code uses as input a 10 km x 10 km raster containing the predicted number of birds in each cell. This raster is 
# first projected into WGS84 (longitude and latitude), with a resolution roughly equivalent to the initial 10 km2 resolution, 
# i.e. 1x1 degrees. Since degree cells at high latitudes do not have the same area as cells at low latitudes, 
# a spatially heterogeneous rescaling is necessary to avoid distorting the values (number of predicted birds per cell). 
# That rescaling is done by dividing the value of the cell by the area of the original cell (10 km2) and multiplying it by the 
# area of the new cell (which depends on the latitude of the cell). 
# 
# To calculate the area of each grid cell, we consider a spherical model of the Earth. The radius of this sphere is the Earth's authalic radius (R = 6371007.2m).
# The curved surface area of a grid cell spanning longitudes lon0 to lon1 (lon1 > lon0) and latitudes lat0 to lat1 (lat1 > lat0) is:
# A = (lon1 - lon0)*(sin(lat1) - sin(lat0)*R^2
# (see answer 2 - https://gis.stackexchange.com/questions/29734/how-to-calculate-area-of-1-x-1-degree-cells-in-a-raster) 
# So for a grid of resolution res_lon and res_lat, a cell centred on lon and lat (all latitudes and resolutions in radians) has an area of:
# A = (lon + res_lon/2 - (lon - res_lon/2))*(sin(lat + res_lat/2) - sin(lat - res_lat/2))*R^2 = res_lon*(sin(lat + res_lat/2) - sin(lat - res_lat/2))

rm(list=ls()) 
################ LOADING PACKAGES ###################

library(raster)
library(rgdal)
library(cowplot)
library(stringr)
library(RColorBrewer)
library(viridis)

sessionInfo()
#R version 4.1.2 (2021-11-01)
#Platform: x86_64-w64-mingw32/x64 (64-bit)
#Running under: Windows 10 x64 (build 19044)
#
#Matrix products: default
#
#locale:
#  [1] LC_COLLATE=English_United Kingdom.1252  LC_CTYPE=English_United Kingdom.1252    LC_MONETARY=English_United Kingdom.1252
#  [4] LC_NUMERIC=C                            LC_TIME=English_United Kingdom.1252    

#attached base packages:
#  [1] stats     graphics  grDevices utils     datasets  methods   base     

#other attached packages:
#  [1] viridis_0.6.2      viridisLite_0.4.0  RColorBrewer_1.1-2 stringr_1.4.0      cowplot_1.1.1      rgdal_1.4-8        raster_3.1-5      
#  [8] sp_1.3-2          

#loaded via a namespace (and not attached):
#  [1] Rcpp_1.0.8       pillar_1.7.0     compiler_4.1.2   tools_4.1.2      lifecycle_1.0.1  tibble_3.1.6     gtable_0.3.0     lattice_0.20-45 
#  [9] pkgconfig_2.0.3  rlang_1.0.1      cli_3.3.0        DBI_1.1.2        gridExtra_2.3    dplyr_1.0.8      generics_0.1.2   vctrs_0.3.8     
#  [17] grid_4.1.2       tidyselect_1.1.2 glue_1.6.2       R6_2.5.1         fansi_1.0.2      ggplot2_3.3.5    purrr_0.3.4      magrittr_2.0.2  
#  [25] scales_1.1.1     codetools_0.2-18 ellipsis_0.3.2   assertthat_0.2.1 colorspace_2.0-3 utf8_1.2.2       stringi_1.7.6    munsell_0.5.0   
#  [33] crayon_1.5.0    

######### GENERAL DIRECTIONS AND FILES ##############

## check we're in the home proj folder "1_full_analysis_petrels"
getwd()

land <- rgdal::readOGR(dsn="input_data/baselayer", layer = "world-dissolved") 

dir_demClasses <- "outputs/03_kernels"

## DIRECTION TO YOUR RESULTS
dir_1by1 <- "outputs/04_aggregate_1by1_grid"

dir.create(dir_1by1) 
dir.create(paste0(dir_1by1,"/maps/"))
dir.create(paste0(dir_1by1,"/na_maps/")) 

####### CONVERT INTO A 1X1 DEGREE RESOLUTION ########

#read in plastics data
plastics <- raster::raster("outputs/00_PlasticsRaster.tif")

plot(log(plastics))

## resolution and projection
r <- raster()
r

yelblus <- c(brewer.pal(n = 9, name = "YlGnBu"),"#00172e")
cols <- colorRampPalette(yelblus)(255)
colsviri <- cols[20:255]

colsinf <- rev(inferno(200))

## rescale to 1
plastics2 <- plastics
plastics2[is.na(plastics2)] <- 0 
p_sum1    <- plastics2/sum(raster::getValues(plastics2))
p_sum1[is.na(plastics)] <- NA

res(p_sum1)
plot(p_sum1)

RES <- res(plastics) # the resolution of the raster (in degrees)
# res_lon = RES[1]*pi/180 (in radians) and res_lat = RES[2]*pi/180 (in radians)
R <- 6371007.2 # the Earth's authalic radius (in meters)
lat <- raster::yFromRow(plastics, 1:nrow(plastics)) # latitude of the centroid of each cell (in degrees, need to be converted in radians)
area <- (sin(pi/180*(lat + RES[2]/2)) - sin(pi/180*(lat - RES[2]/2))) * (RES[1] * pi/180) * R^2
r_area <- raster::setValues(plastics, rep(area, each=ncol(plastics))) # gives the area of each grid cell in meters 
plot(r_area, col=colsviri)

files <- list.files(dir_demClasses, full.names = TRUE,pattern="tif"); files

dat <- data.frame()
result <- c()

nas <- as.data.frame(files)
nas$name <- NA
nas$vals <- NA
nas$nas <- NA
nas$files <- NULL


for (i in 1:length(files)){#

  a <- raster(files[i])
  name <- str_remove(str_remove(a@data@names[1],"_sum"),"_GLS_adult")
  nas$name[i] <- name
  
  a[is.na(a)] <- 0 
  
  b <- sum(raster::getValues(a)) 
  
  ## reprojecting and rescaling
  a_proj <- raster::projectRaster(a, plastics, method = "bilinear")
  a_proj2 <- a_proj * r_area / 100000000 # rescaling the values in each cell
  a_proj2[is.na(a_proj2)] <- 0 
  c <- sum(raster::getValues(a_proj2)) 
  
  
  #find number of NAs for plastics where birds are
  na_a_proj2 <- a_proj2
  na_a_proj2[na_a_proj2 > 0] <- 1
  na_a_proj2[is.na(na_a_proj2)] <- 0
  nas$vals[i] <- sum(na_a_proj2@data@values)
  
  na_p_sum1 <- p_sum1
  na_p_sum1[is.na(na_p_sum1)] <- 1
  na_p_sum1[na_p_sum1 != 1] <- 0
  na_over <- na_a_proj2 * na_p_sum1
  nas$nas[i] <- sum(na_over@data@values) 
  
  a_proj2[is.na(plastics)] <- NA
  
  ## exporting results
  raster_name_1 <- gsub(dir_demClasses, "", files[i])
  print(raster_name_1) 
  
  raster_name_2 <- paste0(dir_1by1, raster_name_1)

  raster::writeRaster(a_proj2, filename=raster_name_2, format="GTiff", overwrite=TRUE)
  
  over <- a_proj2 * p_sum1
      
  over_score <- over
  summary(over_score@data@values)
  over_score[is.na(over_score)] <- 0
  exposure_score <- round(sum(raster::getValues(over_score))*1000000,4)
  
  png(paste0(dir_1by1,"/maps/",name,".png"), width=1399,height=455)
  par(mfrow=c(1,2))
  plot(a_proj2,main=paste0(name," distribution"),col=colsviri,legend=F)
  plot(over,main=paste0("Exposure score = ",exposure_score),
       col=colsinf,legend=F)
  
  dev.off()
  
  if(sum(na_over@data@values)> 0){
  png(paste0(dir_1by1,"/na_maps/",name,".png"), width=1399,height=455)
  par(mfrow=c(1,2))
  plot(a_proj2,main=name,col=colsviri,legend=F)
  plot(na_over,main=paste0("Exposure Nas = ",sum(na_over@data@values)),
       col=colsinf,legend=F)
  dev.off()
  }
  
  name_split <- strsplit(name,"_")[[1]]
  month <- name_split[length(name_split)]
  species <- paste(name_split[1],name_split[2],sep=" ")
  population <- paste(c(name_split[4:length(name_split)-1]),collapse = " ")
  sp_pop <- paste(species,population,sep="_")
  sp_pop_month <- paste(species,population,month,sep="_")
  
  ## check results (checking that total number of predicted birds is unaffected by the projection and aggregation)
  check <- cbind(b,c)
  result <- cbind(species,population,month,sp_pop,sp_pop_month,exposure_score)
  dat <- rbind(dat, as.data.frame(result))
  print(i)
}

write.csv(dat, "outputs/04_exposure_scores_by_month.csv",
          row.names = F)  
head(nas)
nas$percent_na <- nas$nas/nas$vals*100

#2 don't exist
nas_no_na <- subset(nas,vals > 0)
mean(nas_no_na$percent_na)
#1.86% nas 11/4/2021




