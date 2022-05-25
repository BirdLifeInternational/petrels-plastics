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
library(viridis)
library(stringr)
library(RColorBrewer)

######### GENERAL DIRECTIONS AND FILES ##############

## GENERAL DIR
dir <- paste0("C:/Users/bethany.clark/OneDrive - BirdLife International/",
              "Methods") ## copy and paste here your working directory

## DIRECTION TO YOUR RASTERS (ALL DEM CLASSES COMBINED AND BY YEAR QUARTER)
land <- readOGR(dsn=paste0(dir,"/baselayer"), layer = "world-dissolved") #Changed - BC  

dir_demClasses <- paste0(dir,"/scripts_results/03_kernels")

## DIRECTION TO YOUR RESULTS
dir.create(paste0(dir,"/scripts_results/04_aggregate_1by1_grid_br_p2/")) 
dir.create(paste0(dir,"/scripts_results/04_aggregate_1by1_grid_br_p2/maps/")) #added - BC
dir.create(paste0(dir,"/scripts_results/04_aggregate_1by1_grid_br_p2/maps_pdf/")) #added - BC
#dir.create(paste0(dir,"/scripts_results/04_aggregate_1by1_grid_br_mean/")) 
#dir.create(paste0(dir,"/scripts_results/04_aggregate_1by1_grid_br_mean/maps/")) #added - BC
dir_1by1 <- paste0(dir,"/scripts_results/04_aggregate_1by1_grid_br_p2")
#dir_1by1m <- paste0(dir,"/scripts_results/04_aggregate_1by1_grid_br_mean")

####### CONVERT INTO A 1X1 DEGREE RESOLUTION ########

#read in plastics data
#m <- raster("C:/Users/bethany.clark/OneDrive - BirdLife International/Data/VanSebForBeth2.tif")
m <- raster("C:/Users/bethany.clark/OneDrive - BirdLife International/Data/AverageForBeth2.tif")
plot(log(m),col=rev(viridis(255)))

m

yelblus <- c(brewer.pal(n = 9, name = "YlGnBu"),"#00172e")
cols <- colorRampPalette(yelblus)(255)
colsviri <- cols[20:255]

plot(sqrt(m),col=(colsviri))

## rescale to 1
m2 <- m
m2[is.na(m2)] <- 0 
p_sum1    <- m2/sum(getValues(m2))
p_sum1[is.na(m)] <- NA

res(p_sum1)

#plot(p_sum1, main = "million plastic piece per 10km2", col=colsviri)

RES <- res(m) # the resolution of the raster (in degrees)
# res_lon = RES[1]*pi/180 (in radians) and res_lat = RES[2]*pi/180 (in radians)
R <- 6371007.2 # the Earth's authalic radius (in meters)
lat <- yFromRow(m, 1:nrow(m)) # latitude of the centroid of each cell (in degrees, need to be converted in radians)
area <- (sin(pi/180*(lat + RES[2]/2)) - sin(pi/180*(lat - RES[2]/2))) * (RES[1] * pi/180) * R^2
r_area <- setValues(m, rep(area, each=ncol(m))) # gives the area of each grid cell in meters 
plot(r_area, col=colsviri)

pops <- read.csv(paste0(dir,"/scripts_results/05_phenology/pops.csv"))

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

names <- read.csv("C:/Users/bethany.clark/OneDrive - BirdLife International/Requests/Species_list_petrels2.csv")
head(names)

pops$common_name <- names$Common.name[match(pops$species,names$Scientific.name)]
head(pops)
cols_inferno <- rev(inferno(20))
cols_inf <- colorRampPalette(c(cols_inferno))(255)

all_pops <- read.csv(paste0(dir,"/scripts_results/all_locations2.csv"))
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
  
  br <- strsplit(pops$breeding[i],"_")[[1]]
  nonbr <- strsplit(pops$nonbreeding[i],"_")[[1]]
  
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
  
  if(is.na(pops$nonbreeding[i])){
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
  png(paste0(dir_1by1,"/maps2/",pop,"_br_sum.png"), width=1400,height=760)
  par(mfrow=c(1,1))
  plot(over,main=paste0(pops$common_name[i]," ",pops$species[i],", ",pops$pop[i],
                        "\nBreeding season plastic exposure score = ",
                        round(over_val_br,2)),
       cex.main=2,
       col=cols_inf,legend=F)
  plot(land,col="grey75", 
       border = NA, add=T)
  points(colloc$lon_colony,colloc$lat_colony,pch=18,
         cex=2.8,col="#004fd9")
  dev.off()

  
  pops$over_val_br[i] <- over_val_br
  
  #nonbreeding season overlap
  if(is.na(pops$nonbreeding[i])){
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
      png(paste0(dir_1by1,"/maps2/",pop,"_nonbr_sum.png"), width=1400,height=760)
      #pdf(paste0(dir_1by1,"/maps_pdf/",pop,"_nonbr.pdf"),
      #    paper = "a4r",width = 0, height = 0)
      par(mfrow=c(1,1))
      plot(over,main=paste0(pops$common_name[i]," ",pops$species[i],", ",pops$pop[i],
                            "\nNon-breeding season plastic exposure score = ",
                            round(over_val_br,2)),
           cex.main=2,
           col=cols_inf,legend=F)
      plot(land,col="grey75", 
           border = NA, add=T)
      points(colloc$lon_colony,colloc$lat_colony,pch=18,
             cex=2.5,col="#004fd9")
      dev.off()
      
      pops$over_val_nonbr[i] <- over_val_nonbr
      
      

    }
  
  print(i)

}

pops$mean_br <- pops$over_val_br / pops$br_n
pops$mean_nonr <- pops$over_val_nonbr / pops$nonbr_n



write.csv(pops, paste0(dir_1by1, "/results_rasters_br_p2_v2.csv"),
          row.names = F)  
