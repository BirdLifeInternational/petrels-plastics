#Import plastics data, average between three models
#Convert into an Atlantic-centred plastic density raster
#Win Cowger & Beth Clark 2021
rm(list=ls()) 

# Load packages and data ####
library(raster)
library(rgdal)
library(dplyr)
library(RColorBrewer)

sessionInfo()
#R version 4.1.2 (2021-11-01)
#Platform: x86_64-w64-mingw32/x64 (64-bit)
#Running under: Windows 10 x64 (build 19045)

#Matrix products: default

#locale:
#[1] LC_COLLATE=English_United Kingdom.1252 
#[2] LC_CTYPE=English_United Kingdom.1252   
#[3] LC_MONETARY=English_United Kingdom.1252
#[4] LC_NUMERIC=C                           
#[5] LC_TIME=English_United Kingdom.1252    

#attached base packages:
#[1] stats     graphics  grDevices utils     datasets  methods  
#[7] base     

#other attached packages:
# [1] RColorBrewer_1.1-2 dplyr_1.0.8        rgdal_1.4-8       
#[4] raster_3.1-5       sp_1.5-0          

#loaded via a namespace (and not attached):
#[1] Rcpp_1.0.8       rstudioapi_0.13  magrittr_2.0.2   tidyselect_1.1.2
#[5] lattice_0.20-45  R6_2.5.1         rlang_1.0.6      fansi_1.0.2     
#[9] tools_4.1.2      grid_4.1.2       utf8_1.2.2       cli_3.3.0       
#[13] DBI_1.1.2        ellipsis_0.3.2   assertthat_0.2.1 tibble_3.1.6    
#[17] lifecycle_1.0.3  crayon_1.5.0     purrr_0.3.4      vctrs_0.3.8     
#[21] codetools_0.2-18 glue_1.6.2       compiler_4.1.2   pillar_1.7.0    
#[25] generics_0.1.2   pkgconfig_2.0.3 

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

#Read in land file for visualisation:
#Natural Earth land 1:10m polygons version 5.1.1 
#downloaded from www.naturalearthdata.com/
land <- rgdal::readOGR(dsn = "input_data/baselayer", layer = "ne_10m_land") 

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

cols_nmods <- c("#F2F3F400",yelblus[3:5])


plot(sum01_r,col=cols_nmods,breaks = c(-1:3))
plot(land, col="grey75", add=T)

png("outputs/00_plastics_model_coverage.png", 
    width=1379,height=750)
plot(sum01_r, col=cols_nmods, breaks = c(-1:3))
plot(land, col="grey75", add=T)
dev.off()

