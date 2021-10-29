#Plastics provided raster was originally Pacific centred
#but we wanted to plot altantic centred. the original
#edges of the raster have strange and missing values
#so this script takes the mean of the 3 columns either side
#of the problem column and replaces it
#Beth Clark 18/3/2021

rm(list=ls()) 
################ LOADING PACKAGES ###################

library(raster)
library(rgdal)
library(viridis)

dir <- paste0("C:/Users/bethany.clark/OneDrive - BirdLife International/",
              "Methods") ## copy and paste here your working directory


#read in plastics data
plastics <- raster("C:/Users/bethany.clark/OneDrive - BirdLife International/Data/AverageForBeth2.tif")


land <- readOGR(dsn=paste0(dir,"/baselayer"), layer = "world-dissolved") #Changed - BC  

land_ee <- land
crs(land_ee)

#The unedtited version is saved
#"C:/Users/bethany.clark/OneDrive - BirdLife International/Data/Unedited_plastic_raster/AverageForBeth2.tif"
#The alternative version is:
#plastics <- raster("C:/Users/bethany.clark/OneDrive - BirdLife International/Data/VanSebForBeth.tif")
plot(log(plastics))

m <- plastics

plot(log(m))

r178 <- m[cellFromCol(m,178)]
cols <- as.data.frame(r178)
cols$r179 <- m[cellFromCol(m,179)]
cols$r180 <- m[cellFromCol(m,180)]

cols$r182 <- m[cellFromCol(m,182)]
cols$r183 <- m[cellFromCol(m,183)]
cols$r184 <- m[cellFromCol(m,184)]

cols$mean <- rowMeans(cols,na.rm = T)

cols$mean <- ifelse(cols$mean == "NaN",NA,cols$mean)
cols$mean <- ifelse(is.na(cols$r180) & is.na(cols$r182),NA,cols$mean)

m[cellFromCol(m,181)] <- cols$mean

plot(log(m))

redblus <- c("#e6f5ff","#c20000","#a80000")
redblus <- c("#e6f5ff","#a80000")
redblus <- c("#fff7f7","#c20000")
cols <- colorRampPalette(redblus)(10000000)



cols_inf <- rev(inferno(20),type="B")
colsviri <- colorRampPalette(c(colviri))(255)

cols <- rev(inferno(200))
#cols <- rev(magma(200))

plot((m),col=cols)

#save the plot

png(paste0(dir,"/scripts_results/sqrt_plastics.png"), width=2000,height=1125)
par(mfrow=c(1,1))
plot(sqrt(m),col=cols)
plot(land,col="#dbdbdb", 
     border = "#c7c7c7", add=T)
dev.off()
dev.off()

#save the raster

writeRaster(m, filename="C:/Users/bethany.clark/OneDrive - BirdLife International/Data/AverageForBeth2.tif",
            format="GTiff", overwrite=TRUE)


png(paste0(dir,"/scripts_results/plastics.png"), width=2000,height=1125)
par(mfrow=c(1,1))
plot((m),col=cols)
plot(land,col="#dbdbdb", 
     border = "#c7c7c7", add=T)
dev.off()
dev.off()


cap <- m
cap@data@values <- ifelse(m@data@values > 
                            max(m@data@values,na.rm=T)/10, 
                          max(m@data@values,na.rm=T)/10,
                          m@data@values)

png(paste0(dir,"/scripts_results/plastics_cap10percent.png"), width=2000,height=1125)
par(mfrow=c(1,1))
plot((cap),col=cols)
plot(land,col="#dbdbdb", 
     border = "#c7c7c7", add=T)
dev.off()
dev.off()




