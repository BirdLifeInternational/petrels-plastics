#Combine cleaned tracking datasets into populations
#filter out errors for specific datasets
#calculate sample sizes

rm(list=ls())
library(tidyverse)

## set up folders
dir.create("outputs/02_pops/")

################# LOADING SPP DATA ##################
data_std <- "outputs/01_cleaning_data/"
files <- list.files(data_std,pattern = ".csv");files

files_list <- as.data.frame(files)
files_list$species <- NA
files_list$site <- NA
files_list$colony <- NA

for(i in 1:nrow(files_list)){
  
  name <- strsplit(files[i],"_")
  files_list$species[i] <- name[[1]][1]
  files_list$site[i] <- name[[1]][2]
  files_list$colony[i] <- name[[1]][3]
  
}

#Rename sites and populations with shorter names that do not contain
#special characters & group together populations
files_list$pop <- files_list$site
files_list$pop <- ifelse(files_list$pop == "Reunion Island", "Reunion",files_list$pop)
files_list$pop <- ifelse(files_list$pop == "Columbretes", "Balearic Archipelago",files_list$pop)
files_list$pop <- ifelse(files_list$pop %in% c("Filfla","Gozo"), "Malta",files_list$pop)
files_list$pop <- ifelse(files_list$site == "Castle Harbour", "Bermuda Island",files_list$pop)
files_list$pop <- ifelse(files_list$pop == "Canary Islands", "Canarias",files_list$pop)
files_list$pop <- ifelse(files_list$pop == "Gough Island", "Gough",files_list$pop)
files_list$pop <- ifelse(files_list$pop == "South Georgia (Islas Georgias del Sur)", "South Georgia",files_list$pop)
files_list$pop <- ifelse(files_list$pop == "Egg Island", "St Helena",files_list$pop)
files_list$pop <- ifelse(files_list$pop == "Chatham Island", "Chatham Islands",files_list$pop)
files_list$pop <- ifelse(files_list$pop %in% c("Gull Island","Baccalieu Island"), "Newfoundland",files_list$pop)
files_list$pop <- ifelse(files_list$pop %in% c("Iwate","Sanriku Coast","Sanriku"), "Iwate",files_list$pop)
files_list$pop <- ifelse(files_list$pop %in% c("Raso Islet","Sal","Boa Vista","Ilhéus do Rombo","Santiago",
                                               "Santo Antão","Fogo"), "Cape Verde",files_list$pop)

large_sites <- c("Portugal","Australia","New Zealand","New South Wales","Iceland","Auckland",
                 "Western Australia","Izu Shoto","Poor Knights Group")
#check:
#combine isla mocha and juan fernadez
#gough & tristan?
#bon portgae, eastern shore and country island = nova scotia

#kauwahaia == bethell's beach a grisea

files_list$pop <- ifelse(files_list$pop %in% large_sites, files_list$colony,files_list$pop)
files_list$pop <- ifelse(files_list$pop == "Lowendal Islands & Houtman Abrolhos", "Lowendal and Houtman Abrolhoss",files_list$pop)
files_list$pop <- ifelse(files_list$pop %in% c("Mokohinau","Tiritiri Matangi"), "E of Aukland",files_list$pop)

files_list$pop <- ifelse(files_list$species == "Procellaria westlandica", "Punakaiki",files_list$pop)
files_list$pop <- ifelse(files_list$pop == "Kauwahaia Island", "Bethells Beach",files_list$pop)

files_list$pop <- ifelse(files_list$pop %in% c("Bon Portage","Country Island","Eastern Shore Islands"), "Nova Scotia",files_list$pop)
files_list$pop <- ifelse(files_list$pop == "Kauwahaia Island", "Bethells Beach",files_list$pop)

files_list$pop <- ifelse(files_list$pop %in% c("Juan Fernandez Islands","Isla Mocha and Juan Fernandez Archipelago","Mocha Island"), "Isla Mocha and Juan Fernandez",files_list$pop)
files_list$pop <- ifelse(files_list$pop %in% c("Blaskets","High Island"), "Ireland",files_list$pop)

files_list$pop <- ifelse(files_list$colony == "Ihumoana Island", "Bethells Beach",files_list$pop)

files_list$pop <- ifelse(files_list$pop %in% c("Southern Tasmania","Tasmania","Griffiths Island","French Island","Montague Island","Gabo Island"), "SE Australia",files_list$pop)

files_list$pop <- ifelse(files_list$colony %in% c("Skomer","Ramsey","Lundy", "Bardsey","Rum","Copeland"), "UK",files_list$pop)

files_list$pop <- ifelse(files_list$species == "Fulmarus glacialis" & files_list$pop %in% c("Orkney","North Sea"), "Scotland",files_list$pop)

files_list$pop <- ifelse(files_list$species == "Ardenna carneipes" & files_list$site == "New Zealand", "New Zealand",files_list$pop)
files_list$pop <- ifelse(files_list$species == "Ardenna grisea" & files_list$colony %in% c("Kauwahaia Island","Mana Island"), "North Island",files_list$pop)

files_list$pop <- ifelse(files_list$species == "Procellaria conspicillata", "Tristan da Cunha",files_list$pop)

files_list$pop <- ifelse(files_list$species == "Pelecanoides urinatrix" & files_list$colony 
                         %in% c("Tiritiri Matangi","Burgess Island","Kauwahaia Island"), "North Island",files_list$pop)


#Remove scillies fulmars due to only 2 foraging trips
files_list <- subset(files_list, files != "Fulmarus glacialis_Isles of Scilly_St Martins_1237.csv")
files_list <- subset(files_list, files != "Fulmarus glacialis_Isles of Scilly_Annet_1232.csv")
#Delete yelkouan zaklopatica lastovo 1278 & a grisea/p vittata chatahm due to lack of data
files_list <- subset(files_list, files != "Puffinus yelkouan_Lastovo_Zaklopatica_1278.csv")
files_list <- subset(files_list, files != "Ardenna grisea_Chatham Islands_South East Island_zoatrack_112.csv")
files_list <- subset(files_list, files != "Pachyptila vittata_Chatham Islands_South East Island_zoatrack_106.csv")
files_list <- subset(files_list, files != "Pagodroma nivea_Davis_Hop Island_Snow_Petrel_PrydzBay_viaTommy.csv")




files_list$pop <- ifelse(files_list$colony %in% c("Loop islet",
  "Pindaï","Canard islet","Mato islet","Tiam'bouene islet","Nemou islet"), 
  "New Calendonia",files_list$pop)

#View(files_list)

files_list$population <- paste0(files_list$colony,", ",files_list$site)

#Remove brackets from names (they mess up filepaths)
files_list$pop <- ifelse(files_list$pop == 
                             "Falkland Islands (Islas Malvinas)",
                           "Falkland Islands",
                         files_list$pop)

files_list$pop <- ifelse(files_list$pop == 
                           "Phillip Island (Norfolk Island)" ,
                         "Phillip Island" ,
                         files_list$pop)

files_list$sp_pop <- paste(files_list$species,files_list$pop,sep="_")
write.csv(files_list,"outputs/02_pops.csv",row.names = F)

pops <- unique(files_list$sp_pop)
pops

output <- "outputs/02_pops"


for(i in 1:length(pops)){
  sp_pops <- subset(files_list,sp_pop == pops[i])
  
  Data<-do.call("rbind",lapply(as.character(paste0(data_std, sp_pops$files)),read.csv,stringsAsFactors = F))  #Read all the files and combine

  print(sp_pops)
  
  print(nrow(Data))
  #need to remove duplicates
  Data$bird_track <- paste(Data$bird_id,Data$track_id)
  Data <- Data %>% distinct(dtime,latitude,longitude,bird_track, .keep_all = TRUE)  
  print(nrow(Data))
  
  write.csv(Data,paste0(output,"/",pops[i],".csv"),row.names=FALSE)  #Write a csv 
  
}

data_std <- "outputs/02_pops/"  # now the raw data files are here
files <- list.files(data_std);files

#Looked at plots for kernels script and found some issues:

#remove august to jan from balearics
df <- read.csv(paste0(data_std,"Puffinus mauretanicus_Balearic Archipelago.csv"))  

df$months <- substr(df$dtime,6,7)
table(df$months)

df <- subset(df, months %in% c("03","04","05","06","07"))
df$months <- NULL
write.csv(df,paste0(data_std,"Puffinus mauretanicus_Balearic Archipelago.csv"),
          row.names=FALSE) 


#line of points in n far from others, looks like boat
df <- read.csv(paste0(data_std,"Ardenna carneipes_Breaksea Island.csv"))  
plot(df$longitude,df$latitude)

df2 <- subset(df,longitude <121)
plot(df2$longitude,df2$latitude)

head(max(df$latitude))
write.csv(df2,paste0(data_std,"Ardenna carneipes_Breaksea Island.csv"),
          row.names=FALSE) 


#line of points very close to island, seen on phenology plot, all close to colony
#Nov-June
df <- read.csv(paste0(data_std,"Pterodroma phaeopygia_Galapagos.csv"))  
plot(df$longitude)
plot(df$latitude)
df$months <- substr(df$dtime,6,7)

#two gaps with almost no movement
View(df)

df2 <- rbind(df[1:44,],df[622:1068,],df[1702:2204,])
plot(df2$latitude)

#check

df2b <- rbind(df[45:621,],df[1069:1701,])
plot(df2b$latitude)

plot(df$latitude,df$longitude)
plot(df2$latitude,df2$longitude)

#<5 points left in month 6, so they need to be removed
df3 <- subset(df2,months != "06")

#also very few for nov, so remove them too
df3 <- subset(df3,months != "11")

df3$months <- NULL

write.csv(df3,paste0(data_std,"Pterodroma phaeopygia_Galapagos.csv"),
          row.names=FALSE) 

#line of points very close to island, seen on phenology plot, all close to colony
#Nov-June
df <- read.csv(paste0(data_std,"Puffinus huttoni_Te Rae o Atiu.csv"))  
plot(df$longitude)
plot(df$latitude)

#remove bad months
df$month <- as.factor(as.character(substr(df$dtime,6,7)))

head(df)

good_months <- c("01","02")

df2 <- subset(df, month %in% good_months)
plot(df2$longitude)
plot(df2$latitude)

#two gaps with almost no movement
View(df2)

df2$count <- 1:nrow(df2)

df3 <- rbind(df2[1:10,],df2[126:145,],df2[296:361,],df2[467:519,],df2[636:673,])
plot(df3$latitude)

#check

df2b <- rbind(df2[11:125,],df2[146:295,],df2[362:466,],df2[520:635,])
plot(df2b$latitude)

plot(df$latitude,df$longitude)
plot(df3$latitude,df3$longitude)

df3$month <- NULL
df3$count <- NULL

head(df3)

write.csv(df3,paste0(data_std,"Puffinus huttoni_Te Rae o Atiu.csv"),
          row.names=FALSE) 


#points too far N
df <- read.csv(paste0(data_std,"Ardenna bulleri_Aorangi Island.csv"))  
plot(df$longitude,df$latitude)

df2 <- subset(df,latitude <70)
plot(df2$longitude,df2$latitude)

write.csv(df2,paste0(data_std,"Ardenna bulleri_Aorangi Island.csv"),
          row.names=FALSE) 

#points too far N
df <- read.csv(paste0(data_std,"Ardenna carneipes_New Zealand.csv"))  
plot(df$longitude,df$latitude)

df2 <- subset(df,latitude <69)
plot(df2$longitude,df2$latitude)

write.csv(df2,paste0(data_std,"Ardenna carneipes_New Zealand.csv"),
          row.names=FALSE) 

#points too far N
df <- read.csv(paste0(data_std,"Ardenna grisea_North Island.csv"))  
plot(df$longitude,df$latitude)

df2 <- subset(df,latitude <69)
plot(df2$longitude,df2$latitude)

write.csv(df2,paste0(data_std,"Ardenna grisea_North Island.csv"),
          row.names=FALSE) 

#points too far N & S
df <- read.csv(paste0(data_std,"Puffinus puffinus_UK.csv"))  
plot(df$longitude,df$latitude)

df2 <- subset(df,latitude <75)
plot(df2$longitude,df2$latitude)

#loot at the dip between the peaks
df_low <- subset(df,latitude < -50)
hist(df_low$latitude, breaks=40) #at -61

plot(df2$longitude,df2$latitude)
abline(h=-61,col="red")

df3 <- subset(df2,latitude > -61)
plot(df3$longitude,df3$latitude)

write.csv(df3,paste0(data_std,"Puffinus puffinus_UK.csv"),
          row.names=FALSE) 

