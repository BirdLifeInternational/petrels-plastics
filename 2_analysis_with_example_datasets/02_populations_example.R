#Combine cleaned tracking datasets into populations
#filter out errors for specific datasets
#calculate sample sizes

rm(list=ls())
library(tidyverse)

## paste home directory here
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

files_list$pop <- files_list$site

#Rename sites and populations with shorter names that do not contain
#special characters & group together populations

#for each population
#e.g. files_list$pop <- ifelse(files_list$pop %in% c("Colony1","Colony2"), "Population name",files_list$pop)
files_list$pop <- ifelse(files_list$pop == "South Georgia (Islas Georgias del Sur)", "South Georgia",files_list$pop)
large_sites <- c("Portugal")
files_list$pop <- ifelse(files_list$pop %in% large_sites, files_list$colony,files_list$pop)

#Remove and population with too few tracks
files_list <- subset(files_list, files != "filename.csv")


files_list$population <- paste0(files_list$colony,", ",files_list$site)

files_list$sp_pop <- paste(files_list$species,files_list$pop,sep="_")
write.csv(files_list,"outputs/02_pops.csv",row.names = F)

pops <- unique(files_list$sp_pop)
pops

setwd(data_std)                       #Set to the folder with your files
output <- "outputs/02_pops/"

#read in the files combine datasets into population leve files
for(i in 1:length(pops)){
  sp_pops <- subset(files_list,sp_pop == pops[i])
  
  Data<-do.call("rbind",lapply(as.character(sp_pops$files),read.csv,stringsAsFactors = F))  #Read all the files and combine
  print(sp_pops)
  
  print(nrow(Data))
  #need to remove duplicates
  Data$bird_track <- paste(Data$bird_id,Data$track_id)
  Data <- Data %>% distinct(dtime,latitude,longitude,bird_track, .keep_all = TRUE)  
  print(nrow(Data))
  
  write.csv(Data,paste0(output,"/",pops[i],".csv"),row.names=FALSE)  #Write a csv 
  
}

data_std <- output
files <- list.files(data_std);files

#After running script 03 and looking at kernel plots, solve any issues
#and replace pop level tracking files

df <- read.csv(paste0(data_std,"problem_dataset.csv"))  

#code to fix issue

write.csv(df,paste0(data_std,"problem_dataset.csv"),
          row.names=FALSE) 

#read in files to calculate sample sizes

head(pops)
#all pops points locations ####
for(i in 1:length(files)){
  pop <- read.csv(paste0(output,"/",files[i]))
  pop$sp_pop <- substr(files[i],1,nchar(files[i])-4)
  if(i==1){all_pops <- pop} else {all_pops <- rbind(all_pops,pop)}
  print(i)
}

all_pops$unique_id <- paste(all_pops$scientific_name,
                            all_pops$bird_id,
                            all_pops$colony_name,sep="_")

length(unique(all_pops$unique_id))

all_pops$pop <- gsub(".*_","",all_pops$sp_pop)

#summarise by species
sample_sizes <- all_pops %>% 
  group_by(scientific_name) %>%
  summarise(common_name = unique(common_name),
            individuals = length(unique(unique_id)),
            locations = n(),
            n_pops = length(unique(sp_pop)),
            populations= paste(unique(pop),collapse = "_")) %>%
  data.frame();sample_sizes

sum(sample_sizes$individuals)
sum(sample_sizes$n_pops)
sum(sample_sizes$locations)

# tracking years ####

data_std <- "outputs/02_pops/"
files <- list.files(data_std,pattern = ".csv");files
pops <- as.data.frame(files)
pops$sp_pop <- NA
pops$years <- NA
pops$min_year <- NA
pops$max_year <- NA
pops$mean_year <- NA
pops$n_years <- NA

for(i in 1:length(files)){
  pop <- read.csv(paste0(data_std,"/",files[i]))
  
  pops$species[i] <- as.character(pop$scientific_name[1])
  
  pops$sp_pop[i] <- substr(files[i],1,nchar(files[i])-4)
  
  pop$year <- as.numeric(substr(pop$dtime,1,4))
  
  years <- unique(pop$year)
  
  pops$years[i] <- paste(years,collapse="_")
  pops$min_year[i] <- min(years)
  pops$max_year[i] <- max(years)
  pops$mean_year[i] <- mean(years)
  pops$n_years[i] <- length(years)
  pops$yrs09_19[i] <- ifelse(any(2010:2018 %in% years),1,0)
  print(i)
  
  if(i==1){
    all_years <- years 
  } else {
    all_years <- c(all_years,years)
  }
  
}

head(pops)

hist(all_years)

mean(all_years)

length(all_years[all_years > 2008 & all_years < 2020])

length(all_years[all_years > 2008 & all_years < 2020])/length(all_years)

pops$year_range <- paste0(pops$min_year,"-",pops$max_year)

sum(pops$yrs09_19)/148 #change 148 to number of populations

mean(pops$mean_year)
mean(pops$min_year)
mean(pops$max_year)

year_sample_sizes <- pops %>% 
  group_by(species) %>%
  summarise(mean_year = mean(mean_year)) %>%
  data.frame();year_sample_sizes

sample_sizes$mean_tracking_year <- round(year_sample_sizes$mean_year)

write.csv(sample_sizes,"outputs/02_sample_sizes_species.csv",
          row.names = F)

#sample sizes by population
all_pops$unique_months <- substr(all_pops$dtime,1,7)

sample_sizes_pops <- all_pops %>% 
  group_by(sp_pop) %>%
  summarise(scientific_name = unique(scientific_name),
            common_name = unique(common_name),
            population = unique(pop),
            individuals = length(unique(unique_id)),
            locations = n(),
            unique_months = length(unique(unique_months))) %>%
  data.frame();sample_sizes_pops

#add info about tracking years
sample_sizes_pops$n_years <- pops$n_years
sample_sizes_pops$year_range <- pops$year_range
sample_sizes_pops$mean_year <- round(pops$mean_year)
sample_sizes_pops$years  <- pops$years
sample_sizes_pops$sp_pop  <- NULL

head(sample_sizes_pops)

write.csv(sample_sizes_pops,"outputs/02_sample_sizes_pops.csv",
          row.names = F)

#save all tracking locations in 1 csv
write.csv(all_pops,"outputs/02_all_locations.csv", row.names = F)

#save colony locations
all_pops$lat_colony_2dp <- round(all_pops$lat_colony,2)
all_pops$lon_colony_2dp <- round(all_pops$lon_colony,2)

all_pops$sp_pop_colloc <- paste(all_pops$sp_pop,all_pops$lat_colony_2dp,all_pops$lon_colony_2dp,sep="_")

all_colonies <- all_pops %>%
  group_by(sp_pop_colloc) %>%
  summarise(species = scientific_name[1],
            population = pop[1],
            colony = colony_name[1],
            lat_colony = mean(lat_colony_2dp),
            lon_colony = mean(lon_colony_2dp)) %>%
  data.frame() ; all_colonies

all_colonies$sp_pop_colloc <- NULL

write.csv(all_colonies,"outputs/02_colony_locations.csv", row.names = F)
