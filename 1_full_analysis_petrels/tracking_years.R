rm(list=ls())
library(dplyr)

## GENERAL DIR
dir <- paste0("C:/Users/bethany.clark/OneDrive - BirdLife International/",
              "Methods") ## copy and paste here your working directory
dir.create(paste0(dir, "/scripts_results/02_pops/")) 

################# LOADING SPP DATA ##################
data_std <- paste0(dir, "/scripts_results/02_pops/")
files <- list.files(data_std,pattern = ".csv");files
pops <- as.data.frame(files)
pops$sp_pop <- NA
pops$years <- NA
pops$min_year <- NA
pops$max_year <- NA
pops$mean_year <- NA
pops$n_years <- NA

#all pops points locations ####
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

hist(all_years)

mean(all_years)

length(all_years[all_years > 2008 & all_years < 2020])

length(all_years[all_years > 2008 & all_years < 2020])/length(all_years)

pops$year_range <- paste0(pops$min_year,"-",pops$max_year)
  
sum(pops$yrs09_19)/148
  
mean(pops$mean_year)
mean(pops$min_year)
mean(pops$max_year)

sample_sizes <- pops %>% 
  group_by(species) %>%
  summarise(mean_year = mean(mean_year)) %>%
  data.frame();sample_sizes

sample_sizes <- all_pops %>% 
  group_by(scientific_name) %>%
  summarise(common_name = unique(common_name),
            individuals = length(unique(unique_id)),
            locations = n(),
            sites = length(unique(site_name))) %>%
  data.frame();sample_sizes

write.csv(pops,paste0(dir,"/scripts_results/pops_years.csv"),
          row.names = F)
