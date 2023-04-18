## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Creates plot and supplementary table for population eez analysis
## Beth Clark 2022
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

rm(list=ls()) 

library(tidyverse)
library(RColorBrewer)
library(svglite)
library(stringr)

#Read in the outputs from script 13 ####
eezs_used <- read.csv("outputs/13_eezs_used_per_species.csv")
dat <- read.csv("outputs/12_species_country_scores.csv")

#Investigate results ####
head(eezs_used)

#for overlapping claims, 
eezs_used$group <- ifelse(eezs_used$POL_TYPE == "Overlapping claim",
                          paste(eezs_used$SOVEREIGN1,
                                eezs_used$SOVEREIGN2,
                                eezs_used$SOVEREIGN3,sep="/")
                          ,eezs_used$SOVEREIGN1)
eezs_used$group <- ifelse(eezs_used$group =="China/NA/NA",
                          "South China Sea",eezs_used$group)
eezs_used$group <- str_remove(eezs_used$group,"/NA")

#label the high seas group
eezs_used$group <- ifelse(is.na(eezs_used$group),"High Seas",eezs_used$group)
head(eezs_used)
unique(eezs_used$group)

length(unique(eezs_used$group))

# too many to plot all, so need to simplify for visualisation
# order from highest to lowest
# combine low scoring groups together

eezs_group <- eezs_used %>%
  group_by(group) %>% 
  summarise(prop = sum(prop),
            nspp = length(unique(species))) %>%
  data.frame();eezs_group

#Create df to plot top scoring populations ####
dat <- dat[order(-dat$score),]

#loop through each species_country and combine together everything
#below a threshold of proportion of score
score_threshold <- 0.05

i<-1
for(i in 1:nrow(dat)){
  species_df <- eezs_used[eezs_used$sp_country == dat$sp_country[i],]
  
  if(nrow(species_df) > 1){
    species_df_crop <- species_df[species_df$prop >= score_threshold,]
    species_df_crop_low <- species_df[species_df$prop < score_threshold,]
    
    species_df_crop <- species_df_crop[order(-species_df_crop$prop),]
    
    species_df_crop[nrow(species_df_crop)+1,] <- species_df_crop[nrow(species_df_crop),]
    species_df_crop[nrow(species_df_crop),]$prop <- sum(species_df_crop_low$prop)
    species_df_crop[nrow(species_df_crop),]$group <- nrow(species_df_crop_low)
    
    print(sum(species_df_crop$prop)) #should all = 1
  } else {
    species_df_crop <- species_df
  }
  
  if(i == 1){
    all_sp <- species_df_crop
  } else {
    all_sp <- rbind(all_sp,species_df_crop)
  }
  
}
nrow(all_sp)

head(all_sp)

#replace sci names
#comma instead of _ for label
#make own country different
#add IUCN category as label for plot 2
names <- read.csv("input_data/Species_list_IUCN.csv")
head(names)

dat$common_name <- names$common_name[match(dat$species,names$scientific_name)]
dat$iucn <- names$IUCN[match(dat$species,names$scientific_name)]
head(dat)

dat$sci_name_pop <- dat$sp_country 
dat$country <- str_split_fixed(dat$sp_country,"_",n=2)[,2]
dat$common_country <- paste0(dat$common_name,", ",dat$country)

head(dat)
all_sp$common_name <- names$common_name[match(all_sp$species,names$scientific_name)]
all_sp$common_country <- paste0(all_sp$common_name,", ",all_sp$breeding_country)
all_sp$iucn <- names$IUCN[match(all_sp$species,names$scientific_name)]
all_sp$common_country_iucn <- paste0(all_sp$common_country," ",all_sp$iucn)
all_sp$pop_score <- dat$score[match(all_sp$sp_country,dat$sp_country)]

all_sp <- all_sp[order(all_sp$pop_score),]

all_sp$common_country <- as.factor(all_sp$common_country)
all_sp$common_country <- factor(all_sp$common_country, 
                                levels = unique(all_sp$common_country))

all_sp$group_label <- ifelse(all_sp$group == all_sp$breeding_country,
                             paste0(all_sp$group,"*"),
                             all_sp$group)

#Plot results ####
eezs <- ggplot(all_sp,aes(x=prop,y=common_country,
                               label = group_label))+
  geom_col(color="white",fill = "grey70") +
  geom_text(position = position_stack(vjust = 0.5),
            col=all_sp$text_col,size=8) +
  ylab("") +  
  xlab("Proportion of Plastic Encounter Risk")+
  theme_bw()+
  scale_x_continuous(expand = c(0, 0))+
  theme(legend.position = "position.none",
        text = element_text(size=rel(6)),
        axis.title = element_text(size=30),
        axis.text = element_text(colour = "black"));eezs

#Save plots ####
#save plot as png
png("outputs/14_dat_eezs.png", width=2000, height=1350)
par(mfrow=c(1,1))
eezs
dev.off()
dev.off()

#reformat to save plot as svg for journal plot requirements
eezs_svg <- ggplot(all_sp,aes(x=prop,y=common_country,
                                   label = group_label))+
  geom_col(color="white",fill="grey70") +
  geom_text(position = position_stack(vjust = 0.5),
            col=all_sp$text_col,size=4) +
  ylab("") +  
  xlab("\nPercentage of Plastic Encounter Risk")+
  theme_bw()+
  scale_x_continuous(expand = c(0, 0),labels = scales::percent)+
  theme(legend.position = "position.none",
        text = element_text(size=rel(6)),
        axis.title = element_text(size=15),
        axis.text = element_text(colour = "black",size = 12));eezs_svg

#save as svg
ggsave(filename = "outputs/14_dat_eezs.svg",
       plot = eezs_grey_svg, 
       width = 4000, height = 2700, unit = "px") 

#Create table for supplementary ####
i<-1
for(i in 1:nrow(dat)){
  species_df <- eezs_used[eezs_used$sp_country == dat$sp_country[i],]
  
  if(nrow(species_df) > 1){
    species_df_crop <- species_df[species_df$prop >= score_threshold,]
    species_df_crop_low <- species_df[species_df$prop < score_threshold,]
    
    species_df_crop <- species_df_crop[order(-species_df_crop$prop),]
    
    species_df_crop[nrow(species_df_crop)+1,] <- species_df_crop[nrow(species_df_crop),]
    species_df_crop[nrow(species_df_crop),]$prop <- sum(species_df_crop_low$prop)
    species_df_crop[nrow(species_df_crop),]$group <- nrow(species_df_crop_low)
    
    print(sum(species_df_crop$prop))
  } else {
    species_df_crop <- species_df
  }
  
  if(i == 1){
    dat_sup <- species_df_crop
  } else {
    dat_sup <- rbind(dat_sup,species_df_crop)
  }
  
}
dat_sup

pops <- unique(dat_sup$sp_country)
for(i in 1:length(pops)){
  pop <- subset(dat_sup,sp_country == pops[i])
  pop$Label <- 1:nrow(pop)
  if(i == 1){
    all_pops <- pop
  } else {
    all_pops <- rbind(all_pops,pop)
  }
}

head(all_pops,20)

all_pops$Percent <- all_pops$prop*100
all_pops$prop <- NULL

max(all_pops$Label)

pops_table <- all_pops %>% 
  pivot_wider(names_from = Label,values_from = c(group,Percent)) %>%
  select(c(species,breeding_country,group_1,Percent_1,
           group_2,Percent_2,group_3,Percent_3,group_4,Percent_4,
           group_5,Percent_5)) %>%
  data.frame()
head(pops_table)

write.csv(pops_table,"outputs/14_eezs_percent_allpops.csv",
          row.names = F)

