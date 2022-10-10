## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Use eez results and create summary statistics
## Beth Clark 2022
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

rm(list=ls()) 

library(tidyverse)
library(stringr)
library(cowplot)
library(viridis)

## GENERAL DIR

eezs_used <- read.csv("outputs/11_eezs_used_all_species.csv")
dat <- read.csv("outputs/12_species_country_scores.csv")

#plot top scores ####
#subset to top 40 species_countries

#comment out 3 rows below to work out n species per eez
#top40 <- dat[dat$score > 14,]
#t40_eezs <- eezs_used[eezs_used$pop %in% top40$sp_country,]
#eezs_used <- t40_eezs

#too many overlapping claims to ignore
#for overlapping claims, 

#eezs_used$cat <- ifelse(eezs_used$POL_TYPE == "Overlapping claim",
#                        "Overlapping claim",eezs_used$TERRITORY1)

eezs_used$group <- ifelse(eezs_used$POL_TYPE == "Overlapping claim",
                          paste(eezs_used$SOVEREIGN1,
                                eezs_used$SOVEREIGN2,
                                eezs_used$SOVEREIGN3,sep="/")
                          ,eezs_used$SOVEREIGN1)
eezs_used$group <- ifelse(eezs_used$group =="China/NA/NA",
                          "South China Sea",eezs_used$group)
eezs_used$group <- str_remove(eezs_used$group,"/NA")

eezs_used2 <- eezs_used %>% 
  group_by(group,pop) %>%
  summarise( 
    prop = sum(prop),
    pop = pop[1],
    species = species[1],
    breeding_country = breeding_country[1]) %>%
  data.frame; eezs_used2

eezs_used <- eezs_used2

unique(eezs_used$group)

length(unique(eezs_used$group))

head(eezs_used)

eezs_country <- eezs_used %>%
  group_by(breeding_country) %>% 
  summarise(prop = sum(prop),
            npops = length(unique(pop))) %>%
  data.frame();eezs_country

eezs_used$npops <- eezs_country$npops[match(eezs_used$breeding_country,
                                            eezs_country$breeding_country)]

eezs_species <- eezs_used %>%
  group_by(species) %>% 
  summarise(prop = sum(prop),
            npops = length(unique(pop))) %>%
  data.frame();eezs_species

#too much going on, need to simplify
# order from highest to lowest
# group countries below the mean into low impact group

eezs_group <- eezs_used %>%
  group_by(group) %>% 
  summarise(prop = sum(prop),
            npops = length(unique(pop)),
            nspp = length(unique(species))) %>%
  data.frame();eezs_group


#loop through each species_country and lump together everything that falls 
#below threshold of proportion of score

score_threshold <- 0.05

top40 <- subset(dat, score > score_threshold)
#top40 <- dat

top40 <- top40[order(top40$score),]

for(i in 1:nrow(top40)){
  species_df <- eezs_used[eezs_used$pop == top40$sp_country[i],]
  
  if(nrow(species_df > 1)){
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
    all_sp <- species_df_crop
  } else {
    all_sp <- rbind(all_sp,species_df_crop)
  }

}
nrow(all_sp)
#save new df



#change some labels
unique(all_sp$group)
all_sp$group <- ifelse(all_sp$group == "United States","USA",all_sp$group)
all_sp$group <- ifelse(all_sp$group == "United Kingdom","UK",all_sp$group)
all_sp$group <- ifelse(all_sp$group == "Republic of Mauritius","Mauritius",all_sp$group)
all_sp$group <- ifelse(all_sp$group == "Democratic Republic of the Congo","DRC",all_sp$group)
all_sp$group <- ifelse(all_sp$group == "United States/Dominican Republic" ,"USA/Dominican Republic",all_sp$group)
all_sp$group <- ifelse(all_sp$group == "Haiti/United States/Jamaica" ,"Haiti/USA/Jamaica" ,all_sp$group)
all_sp$group <- ifelse(all_sp$group == "France/Madagascar/Republic of Mauritius" ,"FR/MG/MU",all_sp$group)
all_sp$group <- ifelse(all_sp$group == "Madagascar" & 
                         all_sp$species == "Pterodroma baraui","MDG",all_sp$group)
all_sp$group <- ifelse(all_sp$group == "0" ," ",all_sp$group)
all_sp$group <- ifelse(all_sp$group == "1" ,"1   ",all_sp$group)
all_sp$group <- ifelse(all_sp$group == "North Korea" ,"N Korea",all_sp$group)
all_sp$group <- ifelse(all_sp$group == "South Korea" & 
                         all_sp$breeding_country == "South Korea" ,"S Korea*",all_sp$group)
all_sp$group <- ifelse(all_sp$group == "South Africa" ,"S Africa",all_sp$group)
all_sp$group <- ifelse(all_sp$group == "Western Sahara" ,"W Sahara",all_sp$group)
all_sp$group <- ifelse(all_sp$group == "Japan/South Korea" ,"Japan/S Korea",all_sp$group)
all_sp$group <- ifelse(all_sp$group == "Spain/Morocco" ,"Spain*/Morocco",all_sp$group)
all_sp$group <- ifelse(all_sp$group == "Japan/Russia" & 
                       all_sp$breeding_country == "Japan" ,"J*/Russia",all_sp$group)

#all_sp$pop <- as.factor(all_sp$pop)

#all_sp$pop <- factor(all_sp$pop, 
#                           level = top40$sp_country)

#replace sci names
#comma instead of _ for label
#make own country different
#add IUCN category as label for plot 2
names <- read.csv("C:/Users/bethany.clark/OneDrive - BirdLife International/Requests/Species_list_petrels2.csv")
head(names)

top40$common_name <- names$Common.name[match(top40$species,names$Scientific.name)]
top40$iucn <- names$X2019.IUCN.Red.List.Category[match(top40$species,names$Scientific.name)]
head(top40)

dat$sci_name_pop <- dat$species_pop
top40$country <- str_split_fixed(top40$sp_country,"_",n=2)[,2]
top40$common_country <- paste0(top40$common_name,", ",top40$country)

head(top40)
all_sp$common_name <- names$Common.name[match(all_sp$species,names$Scientific.name)]
all_sp$common_country <- paste0(all_sp$common_name,", ",all_sp$breeding_country)
all_sp$iucn <- names$X2019.IUCN.Red.List.Category[match(all_sp$species,names$Scientific.name)]
all_sp$common_country_iucn <- paste0(all_sp$common_country," ",all_sp$iucn)
all_sp$pop_score <- dat$score[match(all_sp$pop,dat$sp_country)]

all_sp <- all_sp[order(all_sp$pop_score),]

#all_sp$common_country_iucn <- as.factor(all_sp$common_country_iucn)
#all_sp$common_country_iucn <- factor(all_sp$common_country_iucn, 
#                           levels = unique(all_sp$common_country_iucn))

all_sp$common_country <- as.factor(all_sp$common_country)
all_sp$common_country <- factor(all_sp$common_country, 
                                     levels = unique(all_sp$common_country))

all_sp$group_label <- ifelse(all_sp$group == all_sp$breeding_country,
                             paste0(all_sp$group,"*"),
                             all_sp$group)

eezs <- ggplot(all_sp,aes(x=prop,y=common_country,
                          fill=prop,label = group_label))+
  geom_col(color="white") +
  scale_fill_viridis(option="inferno",direction=-1) +
  geom_text(position = position_stack(vjust = 0.5),
            col="white",size=8) +
  ylab("") +  
  xlab("Proportion of Plastic Encounter Risk")+
  theme_bw()+
  scale_x_continuous(expand = c(0, 0))+
  theme(legend.position = "position.none",
        text = element_text(size=rel(6)),
        axis.title = element_text(size=30),
        axis.text = element_text(colour = "black"));eezs

png(paste0(dir,"/scripts_results/figures/top40_eezs2_sum",score_threshold,".png"), width=2000,height=1350)
par(mfrow=c(1,1))
eezs
dev.off()
dev.off()

png(paste0(dir,"/scripts_results/figures/top40_eezs2",score_threshold,"legend.png"), width=2000,height=1350)
par(mfrow=c(1,1))
ggplot(all_sp,aes(x=prop,y=common_country_iucn,fill=prop,label = group_label))+
  geom_col(color="white") +
  scale_fill_viridis(option="inferno",direction=-1) +
  geom_text(position = position_stack(vjust = 0.5),
            col="white",size=8) +
  ylab("") +  
  xlab("Proportion of Plastic Encounter Risk")+
  theme_bw()+
  scale_x_continuous(expand = c(0, 0))+
  theme(text = element_text(size=rel(6)),
        axis.title = element_text(size=30),
        axis.text = element_text(colour = "black"),
        legend.text = element_text(color = "black", size = 40),
        legend.key.size = unit(5, 'cm'))
dev.off()
dev.off()

#with score ####

#top40$sp_country <- as.factor(top40$sp_country)

#top40$sp_country <- factor(top40$sp_country, level = top40$sp_country)

#score <- ggplot(top40,aes(x=score,y=sp_country))+
#  geom_col(color="white") +
#  ylab("") +  
#  xlab("Plastic Encounter Risk Score")+
#  theme_bw()+
#  theme(
#        axis.text.y=element_blank(),
#        panel.grid = element_blank(),
#        panel.border = element_blank(),
#        axis.ticks = element_blank(),
#        text = element_text(size=rel(6)),
#        axis.title = element_text(size=26),
#        axis.text.x = element_text(colour = "black"))+
#  scale_x_continuous(expand = c(0, 0));score

#plot_grid(eezs,score, rel_widths = c(3.9, 1))

#png(paste0(dir,"/scripts_results/figures/top40_eezs",score_threshold,"_score.png"), width=2000,height=1125)
#par(mfrow=c(1,1))
#plot_grid(eezs,score, rel_widths = c(10, 1))
#dev.off()
#dev.off()

# with colours to match fig 4b

inferno_cols <- cols <- c((inferno(14)))

all_sp$fill_col <- inferno_cols[14]
all_sp$fill_col <- ifelse(all_sp$group == "High Seas",inferno_cols[1],all_sp$fill_col)
all_sp$fill_col <- ifelse(all_sp$group == "Spain",inferno_cols[2],all_sp$fill_col)
all_sp$fill_col <- ifelse(all_sp$group == "Tunisia",inferno_cols[3],all_sp$fill_col)
all_sp$fill_col <- ifelse(all_sp$group == "Italy",inferno_cols[4],all_sp$fill_col)
all_sp$fill_col <- ifelse(all_sp$group == "Libya",inferno_cols[5],all_sp$fill_col)
all_sp$fill_col <- ifelse(all_sp$group == "Turkey",inferno_cols[6],all_sp$fill_col)
all_sp$fill_col <- ifelse(all_sp$group == "Algeria",inferno_cols[7],all_sp$fill_col)
all_sp$fill_col <- ifelse(all_sp$group == "Malta",inferno_cols[8],all_sp$fill_col)
all_sp$fill_col <- ifelse(all_sp$group == "Greece",inferno_cols[9],all_sp$fill_col)
all_sp$fill_col <- ifelse(all_sp$group == "USA",inferno_cols[10],all_sp$fill_col)
all_sp$fill_col <- ifelse(all_sp$group == "Japan",inferno_cols[11],all_sp$fill_col)
all_sp$fill_col <- ifelse(all_sp$group == "UK",inferno_cols[12],all_sp$fill_col)
all_sp$fill_col <- ifelse(all_sp$group == "New Zealand",inferno_cols[13],all_sp$fill_col)

all_sp$text_col <- ifelse(all_sp$group %in% c("High Seas","Spain", "Tunisia", "Italy", "Libya",
                                            "Turkey", "Algeria", "Malta","Greece", "USA","Japan", 
                                            "UK", "New Zealand"),"white","black")


eezs <- ggplot(all_sp,aes(x=prop,y=common_country,
                          label = group_label))+
  geom_col(color="white",fill=all_sp$fill_col) +
  #scale_fill_viridis(option="inferno",direction=-1,discrete=T) +
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

png(paste0(dir,"/scripts_results/figures/top40_eezs2",score_threshold,"legend_newcols.png"), width=2000,height=1350)
par(mfrow=c(1,1))
eezs <- ggplot(all_sp,aes(x=prop,y=common_country,
                          label = group_label))+
  geom_col(color="white",fill=all_sp$fill_col) +
  #scale_fill_viridis(option="inferno",direction=-1,discrete=T) +
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
dev.off()
dev.off()

#Table for supplementary ####

for(i in 1:nrow(dat)){
  species_df <- eezs_used[eezs_used$pop == dat$sp_country[i],]
  
  if(nrow(species_df > 1)){
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
    all_sp_sup <- species_df_crop
  } else {
    all_sp_sup <- rbind(all_sp_sup,species_df_crop)
  }
  
}
nrow(all_sp_sup)

all_sp_sup$npops <- NULL

pops <- unique(all_sp_sup$pop)
for(i in 1:length(pops)){
  pop <- subset(all_sp_sup,pop == pops[i])
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

pops_table <- all_pops %>% 
  pivot_wider(names_from = Label,values_from = c(group,Percent)) %>%
  select(c(species,breeding_country,group_1,Percent_1,
           group_2,Percent_2,group_3,Percent_3,group_4,Percent_4,
           group_5,Percent_5,group_6,Percent_6,group_7,Percent_7)) %>%
  data.frame()
head(pops_table)

write.csv(pops_table,paste0(dir,"/eezs_percent_allpops.csv"),
          row.names = F)


