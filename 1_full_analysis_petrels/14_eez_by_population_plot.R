## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Creates plot and supplementary table for population eez analysis
## Beth Clark 2022
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

rm(list=ls()) 

library(tidyverse)
library(RColorBrewer)
library(svglite)
library(stringr)

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
#[1] svglite_2.1.1      RColorBrewer_1.1-2 forcats_0.5.1     
#[4] stringr_1.4.0      dplyr_1.0.8        purrr_0.3.4       
#[7] readr_2.1.2        tidyr_1.2.0        tibble_3.1.6      
#[10] ggplot2_3.3.5      tidyverse_1.3.2   

#loaded via a namespace (and not attached):
#[1] Rcpp_1.0.8          pillar_1.7.0        compiler_4.1.2     
#[4] cellranger_1.1.0    dbplyr_2.1.1        tools_4.1.2        
#[7] lubridate_1.8.0     jsonlite_1.8.0      googledrive_2.0.0  
#[10] lifecycle_1.0.3     gargle_1.2.0        gtable_0.3.0       
#[13] pkgconfig_2.0.3     rlang_1.0.6         reprex_2.0.1       
#[16] DBI_1.1.2           cli_3.3.0           rstudioapi_0.13    
#[19] haven_2.4.3         xml2_1.3.3          withr_2.5.0        
#[22] httr_1.4.2          systemfonts_1.0.4   hms_1.1.1          
#[25] generics_0.1.2      vctrs_0.3.8         fs_1.5.2           
#[28] googlesheets4_1.0.0 grid_4.1.2          tidyselect_1.1.2   
#[31] glue_1.6.2          R6_2.5.1            fansi_1.0.2        
#[34] readxl_1.3.1        tzdb_0.2.0          modelr_0.1.8       
#[37] magrittr_2.0.2      backports_1.4.1     scales_1.2.1       
#[40] ellipsis_0.3.2      rvest_1.0.2         assertthat_0.2.1   
#[43] colorspace_2.0-3    utf8_1.2.2          stringi_1.7.6      
#[46] munsell_0.5.0       broom_0.7.12        crayon_1.5.0

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
#(above 15.3, the value if plastic was evenly distributed globally)
dat <- dat[order(-dat$score),]
top_scoring <- subset(dat, score > 15.3)

#loop through each species_country and combine together everything
#below a threshold of proportion of score
score_threshold <- 0.05

i<-1
for(i in 1:nrow(top_scoring)){
  species_df <- eezs_used[eezs_used$sp_country == top_scoring$sp_country[i],]
  
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

#replace sci names
#comma instead of _ for label
#make own country different
#add IUCN category as label for plot 2
names <- read.csv("input_data/Species_list_IUCN.csv")
head(names)

top_scoring$common_name <- names$common_name[match(top_scoring$species,names$scientific_name)]
top_scoring$iucn <- names$IUCN[match(top_scoring$species,names$scientific_name)]
head(top_scoring)

dat$sci_name_pop <- dat$sp_country 
top_scoring$country <- str_split_fixed(top_scoring$sp_country,"_",n=2)[,2]
top_scoring$common_country <- paste0(top_scoring$common_name,", ",top_scoring$country)

head(top_scoring)
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

#set greyscale matching values in Figure 4b
greys <- c(brewer.pal(n = 9, name = "Greys"))
greys_eez <- c(rev(colorRampPalette(greys)(15)))[1:14]

all_sp$fill_col <- greys_eez[14]
all_sp$fill_col <- ifelse(all_sp$group == "High Seas",greys_eez[1],all_sp$fill_col)
all_sp$fill_col <- ifelse(all_sp$group == "Spain",greys_eez[2],all_sp$fill_col)
all_sp$fill_col <- ifelse(all_sp$group == "Tunisia",greys_eez[3],all_sp$fill_col)
all_sp$fill_col <- ifelse(all_sp$group == "Italy",greys_eez[4],all_sp$fill_col)
all_sp$fill_col <- ifelse(all_sp$group == "Libya",greys_eez[5],all_sp$fill_col)
all_sp$fill_col <- ifelse(all_sp$group == "Turkey",greys_eez[6],all_sp$fill_col)
all_sp$fill_col <- ifelse(all_sp$group == "Algeria",greys_eez[7],all_sp$fill_col)
all_sp$fill_col <- ifelse(all_sp$group == "Malta",greys_eez[8],all_sp$fill_col)
all_sp$fill_col <- ifelse(all_sp$group == "Greece",greys_eez[9],all_sp$fill_col)
all_sp$fill_col <- ifelse(all_sp$group == "USA",greys_eez[10],all_sp$fill_col)
all_sp$fill_col <- ifelse(all_sp$group == "Japan",greys_eez[11],all_sp$fill_col)
all_sp$fill_col <- ifelse(all_sp$group == "UK",greys_eez[12],all_sp$fill_col)
all_sp$fill_col <- ifelse(all_sp$group == "New Zealand",greys_eez[13],all_sp$fill_col)

all_sp$text_col <- ifelse(all_sp$group %in% c("High Seas", "Spain", "Tunisia", "Italy", "Libya",
                                              "Turkey", "Algeria", "Malta", "Greece"), "white", "black")

eezs_grey <- ggplot(all_sp,aes(x=prop,y=common_country,
                               label = group_label))+
  geom_col(color="white",fill=all_sp$fill_col) +
  geom_text(position = position_stack(vjust = 0.5),
            col=all_sp$text_col,size=8) +
  ylab("") +  
  xlab("Proportion of Plastic Encounter Risk")+
  theme_bw()+
  scale_x_continuous(expand = c(0, 0))+
  theme(legend.position = "position.none",
        text = element_text(size=rel(6)),
        axis.title = element_text(size=30),
        axis.text = element_text(colour = "black"));eezs_grey

#Save plots ####
#save plot as png
png("outputs/14_top_scoring_eezs.png", width=2000, height=1350)
par(mfrow=c(1,1))
eezs_grey
dev.off()
dev.off()

#reformat to save plot as svg for journal plot requirements
eezs_grey_svg <- ggplot(all_sp,aes(x=prop,y=common_country,
                                   label = group_label))+
  geom_col(color="white",fill=all_sp$fill_col) +
  geom_text(position = position_stack(vjust = 0.5),
            col=all_sp$text_col,size=4) +
  ylab("") +  
  xlab("\nPercentage of Plastic Encounter Risk")+
  theme_bw()+
  scale_x_continuous(expand = c(0, 0),labels = scales::percent)+
  theme(legend.position = "position.none",
        text = element_text(size=rel(6)),
        axis.title = element_text(size=15),
        axis.text = element_text(colour = "black",size = 12));eezs_grey_svg

#save as svg
ggsave(filename = "outputs/14_top_scoring_eezs.svg",
       plot = eezs_grey_svg, 
       width = 4000, height = 2700, unit = "px") 

#save source data for fig 4c
source_dat <- all_sp %>%
  select(common_country,group_label,prop)
write.csv(source_dat,"outputs/14_eezs_percent_topscoring.csv",
          row.names = F)

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
           group_5,Percent_5,group_6,Percent_6,group_7,Percent_7)) %>%
  data.frame()
head(pops_table)

write.csv(pops_table,"outputs/14_eezs_percent_allpops.csv",
          row.names = F)

