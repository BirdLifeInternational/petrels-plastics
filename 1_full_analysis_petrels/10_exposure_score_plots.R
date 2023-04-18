## Read in exposure scores grouped by population, season
## and species, and plot results including IUCN redlist
## threat categories.
## Beth Clark 2022

rm(list=ls()) 

library(tidyverse)
library(viridis)
library(cowplot)
library(ggtext)
library(svglite)
se <- function(x) sqrt(var(x)/length(x))

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
#[1] svglite_2.1.1     ggtext_0.1.1      cowplot_1.1.1    
#[4] viridis_0.6.2     viridisLite_0.4.0 forcats_0.5.1    
#[7] stringr_1.4.0     dplyr_1.0.8       purrr_0.3.4      
#[10] readr_2.1.2       tidyr_1.2.0       tibble_3.1.6     
#[13] ggplot2_3.3.5     tidyverse_1.3.2  

#loaded via a namespace (and not attached):
#[1] tidyselect_1.1.2    haven_2.4.3         lattice_0.20-45    
#[4] gargle_1.2.0        colorspace_2.0-3    vctrs_0.3.8        
#[7] generics_0.1.2      utf8_1.2.2          rlang_1.0.6        
#[10] gridtext_0.1.4      pillar_1.7.0        glue_1.6.2         
#[13] withr_2.5.0         DBI_1.1.2           sp_1.5-0           
#[16] dbplyr_2.1.1        modelr_0.1.8        readxl_1.3.1       
#[19] lifecycle_1.0.3     munsell_0.5.0       gtable_0.3.0       
#[22] cellranger_1.1.0    rvest_1.0.2         raster_3.1-5       
#[25] codetools_0.2-18    tzdb_0.2.0          fansi_1.0.2        
#[28] broom_0.7.12        Rcpp_1.0.8          scales_1.2.1       
#[31] backports_1.4.1     googlesheets4_1.0.0 jsonlite_1.8.0     
#[34] systemfonts_1.0.4   fs_1.5.2            gridExtra_2.3      
#[37] hms_1.1.1           stringi_1.7.6       grid_4.1.2         
#[40] cli_3.3.0           tools_4.1.2         magrittr_2.0.2     
#[43] crayon_1.5.0        pkgconfig_2.0.3     ellipsis_0.3.2     
#[46] xml2_1.3.3          reprex_2.0.1        googledrive_2.0.0  
#[49] lubridate_1.8.0     assertthat_0.2.1    httr_1.4.2         
#[52] rstudioapi_0.13     R6_2.5.1            compiler_4.1.2  

## check we're still in rproj home directory "1_full_analysis_petrels"
getwd()

pops <- read.csv("outputs/05_exposure_scores_by_population.csv")
seasons <- read.csv("outputs/07_exposure_scores_by_season.csv")
species <- read.csv("outputs/08_exposure_scores_by_species.csv")

summary(species$species_exposure)

#add common name & IUCN
names_iucn <- read.csv("input_data/Species_list_IUCN.csv")
head(names_iucn)

pops$common_name <- names_iucn$common_name[match(pops$species,names_iucn$scientific_name)]
pops$iucn <- names_iucn$IUCN[match(pops$species,names_iucn$scientific_name)]
head(pops)

seasons$species <- pops$species[match(seasons$species_pop,pops$sp_pop)]
seasons$common_name <- names_iucn$common_name[match(seasons$species,names_iucn$scientific_name)]
seasons$iucn <- names_iucn$IUCN[match(seasons$species,names_iucn$scientific_name)]

species$common_name <- names_iucn$common_name[match(species$species,names_iucn$scientific_name)]
species$iucn <- names_iucn$IUCN[match(species$species,names_iucn$scientific_name)]
head(species)

#plot season scores for populations with greatest differences ####
#calculate season differences 
head(seasons)
seasons$tracks_breeding <- NULL
seasons$tracks_nonbreeding <- NULL
seasons$ref_breeding <- NULL
seasons$ref_nonbreeding <- NULL
seasons$season_diff_raw <- seasons$br_exposure - seasons$nonbr_exposure 
seasons$season_diff <- abs(seasons$br_exposure - seasons$nonbr_exposure)
head(seasons)

#pivot longer for plotting
seasons_plot <- tidyr::pivot_longer(seasons, c(br_exposure, nonbr_exposure),
                                    names_to = "season", 
                                    values_to = "season_exposure") %>% 
  data.frame();head(seasons_plot)

#remove non breeding seasons that don't exist
seasons_plot <- subset(seasons_plot, !is.na(seasons_plot$season_exposure)) #

hist(seasons_plot$season_exposure)

#correct population names for plotting
seasons_plot$sp_pop <- seasons_plot$species_pop 
seasons_plot$pop <- gsub(".*_","",seasons_plot$sp_pop)
seasons_plot$species_pop <- paste0(seasons_plot$common_name,", ",
                                  seasons_plot$pop)

seasons_plot$species_pop <-  ifelse(seasons_plot$species_pop == "Pink-footed Shearwater, Isla Mocha and Juan Fernandez",
                           "Pink-footed Shearwater, Isla Mocha & Juan Fernandez",
                           seasons_plot$species_pop)
seasons_plot$species_pop <-  ifelse(seasons_plot$species_pop == "Black-capped Petrel, Sierra de Bahoruco",
                           "Black-capped Petrel, Dominican Republic",
                           seasons_plot$species_pop)
seasons_plot$species_pop <-  ifelse(seasons_plot$species_pop ==  "Mottled Petrel, Lake HaurokoPetrel Island",
                           "Mottled Petrel, Lake Hauroko Petrel Island",
                           seasons_plot$species_pop)
seasons_plot$species_pop <-  ifelse(seasons_plot$species_pop ==  "Wedge-tailed Shearwater, New Calendonia",
                           "Wedge-tailed Shearwater, New Caledonia",
                           seasons_plot$species_pop)
seasons_plot$species_pop <-  ifelse(seasons_plot$species_pop ==  "Bulwer's Petrel, Cape Verde",
                           "Bulwer's Petrel, Cabo Verde",
                           seasons_plot$species_pop)
seasons_plot$species_pop <-  ifelse(seasons_plot$species_pop ==  "Cape Verde Shearwater, Cape Verde",
                           "Cape Verde Shearwater, Cabo Verde",
                           seasons_plot$species_pop)
seasons_plot$species_pop <-  ifelse(seasons_plot$species_pop ==  "Cape Verde Storm-petrel, Cape Verde",
                           "Cape Verde Storm-petrel, Cabo Verde",
                           seasons_plot$species_pop)
seasons_plot$species_pop <-  ifelse(seasons_plot$species_pop ==  "Tahiti Petrel, New Calendonia",
                           "Tahiti Petrel, New Caledonia",
                           seasons_plot$species_pop)
seasons_plot$species_pop <-  ifelse(seasons_plot$species_pop ==  "Audubon's Shearwater, Cape Verde",
                           "Audubon's Shearwater, Cabo Verde",
                           seasons_plot$species_pop)
seasons_plot$species_pop <-  ifelse(seasons_plot$species_pop ==  "Cape Verde Petrel, Cape Verde",
                           "Cape Verde Petrel, Cabo Verde",
                           seasons_plot$species_pop)
seasons_plot$species_pop <-  ifelse(seasons_plot$species_pop ==  "Hutton's Shearwater, Te Rae o Atiu",
                           "Hutton's Shearwater, Kaikoura",
                           seasons_plot$species_pop)
seasons_plot$species_pop <-  ifelse(seasons_plot$species_pop ==  "Grey-faced Petrel, Bethells Beach",
                           "Grey-faced Petrel, Te Hanga/Bethells Beach" ,
                           seasons_plot$species_pop)
seasons_plot$species_pop <-  ifelse(seasons_plot$species_pop ==  "Sooty Shearwater, Codfish Island",
                           "Sooty Shearwater, Codfish Island/Whenua Hou",
                           seasons_plot$species_pop)
seasons_plot$species_pop <-  ifelse(seasons_plot$species_pop ==  "Cook's Petrel, Codfish Island",
                           "Cook's Petrel, Codfish Island/Whenua Hou",
                           seasons_plot$species_pop)
seasons_plot$species_pop <-  ifelse(seasons_plot$species_pop ==  "Sooty Shearwater, Falkland Islands",
                           "Sooty Shearwater, Falkland Islands (Malvinas)",
                           seasons_plot$species_pop)
seasons_plot$species_pop <-  ifelse(seasons_plot$species_pop ==  "Sooty Shearwater, Falkland Islands",
                           "Sooty Shearwater, Falkland Islands (Malvinas)",
                           seasons_plot$species_pop)

head(seasons_plot)
max(seasons_plot$season_diff,na.rm=T)
mean(seasons_plot$season_diff,na.rm=T)

#20 species with biggest season differences
abovex <- subset(seasons_plot,season_diff > 23)
nrow(abovex)

abovex$species_pop <- ifelse(abovex$species_pop == "Scopoli's Shearwater, La Maddalena Archipelago",
                             "Scopoli's Shearwater, La Maddalena",abovex$species_pop)

abovex$species_pop <- paste0(abovex$species_pop," ")

seasons <- ggplot(abovex,aes(reorder(species_pop, season_diff),season_exposure,
                             season_exposure,group=species_pop)) +
  theme_bw()+
  coord_flip() +
  xlab("") +
  scale_colour_manual(values = c("grey","black"))+
  geom_line(colour="#696969",size=2)+   geom_point(aes(colour=season),size=8) +
  theme(legend.position = "none")+
  ylab("\nSeason-specific plastic exposure risk for tracked petrels")+ 
  theme(text=element_text(size = 42),
        axis.text = element_text(colour="black"));seasons

png("outputs/10_season_differences.png", 
    width=1350,height=1840) 
par(mfrow=c(1,1))
seasons
dev.off()
dev.off()

#format to save as svg file
seasons_svg <- ggplot(abovex,aes(reorder(species_pop, season_diff),season_exposure,
                             season_exposure,group=species_pop)) +
  theme_bw()+
  coord_flip() +
  xlab("") +
  scale_colour_manual(values = c("grey","black"))+
  geom_line(colour="#696969")+   geom_point(aes(colour=season)) +
  theme(legend.position = "none")+
  ylab("\nSeason-specific plastic exposure risk for tracked petrels")+ 
  theme(axis.text = element_text(colour="black"));seasons_svg

ggsave(filename = "outputs/10_season_differences.svg",
       plot = seasons_svg, width=5.1, height=6.74)  

write.csv(abovex,"outputs/10_season_differences_plot.csv",
          row.names = F)

#plot species scores ####
head(species)
species$species_label <- paste0(species$common_name,
                               " (",species$n_pops,",",
                               species$seasons,") ",
                               species$iucn)

#split in half for plotting
species$half <- ifelse(species$species_exposure > 
                       median(species$species_exposure),"upr","lwr")
table(species$half)
upper <- species[species$half=="upr",]
lower <- species[species$half=="lwr",]

max(species$species_exposure)

upr_half <- ggplot(upper,
                   aes(reorder(species,species_exposure),
                       species_exposure)) +
  geom_point(size=4) +
  coord_flip() +
  xlab("") + ylab("Plastic exposure risk score") +
  scale_y_continuous(limits = c(0,575),expand = c(0,0))+
  theme(axis.ticks = element_blank())+
  theme_bw() 
lwr_half <- ggplot(lower,
                   aes(reorder(species,species_exposure),
                       species_exposure)) +
  geom_point(size=4) +
  coord_flip() +
  xlab("") + ylab("Plastic exposure risk score") +
  scale_y_continuous(limits = c(0,575),expand = c(0,0))+
  theme(axis.ticks = element_blank())+
  theme_bw() 
plot_grid(upr_half,lwr_half)

#add the population level scores ####

head(pops)
max(pops$population_exposure)

pops$species_label <- species$species_label[
                          match(pops$species,
                                species$species)]
pops_upper <- subset(pops,species %in% upper$species)
pops_lower <- subset(pops,species %in% lower$species)

#add in dashed line for the score if plastic was evenly distributed
#read in plastics data
plastics <- raster::raster("outputs/00_PlasticsRaster.tif")
plastics[is.na(plastics)] <- 0 
p_sum1 <- plastics/sum(raster::getValues(plastics))
p_sum1_mean <- p_sum1
p_sum1_mean[!is.na(plastics)] <- mean(raster::getValues(p_sum1))
exp <- p_sum1_mean*p_sum1
mean_plastic_score <- sum(raster::getValues(exp))*1000000
mean_plastic_score

#how many species above this?
above_mean <- subset(species,species_exposure > mean_plastic_score)
above_mean
table(above_mean$iucn) 

upr_half <- ggplot(upper, aes(reorder(species_label,
                                      species_exposure),
                                      species_exposure)) +
  geom_point(size=6) +
  geom_point(aes(species_label,population_exposure),
             data = pops_upper,
             size=7,alpha=0.2,shape=18) +
  geom_hline(yintercept = mean_plastic_score, 
             linetype='dashed')+
  coord_flip() +
  xlab("") + ylab("Plastic exposure risk score") +
  scale_y_continuous(limits = c(0,700),expand = c(0,0))+
  theme(axis.ticks = element_blank())+
  theme_bw()+ 
  theme(text=element_text(size = 25),
        axis.text = element_text(colour="black"));upr_half

lwr_half <- ggplot(lower, aes(reorder(species_label,
                                      species_exposure),
                                      species_exposure)) +
  geom_point(size=6) +
  geom_point(aes(species_label,population_exposure),
             data = pops_lower,
             size=7,alpha=0.2,shape=18) +
  geom_hline(yintercept = mean_plastic_score, 
             linetype='dashed')+
  coord_flip() +
  xlab("") + ylab("Plastic exposure risk score") +
  scale_y_continuous(limits = c(0,700),expand = c(0,0))+
  theme(axis.ticks = element_blank())+
  theme_bw() + 
  theme(text=element_text(size = 25),
        axis.text = element_text(colour="black"))
plot_cols <- plot_grid(upr_half,lwr_half)

png("outputs/10_species_exposure_scores.png", 
    width=2000,height=1125)
par(mfrow=c(1,1))
plot_cols
dev.off()
dev.off()

#plot as svg
upr_half <- ggplot(upper, aes(reorder(species_label,
                                      species_exposure),
                              species_exposure)) +
  geom_point() +
  geom_point(aes(species_label,population_exposure),
             data = pops_upper,
             alpha=0.2,shape=18,size=3) +
  geom_hline(yintercept = mean_plastic_score, 
             linetype='dashed')+
  coord_flip() +
  xlab("") + ylab("Plastic exposure risk score") +
  scale_y_continuous(limits = c(0,700),expand = c(0,0))+
  theme(axis.ticks = element_blank())+
  theme_bw()+ 
  theme(axis.text = element_text(colour="black"));upr_half

lwr_half <- ggplot(lower, aes(reorder(species_label,
                                      species_exposure),
                              species_exposure)) +
  geom_point() +
  geom_point(aes(species_label,population_exposure),
             data = pops_lower,
             alpha=0.2,shape=18, size = 3) +
  geom_hline(yintercept = mean_plastic_score, 
             linetype='dashed')+
  coord_flip() +
  xlab("") + ylab("Plastic exposure risk score") +
  scale_y_continuous(limits = c(0,700),expand = c(0,0))+
  theme(axis.ticks = element_blank())+
  theme_bw() + 
  theme(axis.text = element_text(colour="black"))
plot_cols <- plot_grid(upr_half,lwr_half)

ggsave(filename = "outputs/10_species_exposure_scores.svg",
       plot = plot_cols, 
       width = 4000, height = 2250, unit = "px")  


# plot iucn red list categories ####
head(species)

iucn <- species %>% 
  group_by(iucn) %>%
  summarise(sum = sum(species_exposure),
            mean = mean(species_exposure),
            n = n()) %>%
  data.frame();iucn

iucn$Exposure <- iucn$sum/sum(iucn$sum)
iucn$Species <- iucn$n/sum(iucn$n)

iucn_bars <- pivot_longer(iucn, c(Species,Exposure),
                          names_to = "type", 
                          values_to = "prop") %>% 
  data.frame();iucn_bars

iucn_bars$type <- factor(iucn_bars$type,levels=c("Species","Exposure"))

all_iucn <- ggplot(iucn_bars,
                   aes(y=prop,x=type,label=iucn,
                       fill=factor(iucn,levels=c("CR","EN","VU","NT","LC")))) +
  geom_bar(position="stack", stat="identity") +
  scale_fill_manual(values = c("#A60808","#E22929","#EA6666","#F3A3A3",
                               "grey"))+
  theme(axis.ticks = element_blank())+
  theme_bw() + 
  theme(legend.position = "none")+
  labs(fill = "IUCN\nRed List\nCategory")+
  scale_y_continuous(labels = scales::percent,expand=c(0,0))+
  xlab("")+ylab("")+
  theme(plot.margin=unit(c(1,1,-1,-1),"cm"))+
  theme(text=element_text(size = 55,colour="black"));all_iucn

png("outputs/10_iucn_redlist.png", 
    width=700,height=2000)
all_iucn
dev.off()
dev.off()

all_iucn_svg <- ggplot(iucn_bars,
                   aes(y=prop,x=type,label=iucn,
                       fill=factor(iucn,levels=c("CR","EN","VU","NT","LC")))) +
  geom_bar(position="stack", stat="identity") +
  scale_fill_manual(values = c("#A60808","#E22929","#EA6666","#F3A3A3",
                               "grey"))+
  theme(axis.ticks = element_blank())+
  theme_bw() + 
  theme(legend.position = "none")+
  labs(fill = "IUCN\nRed List\nCategory")+
  scale_y_continuous(labels = scales::percent,expand=c(0,0))+
  xlab("")+ylab("")+
  #theme(plot.margin=unit(c(1,1,-1,-1),"cm"))+
  theme(text=element_text(colour="black"));all_iucn_svg

ggsave(filename = "outputs/10_iucn_redlist.svg",
       plot = all_iucn_svg, 
       width = 700, height = 2000, unit = "px") 

write.csv(iucn,"outputs/10_iucn_redlist.csv",
          row.names = F)
