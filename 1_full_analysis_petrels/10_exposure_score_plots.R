## Read in exposure scores grouped by population, season
## and species, and plot results
## Beth Clark 2022

rm(list=ls()) 

library(tidyverse)
library(viridis)
library(cowplot)
library(ggtext)
se <- function(x) sqrt(var(x)/length(x))

## paste home directory here
dir <- "C:/Users/bethany.clark/OneDrive - BirdLife International/Methods"

pops <- read.csv(paste0(dir,"/outputs/05_exposure_scores_by_population.csv"))
seasons <- read.csv(paste0(dir,"/outputs/07_exposure_scores_by_season.csv"))
species <- read.csv(paste0(dir,"/outputs/08_exposure_scores_by_species.csv"))

#add common name & IUCN
names_iucn <- read.csv(paste0(dir,"/input_data/Species_list_IUCN.csv"))
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
seasons_plot <- pivot_longer(seasons, c(br_exposure, nonbr_exposure),
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

dat$half_s <- ifelse(dat$sp_mean < median(dat$sp_mean),"lwr","upr")
table(dat$half)
#should be upper
upp <- c("Fork-tailed Storm-petrel")
dat$half_s <- ifelse(dat$species %in% upp,"upr",dat$half_s)




upr_seasons <- ggplot(dat[dat$half_s=="upr",],aes(reorder(species, sp_mean),season_over_val,
                                                  season_over_val,colour=season)) +
  theme_bw()+
  geom_point() +
  coord_flip() +
  xlab("") +
  scale_y_continuous(limits = c(0,990),expand = c(0,0))+
  scale_colour_viridis(discrete=T)+
  theme(legend.position = "none")

lwr_seasons <- ggplot(dat[dat$half_s=="lwr",],aes(reorder(species, sp_mean),season_over_val,
                                                  season_over_val,colour=season)) +
  theme_bw()+
  geom_point() +
  coord_flip() +
  xlab("") +
  scale_y_continuous(limits = c(0,990),expand = c(0,0))+
  scale_colour_viridis(discrete=T)+
  theme(legend.position = "none")
plot_grid(upr_seasons,lwr_seasons)
summary(factor(dat$half_s))

upr_seasons

dat_allseasons <- drop_na(dat)
allseasons <- subset(dat,species_pop %in% dat_allseasons$species_pop)
head(allseasons)


seasons <- ggplot(allseasons,aes(reorder(species, sp_mean),season_over_val,
                                 season_over_val,colour=season)) +
  theme_bw()+
  geom_point() +
  coord_flip() +
  xlab("") +
  scale_y_continuous(limits = c(0,1000),expand = c(0,0))+
  scale_colour_manual(values = c("#fcba03","#8b00d6"))+
  theme(legend.position = "none");seasons

head(allseasons)

allseasons$half <- ifelse(allseasons$sp_mean < median(dat$sp_mean),"lwr","upr")
table(dat$half)
#should be upper
upp <- c("Calonectris edwardsii",
         "Pelecanoides urinatrix", 
         "Pterodroma macroptera")
dat$half_s <- ifelse(dat$species %in% upp,"upr",dat$half_s)
table(dat$half_s)


seasons_upr <- ggplot(allseasons[allseasons$half=="upr",],aes(reorder(species, sp_mean),season_over_val,
                                                              season_over_val,colour=season)) +
  theme_bw()+
  geom_point() +
  coord_flip() +
  xlab("") +
  scale_y_continuous(limits = c(0,600),expand = c(0,0))+
  scale_colour_manual(values = c("#fcba03","#8b00d6"))+
  theme(legend.position = "none");seasons
seasons_lwr <- ggplot(allseasons[allseasons$half=="lwr",],aes(reorder(species, sp_mean),season_over_val,
                                                              season_over_val,colour=season)) +
  theme_bw()+
  geom_point() +
  coord_flip() +
  xlab("") +
  scale_y_continuous(limits = c(0,600),expand = c(0,0))+
  scale_colour_manual(values = c("#fcba03","#8b00d6"))+
  theme(legend.position = "none");seasons
plot_grid(seasons_upr,seasons_lwr)

#just biggest season diffs ####
abovex <- subset(dat,season_diff > 23.3)
nrow(abovex)

abovex$species_pop <- ifelse(abovex$species_pop == "Scopoli's Shearwater, La Maddalena Archipelago",
                             "Scopoli's Shearwater, La Maddalena",abovex$species_pop)

abovex$species_pop <- paste0(abovex$species_pop," ")

seasons <- ggplot(abovex,aes(reorder(species_pop, season_diff),season_over_val,
                             season_over_val,group=species_pop)) +
  theme_bw()+
  coord_flip() +
  xlab("") +
  #scale_y_continuous(limits = c(0,600),expand = c(0,0))+
  scale_colour_manual(values = c("grey","black"))+
  geom_line(colour="#696969",size=2)+   geom_point(aes(colour=season),size=8) +
  theme(legend.position = "none")+
  ylab("\nSeason-specific plastic encounter risk")+ 
  theme(text=element_text(size = 42),
        axis.text = element_text(colour="black"));seasons


png(paste0(dir,"/scripts_results/season_diffs_tall.png"), 
    width=1350,height=1840) 
par(mfrow=c(1,1))
seasons
dev.off()
dev.off()

head(allseasons)
nrow(allseasons)
166/2

count <- subset(allseasons,season == "br_n")
nrow(count)
length(unique(count$species))

count$dir <- ifelse(count$season_diff_raw > 5, "high","mid")
count$dir <- ifelse(count$season_diff_raw < -5, "low",count$dir)

table(count$dir)
745869

#plot species scores ####
head(species)
species$species_label <- paste0(species$common_name,
                               " (",species$n_pops,",",
                               species$seasons,") ",
                               species$iucn)



#split in half for plotting
df_sp$half <- ifelse(df_sp$mean < median(df_sp$mean),"lwr","upr")
table(df_sp$half)

df_sp <- df_sp[order(-df_sp$mean),]

upper <- df_sp[df_sp$half=="upr",]
lower <- df_sp[df_sp$half=="lwr",]

upr_half <- ggplot(upper,
                   aes(reorder(species,mean),mean)) +
  geom_point(size=4) +
  #geom_errorbar(aes(min = lwr,max=upr),width=0) +
  coord_flip() +
  xlab("") + ylab("Species mean overlap score") +
  scale_y_continuous(limits = c(0,575),expand = c(0,0))+
  theme(axis.ticks = element_blank())+
  theme_bw() + 
  theme(text=element_text(size = 25))#+
  #theme(axis.text.y = element_markdown(colour = upper$label_col))
lwr_half <- ggplot(lower,
                   aes(reorder(species,mean),mean)) +
  geom_point(size=4) +
  #geom_errorbar(aes(min = lwr,max=upr),width=0) +
  coord_flip() +
  xlab("") + ylab("Species mean overlap score") +
  scale_y_continuous(limits = c(0,575),expand = c(0,0))+
  theme(axis.ticks = element_blank())+
  theme_bw() + 
  theme(text=element_text(size = 25))#+
  #theme(axis.text.y = element_markdown(colour = lower$label_col))
plot_grid(upr_half,lwr_half)

png(paste0(dir,"/scripts_results/ranked_spp.png"), 
    width=2000,height=1125)
par(mfrow=c(1,1))
plot_grid(upr_half,lwr_half)
dev.off()
dev.off()

dat$sp_iucn <- paste(dat$species,dat$iucn,sep=" ")

dat_upper <- dat[dat$species %in% upper$common_name,]
dat_lower <- dat[dat$species %in% lower$common_name,]


# iucn ####

df_sp$rank <- 77:1

iucn <- df_sp %>% 
  group_by(iucn) %>%
  summarise(sum = sum(mean),
            mean = mean(mean),
            rank = sum(rank),
            n = n(),
            allsp_mean = mean(df_sp$mean),
            allsp_rank = sum(df_sp$rank)) %>%
  data.frame();iucn

df_sp$lc_t <- ifelse(df_sp$iucn == "LC","LC","T")
iucn$Exposure <- iucn$sum/sum(iucn$sum)
iucn$Species <- iucn$n/sum(iucn$n)
iucn$rank_prop <- iucn$rank/sum(iucn$rank)

lc_t<- df_sp %>% 
  group_by(lc_t) %>%
  summarise(mean = mean(mean),
            allsp_mean = mean(df_sp$mean)) %>%
  data.frame();lc_t

iucn_bars <- pivot_longer(iucn, c(Species,Exposure),
                     names_to = "type", 
                     values_to = "prop") %>% 
  data.frame();iucn_bars

iucn_bars$type <- factor(iucn_bars$type,levels=c("Species","Exposure"))

iucn_means <- dat %>% 
  group_by(iucn) %>%
  summarise(mean = mean(sp_mean),
            allsp_mean = mean(dat$sp_mean)) %>%
  data.frame();iucn_means

all_iucn <- ggplot(iucn_bars,
                   aes(y=prop,x=type,label=iucn,
                       fill=factor(iucn,levels=c("CR","EN","VU","NT","LC")))) +
  geom_bar(position="stack", stat="identity") +
  #geom_label()+
  #geom_text(position = position_stack(vjust = 0.5),
  #          size=22,col="white") +
  scale_fill_manual(values = c("#A60808","#E22929","#EA6666","#F3A3A3",
                               "grey"))+
  theme(axis.ticks = element_blank())+
  theme_bw() + 
  #theme(legend.position = "none")+
  labs(fill = "IUCN\nRed List\nCategory")+
  scale_y_continuous(labels = scales::percent,expand=c(0,0))+
  xlab("")+ylab("")+
  theme(plot.margin=unit(c(1,1,-1,-1),"cm"))+
  theme(text=element_text(size = 55,colour="black"));all_iucn#+



png(paste0(dir,"/scripts_results/iucn.png"), 
    width=700,height=2000)
all_iucn
dev.off()
dev.off()

#theme(axis.text.y = element_markdown(colour = lower$label_col))
plot_grid(upr_half,lwr_half)



#species + pop scores ####
upr_half <- ggplot(upper, aes(reorder(species,mean),mean)) +
  geom_point(size=6) +
  geom_point(aes(sp_iucn,yr_overval),data = dat_upper,
             size=7,alpha=0.2,shape=18) +
  #geom_errorbar(aes(min = lwr,max=upr),width=0) +
  coord_flip() +
  xlab("") + ylab("Species mean overlap score") +
  scale_y_continuous(limits = c(0,700),expand = c(0,0))+
  theme(axis.ticks = element_blank())+
  theme_bw()+ 
  theme(text=element_text(size = 25))#+
  #theme(axis.text.y = element_markdown(colour = upper$label_col))
lwr_half <- ggplot(lower, aes(reorder(species,mean),mean)) +
  geom_point(size=6) +
  geom_point(aes(sp_iucn,yr_overval),data = dat_lower,
             size=7,alpha=0.2,shape=18) +
  #geom_errorbar(aes(min = lwr,max=upr),width=0) +
  coord_flip() +
  xlab("") + ylab("Species mean overlap score") +
  scale_y_continuous(limits = c(0,700),expand = c(0,0))+
  theme(axis.ticks = element_blank())+
  theme_bw() + 
  theme(text=element_text(size = 25))#+
#theme(axis.text.y = element_markdown(colour = lower$label_col))
plot_grid(upr_half,lwr_half)

png(paste0(dir,"/scripts_results/ranked_spp_pops.png"), 
    width=2000,height=1125)
par(mfrow=c(1,1))
plot_grid(upr_half,lwr_half)
dev.off()
dev.off()



write.csv(df_sp, paste0(dir,"/scripts_results/supplementary_csvs/species_scores.csv"),
          row.names = F)
