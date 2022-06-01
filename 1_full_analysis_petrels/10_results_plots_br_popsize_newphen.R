
rm(list=ls()) 

library(tidyverse)
library(viridis)
library(cowplot)
library(ggtext)
se <- function(x) sqrt(var(x)/length(x))

## paste home directory here
dir <- "C:/Users/bethany.clark/OneDrive - BirdLife International/Methods"

seasons <- read.csv(paste0(dir,"/outputs/07_exposure_scores_by_season.csv"))
pops <- read.csv(paste0(dir,"/outputs/05_exposure_scores_by_population.csv"))
species <- read.csv(paste0(dir,"/outputs/08_exposure_scores_by_species"))

head(seasons)

dir_1by1 <- paste0(dir,"/scripts_results/04_aggregate_1by1_grid_br_p2")


#add common name & IUCN & pop size weighting
pops <- unlist(str_split(dat$species_pop,"_"))
dat$species <- pops[seq(1,length(pops),by=2)]
dat$pop <- pops[seq(2,length(pops),by=2)]
dat$pop_size_weight <- pop_size_weight$weighting
head(pop_size_weight)
head(dat)

names <- read.csv(paste0(homedir,"/input_data/Species_list_IUCN.csv"))
head(names)

dat$common_name <- names$Common.name[match(dat$species,names$Scientific.name)]
dat$iucn <- names$X2019.IUCN.Red.List.Category[match(dat$species,names$Scientific.name)]
head(dat)

dat$sci_name_pop <- dat$species_pop
dat$species_pop <- paste0(dat$common_name,", ",dat$pop)
dat$sci_name <- dat$species
dat$species <- dat$common_name

dat$season_diff_raw <- dat$mean_br - dat$mean_nonbr
dat$season_diff <- abs(dat$mean_br - dat$mean_nonbr)
summary(dat)

head(dat)
dat$n_months <- dat$br_n + dat$nonbr_n

#save for supplementary materials

dat_sup <- dat %>%
  select(c(iucn,sci_name_pop,br_n,nonbr_n,
             over_val_br,over_val_nonbr,species_pop,season_diff,
             season_diff_raw,n_months)) %>%
  data.frame();head(dat_sup)

dat_sup1 <- dat %>%
  select( c(common_name,sci_name,pop,
            yr_overval,breeding,nonbreeding,mean_br,mean_nonbr)) %>%
  data.frame();head(dat_sup1)

write.csv(dat_sup1, paste0(dir,"/scripts_results/supplementary_csvs/pops_seasons.csv"),
          row.names = F)

dat1 <- pivot_longer(dat, c(br_n, nonbr_n),
                     names_to = "season", 
                     values_to = "n_season") %>% 
  data.frame();head(dat1)

dat2 <- pivot_longer(dat, c(mean_br, mean_nonbr),
                     names_to = "season", 
                     values_to = "over_val") %>% 
  data.frame();head(dat2)

dat3 <- pivot_longer(dat, c(breeding, nonbreeding),
                     names_to = "season", 
                     values_to = "months") %>% 
  data.frame();head(dat3)

dat <- dat1 
dat$months <- dat3$months
dat$season_over_val <- dat2$over_val

head(dat)


#remove non breeding seasons that don't exist
dat <- subset(dat, !is.na(dat$months)) #

hist(dat$season_over_val)

unique(dat$species_pop)

#correct population names for plotting
head(dat)
dat$species_pop <-  ifelse(dat$species_pop == "Pink-footed Shearwater, Isla Mocha and Juan Fernandez",
                           "Pink-footed Shearwater, Isla Mocha & Juan Fernandez",
                           dat$species_pop)
dat$species_pop <-  ifelse(dat$species_pop == "Black-capped Petrel, Sierra de Bahoruco"  ,
                           "Black-capped Petrel, Dominican Republic"  ,
                           dat$species_pop)
dat$species_pop <-  ifelse(dat$species_pop ==  "Mottled Petrel, Lake HaurokoPetrel Island",
                           "Mottled Petrel, Lake Hauroko Petrel Island",
                           dat$species_pop)
dat$species_pop <-  ifelse(dat$species_pop ==  "Wedge-tailed Shearwater, New Calendonia" ,
                           "Wedge-tailed Shearwater, New Caledonia" ,
                           dat$species_pop)
dat$species_pop <-  ifelse(dat$species_pop ==  "Bulwer's Petrel, Cape Verde"  ,
                           "Bulwer's Petrel, Cabo Verde"   ,
                           dat$species_pop)
dat$species_pop <-  ifelse(dat$species_pop ==  "Cape Verde Shearwater, Cape Verde"   ,
                           "Cape Verde Shearwater, Cabo Verde"   ,
                           dat$species_pop)
dat$species_pop <-  ifelse(dat$species_pop ==  "Cape Verde Storm-petrel, Cape Verde"   ,
                           "Cape Verde Storm-petrel, Cabo Verde"   ,
                           dat$species_pop)
dat$species_pop <-  ifelse(dat$species_pop ==  "Tahiti Petrel, New Calendonia"   ,
                           "Tahiti Petrel, New Caledonia"     ,
                           dat$species_pop)
dat$species_pop <-  ifelse(dat$species_pop ==  "Audubon's Shearwater, Cape Verde"     ,
                           "Audubon's Shearwater, Cabo Verde"   ,
                           dat$species_pop)
dat$species_pop <-  ifelse(dat$species_pop ==  "Cape Verde Petrel, Cape Verde"   ,
                           "Cape Verde Petrel, Cabo Verde"   ,
                           dat$species_pop)

dat$species_pop <-  ifelse(dat$species_pop ==  "Hutton's Shearwater, Te Rae o Atiu"   ,
                           "Hutton's Shearwater, Kaikoura "   ,
                           dat$species_pop)
dat$species_pop <-  ifelse(dat$species_pop ==  "Grey-faced Petrel, Bethells Beach"     ,
                           "Grey-faced Petrel, Te Hanga/Bethells Beach" ,
                           dat$species_pop)
dat$species_pop <-  ifelse(dat$species_pop ==  "Sooty Shearwater, Codfish Island"       ,
                           "Sooty Shearwater, Codfish Island/Whenua Hou"  ,
                           dat$species_pop)
dat$species_pop <-  ifelse(dat$species_pop ==  "Cook's Petrel, Codfish Island"       ,
                           "Cook's Petrel, Codfish Island/Whenua Hou"  ,
                           dat$species_pop)
dat$species_pop <-  ifelse(dat$species_pop ==  "Sooty Shearwater, Falkland Islands"       ,
                           "Sooty Shearwater, Falkland Islands (Malvinas)"   ,
                           dat$species_pop)
dat$species_pop <-  ifelse(dat$species_pop ==  "Sooty Shearwater, Falkland Islands"       ,
                           "Sooty Shearwater, Falkland Islands (Malvinas)"   ,
                           dat$species_pop)


#split in half,
dat$half <- ifelse(dat$yr_overval < median(dat$yr_overval),"lwr","upr")

summary(dat)

#plot of all pops in order in 2 columns
upr_half <- ggplot(dat[dat$half=="upr",],
                   aes(reorder(species_pop,yr_overval),yr_overval)) +
  geom_point() +
  coord_flip() +
  xlab("") + ylab("Species mean overlap score") +
  scale_y_continuous(limits = c(0,710),expand = c(0,0)) +
  theme(axis.ticks = element_blank()) +
  theme_bw() + 
  theme(text=element_text(size = 20))
  #; upr_half
lwr_half <- ggplot(dat[dat$half=="lwr",],
                   aes(reorder(species_pop,yr_overval),yr_overval)) +
  geom_point() +
  coord_flip() +
  xlab("") + ylab("Species mean overlap score") +
  scale_y_continuous(limits = c(0,710),expand = c(0,0))+
  theme(axis.ticks = element_blank())+
  theme_bw() + 
  theme(text=element_text(size = 20))
plot_grid(upr_half,lwr_half)

png(paste0(dir,"/scripts_results/ranked_pops.png"), width=1600,height=1800)
par(mfrow=c(1,1))
plot_grid(upr_half,lwr_half)
dev.off()
dev.off()


head(dat)




table(df_season$season)

dat$sp_mean <- df_sp$mean[match(dat$species,df_sp$species)]
df_sp$iucn <- dat$iucn[match(df_sp$species,dat$species)]

table(df_sp$iucn)

df_sp$common_name <- df_sp$species
df_sp$species <- paste(df_sp$common_name,
                       df_sp$iucn,sep=" ")

#test for difference when including population weightings or not
just_multipops <- subset(df_sp,n != 1)
cor.test(just_multipops$mean,just_multipops$wmean)
plot(just_multipops$mean,just_multipops$wmean)
plot(just_multipops$mean,just_multipops$wmean, xlim = c(0,25),ylim = c(0,25))
#weighted mean very highly correlated with unweighted mean

write.csv(df_sp, paste0(dir,"/scripts_results/supplementary_csvs/species_scores.csv"),
          row.names = F)


df_sp$label_col <- ifelse(df_sp$iucn == "LC","black","#d41002")

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

# season ####

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
