rm(list=ls()) 

library(tidyverse)
library(viridis)
library(cowplot)
se <- function(x) sqrt(var(x)/length(x))

dir <- paste0("C:/Users/bethany.clark/OneDrive - BirdLife International/",
              "Methods") ## copy and paste here your working directory

dir_1by1 <- paste0(dir,"/scripts_results/04_aggregate_1by1_grid_br")

dat_all <- read.csv(paste0(dir_1by1, "/results_rasters_br.csv"))
names(dat_all)[9] <-  "mean_nonbr"

dat <- subset(dat_all, species_pop != "Ardenna pacifica_Lowendal & Houtman Abrolhoss")



dat$season_diff_raw <- dat$mean_br - dat$mean_nonbr
dat$season_diff <- abs(dat$mean_br - dat$mean_nonbr)




summary(dat)

head(dat)
dat$n_months <- dat$br_n + dat$nonbr_n
dat$yr_overval <- ((dat$br_n*dat$mean_br) + 
                  (dat$nonbr_n*dat$mean_nonbr))/dat$n_months
dat$yr_overval <- ifelse(is.na(dat$yr_overval),dat$mean_br,dat$yr_overval )

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

dat <- dat1 %>% dplyr::select(species_pop,season,n_months,n_season,
                       yr_overval,season_diff,season_diff_raw)
dat$months <- dat3$months
dat$over_val <- dat2$over_val

head(dat)

#remove non breeding seasons that don't exist
dat <- subset(dat, !is.na(dat$months)) #not doing it!!!!!!!!!!1

hist(dat$over_val)


#split in half,
dat$half <- ifelse(dat$yr_overval < median(dat$yr_overval),"lwr","upr")

summary(dat)

#plot of all pops in order in 2 columns
upr_half <- ggplot(dat[dat$half=="upr",],
                   aes(reorder(species_pop,yr_overval),yr_overval)) +
  geom_point() +
  coord_flip() +
  xlab("") + ylab("Species mean overlap score") +
  scale_y_continuous(limits = c(0,610),expand = c(0,0))+
  theme(axis.ticks = element_blank())+
  theme_bw()#; upr_half
lwr_half <- ggplot(dat[dat$half=="lwr",],
                   aes(reorder(species_pop,yr_overval),yr_overval)) +
  geom_point() +
  coord_flip() +
  xlab("") + ylab("Species mean overlap score") +
  scale_y_continuous(limits = c(0,610),expand = c(0,0))+
  theme(axis.ticks = element_blank())+
  theme_bw()
plot_grid(upr_half,lwr_half)

dat$species <- sub(pattern="_.*",x=dat$species_pop,replacement = "")

df_sp <- dat %>% 
  group_by(species,season) %>%
  summarise(mean = mean(yr_overval),
            se = se(yr_overval),
            lwr = mean-se,
            upr = mean+se,
            s_mean = mean(over_val),
            s_se = se(over_val),
            s_lwr = s_mean-s_se,
            s_upr = s_mean+s_se) %>%
  data.frame();head(df_sp)
dat$sp_mean <- df_sp$mean[match(dat$species,df_sp$species)]

#split in half,
df_sp$half <- ifelse(df_sp$mean < median(df_sp$mean),"lwr","upr")

upr_half <- ggplot(df_sp[df_sp$half=="upr",],
                   aes(reorder(species,mean),mean)) +
  geom_point() +
  geom_errorbar(aes(min = lwr,max=upr),width=0) +
  coord_flip() +
  xlab("") + ylab("Species mean overlap score") +
  scale_y_continuous(limits = c(0,610),expand = c(0,0))+
  theme(axis.ticks = element_blank())+
  theme_bw()
lwr_half <- ggplot(df_sp[df_sp$half=="lwr",],
                   aes(reorder(species,mean),mean)) +
  geom_point() +
  geom_errorbar(aes(min = lwr,max=upr),width=0) +
  coord_flip() +
  xlab("") + ylab("Species mean overlap score") +
  scale_y_continuous(limits = c(0,610),expand = c(0,0))+
  theme(axis.ticks = element_blank())+
  theme_bw()
plot_grid(upr_half,lwr_half)

dat$half_s <- ifelse(dat$sp_mean < median(dat$sp_mean),"lwr","upr")
table(dat$half_s)
#should be upper
#upp <- c("Procellaria aequinoctialis", "Calonectris edwardsii", "Hydrobates castro",
#         "Puffinus gavia", "Pelecanoides urinatrix", "Hydrobates monteiroi")
#dat$half_s <- ifelse(dat$species %in% upp,"upr",dat$half_s)
table(dat$half_s)

upr_seasons <- ggplot(dat[dat$half_s=="upr",],aes(reorder(species, sp_mean),over_val,
                                                over_val,colour=season)) +
  theme_bw()+
  geom_point() +
  coord_flip() +
  xlab("") +
  scale_y_continuous(limits = c(0,600),expand = c(0,0))+
  scale_colour_viridis(discrete=T)+
  theme(legend.position = "none")

lwr_seasons <- ggplot(dat[dat$half_s=="lwr",],aes(reorder(species, sp_mean),over_val,
               over_val,colour=season)) +
  theme_bw()+
  geom_point() +
  coord_flip() +
  xlab("") +
  scale_y_continuous(limits = c(0,600),expand = c(0,0))+
  scale_colour_viridis(discrete=T)+
  theme(legend.position = "none")
plot_grid(upr_seasons,lwr_seasons)
summary(factor(dat$half_s))

upr_seasons

dat_allseasons <- drop_na(dat_all)
allseasons <- subset(dat,species_pop %in% dat_allseasons$species_pop)
head(allseasons)


seasons <- ggplot(allseasons,aes(reorder(species, sp_mean),over_val,
                                 over_val,colour=season)) +
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
upp <- c("Procellaria aequinoctialis",
         "Calonectris edwardsii",
         "Hydrobates castro",
         "Puffinus gavia",
         "Pelecanoides urinatrix",
         "Hydrobates monteiroi")
dat$half_s <- ifelse(dat$species %in% upp,"upr",dat$half_s)
table(dat$half_s)


seasons_upr <- ggplot(allseasons[allseasons$half=="upr",],aes(reorder(species, sp_mean),over_val,
                                 over_val,colour=season)) +
  theme_bw()+
  geom_point() +
  coord_flip() +
  xlab("") +
  scale_y_continuous(limits = c(0,600),expand = c(0,0))+
  scale_colour_manual(values = c("#fcba03","#8b00d6"))+
  theme(legend.position = "none");seasons
seasons_lwr <- ggplot(allseasons[allseasons$half=="lwr",],aes(reorder(species, sp_mean),over_val,
                                 over_val,colour=season)) +
  theme_bw()+
  geom_point() +
  coord_flip() +
  xlab("") +
  scale_y_continuous(limits = c(0,600),expand = c(0,0))+
  scale_colour_manual(values = c("#fcba03","#8b00d6"))+
  theme(legend.position = "none");seasons
plot_grid(seasons_upr,seasons_lwr)


abovex <- subset(dat,season_diff > 10)

seasons <- ggplot(abovex,aes(reorder(species_pop, season_diff),over_val,
                              over_val,group=species_pop)) +
  theme_bw()+
  coord_flip() +
  xlab("") +
  scale_y_continuous(limits = c(0,600),expand = c(0,0))+
  scale_colour_manual(values = c("#fcba03","#8b00d6"))+
  geom_line()+   geom_point(aes(colour=season),size=2) +
  theme(legend.position = "none");seasons

head(allseasons)
nrow(allseasons)
166/2

count <- subset(allseasons,season == "br_n")
nrow(count)
length(unique(count$species))

count$dir <- ifelse(count$season_diff_raw > 5, "high","mid")
count$dir <- ifelse(count$season_diff_raw < -5, "low",count$dir)

table(count$dir)
