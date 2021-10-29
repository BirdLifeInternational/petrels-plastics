rm(list=ls()) 

library(tidyverse)
library(viridis)
library(cowplot)
se <- function(x) sqrt(var(x)/length(x))

pop_sizes <- read.csv("C:/Users/bethany.clark/OneDrive - BirdLife International/Data/population_sizes.csv")
pop_sizes[c(1,6,8:10)] <- NULL
head(pop_sizes)

dir <- paste0("C:/Users/bethany.clark/OneDrive - BirdLife International/",
              "Methods") ## copy and paste here your working directory

dir_1by1 <- paste0(dir,"/scripts_results/04_aggregate_1by1_grid")

dat <- read.csv(paste0(dir_1by1, "/check-results_rasters_over.csv"))

range(dat$over_val) #check any 0s

#remove months with no plastics data - there are none now
dat <- subset(dat, over_val > 0)

hist(dat$over_val)

dat$month <- as.factor(dat$month)
summary(dat)

dat$species <- str_replace(dat$species,"_"," ")

dat$population <- as.character(dat$population)
dat$pop <- substr(dat$population,1,nchar(dat$population)-3)
head(dat)

species <- unique(dat$species)

pop_counts <- as.data.frame(species)
pop_counts$number <- NA

for(i in 1:length(species)){
  one_sp <- dat[dat$species == species[i],]
  unique_pops <- unique(one_sp$pop)
  pop_counts$number[i] <- length(unique_pops)
}

pops <- unique(dat$pop)

pop_means <- as.data.frame(pops)
pop_means$mean <- NA
pop_means$species <- NA
pop_means$n_months <- NA

for(i in 1:length(pops)){
  one_pop <- subset(dat,pop==pops[i])
  pop_means$species[i] <- one_pop$species[1]
  pop_means$n_months[i] <- nrow(one_pop)
}

for(i in 1:length(pops)){
  one_pop <- subset(dat,pop==pops[i])
  pop_means$mean[i] <- mean(one_pop$over_val)
}

pop_means$pop_num <- NA
pop_means$pop_num <- pop_counts$number[match(pop_means$species,
                                             pop_counts$species)]

pop_means$mean <- ifelse(pop_means$pop_num != 1 & pop_means$n_months != 12, NA, pop_means$mean)

df_sp <- pop_means %>% 
  group_by(species) %>%
  summarise(mean = mean(mean)) %>%
  data.frame()


df_pop <-  dat %>% 
  group_by(pop) %>%
  summarise(mean_overval = mean(over_val),
            se = se(over_val),
            lwr = mean_overval-se,
            upr = mean_overval+se,
            n = n(),
            species = species[1]) %>%
  data.frame()
dat$pop_mean <- df_pop$mean[match(dat$pop,df_pop$pop)]


df_sp <- df_pop %>% 
  group_by(species) %>%
  summarise(mean = mean(mean_overval),
            se = se(mean_overval),
            lwr = mean-se,
            upr = mean+se) %>%
  data.frame()
dat$sp_mean <- df_sp$mean[match(dat$species,df_sp$species)]

dat$multi_pop <- ifelse(dat$sp_mean == dat$pop_mean,"no","yes")

#split in half,
df_sp$half <- ifelse(df_sp$mean < median(df_sp$mean),"lwr","upr")



upr_half <- ggplot(df_sp[df_sp$half=="upr",],
                   aes(reorder(species,mean),mean)) +
  geom_point() +
  geom_errorbar(aes(min = lwr,max=upr),width=0) +
  coord_flip() +
  xlab("") + ylab("Species mean overlap score") +
  scale_y_continuous(limits = c(0,710),expand = c(0,0))+
  theme(axis.ticks = element_blank())+
  theme_bw()
lwr_half <- ggplot(df_sp[df_sp$half=="lwr",],
                   aes(reorder(species,mean),mean)) +
  geom_point() +
  geom_errorbar(aes(min = lwr,max=upr),width=0) +
  coord_flip() +
  xlab("") + ylab("Species mean overlap score") +
  scale_y_continuous(limits = c(0,710),expand = c(0,0))+
  theme(axis.ticks = element_blank())+
  theme_bw()
plot_grid(upr_half,lwr_half)

png(paste0(dir,"/scripts_results/species_scores.png"), 
    width=3800,height=2000, res=300)
par(mfrow=c(1,1))
plot_grid(upr_half,lwr_half)
dev.off()
#dev.off()

ggplot(dat,aes(reorder(pop,pop_mean),pop_mean)) +
  theme_bw()+
  geom_point() +
  coord_flip() +
  xlab("") +
  #scale_y_continuous(limits = c(0,710),expand = c(0,0))+
  scale_colour_viridis(discrete=T)+
  theme(axis.ticks = element_blank())

ggplot(dat,aes(reorder(species, sp_mean),over_val,
               over_val,colour=month)) +
  theme_bw()+
  geom_point() +
  coord_flip() +
  xlab("") +
  #scale_y_continuous(limits = c(0,600),expand = c(0,0))+
  scale_colour_viridis(discrete=T)

#mean(df_pop$mean[1:11])
#df_pop2 <- sort(df_pop$mean, decreasing = T)
#mean(df_pop2[1:11]) #med
#mean(df_pop2[12:77])
#mean(df_pop2)

multi_pops <- subset(dat,multi_pop == "yes")

ggplot(multi_pops,
       aes(pop,over_val,colour=month)) +
  theme_bw()+
  geom_point() +
  coord_flip() +
  xlab("") +
  scale_y_continuous(limits = c(0,950),expand = c(0,0))+
  scale_colour_viridis(discrete=T)+
  theme(axis.ticks = element_blank())

ggplot(multi_pops[multi_pops$species!="Calonectris diomedea"&
                  multi_pops$species!="Pterodroma cookii"&
                  multi_pops$species!="Puffinus yelkouan",],
       aes(pop,over_val,colour=month)) +
  theme_bw()+
  geom_point() +
  coord_flip() +
  xlab("") +
  scale_y_continuous(expand = c(0,0))+
  scale_colour_viridis(discrete=T)+
  theme(axis.ticks = element_blank())



