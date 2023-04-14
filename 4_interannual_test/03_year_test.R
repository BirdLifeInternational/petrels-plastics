#Clear r workspace and load packages
rm(list=ls()) 

library(DHARMa)
library(tidyverse)
library(viridis)

#Set theme for plots
ms_theme <- theme_bw()+
  theme(text = element_text(size=18)) +
  theme(legend.text=element_text(size=15)) +
  theme(axis.text=element_text(colour="black"))

#Read in the data
years <- read.csv("outputs/02_exposure_scores_by_year.csv")
table(years$sp_pop)

#find all populations with matching months tracked in >3 different years

Ardenna_gravis_Gough  <- subset(years, sp_pop == "Ardenna gravis_Gough"&
                                  year %in% c(2009, 2010, 2011, 2013))
Calonectris_borealis_Canarias  <- subset(years, sp_pop == "Calonectris borealis_Canarias" &
                                         year %in% c(2008, 2009, 2010, 2011, 2012))
Calonectris_diomedea_Balearic_Archipelago <- subset(years, sp_pop == "Calonectris diomedea_Balearic Archipelago"&
                                                      year %in% c(2010, 2011, 2012, 2013, 2014))
Calonectris_edwardsii_Cape_Verde <- subset(years, sp_pop == "Calonectris edwardsii_Cape Verde"&
                                             year %in% c(2008, 2009, 2010, 2011, 2012, 2013, 2014))
Fulmarus_glacialis_Breidafjordur <- subset(years, sp_pop == "Fulmarus glacialis_Breidafjordur"&
                                             year %in% c(2015, 2016, 2017, 2018))
Fulmarus_glacialis_Norway <- subset(years, sp_pop == "Fulmarus glacialis_Norway"&
                                      year %in% c(2012, 2016, 2017, 2018))
Fulmarus_glacialis_Scotland  <- subset(years, sp_pop == "Fulmarus glacialis_Scotland" &
                                 year %in% c(2007, 2009, 2010, 2012, 2015, 2016, 2017, 2018))
Procellaria_cinerea_Kerguelen <- subset(years, sp_pop == "Procellaria cinerea_Kerguelen"&
                                          year %in% c(2008, 2009, 2010, 2011, 2012))
Pterodroma_deserta_Madeira <- subset(years, sp_pop == "Pterodroma deserta_Madeira"&
                                       year %in% c(2009, 2010, 2011, 2012))
Puffinus_lherminieri_Cape_Verde <- subset(years, sp_pop == "Puffinus lherminieri_Cape Verde" &
                                            year %in% c(2008, 2009, 2010, 2011, 2012, 2013))
Puffinus_puffinus_Heimaey <- subset(years, sp_pop == "Puffinus puffinus_Heimaey"&
                                      year %in% c(2007, 2008, 2009, 2010, 2011, 2012, 2013))
Puffinus_puffinus_UK <- subset(years, sp_pop == "Puffinus puffinus_UK" &
                                 year %in% c(2007, 2008, 2009, 2010, 2011))
Puffinus_yelkouan_Iles_Hyeres <- subset(years, sp_pop == "Puffinus yelkouan_Iles Hyeres"&
                                          year %in% c(2007, 2008, 2009, 2012))


multiyear <- rbind(Ardenna_gravis_Gough, 
                   Calonectris_borealis_Canarias,
                   Calonectris_diomedea_Balearic_Archipelago,
                   Calonectris_edwardsii_Cape_Verde,
                   Fulmarus_glacialis_Breidafjordur,
                   Fulmarus_glacialis_Norway,
                   Fulmarus_glacialis_Scotland,
                   Procellaria_cinerea_Kerguelen,
                   Pterodroma_deserta_Madeira,
                   Puffinus_lherminieri_Cape_Verde,
                   Puffinus_puffinus_Heimaey,
                   Puffinus_puffinus_UK,
                   Puffinus_yelkouan_Iles_Hyeres)
sort(unique(multiyear$sp_pop))

high_scorers <- subset(multiyear,exposure_score > 100)
low_scorers <- subset(multiyear,exposure_score < 100)




high <- ggplot(data=high_scorers,aes(x = year, y= exposure_score, fill=sp_pop, colour=sp_pop)) + 
  geom_point(aes(size=4, 
                 alpha = 0.6))+
  geom_smooth(method=lm, level = 0)+
  scale_y_continuous(expand = c(0,0),limits = c(0, 700)) +
  scale_colour_manual(values = viridis(4, option = "inferno")[2:3])+
  scale_fill_manual(values = viridis(4, option = "inferno")[2:3])+
  ylab("Exposure Score")+xlab("Year")+
  ms_theme;high

low <- ggplot(data=low_scorers,aes(x = year, y= exposure_score, fill=sp_pop, colour=sp_pop)) + 
  scale_y_continuous(expand = c(0,0),limits = c(0, 12)) +
  geom_point(aes(size=4, 
                 alpha = 0.6))+
  geom_smooth(method=lm, level =0)+
  scale_colour_viridis_d(option = "inferno")+
  scale_fill_viridis_d(option = "inferno")+
  ylab("Exposure Score")+xlab("Year")+
  ms_theme;low

hist(multiyear$exposure_score)

#Run a generalised linear model with a Gamma distribution 
m1 <- glm(exposure_score ~ sp_pop + year, 
          family = Gamma,
          data = multiyear)
plot(simulateResiduals(m1))
drop1(m1, test = "Chisq")
summary(m1)

table(multiyear$sp_pop)

png("outputs/03_year_plot_high.png", 
    width=1400,height=900)
high
dev.off()
dev.off()

png("outputs/03_year_plot_low.png", 
    width=1400,height=900)
low
dev.off()
dev.off()

high_svg <- ggplot(data=high_scorers,aes(x = year, y= exposure_score, 
                                         fill=sp_pop, colour=sp_pop)) + 
  geom_point(aes(alpha = 0.6))+
  geom_smooth(method=lm, level = 0)+
  scale_y_continuous(expand = c(0,0),limits = c(0, 700)) +
  scale_colour_manual(values = viridis(4, option = "inferno")[2:3])+
  scale_fill_manual(values = viridis(4, option = "inferno")[2:3])+
  ylab("Exposure risk score")+xlab("Year")+
  ms_theme;high_svg

low_svg <- ggplot(data=low_scorers,aes(x = year, y= exposure_score, 
                                       fill=sp_pop, colour=sp_pop)) + 
  scale_y_continuous(expand = c(0,0),limits = c(0, 12)) +
  geom_point(aes(alpha = 0.6))+
  geom_smooth(method=lm, level =0)+
  scale_colour_viridis_d(option = "inferno")+
  scale_fill_viridis_d(option = "inferno")+
  ylab("Exposure risk score")+xlab("Year")+
  ms_theme;low_svg


ggsave(filename = "outputs/03_year_plot_high.svg",
       plot = high_svg, 
       width = 2800, height = 1800, unit = "px") 

ggsave(filename = "outputs/03_year_plot_low.svg",
       plot = low_svg, 
       width = 2800, height = 1800, unit = "px") 

write.csv(multiyear,"outputs/03_exposure_scores_by_year_multiyear.csv",
          row.names = F)
