#Prepare Data
#######################################################################################################
library(dplyr)
library(tidyr)
library(lubridate)
library(ggplot2)
library(RColorBrewer)
library(ggthemes)

# Stuff I like for plot look...(Jim I)
mytheme <- theme(panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank(), panel.grid.major.y = element_line(colour="grey80", linetype="dashed"))
mytheme <- mytheme + theme(text=element_text(size=12)) + theme(axis.title.x=element_text(size=16) ,axis.title.y=element_text(size=16))
mytheme <- mytheme + theme(panel.background = element_rect(fill="white"), panel.border = element_rect(colour="black",fill=NA,size=1.2))


# Read in raw dataframe (rdf) and survey raw dataframe (srdf)
rdf <- read.csv("J:/FIT_MARE/FIT-Atka/Atka_Maturity/AM_Maturity_Morgan_2016/Lengthweight-master_fishery data/atka_fish_age_export.csv",as.is=TRUE)
rdf$HAUL_OFFLOAD_DATE <- dmy(rdf$HAUL_OFFLOAD_DATE)
rdf$mo <- months(rdf$HAUL_OFFLOAD_DATE)
## Read in raw survey dataframe (rsdf) for bottom temp data
rsdf <- read.csv("J:/FIT_MARE/FIT-Atka/Atka_Maturity/AM_Maturity_Morgan_2016/R Work/Morgan_ALL2.csv")

# Start cleaning up rdf and name it df 
## select area, rename etc
df <- as.data.frame(filter(rdf,WEIGHT>0,NMFS_AREA==541)) %>% select(age=AGE,sex=SEX,Length=LENGTH,wt=WEIGHT,yr=YEAR,dt=HAUL_OFFLOAD_DATE, mo, biom, avg_bm, mean_bt_yr, mean_bt_all)
## Now narrow down to main data range to be less than 45 cm. Grouping my length means when transmute is called, it does it on that level.
df <- filter(df,Length>=30, Length<=45,wt<1.8, sex=="M" | sex=="F") %>% group_by(Length) %>% transmute(w_bar_L=mean(wt),wtstd = wt/w_bar_L, bmstd = biom/avg_bm, btstd = mean_bt_yr/mean_bt_all, wt,mo,yr,biom, avg_bm, mean_bt_yr, mean_bt_all, sex)


# Filter rsdf by Area
rsdf <- rsdf %>% filter(Area == 'Seguam')


#Plot Data
######################################################################################################

# Plot bottom temp by month all years combined and then facet wrap by year to see how much variation between month over the course of a year
ggplot(rsdf)+
  geom_boxplot(aes(x=factor(Month), y=BTavg))+
  mytheme+
  facet_wrap(~Year)


# Plot October bottom temp (BTavg) for years collected on survey: 2002, 2003, 2004, 2006, 2007, 2011, 2014
## Filter rsdf for data only collected in october
bt_oct <- rsdf %>% 
  filter(Month == '10')

ggplot(bt_oct)+
  geom_boxplot(aes(x=factor(Year), y=BTavg))+
  mytheme

# Plot 
  
  