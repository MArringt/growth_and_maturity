setwd('E:/R Work')

#load in full dataset from DB
rsdf <- read.csv("E:/R Work/Morgan_ALL2.csv")

#load packages
library(dplyr)
library(ggplot2)

# Stuff Jim I. likes for plot look...
mytheme <- theme(panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank(), panel.grid.major.y = element_line(colour="grey80", linetype="dashed"))
mytheme <- mytheme + theme(text=element_text(size=14)) + theme(axis.title.x=element_text(size=16) ,axis.title.y=element_text(size=16))
mytheme <- mytheme + theme(panel.background = element_rect(fill="white"), panel.border = element_rect(colour="black",fill=NA,size=1.2))

# Add biomass bm to 
bm_s <- data.frame(Year = c(1999, 2001, 2002, 2003, 2004, 2005, 2006, 2007, 2011, 2012, 2014, 2015), biom = (c(592790, 633790, 998810, 1176800, 1315000, 1169600, 1030700, 928960, 657100, 613690, 581180, 589050)))
bm_s <- transmute(bm_s, avg_bm= mean(biom, na.rm = TRUE), Year, biom)
sdf <- left_join(rsdf, bm_s, by="Year")

#Add mean bottom temp mbt to sdf
mbt <- sdf%>%
  group_by(Year)%>%
  summarize(mean_bt_yr=mean(BTavg, na.rm=TRUE))
mbt <- transmute(mbt, mean_bt=mean(mean_bt_yr, na.rm=TRUE), Year, mean_bt_yr)
sdf <- left_join(sdf, mbt, by="Year")

#Add satellite chlorophyll and SST to dataset








# Now narrow down to main data range to be less than 45 cm and make new field 
sdf <- sdf%>%
  filter(Length.cm>=30, Length.cm<=45) %>%
  filter(Area == "Seguam")%>%
  group_by(Length.cm) %>% 
  transmute(w_bar_L=mean(Weight),wtstd = Weight/w_bar_L, bmstd = biom/avg_bm, btstd = mean_bt_yr/mean_bt, Weight,Month,Year,biom, avg_bm, BTavg)

# Look by year
ggplot(sdf) + geom_boxplot(aes(x=as.factor(Year),y=wtstd,fill=as.factor(Year))) + ylim(c(0,2)) + xlab('Year') + ylab('Standardized weight (30-45 cm)') + mytheme
# Look by length
ggplot(sdf) + geom_boxplot(aes(x=as.factor(Length.cm),y=wtstd,fill=as.factor(Length.cm))) + ylim(c(0,2)) + xlab('Length (cm)') + ylab('Standardized weight (30-45 cm)') + mytheme
# Look by month (but order messed up) *added code to reorder months, need help with axis ticks
ggplot(sdf) + geom_boxplot(aes(x=factor(Month),y=wtstd,fill=factor(Month))) + ylim(c(0,2)) + xlab('Month') + ylab('Standardized weight (30-45 cm)') + mytheme
ggplot(sdf) + geom_boxplot(aes(x=factor(Month),y=wtstd,fill=factor(Month))) + ylim(c(0,2)) + xlab('Month') + ylab('Standardized weight (30-45 cm)') + mytheme +facet_wrap(~Year)

#Plot bm on stdwt by year
ggplot(sdf) +
  geom_boxplot(aes(x=as.factor(Year),y=wtstd,fill=as.factor(Year))) + 
  ylim(c(0,2)) +
  xlab('Year') + 
  ylab('Standardized weight (30-45 cm)') +
  geom_line(data=sdf, aes(x=as.factor(Year), y=bmstd, group=1, color="Standardized Biomass"))+
  scale_color_manual(values = "black", name=" ")+ 
  guides(fill=guide_legend(title=NULL))+
  mytheme

# Plot standardized bottom temperature on standardized weight by year
ggplot(sdf) +
  geom_boxplot(aes(x=as.factor(Year),y=wtstd,fill=as.factor(Year))) + 
  ylim(c(0,2)) +
  xlab('Year') + 
  ylab('Standardized weight (30-45 cm)') +
  geom_line(data=sdf, aes(x=as.factor(Year), y=btstd, group=1, color="Standardized Bottom Temp"))+
  scale_color_manual(values = "black", name=" ")+ 
  guides(fill=guide_legend(title=NULL))+
  mytheme

# Plot wtstd for october
## filter by october
sdf_oct <- sdf %>% filter(Month == 10) %>%
  group_by(Year)%>%
  mutate(bt_oct=median(BTavg, na.rm=T))


ggplot(sdf_oct)+
  geom_boxplot(aes(x=factor(Year), y=wtstd))+
  ylim(c(0,2))+ 
  xlab('Year')+ 
  ylab('Standardized weight by sex (30-45 cm)')+
  mytheme

## add diverging average bottom temperature in october per year
ggplot(sdf_oct)+
  geom_boxplot(aes(x=factor(Year), y=wtstd, fill=factor(bt_oct)))+
  scale_fill_brewer(palette = "RdYlBu", direction = -1)+
  ylim(c(0,2))+ 
  xlab('Year')+ 
  ylab('Standardized weight by sex (30-45 cm)')+
  mytheme
