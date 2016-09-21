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


# Read in raw dataframe
Morgan_ALL2 <- read.csv("J:/FIT_MARE/FIT-Atka/Atka_Maturity/AM_Maturity_Morgan_2016/R Work/Morgan_ALL2.csv")

# Filter data
df <- Morgan_ALL2%>%
  filter(Length.cm > 20 & Length.cm < 50)%>%
  filter(Mature == '0' | Mature == '1')%>%
  filter(Sex == '2')%>%
  filter(Area == 'Seguam')

# Calculate proportion of mature per cm
L_prop <- df%>%
  group_by(Length.cm, Mature)%>%
  summarize(number = n())%>%
  mutate(prop_mat = number/sum(number))

# Add rows for 26.0, 27.0, 28.0, 29.0
L_prop2 <- data.frame(Length.cm=c(26.0, 28.0, 29.0), Mature = c(1,1,1), number = c(0,0,0), prop_mat = c(0,0,0))

# Join
L_prop <- bind_rows(L_prop, L_prop2)

# Filter for only proportion of mature fish and reorder
L_prop_mat <- L_prop%>%
  filter(Mature == 1)%>%
  arrange(Length.cm)

# Plot that....
ggplot(L_prop_mat)+
  geom_point(aes(x = Length.cm, y = prop_mat))+
  mytheme

# Proportion mature by year
L_prop_yr <- df%>%
  group_by(Year, Length.cm, Mature)%>%
  summarize(number = n())%>%
  mutate(prop_mat = number/sum(number))

# Add rows for 26.0, 27.0, 28.0, 29.0
L_prop_yr2 <- data.frame(Year = c(2003, 2003, 2003, 2003, 2004, 2004, 2005), Length.cm=c(26.0, 28.0, 29.0, 30.0, 30.0, 31.0, 37.0), Mature = c(1,1,1,1,1,1,1), number = c(0,0,0,0,0,0,0), prop_mat = c(0,0,0,0,0,0,0))

# Join
L_prop_yr <- bind_rows(L_prop_yr, L_prop_yr2)

# Filter for only proportion of mature fish and reorder
L_prop_yr_mat <- L_prop_yr%>%
  filter(Mature == 1)%>%
  arrange(Year, Length.cm)

# Plot that....
ggplot(L_prop_yr_mat)+
  geom_point(aes(x = Length.cm, y = prop_mat))+
  mytheme+
  facet_wrap(~Year)

