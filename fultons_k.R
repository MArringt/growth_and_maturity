#load in full dataset from DB
Morgan_ALL2 <- read.csv("J:/FIT_MARE/FIT-Atka/Atka_Maturity/AM_Maturity_Morgan_2016/R Work/Morgan_ALL2.csv")

#load packages
library(dplyr)
library(ggplot2)

#calcuate K for each fish, need to  write an if/else to calculate K to add to data.frame as a new column
Fultons_k <- function(m){
  l <- m$Length.cm
  w <- m$Weight
  ifelse(l > 1 & w > 1, w / l^3 * 100, print(N/A))
} 

#apply this function to Morgan_ALL
fultons_k <- Fultons_k(Morgan_ALL2)

#bind new column to original dataset
Morgan_ALL2 <- cbind(Morgan_ALL2, fultons_k) #dataset first, column to add second. SUCCESS

#build dataframe with CruiseID, SpecNumber, Weight, Length.cm, fultons_k
lk_data <- Morgan_ALL2%>%
  select(CruiseID, SpecNumber, Weight, Length.cm, fultons_k)%>%
  filter(fultons_k < 3)

#scatter plot of K vs Length
ggplot(lk_data, aes(Length.cm, fultons_k))+
  geom_point()

####scatter plot of K vs temp in Seguam in September and October over years
###filter data by Seguam, September and October
temp_k <- Morgan_ALL2%>%
  filter(Area == "Seguam", Month == "9" | Month == 10, BTavg > 0, fultons_k > 0)

##scatter plot
ggplot(temp_k, aes(BTavg, fultons_k))+
  geom_point()+
  aes(fill = factor(Year), color = factor(Year))+
  scale_color_manual(values= c("aquamarine2", "navy", "brown2", "cornflowerblue", "purple2", "darkolivegreen4", "goldenrod"))+


##Fultons_k by year in a boxplot
ggplot(temp_k, aes(factor(Year), fultons_k))+
  geom_boxplot()+
  facet_grid(~Sex)

##Fultons_k by year in a boxplot with avg bottom temp gradient
#avg bottom temp by year from all hauls in Seguam in months 9 and 10
year_avg_bt <- temp_k%>%
  group_by(Year)%>%
  summarize(avg_bt_peryear = mean(BTavg))

#left join
test<-left_join(temp_k, year_avg_bt, by=c("Year"))

#scatter plot
ggplot(test, aes(factor(Year), fultons_k, fill= factor(avg_bt_peryear)))+
  geom_boxplot()+
  scale_fill_brewer(palette = "RdYlBu", direction = -1)+
  facet_grid(~Sex)

#something to try
scale_color_gradient(low="#c6dbef", high="#08306b")+

