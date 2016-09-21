knitr::opts_chunk$set(echo = TRUE, error=FALSE, message=FALSE, warning=FALSE)
options(scipen=999) # remove scientific notation

# A very cursory Exploratory data analysis

# Load libraries

library(dplyr)
library(mgcv)
library(ggplot2)
theme_set(theme_bw(base_size=12, base_family='Times')+ 
            theme(panel.grid.major = element_blank(), 
                  panel.grid.minor = element_blank()))
library(lubridate)
library(car)

# Load and clean data

## Note: for joining dataframes I prefer to use code as opposed to Excel, in particular the merge function in base R or the _join commands in dplyr, particularly the left_join and full_join are handy - linky, linky http://stat545.com/bit001_dplyr-cheatsheet.html

# this is the most updated version of data sent to me 2016-9-13 from Morgan 
atka <- read.csv("J:/FIT_MARE/FIT-Atka/Atka_Maturity/AM_Maturity_Morgan_2016/R Work/ben_maturitydata.csv")
glimpse(atka)

atka %>% select(CruiseID, Haul, Area, Stratum, Date, EQLatDD,EQLongDD, AvgBtmDep,Sex,Length, Weight,OvaryWt, Age, Mature) %>% 
  #change names to reflect numeric (lower case) or factor (capitolized) and define data type
  transmute(Area = Area,
            ID = factor(paste0(CruiseID, Haul/10000)), 
            stratum = Stratum, 
            date = as.Date(Date, format='%m/%d/%Y'),
            year = as.numeric(format(date, "%Y")),
            Year = factor(year),
            lat = EQLatDD,
            lon = EQLongDD,
            btemp = AvgBtmDep,
            sex = Sex,
            Sex = factor(sex),
            length = ifelse(Length<150, Length,Length/10),
            weight = Weight,
            oweight = OvaryWt,
            age = Age,
            Age = factor(age),
            mature = Mature,
            Mature = factor(mature)) %>% filter(sex<3, weight<2000)-> dat
# Check for any data issues, any oddities should be doublechecked.

ggplot(dat, aes(length))+geom_histogram()


max(dat$length, na.rm=T)
## [1] 49
min(dat$length, na.rm=T)
## [1] 17.2
ggplot(dat, aes(weight))+geom_histogram()


max(dat$weight, na.rm=T) # I trimmed an obvious outlier (6000) in the data section
## [1] 1600
min(dat$weight, na.rm=T)
## [1] 72
ggplot(dat, aes(age))+geom_histogram()


max(dat$age, na.rm=T)
## [1] 13
min(dat$age, na.rm=T)
## [1] 1
ggplot(dat, aes(lat))+geom_histogram()


max(dat$lat, na.rm=T)
## [1] 52.95
min(dat$lat, na.rm=T)
## [1] 51.2545
ggplot(dat, aes(lon))+geom_histogram()


max(dat$lon, na.rm=T) #problem with longitude (should be negative values)
## [1] 179.412
min(dat$lon, na.rm=T)
## [1] -179.9337
#For the moment I’ll just adjust the longitude, but it should be doublechecked

dat %>% mutate(lon =ifelse(lon>0,-lon,lon)) -> dat
ggplot(dat, aes(lon))+geom_histogram()


# Filter data by sex

dat %>% filter(sex==2) -> fem
dat %>% filter(sex==1) -> mal
# Filter by location

fem %>% filter(Area=='Seguam') -> seg
# Get a feel for the data. Let the plotting begin.

# Examine some binomial GLMs - pay attention to the dataset being used

ggplot(fem, aes(length,mature))+geom_jitter(alpha=.2,height=.15)+stat_smooth(method='glm',method.args=list(family='binomial'), alpha=.15)+ylim(0,1)


ggplot(fem, aes(age, mature))+geom_jitter(alpha=.2, height=.15)+stat_smooth(method='glm', method.args=list(family='binomial'))+ylim(0,1)+xlim(0,15)


ggplot(fem, aes(weight, mature))+geom_jitter(alpha=.2, height=.15)+stat_smooth(method='glm', method.args=list(family='binomial'))+ylim(0,1)+xlim(0,1500)


# Examine some GAMs

ggplot(dat, aes(age, weight, color=Sex))+geom_jitter(alpha=.2)+
  stat_smooth(method='gam', formula=y~s(x, k=3))+ylim(0,1500)


### Interesting that male weight is greater until about 9 yrs. - likely to be a confounded by sampling a number of discrete areas?

ggplot(dat, aes(age, length, color=Sex))+geom_jitter(alpha=.2, height=.15)+stat_smooth(method='gam', formula=y~s(x, k=3))


ggplot(dat, aes(length, weight, color=Sex))+geom_jitter(alpha=.2, height=.15)+
  stat_smooth(method='gam', formula=y~s(x, k=3))+ylim(0,1500)


ggplot(dat, aes(Year, length, fill=Sex))+geom_boxplot(alpha=.3)


ggplot(dat, aes(Year, length, fill=Area))+geom_boxplot(alpha=.3)


ggplot(dat, aes(Year, weight, fill=Sex))+geom_boxplot(alpha=.3)


ggplot(fem, aes(length,weight,color=Year, fill=Year))+geom_jitter(width=.5, alpha=.15)+stat_smooth(alpha=.2)


# Make some models - only using Seguam data as it is most consistent over time.

seg %>% filter(length>0, weight>0, Mature!='NA') %>% mutate(dum=1) -> seglw
seglw <- droplevels(seglw)
# What does body condition look like?

bc <- gam(weight~s(length,k=4), data = seglw, family=Gamma(link='log'))
plot(predict(bc), resid(bc))
abline(h=0, lty=4, col=2)

seglw$w <- as.numeric(predict(bc, seglw, type='response'))
seglw %>% mutate(Kn = weight/w*100) ->seglw

ggplot(seglw, aes(Year, Kn))+geom_boxplot()+geom_hline(yintercept=100, lty=4)


# Look at maturity by length and maturity by length and Kn

m.length <- gam(Mature~s(length, k=4), data=seglw, family=binomial, gamma=1.4)

m.Kn <- gam(Mature~s(length, k=4)+s(Kn, k=3), data=seglw, family=binomial, gamma=1.4)
AIC(m.length, m.Kn)
##                df      AIC
## m.length 3.414412 841.5947
## m.Kn     5.307358 824.0759
plot(m.Kn, page=1)


summary(m.Kn)
## 
## Family: binomial 
## Link function: logit 
## 
## Formula:
## Mature ~ s(length, k = 4) + s(Kn, k = 3)
## 
## Parametric coefficients:
##             Estimate Std. Error z value            Pr(>|z|)    
## (Intercept)  1.41577    0.09754   14.52 <0.0000000000000002 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Approximate significance of smooth terms:
##             edf Ref.df Chi.sq              p-value    
## s(length) 2.448  2.774 180.48 < 0.0000000000000002 ***
## s(Kn)     1.859  1.980  19.19             0.000191 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## R-sq.(adj) =  0.316   Deviance explained = 26.8%
## UBRE = -0.16835  Scale est. = 1         n = 996
seglw$predl <- predict(m.length, data=seglw, type='response')
seglw$predKn <- predict(m.Kn, data=seglw, type='response')


ggplot(seglw, aes(length, mature))+geom_point()+geom_line(aes(length, predl))+
  stat_summary(aes(length,predKn), color=4, fun.y=mean, geom='line')


ggplot(seglw, aes(length, mature))+stat_smooth(method='gam', formula=y~s(x, k=3), method.args=list(family=binomial))


m1 <- gam(Mature~s(length, k=4)+te(lon,lat)+Age+Year+s(ID, bs='re', by=dum), data=seglw, family=binomial, gamma=1.4)
summary(m1)
## 
## Family: binomial 
## Link function: logit 
## 
## Formula:
## Mature ~ s(length, k = 4) + te(lon, lat) + Age + Year + s(ID, 
##     bs = "re", by = dum)
## 
## Parametric coefficients:
##                   Estimate     Std. Error z value Pr(>|z|)    
## (Intercept)      -50.06600 23726566.40124   0.000 0.999998    
## Age3              49.51473 23726566.40124   0.000 0.999998    
## Age4              51.00152 23726566.40124   0.000 0.999998    
## Age5              50.73234 23726566.40124   0.000 0.999998    
## Age6              50.84704 23726566.40124   0.000 0.999998    
## Age7              49.62922 23726566.40124   0.000 0.999998    
## Age8              95.33215 28772688.06208   0.000 0.999997    
## Age9              51.22602 23726566.40124   0.000 0.999998    
## Age10             50.83703 23726566.40124   0.000 0.999998    
## Age11             95.97045 34732136.82811   0.000 0.999998    
## Age12             96.00530 31832529.22506   0.000 0.999998    
## Age13             95.92875 71179699.22180   0.000 0.999999    
## Year2003           1.33935        0.34655   3.865 0.000111 ***
## Year2004           0.76645        0.32802   2.337 0.019459 *  
## Year2005           0.31885        1.03404   0.308 0.757814    
## Year2011          -0.04877        0.52081  -0.094 0.925386    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Approximate significance of smooth terms:
##               edf  Ref.df Chi.sq     p-value    
## s(length)   2.045   2.426 33.888 0.000000104 ***
## te(lon,lat) 3.000   3.000  1.263     0.73786    
## s(ID):dum   1.523 132.000  2.167     0.00194 ** 
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## R-sq.(adj) =  0.403   Deviance explained = 36.7%
## UBRE = -0.10244  Scale est. = 1         n = 648
plot(m1, page=1, shade=T)


location doesn’t look very important for these data, drop it out of the model

m1a <- gam(Mature~s(length, k=4)+Age+Year+s(ID, bs='re', by=dum), data=seglw, family=binomial, gamma=1.4)
summary(m1a)
## 
## Family: binomial 
## Link function: logit 
## 
## Formula:
## Mature ~ s(length, k = 4) + Age + Year + s(ID, bs = "re", by = dum)
## 
## Parametric coefficients:
##                  Estimate    Std. Error z value Pr(>|z|)    
## (Intercept)      -51.4137 23726566.4215   0.000  1.00000    
## Age3              50.8387 23726566.4215   0.000  1.00000    
## Age4              52.3644 23726566.4215   0.000  1.00000    
## Age5              52.1142 23726566.4215   0.000  1.00000    
## Age6              52.2586 23726566.4215   0.000  1.00000    
## Age7              51.0624 23726566.4215   0.000  1.00000    
## Age8              99.7185 28772687.4794   0.000  1.00000    
## Age9              52.6191 23726566.4215   0.000  1.00000    
## Age10             52.3098 23726566.4215   0.000  1.00000    
## Age11            100.3122 34732136.8414   0.000  1.00000    
## Age12            100.3721 31832529.2255   0.000  1.00000    
## Age13            100.5213 71179699.2233   0.000  1.00000    
## Year2003           1.2791        0.3374   3.791  0.00015 ***
## Year2004           0.7555        0.3219   2.347  0.01892 *  
## Year2005           0.1234        0.9855   0.125  0.90035    
## Year2011          -0.1999        0.4905  -0.408  0.68354    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Approximate significance of smooth terms:
##                edf  Ref.df Chi.sq     p-value    
## s(length) 2.061359   2.442 34.026 0.000000101 ***
## s(ID):dum 0.005448 135.000  0.008     0.00382 ** 
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## R-sq.(adj) =  0.402   Deviance explained = 36.1%
## UBRE = -0.11355  Scale est. = 1         n = 648
AIC(m1,m1a)
##           df      AIC
## m1  22.56843 563.5657
## m1a 18.06681 559.9648
plot(m1a, page=1, shade=T)


m2 <- gam(Mature~s(weight, k=4)+Age+Year+s(ID, bs='re', by=dum), data=seglw, family=binomial, gamma=1.4)
summary(m2)
## 
## Family: binomial 
## Link function: logit 
## 
## Formula:
## Mature ~ s(weight, k = 4) + Age + Year + s(ID, bs = "re", by = dum)
## 
## Parametric coefficients:
##                  Estimate    Std. Error z value   Pr(>|z|)    
## (Intercept)      -82.0850 23726566.3519   0.000    1.00000    
## Age3              81.0492 23726566.3519   0.000    1.00000    
## Age4              82.8428 23726566.3519   0.000    1.00000    
## Age5              82.8170 23726566.3519   0.000    1.00000    
## Age6              83.0388 23726566.3519   0.000    1.00000    
## Age7              81.8651 23726566.3519   0.000    1.00000    
## Age8             134.6070 28772687.9121   0.000    1.00000    
## Age9              83.5899 23726566.3519   0.000    1.00000    
## Age10             83.1077 23726566.3519   0.000    1.00000    
## Age11            135.2303 34732136.8268   0.000    1.00000    
## Age12            135.3899 31832529.1736   0.000    1.00000    
## Age13            135.8124 71179699.2001   0.000    1.00000    
## Year2003           1.7227        0.3828   4.501 0.00000678 ***
## Year2004           0.9375        0.3359   2.791    0.00526 ** 
## Year2005          -0.0241        0.9727  -0.025    0.98023    
## Year2011          -0.3602        0.4787  -0.752    0.45179    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Approximate significance of smooth terms:
##                edf  Ref.df Chi.sq   p-value    
## s(weight) 2.504608   2.835 32.155 0.0000101 ***
## s(ID):dum 0.008087 135.000  0.011   0.00339 ** 
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## R-sq.(adj) =   0.39   Deviance explained = 34.8%
## UBRE = -0.096162  Scale est. = 1         n = 648
AIC(m1,m1a,m2)
##           df      AIC
## m1  22.56843 563.5657
## m1a 18.06681 559.9648
## m2  18.51269 570.8770
plot(m2, page=1, shade=T)


m3 <- gam(Mature~+Age+Year+s(ID, bs='re', by=dum), data=seglw, family=binomial, gamma=1.4)
summary(m3)
## 
## Family: binomial 
## Link function: logit 
## 
## Formula:
## Mature ~ +Age + Year + s(ID, bs = "re", by = dum)
## 
## Parametric coefficients:
##                   Estimate     Std. Error z value Pr(>|z|)  
## (Intercept)      -60.65019 23726566.63922   0.000   1.0000  
## Age3              59.17782 23726566.63922   0.000   1.0000  
## Age4              61.66847 23726566.63922   0.000   1.0000  
## Age5              62.14935 23726566.63922   0.000   1.0000  
## Age6              62.73031 23726566.63922   0.000   1.0000  
## Age7              61.72388 23726566.63922   0.000   1.0000  
## Age8             108.81106 28772688.35704   0.000   1.0000  
## Age9              63.41185 23726566.63922   0.000   1.0000  
## Age10             62.98624 23726566.63922   0.000   1.0000  
## Age11            109.05560 34732136.99055   0.000   1.0000  
## Age12            109.33558 31832529.38775   0.000   1.0000  
## Age13            109.21650 71179699.29668   0.000   1.0000  
## Year2003           0.59066        0.28693   2.059   0.0395 *
## Year2004           0.37322        0.29247   1.276   0.2019  
## Year2005           0.04796        0.98672   0.049   0.9612  
## Year2011          -0.29534        0.46984  -0.629   0.5296  
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Approximate significance of smooth terms:
##                edf Ref.df Chi.sq p-value   
## s(ID):dum 0.005067    136  0.007 0.00415 **
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## R-sq.(adj) =  0.353   Deviance explained = 30.6%
## UBRE = -0.053478  Scale est. = 1         n = 648
AIC(m1,m1a,m2,m3)
##           df      AIC
## m1  22.56843 563.5657
## m1a 18.06681 559.9648
## m2  18.51269 570.8770
## m3  16.00507 600.5424
plot(m3, page=1, shade=T)

m4 <- gam(Mature~s(length, k=4)+Year+s(ID, bs='re', by=dum), data=seglw, family=binomial, gamma=1.4)
summary(m4)
## 
## Family: binomial 
## Link function: logit 
## 
## Formula:
## Mature ~ s(length, k = 4) + Year + s(ID, bs = "re", by = dum)
## 
## Parametric coefficients:
##             Estimate Std. Error z value       Pr(>|z|)    
## (Intercept)   0.5804     0.1553   3.738       0.000186 ***
## Year2003      1.7472     0.2855   6.119 0.000000000941 ***
## Year2004      0.8721     0.2688   3.245       0.001175 ** 
## Year2005      0.5671     0.8754   0.648       0.517127    
## Year2006      2.3101     0.4298   5.375 0.000000076448 ***
## Year2007      1.6367     1.0484   1.561       0.118497    
## Year2011      0.2591     0.4388   0.590       0.554889    
## Year2014      0.4219     0.3218   1.311       0.189890    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Approximate significance of smooth terms:
##             edf  Ref.df Chi.sq              p-value    
## s(length) 2.344   2.688 144.36 < 0.0000000000000002 ***
## s(ID):dum 7.776 201.000  11.47            0.0000942 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## R-sq.(adj) =  0.365   Deviance explained = 33.2%
## UBRE = -0.20428  Scale est. = 1         n = 996
AIC(m1,m1a,m2,m3,m4)
##           df      AIC
## m1  22.56843 563.5657
## m1a 18.06681 559.9648
## m2  18.51269 570.8770
## m3  16.00507 600.5424
## m4  18.11980 778.0414
plot(m3, page=1, shade=T)


Weight is less informative than length (remember this is just for these data on Seguam Pass females). Overall length is a better predictor. Location (within Seguam) does not appear to be important, however the random effect is important - fish caught within a haul are more similar to each other than fish from other hauls.

Examine model output

newd <- expand.grid(length=26:49, age=as.numeric(levels(seglw$Age)), year = as.numeric(levels(seglw$Year)), ID ='MS2014012e-04', dum=0 )
newd %>% mutate(Age=factor(age), Year=factor(year)) -> newd
newd$pred <- predict(m1a, data = newd)


First l50 is calculated as $L_{50}=-\alpha/\beta$
  
  Create a function to calculate this value at 50% (or 75%, etc)
lvalue <- function(coef,p) (log(p/(1-p))-coef[1])/coef[2]
Run the model and bootstrap it (using bootCase from the car package).

fit<-glm(Mature~length,family=binomial(link=logit),data=seglw)

bs <- bootCase(fit,B=1000)
Calculate L50

l50 <- lvalue(coef(fit),.5)
Calculate 95% L50 confidence intervals

l50boots <- apply(bs,1,lvalue,p=0.5)
l50ci <- quantile(l50boots,c(0.025,0.975))
The \(L_{50}\) is 34.2 with 95% CIs of 33.5, 34.8