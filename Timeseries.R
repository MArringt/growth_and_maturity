# Timeseries.R 

# This example extracts a time-series of monthly satellite chlorophyll 
# data for the period 1998-2016 from three different satellites: 
# SeaWIFS, MODIS and VIIRS, using the `xracto_3D` function. 
# The extract is for an area at the mouth of theJuan de Fuca strait, 
# the region is defined in lines 23-24
# xtracto_3D extracts data in a 3D bounding box where 
# xpos has the minimum and maximum, 
# ypos has the minimum and maximum latitude, and 
# tpos has the starting and ending time, given as "YYYY-MM-DD" describing the bounding box.

install.packages("maps")
install.packages("mapdata")
install.packages("maptools")
install.packages("mapproj")
install.packages("httr")
install.packages("ncdf4")
install.packages("sp")
install.packages("devtools")
install.packages("lubridate")
install.packages("ggplot2")
install.packages("dplyr")

devtools::install_github("rmendels/xtractomatic")

library("xtractomatic")
library(lubridate)
library(maps)
library(mapdata)
library(maptools)
library(mapproj) 
library(ggplot2)
library(dplyr)
library(RColorBrewer)

# Theme to apply to figures
mytheme <- theme(panel.background = element_rect(fill="black"), panel.border = element_rect(colour="black",fill=NA,size=1.2), plot.background = element_rect(fill = "transparent",color = NA), legend.background = element_rect(fill = NA, color = NA))

# First we define the longitude-latitude boundaries of the box: 
 
xpos<-c(-174,-172.4)
ypos<-c(54.50625,51.50625)
xpos2<-c(-175.16625,-170.59125)
ypos2<-c(53.06625,49.92375)
coast<-map("world2Hires", xlim=c(186,187.6), ylim=c(51.50625,53.06625))
coast2<-map("world2Hires", xlim=c(184.83375,189.40875), ylim=c(49.92375,54.50625))
# Below is just for cosmetics, to make a nice map title
ttext<-paste(paste(abs(xpos), collapse="-"),"W, ", paste(ypos, collapse="-"),"N")

# get satellite data for the entire length of the available timeseries. 
# xtraco_3D will not work if dates are entered that are not part of the timeseries, 
# so they must be defined separately for each sensor, though "last" will get the 
# last date in the timeseries.   
  
tpos<-c("1998-01-16","last")
chlSeaWiFS<-xtracto_3D(xpos2, ypos2, tpos, 'erdSW1chlamday')
chlSeaWiFS$time<-as.Date(chlSeaWiFS$time)
# Convert list to data.frame because I know how to deal with it better
lonlat <- expand.grid(chlSeaWiFS$longitude, chlSeaWiFS$latitude, chlSeaWiFS$time)
sw_data <- as.vector(chlSeaWiFS$data)
SW_df <- cbind(lonlat,sw_data)
SW_df <- as.data.frame(SW_df)
SW_df <- SW_df%>%
  rename(x = Var1)%>%
  rename(y = Var2)%>%
  rename(date = Var3)%>%
  rename(swdata = sw_data)%>%
  mutate(mo = month(date))%>%
  mutate(yr = year(date))
# Map SeaWiFS data, each month per year a different map

SW_df3 <- SW_df%>%
  filter(swdata>0)
SW_df3$x <- SW_df3$x+360

coast<-map("world2Hires", xlim=c(186,187.6), ylim=c(51.50625,53.06625))
chl_col<- colorRampPalette(brewer.pal(7, "Spectral"))

for (d in unique(SW_df3$date)) {
  plot<-ggplot(SW_df3[which(SW_df3$date==d),], aes(x = x, y = y, fill = log(swdata+1))) +
    geom_raster(interpolate = FALSE) +
    scale_fill_gradientn(colors=rev(rainbow(7)),breaks=c(0,1,2),limits=c(0,2))+     # edit fill, all plots on same scale, set color palette
    geom_polygon(data = coast2, aes(x=long, y = lat, group = group), fill = "grey80") +  # base map
    theme_bw(base_size = 20) + ylab("Latitude") + 
    xlab("Longitude") +
    coord_fixed(1.3, xlim=c(184.83375,189.40875), ylim=c(49.92375,54.50625))+
    guides(fill=FALSE)+   # hide legend
    mytheme
ggsave(plot, filename=paste("J:/FIT_MARE/FIT-Atka/Atka_Maturity/AM_Maturity_Morgan_2016/Satellite Data Info/All SeaWiFS maps/",d,".png",  sep=""),  width=12, height=9, units="in", bg = "transparent")
}
# Grab legend seperately
## One plot just to steal legend
plot_leg<-ggplot(SW_df3[which(SW_df3$date=='1998-02-16'),], aes(x = x, y = y, fill = log(swdata+1))) +
  geom_raster(interpolate = FALSE) +
  scale_fill_gradientn(colors=rev(rainbow(7)),breaks=c(0,1,2),limits=c(0,2))+
  geom_polygon(data = coast, aes(x=long, y = lat, group = group), fill = "grey80") +
  theme_bw(base_size = 20) + ylab("Latitude") + 
  xlab("Longitude") +
  coord_fixed(1.3,xlim=c(186,187.6), ylim=c(51.50625,53.06625))+
  guides(fill= guide_colorbar(title = "Chlorophyll-a"))+
  mytheme
## function to grab legend
g_legend <- function(a.gplot){
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)
}
## draw legend from plot_leg
mylegend <- g_legend(plot_leg)
library(grid)
grid.draw(mylegend)
# Work with actual data numbers from SeaWiFS
SW_df2 <- SW_df %>%
  filter(swdata>=0)

SW_df2<-SW_df2%>%
  group_by(yr, mo)%>%
  mutate(swdata, n = n())%>%
  mutate(avg_ym = mean(swdata)) # average chl value for each month in each year

SW_df2$avg_ch <- mean(SW_df2$avg_ym) # make another column that has entire average chl taken by averaging the average of each month, weighting them all the same irrespective of num of data points

SW_df2 <- SW_df2%>%
  mutate(cstd=avg_ym/avg_ch) # make another column which is standardized chl which takes average of month/total average

SW_df2 <- SW_df2%>%   
  group_by(mo)%>%
  mutate(avg_mo=mean(avg_ym)) # make another column which is the average of all monthly averages
# Plotting SeaWiFS
## Available data
ggplot(SW_df2)+
  geom_point(aes(x=factor(mo), y=n))+
  facet_wrap(~yr)
## Raw data
ggplot(SW_df2)+
  geom_boxplot(aes(x=factor(mo), y= swdata))+
  ylim(c(0,10))+
  facet_wrap(~yr)
## Working with standardized chlorophyll
ggplot(SW_df2)+
  geom_boxplot(aes(x=factor(mo), y=cstd))+  # standardized chl boxplot
  facet_wrap(~yr)

ggplot(SW_df2)+
  geom_line(aes(x=factor(mo), y=cstd, group=1, color="Standardized Chlorphyll"))+   # standardized chl line graph facetted by year
  facet_wrap(~yr)

ggplot(SW_df2)

# Next dataset in timeseries
tpos<-c('2003-01-16',"last")
chlMODIS<-xtracto_3D(xpos, ypos, tpos,'mhchlamday')
chlMODIS$time<-as.Date(chlMODIS$time)
# Convert list to data.frame because I know how to deal with it better
lonlat <- expand.grid(chlMODIS$longitude, chlMODIS$latitude, chlMODIS$time)
m_data <- as.vector(chlMODIS$data)
m_df <- cbind(lonlat,m_data)
m_df <- as.data.frame(m_df)
m_df <- m_df%>%
  rename(x = Var1)%>%
  rename(y = Var2)%>%
  rename(date = Var3)%>%
  rename(mdata = m_data)%>%
  mutate(mo = month(date))%>%
  mutate(yr = year(date))
# Filter our NA values for plotting and convert longitute to 0-360
m_df2 <- m_df %>%
  filter(mdata>=0)
m_df2$x <- m_df2$x+360
# Get data counts for each month in each year
m_sum <- m_df2%>%
  group_by(yr, mo)%>%
  summarize(length(mo))
# Mapping data
for (d in unique(m_df2$date)) {
  plot<-ggplot(m_df2[which(m_df2$date==d),], aes(x = x, y = y, fill = log(mdata+1))) +
    geom_raster(interpolate = FALSE) +
    scale_fill_gradientn(colors=rev(rainbow(7)),breaks=c(0,1,2),limits=c(0,2))+     # edit fill, all plots on same scale, set color palette
    geom_polygon(data = coast, aes(x=long, y = lat, group = group), fill = "grey80") +  # base map
    theme_bw(base_size = 20) + ylab("Latitude") + 
    xlab("Longitude") +
    coord_fixed(1.3,xlim=c(186,187.6), ylim=c(51.50625,53.06625))+
    guides(fill=FALSE)+   # hide legend
    mytheme
  ggsave(plot, filename=paste("J:/FIT_MARE/FIT-Atka/Atka_Maturity/AM_Maturity_Morgan_2016/Satellite Data Info/All MODIS maps/",d,".png",  sep=""),  width=12, height=9, units="in", bg = "transparent")
}

# box plot of cholorphyll values 
ggplot(m_df2)+
  geom_boxplot(aes(x=factor(mo), y= mdata))+
  ylim(c(0,10))+
  facet_wrap(~yr)

# Next dataset in timeseries
tpos<-c("2012-01-15","last")
chlVIIRS<-xtracto_3D(xpos, ypos, tpos, 'erdVH2chlamday')
chlVIIRS$time <- as.Date(chlVIIRS$time)
# Convert list to data.frame because I know how to deal with it better
lonlat <- expand.grid(chlVIIRS$longitude, chlVIIRS$latitude, chlVIIRS$time)
v_data <- as.vector(chlVIIRS$data)
v_df <- cbind(lonlat,v_data)
v_df <- as.data.frame(v_df)
v_df <- v_df%>%
  rename(x = Var1)%>%
  rename(y = Var2)%>%
  rename(date = Var3)%>%
  rename(vdata = v_data)%>%
  mutate(mo = month(date))%>%
  mutate(yr = year(date))
# Filter our NA values for plotting
v_df2 <- v_df %>%
  filter(vdata>=0)
v_df2$x <- v_df2$x+360
# Get data counts for each month in each year
v_sum <- v_df2%>%
  group_by(yr, mo)%>%
  summarize(length(mo))
# Mapping data
for (d in unique(v_df2$date)) {
  plot<-ggplot(v_df2[which(v_df2$date==d),], aes(x = x, y = y, fill = log(vdata+1))) +
    geom_raster(interpolate = FALSE) +
    scale_fill_gradientn(colors=rev(rainbow(7)),breaks=c(0,1,2),limits=c(0,2))+     # edit fill, all plots on same scale, set color palette
    geom_polygon(data = coast, aes(x=long, y = lat, group = group), fill = "grey80") +  # base map
    theme_bw(base_size = 20) + ylab("Latitude") + 
    xlab("Longitude") +
    coord_fixed(1.3,xlim=c(186,187.6), ylim=c(51.50625,53.06625))+
    guides(fill=FALSE)+   # hide legend
    mytheme
  ggsave(plot, filename=paste("J:/FIT_MARE/FIT-Atka/Atka_Maturity/AM_Maturity_Morgan_2016/Satellite Data Info/All VIIRS maps/",d,".png",  sep=""),  width=12, height=9, units="in", bg = "transparent")
}






# box plot of cholorphyll values 
ggplot(v_df2)+
  geom_boxplot(aes(x=factor(mo), y= vdata))+
  ylim(c(0,10))+
  facet_wrap(~yr)

# Find monthly average per year of cholorphyll-a over entire range
# Now spatially average all the data within the box for each dataset  

chlMODIS$avg <- apply(chlMODIS$data, c(3),function(x) mean(na.omit(x)))
chlVIIRS$avg <- apply(chlVIIRS$data, c(3),function(x) mean(na.omit(x)))


#****************************
# Plot time series result
#****************************





plot(chlSeaWiFS$time, chlSeaWiFS$avg, 
     type='b', bg="blue", pch=21, xlab="", 
     xlim=as.Date(c("1998-01-01","2015-07-30")),
     ylim=c(0,20),
     ylab="Chl (mg/m^3)", main=ttext)
abline(v=seq(as.Date("1998/1/1"), as.Date("2015/1/1"), "year"),lty=1,col='gray')
axis(2)

# Now add MODIS and VIIRS data 
points(chlMODIS$time, chlMODIS$avg, type='b', bg="red", pch=21)
points(chlVIIRS$time, chlVIIRS$avg, type='b', bg="green", pch=21)

text(as.Date("1998-01-01"),19, "SeaWiFS",col="blue", pos=4)
text(as.Date("1998-01-01"),17, "MODIS",col="red", pos=4)
text(as.Date("1998-01-01"),15, "VIIRS",col="green", pos=4)


# Now make a quick map to show the location if the box selected. 

quartz()  # open a new plot window, call needs to be windows() on a PC 

#xpos=xpos-360
xlim=c(xpos[2]+.05*xpos[2],xpos[1]-.05*xpos[1])
ylim=c(ypos[1]-.05*ypos[1],ypos[2]+.05*ypos[2])

map(database="worldHires",
    xlim=xlim,ylim=ylim,
    col="grey80",fill=TRUE)
map.axes()
rect(xpos[1],ypos[1],xpos[2],ypos[2])          # outline area of timeseries average 

