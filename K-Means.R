library(measurements)
library(lubridate)
library(geosphere)
library(rgeos)
library(plotKML)
library(rgdal)
library(spatstat)
library(maptools)
library(ggplot2)
library(plyr)

##################################################################################################################
##########################################WORKING WITH SHAPEFILES#################################################
##################################################################################################################

#IMPORT SHAPEFILE OF NEIGHBORHOODS.#
setwd("/Users/.../neighborhoods")

full<-readOGR(dsn=".",layer="nynta")

#CONVERT THE COORDINATE SYSTEM OF THE NEIGHBORHOOD TRACT SHAPEFILE INTO LATITUDE/LONGITUDE.#
full=spTransform(full,CRS("+proj=longlat +datum=WGS84"))

#SELECTING AREAS#
nyc_map<-full
plot(nyc_map)

washington<-full[full$NTAName=="Washington Heights North" | 
                   full$NTAName=="Washington Heights South" | 
                   full$NTAName=="Manhattanville" |
                   full$NTAName=="Hamilton Heights" |
                   full$NTAName== "Central Harlem North-Polo Grounds" |
                   full$NTAName=="Morningside Heights" |
                   full$NTAName=="Central Harlem South" |
                   full$NTAName=="East Harlem North", ]
sub_map<-washington


boundary<-as(sub_map, "owin")
plot(sub_map)
##################################################################################################################
#############################################WORKING WITH CRIME DATA##############################################
##################################################################################################################

crime<-read.csv("/Users/.../crime.csv", sep=",", header = TRUE, na.strings=c(""," ","NA"))
x<-crime[,c(23,22,11,12,14,6)]
x<-x[complete.cases(x), ]
str(x)

#####SUBSET DATA#####
x$RPT_DT <- mdy(x$RPT_DT)
year<-split(x, format(as.Date(x$RPT_DT), "%Y"))

year_2016<-year$`2016`
month<-split(year_2016, format(as.Date(year_2016$RPT_DT), "%m"))

year_2016_july<-month$`07`

sub_data<-year_2016_july 
str(sub_data)

##################################################################################################################
############################################K-Means Function######################################################
##################################################################################################################
crime_points <- sub_data[,c(1,2)]
l<-SpatialPoints(crime_points)
proj4string(l)=proj4string(sub_map)
inside_points<-over(l,sub_map)
inside_points<-!is.na(inside_points)

a<-as.data.frame(inside_points)
b<-subset(a, NTAName=="TRUE")
s<-rownames(b)
t<-sub_data[s,c(1:2)]
plot_sub_data<-t # I will use this for plotting on shapfile of selected regions
t<-cbind(jitter(t[,1]), jitter(t[,2]))
l<-SpatialPoints(t)
p<-as(l, "ppp")
p$window=boundary

#envelope with simulations#
Lsim<-envelope(p, fun=Lest, nsim=1000, correction="trans")
plot(Lsim, .-r ~r, xlab='Feet', xlim=c(0,.0125), ylim=c(-.0012,.0012), main="Univariate L(r)-r Function: Upper Manhattan")

##################################################################################################################
##################################################################################################################
################################Checking for Duplicates##########################################################
dim(plot_sub_data[duplicated(plot_sub_data$Longitude),])[1]
crime_points_new<-cbind(jitter(plot_sub_data[,1]), jitter(plot_sub_data[,2]))
crime_points_new<-as.data.frame(crime_points_new)
dim(crime_points_new[duplicated(crime_points_new$Longitude),])[1]
##################################################################################################################
##################################################################################################################
########################################Plotting CRIME on shapefile###############################################
##################################################################################################################
#FORTIFY 'map' FILE, SO IT CAN BE CONVERTED INTO A 'data.frame' OBJECT FROM A SPATIALPOLYGONSDATAFRAME.#
nyc_map@data$id<-rownames(nyc_map@data)
map1.df<-fortify(nyc_map)
map1.df<-join(map1.df, nyc_map@data, by="id")  

image <- ggplot(map1.df,aes(x=long,y=lat, group=group))
image <- image + geom_path(color="black")
image <- image + coord_equal()
image <- image + ggtitle("Crime Data for July 2016: All NYC")
image <- image + geom_point(data=sub_data, aes(x=Longitude, y=Latitude), color="orange", alpha=1, inherit.aes=FALSE)
print(image)

##################################################################################################################
########################################Plotting subsetted CRIME on shapefile###############################################
##################################################################################################################
#FORTIFY 'map' FILE, SO IT CAN BE CONVERTED INTO A 'data.frame' OBJECT FROM A SPATIALPOLYGONSDATAFRAME.#
sub_map@data$id<-rownames(sub_map@data)
map1.df<-fortify(sub_map)
map1.df<-join(map1.df,sub_map@data, by="id")  

image <- ggplot(map1.df,aes(x=long,y=lat, group=group))
image <- image + geom_path(color="black")
image <- image + coord_equal()
image <- image + ggtitle("Crime Data for Jan 2016: Upper Manhattan")
image <- image + geom_point(data=plot_sub_data, aes(x=Longitude, y=Latitude), color="orange", alpha=1, inherit.aes=FALSE)
print(image)

##################################################################################################################
##################################################################################################################
##################################################################################################################
