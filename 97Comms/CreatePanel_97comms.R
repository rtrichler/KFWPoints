
##Create KFWPoints shapefile
# Includes never demarcated comms (45) + comms demarcated 2002 or later (52)

library(maptools)
library(reshape)
library(splitstackshape)
library(ggplot2)
library(shapefiles)

library(devtools)
devtools::install_github("itpir/SCI@master")
library(SCI)
# library(stargazer)
# library(lmtest)
# library(multiwayvcov)
loadLibs()


# Load in data extract for 10,008 points

kfw_points=read.csv("/Users/rbtrichler/Documents/AidData/KFW Brazil Eval/KFW_Points/Extract_97comms/merge_97comms.csv")

#rename vars
names(kfw_points)[names(kfw_points)=="x1"] <- "long"
names(kfw_points)[names(kfw_points)=="x2"] <- "lat"
names(kfw_points)[names(kfw_points)=="dist_to_all_rivs.na.mean"]<-"Riv_Dist"
names(kfw_points)[names(kfw_points)=="dist_to_roads.na.mean"]<-"Road_Dist"
names(kfw_points)[names(kfw_points)=="srtm_elevation_500m.na.mean"]<-"Elevation"
names(kfw_points)[names(kfw_points)=="srtm_slope_500m.na.mean"]<-"Slope"
names(kfw_points)[names(kfw_points)=="accessibility_map.na.mean"]<-"urbtravtime"

#rename vars to match names in other analysis code
names(kfw_points)<-gsub("gpw_v._density.","Pop_",names(kfw_points))
names(kfw_points)<-gsub("v4composites_calibrated.","ntl_",names(kfw_points))

names(kfw_points)<-gsub("udel_air_temp_v4_01_yearly_mean.","MeanT_",names(kfw_points))
names(kfw_points)<-gsub("udel_air_temp_v4_01_yearly_min.","MinT_",names(kfw_points))
names(kfw_points)<-gsub("udel_air_temp_v4_01_yearly_max.","MaxT_",names(kfw_points))

names(kfw_points)<-gsub("udel_precip_v4_01_yearly_mean.","MeanP_",names(kfw_points))
names(kfw_points)<-gsub("udel_precip_v4_01_yearly_min.","MinP_",names(kfw_points))
names(kfw_points)<-gsub("udel_precip_v4_01_yearly_max.","MaxP_",names(kfw_points))

names(kfw_points)<-gsub("modis_yearly_ndvi_max.","MaxL_",names(kfw_points))
names(kfw_points)<-gsub(".mean","",names(kfw_points))

#merge in shapefile with 97 comms + PPTAL program information

dta_Shp=readShapePoly("/Users/rbtrichler/Documents/AidData/KFW Brazil Eval/KFW_Points/Extract_97comms/Input_Shp/extractshp_97comms.shp")

kfw_points=merge(kfw_points,dta_Shp@data,by="reu_id")

#interpolate pop data to create yearly values
#drop duplicate Pop_2000
kfw_points<-kfw_points[,-(12)]
#2000-2005
kfw_points4.1<-kfw_points
kfw_points4.1$Pop_2000.05<-(kfw_points4.1$Pop_2005-kfw_points4.1$Pop_2000)/5
kfw_points4.1$Pop_2001<-kfw_points4.1$Pop_2000+kfw_points4.1$Pop_2000.05
kfw_points4.1$Pop_2002<-kfw_points4.1$Pop_2001+kfw_points4.1$Pop_2000.05
kfw_points4.1$Pop_2003<-kfw_points4.1$Pop_2002+kfw_points4.1$Pop_2000.05
kfw_points4.1$Pop_2004<-kfw_points4.1$Pop_2003+kfw_points4.1$Pop_2000.05
#2005-2010
kfw_points4.1$Pop_2005.10<-(kfw_points4.1$Pop_2010-kfw_points4.1$Pop_2005)/5
kfw_points4.1$Pop_2006<-kfw_points4.1$Pop_2005+kfw_points4.1$Pop_2005.10
kfw_points4.1$Pop_2007<-kfw_points4.1$Pop_2006+kfw_points4.1$Pop_2005.10
kfw_points4.1$Pop_2008<-kfw_points4.1$Pop_2007+kfw_points4.1$Pop_2005.10
kfw_points4.1$Pop_2009<-kfw_points4.1$Pop_2008+kfw_points4.1$Pop_2005.10
#2010-2014
kfw_points4.1$Pop_2010.15<-(kfw_points4.1$Pop_2015-kfw_points4.1$Pop_2010)/5
kfw_points4.1$Pop_2011<-kfw_points4.1$Pop_2010+kfw_points4.1$Pop_2010.15
kfw_points4.1$Pop_2012<-kfw_points4.1$Pop_2011+kfw_points4.1$Pop_2010.15
kfw_points4.1$Pop_2013<-kfw_points4.1$Pop_2012+kfw_points4.1$Pop_2010.15
kfw_points4.1$Pop_2014<-kfw_points4.1$Pop_2013+kfw_points4.1$Pop_2010.15
#drop unused pop columns
kfw_points4.1<-kfw_points4.1[,-grep("(Pop_2000.05)",names(kfw_points4.1))]
kfw_points4.1<-kfw_points4.1[,-grep("(Pop_2005.10)",names(kfw_points4.1))]
kfw_points4.1<-kfw_points4.1[,-grep("(Pop_2010.15)",names(kfw_points4.1))]
kfw_points4.1<-kfw_points4.1[,-grep("(Pop_2015)",names(kfw_points4.1))]

kfw_points<-kfw_points4.1

## Write csv

write.csv(kfw_points,"/Users/rbtrichler/Documents/AidData/KFW Brazil Eval/KFW_Points/Extract_97comms/kfw_points_processed_97comms.csv")

## Create panel

#drop out unsed time-varying vars
reshape <- kfw_points
reshape<-reshape[,order(names(reshape))]
reshape1<-reshape[,-grep("(_198)",names(reshape))]
reshape2<-reshape1[,-grep("(_199)",names(reshape1))]
reshape3<-reshape2[,-grep("(ntl)",names(reshape2))]
reshape4<-reshape3[,-grep("(_2015)",names(reshape3))]
reshape5<-reshape4[,-grep("(_2000)",names(reshape4))]

#prep to convert to long form panel dataset
kfw_wide<-reshape5
kfw_wide<-kfw_wide[,order(names(kfw_wide))]

#----------------------------------
#Convert to long form panel dataset
#----------------------------------

MeanT<-grep("MeanT_",names(kfw_wide))
MeanP<-grep("MeanP_",names(kfw_wide))
MinT<-grep("MinT_",names(kfw_wide))
MaxT<-grep("MaxT_",names(kfw_wide))
MinP<-grep("MinP_",names(kfw_wide))
MaxP<-grep("MaxP_",names(kfw_wide))
MaxL<-grep("MaxL_",names(kfw_wide))
Pop<-grep("Pop_",names(kfw_wide))

all_reshape <- c(MeanT,MeanP,MaxT,MaxP,MinP,MinT,MaxL,Pop)
psm_Long <- reshape(kfw_wide, varying=all_reshape, direction="long",idvar="point_id",sep="_",timevar="Year")

#Create years to demarcation
psm_Long$yrtodem <- NA
psm_Long$yrtodem=psm_Long$Year - psm_Long$demend_y

#Create demarcation treatment variable, using demend_y
#0 in years prior to demarcation, turns to 1 in year of demarcation
psmtest3 <- psm_Long
psmtest3$trtdem <- 0
psmtest3$trtdem[which(psmtest3$Year<psmtest3$demend_y)]<-0
psmtest3$trtdem[which(psmtest3$Year>=psmtest3$demend_y)]<-1

psm_Long <- psmtest3

#create categorical variable for distance to boundary, which is in decimal degrees
#at equator, 5km=.044 decimal degrees
psm_Long$HubDistCat<-0
psm_Long$HubDistCat[psm_Long$hub_distan>.044]<-1

#create arc of deforestation variable
psm_Long$arc<-0
psm_Long$arc[which(psm_Long$UF=="PA" | psm_Long$UF=="TO")] <- 1

#check panel
sub<-kfw_wide[kfw_wide$point_id==1,]
View(sub)
View(sub[,(100:161)])

#WRITE PANEL DATASET

write.csv(psm_Long,"/Users/rbtrichler/Documents/AidData/KFW Brazil Eval/KFW_Points/Extract_97comms/psm_Long.csv")


#SUMMARY STATS

#create weight based on size of community (number of observations for each community)

weight<-as.data.frame(table(psm_Long$reu_id))
psm_Long<-merge(psm_Long,weight,by.x="reu_id",by.y="Var1")
psm_Long$gridwt<-1/psm_Long$Freq


stargazer(psm_Long, type="html",
          keep=c("MaxL","Slope","Road","Riv","Elevation","terrai_are","Pop","Mean","Min","MaxT",
                 "MaxP","hub_distan"),
          covariate.labels=c("Elevation (m)","Distance to Boundary (decimal degrees)",
                              "Distance to River (m)","Distance to Road (m)","Slope (degree)",
                            "Area (hectares)","Mean Temperature","Mean Precipitation",
                            "Max Temperature","Max Precipitation",
                             "Min Precipitation","Min Temperature",
                             "NDVI","Population Density"),
          omit.summary.stat=c("n"))

#the above outputs the summary stats table, which has accurate min and max
# but have to do the weighted mean and sd manually and then replace in html (or in Excel if formatting there)

library(SDMTools)
wt.mean(psm_Long$Pop,psm_Long$gridwt)
wt.sd(psm_Long$Pop,psm_Long$gridwt)
summary(psm_Long$Pop)

