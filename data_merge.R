
##Join extracted covar data at pixel level

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

#Obtain points data as shape file
kfw_points = readShapePoints("/Users/rbtrichler/Documents/AidData/KFW Brazil Eval/KFW_Points/SethExtract_10k/kfw_10k_sample.shp")

##Merge in MODIS data

#Obtain info about points from csv 
kfw_points_csv <- read.csv("/Users/rbtrichler/Documents/AidData/KFW Brazil Eval/KFW_Points/kfw_modis_yearly_merge.csv")
#Drop duplicative columns
kfw_points_csv <- kfw_points_csv[,-(1:3),drop=TRUE]
#Change modis column names to MaxL_ (to work with previous R analysis code)
names(kfw_points_csv)=gsub("kmym","MaxL",names(kfw_points_csv),fixed=TRUE)
names(kfw_points_csv)=gsub("e","",names(kfw_points_csv),fixed=TRUE)
#Merge modis data with shapefile
kfw_points1= merge (kfw_points, kfw_points_csv, by.x="ad_id", by.y="ad_id")
View(kfw_points1)
#Drop out MaxL_2015 because it's incomplete
MaxL_2015<-names(kfw_points1) %in% c("MaxL_2015")
kfw_points2<-kfw_points1[!MaxL_2015]
#Drop obs with NA as MODIS value for any year
#kfw_points2[!complete.cases(kfw_points2@data),]
kfw_points2<-kfw_points2[complete.cases(kfw_points2@data),]
#Drop obs below threshold - MODIS value <150 in year 2000
kfw_points2<-kfw_points2[kfw_points2$MaxL_2000>=150,]

##Merge Distance to Boundaries shapefile
distbound<-read.csv("/Users/rbtrichler/Documents/AidData/KFW Brazil Eval/KFW_Points/DistanceToBoundary/kfw_10k_dist_results.csv")
#drop duplicate columns
distbound<-distbound[,-(1:3),drop=TRUE]
#merge, adding HubName and HubDist
kfw_points3=merge(kfw_points2, distbound, by.x="ad_id", by.y="ad_id")

##Merge in covariates (pop, ntl, slope, elevation,temp,precip)

covars<-read.csv("/Users/rbtrichler/Documents/AidData/KFW Brazil Eval/KFW_Points/Covars/kfw_10k_sample_merge_LTDR.csv")
#drop old LTDR NDVI values and duplicate columns
covars<-covars[,-(32:64),drop=TRUE]
covars<-covars[,-(1:3),drop=TRUE]
#subset temp and precip in order to manipulate
air_temp<-covars[c(1,29:292)]
precip<-covars[c(1,293:556)]
#drop temp and precip from main covars dataset
covars<-covars[,-(29:556),drop=TRUE]
#rename existing covars to fit formatting from old code
covars_names <- covars
names(covars_names)=gsub("e","",names(covars_names),fixed=TRUE)
names(covars_names)=gsub("am50_","urbtravtime",names(covars_names),fixed=TRUE)
names(covars_names)=gsub("gpw3","Pop",names(covars_names),fixed=TRUE)
names(covars_names)=gsub("sslp_","Slope",names(covars_names),fixed=TRUE)
names(covars_names)=gsub("alp4","ntl",names(covars_names),fixed=TRUE)
names(covars_names)=gsub("dari_","Riv_Dist",names(covars_names),fixed=TRUE)
names(covars_names)=gsub("slv_","Elevation",names(covars_names),fixed=TRUE)
names(covars_names)=gsub("droa_","Road_dist",names(covars_names),fixed=TRUE)
kfw_points4=merge(kfw_points3, covars_names, by.x="ad_id", by.y="ad_id")

#add in gpw4 pop estimates (not available for original data merge, added in later)
gpw4<-read.csv("/Users/rbtrichler/Documents/AidData/KFW Brazil Eval/KFW_Points/Covars/merge_kfw_10k_sample_gpw4.csv")
gpw4<-gpw4[,c("ad_id","gpw_v4_density.2005.mean","gpw_v4_density.2010.mean","gpw_v4_density.2015.mean")]
names(gpw4)=gsub("gpw_v4_density.","Pop_",names(gpw4),fixed=TRUE)
names(gpw4)=gsub(".mean","",names(gpw4),fixed=TRUE)
kfw_points4.1=merge(kfw_points4,gpw4)

#interpolate pop data to create yearly values
#2000-2005
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

kfw_points4<-kfw_points4.1


##get temp data (min, max, mean) by year
for (i in 2:length(air_temp))
{
  year = substr(colnames(air_temp)[i], 6, 9)
  month = substr(colnames(air_temp)[i], 10, 11)
  dt = paste(year,"-",month,sep="")
  colnames(air_temp)[i] <- dt
}

air_temp_ts <- melt(air_temp,id="ad_id")
air_temp_ts <- cSplit(air_temp_ts, "variable", "-")
air_temp_ts_mean <- aggregate(value ~ variable_1 + ad_id, air_temp_ts, FUN=mean)
air_temp_ts_max <- aggregate(value ~ variable_1 + ad_id, air_temp_ts, FUN=max)
air_temp_ts_min <- aggregate(value ~ variable_1 + ad_id, air_temp_ts, FUN=min)

air_temp_mean <- reshape(air_temp_ts_mean, idvar=c("ad_id"), direction="wide", timevar="variable_1")
air_temp_max <- reshape(air_temp_ts_max, idvar=c("ad_id"), direction="wide", timevar="variable_1")
air_temp_min <- reshape(air_temp_ts_min, idvar=c("ad_id"), direction="wide", timevar="variable_1")
#rename vars
for (i in 2:length(air_temp_mean))
{
  colnames(air_temp_mean)[i] <- sub("value.","MeanT_",colnames(air_temp_mean)[i])
  colnames(air_temp_max)[i] <- sub("value.","MaxT_",colnames(air_temp_max)[i])
  colnames(air_temp_min)[i] <- sub("value.","MinT_",colnames(air_temp_min)[i])
}

kfw_points5=merge(kfw_points4, air_temp_mean, by.x="ad_id", by.y="ad_id")
kfw_points6=merge(kfw_points5, air_temp_max, by.x="ad_id", by.y="ad_id")
kfw_points7=merge(kfw_points6, air_temp_min, by.x="ad_id", by.y="ad_id")

## get precip data (min, max, mean) by year
for (i in 2:length(precip))
{
  year = substr(colnames(precip)[i], 6, 9)
  month = substr(colnames(precip)[i], 10, 11)
  dt = paste(year,"-",month,sep="")
  colnames(precip)[i] <- dt
}

precip_ts <- melt(precip,id="ad_id")
precip_ts <- cSplit(precip_ts, "variable", "-")
precip_ts_mean <- aggregate(value ~ variable_1 + ad_id, precip_ts, FUN=mean)
precip_ts_max <- aggregate(value ~ variable_1 + ad_id, precip_ts, FUN=max)
precip_ts_min <- aggregate(value ~ variable_1 + ad_id, precip_ts, FUN=min)
precip_mean <- reshape(precip_ts_mean, idvar=c("ad_id"), direction="wide", timevar="variable_1")
precip_max <- reshape(precip_ts_max, idvar=c("ad_id"), direction="wide", timevar="variable_1")
precip_min <- reshape(precip_ts_min, idvar=c("ad_id"), direction="wide", timevar="variable_1")

#Rename vars
for (i in 2:length(precip_mean))
{
  colnames(precip_mean)[i] <- sub("value.","MeanP_",colnames(precip_mean)[i])
  colnames(precip_max)[i] <- sub("value.","MaxP_",colnames(precip_max)[i])
  colnames(precip_min)[i] <- sub("value.","MinP_",colnames(precip_min)[i])
}

kfw_points8=merge(kfw_points7, precip_mean, by.x="ad_id", by.y="ad_id")
kfw_points9=merge(kfw_points8, precip_max, by.x="ad_id", by.y="ad_id")
kfw_points10=merge(kfw_points9, precip_min, by.x="ad_id", by.y="ad_id")

#Obtain community level info
kfw_comm = read.csv("/Users/rbtrichler/Documents/AidData/Git Repos/KFW_Amazon/input_data/KFW_treatment.csv")
#kfw_comm2001<-kfw_comm[kfw_comm$demend_y>=2001,]
kfw_points11=merge(kfw_points10, kfw_comm, by.x="group_id", by.y="reu_id")
names(kfw_points11)[1]="reu_id"
#change area to numeric
kfw_points11@data["terrai_are"] <- lapply(kfw_points11@data["terrai_are"], function(x) as.numeric(gsub("Ha","",x)))


##Create pre-trends
kfw_points12<-kfw_points11
kfw_points12$pt_MeanT <- timeRangeTrend(kfw_points12,"MeanT_[0-9][0-9][0-9][0-9]",1982,2000,"ad_id")
kfw_points12$pt_MinT <- timeRangeTrend(kfw_points12,"MinT_[0-9][0-9][0-9][0-9]",1982,2000,"ad_id")
kfw_points12$pt_MaxT <- timeRangeTrend(kfw_points12,"MaxT_[0-9][0-9][0-9][0-9]",1982,2000,"ad_id")
kfw_points12$pt_MeanP <- timeRangeTrend(kfw_points12,"MeanP_[0-9][0-9][0-9][0-9]",1982,2000,"ad_id")
kfw_points12$pt_MinP <- timeRangeTrend(kfw_points12,"MinP_[0-9][0-9][0-9][0-9]",1982,2000,"ad_id")
kfw_points12$pt_MaxP <- timeRangeTrend(kfw_points12,"MaxP_[0-9][0-9][0-9][0-9]",1982,2000,"ad_id")
kfw_points12$pt_Pop <- timeRangeTrend(kfw_points12,"Pop_[0-9][0-9][0-9][0-9]",1990,2000,"ad_id")

## Write Final Shapefile, with pre-trends
writePointsShape(kfw_points12,"/Users/rbtrichler/Documents/AidData/KFW Brazil Eval/KFW_Points/ProcessedData/kfw_points_processed.shp")


