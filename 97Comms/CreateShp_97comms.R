


library(devtools)
devtools::install_github("itpir/SCI@master")
library(SCI)
library(stargazer)
loadLibs()
library(MatchIt)
library(rgeos)
library(maptools)
library(rgdal)
library(sp)
#-------------------------------------------------
#-------------------------------------------------
#Load in Processed Data - produced from script KFW_dataMerge.r
#-------------------------------------------------
#-------------------------------------------------

shpfile = "processed_data/kfw_analysis_inputs.shp"
dta_Shp = readShapePoly(shpfile)


#Eliminate non-PPTAL indigenous lands
dta_Shp@data$proj_check <- 0
dta_Shp@data$proj_check[is.na(dta_Shp@data$reu_id)] <- 1
proj_Shp <- dta_Shp[dta_Shp@data$proj_check !=1,]
dta_Shp <- proj_Shp

projtable <- table(proj_Shp@data$proj_check)
View(projtable)

#Eliminate communities demarcated before 2002

dta_Shp@data$earlydem<-0
dta_Shp@data$earlydem[dta_Shp@data$demend_y<2002]<-1
dta_Shp<-dta_Shp[dta_Shp@data$earlydem==0,]

#Eliminate all data but id fields and community program info
dta_Shp<-dta_Shp[,-(3:356)]
dta_Shp<-dta_Shp[,-(36:69)]

#Write shapefile for use to resample points to extract GIMMS data
writePolyShape(dta_Shp,"/Users/rbtrichler/Documents/AidData/KFW Brazil Eval/KFW_Points/Extract_97comms/Input_Shp/extractshp_97comms.shp")


