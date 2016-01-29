#-------------------------------------------------
#-------------------------------------------------
#Panel Models - KFW Grid
#Testing in Panel the impact of being treated with demarcation
#On the Max Level of NDVI, measured as the yearly max NDVI value (LTDR)
#-------------------------------------------------
#-------------------------------------------------
library(devtools)
devtools::install_github("itpir/SCI@master")
library(SCI)
library(stargazer)
library(lmtest)
library(multiwayvcov)
loadLibs()
#-------------------------------------------------
#-------------------------------------------------
#Load in Processed Data - produced from script KFW_dataMerge.r
#-------------------------------------------------
#-------------------------------------------------

shpfile = "/Users/rbtrichler/Documents/AidData/KFW Brazil Eval/KFW_Points/ProcessedData/kfw_points_processed.shp"
dta_Shp = readShapePoints(shpfile)

#-------------------------------------------------
#-------------------------------------------------
#Convert from a wide-form dataset for the Cross-sectional 
#to a long-form dataset for the panel model.
#-------------------------------------------------
#-------------------------------------------------

varList = c("MaxL_")
psm_Long <- BuildTimeSeries(dta=dta_Shp,idField="ad_id",varList_pre=varList,2000,2014,colYears=c("demend_y","apprend_y","regend_y"),
                            interpYears=c("Slope","Road_dist","Riv_Dist","UF","Elevation","urbtravtim","terrai_are","Pop_","MeanT_","MeanP_","MaxT_","MaxP_","MinP_","MinT_","ntl_","HubDist", "reu_id"))
psm_Long$Year <- as.numeric(psm_Long$Year)

write.csv(psm_Long,file="/Users/rbtrichler/Documents/AidData/KFW Brazil Eval/KFW_Points/ProcessedData/psm_Long.csv")

#merge in distance to boundary
distbound<-read.csv("/Users/rbtrichler/Documents/AidData/KFW Brazil Eval/KFW_Points/DistanceToBoundary/kfw_10k_dist_results.csv")
distbound<-distbound[,-(1:3),drop=TRUE]
psm_Long_distbound= merge(psm_Long, distbound,by.x="ad_id", by.y="ad_id")
psm_Long <- psm_Long_distbound

pModelMax_A <- "MaxL_ ~ TrtMnt_demend_y + factor(reu_id)"
pModelMax_B <- "MaxL_ ~ TrtMnt_demend_y + MeanT_ + MeanP_ + Pop_ + MaxT_ + MaxP_ + MinT_ + MinP_  + factor(reu_id) "
pModelMax_C <- "MaxL_ ~ TrtMnt_demend_y + MeanT_ + MeanP_ + Pop_ + MaxT_ + MaxP_ + MinT_ + MinP_  + factor(reu_id) + Year"
pModelMax_D <- "MaxL_ ~ TrtMnt_demend_y + MeanT_ + MeanP_ + Pop_ + MaxT_ + MaxP_ + MinT_ + MinP_ + HubDist*TrtMnt_demend_y + factor(reu_id) + Year"

pModelMax_A_fit <- Stage2PSM(pModelMax_A ,psm_Long,type="cmreg", table_out=TRUE, opts=c("reu_id","Year"))
pModelMax_B_fit <- Stage2PSM(pModelMax_B ,psm_Long,type="cmreg", table_out=TRUE, opts=c("reu_id","Year"))
pModelMax_C_fit <- Stage2PSM(pModelMax_C ,psm_Long,type="cmreg", table_out=TRUE, opts=c("reu_id","Year"))
pModelMax_D_fit <- Stage2PSM(pModelMax_D ,psm_Long,type="cmreg", table_out=TRUE, opts=c("reu_id","Year"))


#------------------------------------------------------------------------
#------------------------------------------------------------------------

stargazer(pModelMax_A_fit $cmreg,pModelMax_B_fit $cmreg,pModelMax_C_fit $cmreg,pModelMax_D_fit$cmreg,type="html",align=TRUE,keep=c("TrtMnt","MeanT_","MeanP_","Pop_","MaxT_","MaxP_","MinT_","MinP_","Year"),
          #covariate.labels=c("TrtMnt_demend_y","MeanT","MeanP","Pop","MaxT","MaxP","MinT","MinP","Year"),
          omit.stat=c("f","ser"),
          title="Regression Results",
          dep.var.labels=c("Max NDVI")
)

