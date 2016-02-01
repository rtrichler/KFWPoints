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

#Rename pre-trends, created in data merge step
names(dta_Shp@data)[names(dta_Shp@data)=="pre_trend_"] <- "pre_trend_temp_mean"
names(dta_Shp@data)[names(dta_Shp@data)=="pre_trend_.1"] <- "pre_trend_temp_min"
names(dta_Shp@data)[names(dta_Shp@data)=="pre_trend_.2"] <- "pre_trend_temp_max"
names(dta_Shp@data)[names(dta_Shp@data)=="pre_trend_.3"] <- "pre_trend_precip_mean"
names(dta_Shp@data)[names(dta_Shp@data)=="pre_trend_.4"] <- "pre_trend_precip_min"
names(dta_Shp@data)[names(dta_Shp@data)=="pre_trend_.5"] <- "pre_trend_precip_max"
names(dta_Shp@data)[names(dta_Shp@data)=="pre_trend_.6"] <- "pre_trend_pop"

#-------------------------------------------------
#-------------------------------------------------
#Define the Treatment Variable and Population
#-------------------------------------------------
#-------------------------------------------------
#Make a binary to test treatment..
dta_Shp@data["TrtBin"] <- 0
dta_Shp@data$TrtBin[dta_Shp@data$demend_y <= 2003] <- 1
dta_Shp@data$TrtBin[(dta_Shp@data$demend_m >2) & (dta_Shp@data$demend_y==2003)] <- 0
trttable <- table (dta_Shp@data$TrtBin)
View(trttable)


#-------------------------------------------------
#-------------------------------------------------
#Define and run the first-stage of the PSM, calculating propensity scores
#-------------------------------------------------
#-------------------------------------------------
psmModel <-  "TrtBin ~ terrai_are + Pop_2000 + MeanT_2000 + MinT_2000 + MaxT_2000+
MeanP_2000 + MinP_2000 + MaxP_2000 + Slope + Elevation + MaxL_2000 + Riv_Dist + Road_dist+ HubDist"

psmModel <-  "TrtBin ~ terrai_are + Pop_2000 + MeanT_2000 + MinT_2000 + MaxT_2000+
MeanP_2000 + MinP_2000 + MaxP_2000 + Slope + Elevation + MaxL_2000 + Riv_Dist + Road_dist+ HubDist+
pre_trend_temp_mean + pre_trend_temp_min + pre_trend_temp_max + pre_trend_precip_mean + pre_trend_precip_min +
pre_trend_precip_max + pre_trend_pop"

psmRes <- SCI::SpatialCausalPSM(dta_Shp,mtd="logit",psmModel,drop="support",visual=TRUE)


#-------------------------------------------------
#-------------------------------------------------
#Based on the Propensity Score Matches, pair comprable treatment and control units.
#-------------------------------------------------
#-------------------------------------------------
drop_set<- c(drop_unmatched=TRUE,drop_method="None")
psm_Pairs <- SAT(dta = psmRes$data, mtd = "fastNN",constraints=c(groups="UF"),psm_eq = psmModel, ids = "ad_id", drop_opts = drop_set, visual="TRUE", TrtBinColName="TrtBin")
#c(groups=c("UF"),distance=NULL)
trttable <- table (psm_Pairs@data$TrtBin)
View(trttable)

#create dataset with ad_id for psm_Pairs
psm_Pairs_trim <- subset(psm_Pairs@data, select=c(ad_id, PSM_distance))

#-------------------------------------------------
#-------------------------------------------------
#Convert from a wide-form dataset for the Cross-sectional 
#to a long-form dataset for the panel model.
#-------------------------------------------------
#-------------------------------------------------

# varList = c("MaxL_")
# psm_Long <- BuildTimeSeries(dta=dta_Shp,idField="ad_id",varList_pre=varList,2000,2014,colYears=c("demend_y","apprend_y","regend_y"),
#                             interpYears=c("Slope","Road_dist","Riv_Dist","UF","Elevation","urbtravtim","terrai_are","Pop_","MeanT_","MeanP_","MaxT_","MaxP_","MinP_","MinT_","ntl_","HubName","HubDist", "reu_id"))
# psm_Long$Year <- as.numeric(psm_Long$Year)
# 
# write.csv(psm_Long,file="/Users/rbtrichler/Documents/AidData/KFW Brazil Eval/KFW_Points/ProcessedData/psm_Long.csv")
psm_Long= read.csv("/Users/rbtrichler/Documents/AidData/KFW Brazil Eval/KFW_Points/ProcessedData/psm_Long.csv")

#trim according to psm_Pairs_trim
psm_Long=merge(psm_Long, psm_Pairs_trim, by.x="ad_id",by.y="ad_id")

#create categorical variable for distance to boundary
psm_Long$HubDistCat<-0
psm_Long$HubDistCat[psm_Long$HubDist>5]<-1

pModelMax_A <- "MaxL_ ~ TrtMnt_demend_y + factor(reu_id)"
pModelMax_B <- "MaxL_ ~ TrtMnt_demend_y + MeanT_ + MeanP_ + Pop_ + MaxT_ + MaxP_ + MinT_ + MinP_  + factor(reu_id) "
pModelMax_C <- "MaxL_ ~ TrtMnt_demend_y + MeanT_ + MeanP_ + Pop_ + MaxT_ + MaxP_ + MinT_ + MinP_  + factor(reu_id) + Year"
pModelMax_D <- "MaxL_ ~ TrtMnt_demend_y + MeanT_ + MeanP_ + Pop_ + MaxT_ + MaxP_ + MinT_ + MinP_ + HubDist*TrtMnt_demend_y + factor(reu_id) + Year"
pModelMax_E <- "MaxL_ ~ TrtMnt_demend_y + MeanT_ + MeanP_ + Pop_ + MaxT_ + MaxP_ + MinT_ + MinP_ + HubDistCat*TrtMnt_demend_y + factor(reu_id) + Year"

pModelMax_A_fit <- Stage2PSM(pModelMax_A ,psm_Long,type="cmreg", table_out=TRUE, opts=c("reu_id","Year"))
pModelMax_B_fit <- Stage2PSM(pModelMax_B ,psm_Long,type="cmreg", table_out=TRUE, opts=c("reu_id","Year"))
pModelMax_C_fit <- Stage2PSM(pModelMax_C ,psm_Long,type="cmreg", table_out=TRUE, opts=c("reu_id","Year"))
pModelMax_D_fit <- Stage2PSM(pModelMax_D ,psm_Long,type="cmreg", table_out=TRUE, opts=c("reu_id","Year"))
pModelMax_E_fit <- Stage2PSM(pModelMax_E ,psm_Long,type="cmreg", table_out=TRUE, opts=c("reu_id","Year"))


#------------------------------------------------------------------------
#------------------------------------------------------------------------

stargazer(pModelMax_A_fit $cmreg,pModelMax_B_fit $cmreg,pModelMax_C_fit $cmreg,pModelMax_D_fit$cmreg,
          pModelMax_E_fit$cmreg,
          type="html",align=TRUE,keep=c("TrtMnt","MeanT_","MeanP_","Pop_","MaxT_","MaxP_","MinT_","MinP_","Year"),
          #covariate.labels=c("TrtMnt_demend_y","MeanT","MeanP","Pop","MaxT","MaxP","MinT","MinP","Year"),
          omit.stat=c("f","ser"),
          title="Regression Results",
          dep.var.labels=c("Max NDVI")
)


