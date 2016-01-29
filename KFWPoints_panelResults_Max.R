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

shpfile = "/Users/rbtrichler/Documents/AidData/KFW Brazil Eval/GridDataProcessed/OhFive_gridanalysis_inputs_wpretrends.shp"
dta_Shp = readShapePoly(shpfile)

#Rename pre-trends, created in data merge step
names(dta_Shp@data)[names(dta_Shp@data)=="pre_trend_"] <- "pre_trend_NDVI_max"
names(dta_Shp@data)[names(dta_Shp@data)=="pre_trend_.1"] <- "pre_trend_temp_mean"
names(dta_Shp@data)[names(dta_Shp@data)=="pre_trend_.2"] <- "pre_trend_temp_max"
names(dta_Shp@data)[names(dta_Shp@data)=="pre_trend_.3"] <- "pre_trend_temp_min"
names(dta_Shp@data)[names(dta_Shp@data)=="pre_trend_.4"] <- "pre_trend_precip_mean"
names(dta_Shp@data)[names(dta_Shp@data)=="pre_trend_.5"] <- "pre_trend_precip_max"
names(dta_Shp@data)[names(dta_Shp@data)=="pre_trend_.6"] <- "pre_trend_precip_min"

#-------------------------------------------------
#-------------------------------------------------
#Define the Treatment Variable and Population
#-------------------------------------------------
#-------------------------------------------------
#Make a binary to test treatment..
dta_Shp@data["TrtBin"] <- 0
dta_Shp@data$TrtBin[dta_Shp@data$demend_y <= 2001] <- 1
dta_Shp@data$TrtBin[(dta_Shp@data$demend_m > 4) & (dta_Shp@data$demend_y==2001)] <- 0
trttable <- table (dta_Shp@data$TrtBin)
View(trttable)

#Remove units that did not ever receive any treatment (within-sample test)
dta_Shp@data$NA_check <- 0
dta_Shp@data$NA_check[is.na(dta_Shp@data$demend_y)] <- 1
int_Shp <- dta_Shp[dta_Shp@data$NA_check != 1,]
dta_Shp <- int_Shp



#-------------------------------------------------
#-------------------------------------------------
#Define and run the first-stage of the PSM, calculating propensity scores
#-------------------------------------------------
#-------------------------------------------------
psmModel <-  "TrtBin ~ terrai_are + Pop_1990 + MeanT_1995 + pre_trend_temp_mean + pre_trend_temp_min + 
pre_trend_temp_max + MeanP_1995 + pre_trend_precip_min + pre_trend_NDVI_max + Slope + Elevation + MaxL_1995 + Riv_Dist + Road_dist +
pre_trend_precip_mean + pre_trend_precip_max"

psmRes <- SCI::SpatialCausalPSM(dta_Shp,mtd="logit",psmModel,drop="support",visual=FALSE)


#-------------------------------------------------
#-------------------------------------------------
#Based on the Propensity Score Matches, pair comprable treatment and control units.
#-------------------------------------------------
#-------------------------------------------------
drop_set<- c(drop_unmatched=TRUE,drop_method="None",drop_thresh=0.25)
psm_Pairs <- SAT(dta = psmRes$data, mtd = "fastNN",constraints=c(groups="UF"),psm_eq = psmModel, ids = "GridID", drop_opts = drop_set, visual="TRUE", TrtBinColName="TrtBin")
#c(groups=c("UF"),distance=NULL)
trttable <- table (psm_Pairs@data$TrtBin)
View(trttable)


#-------------------------------------------------
#-------------------------------------------------
#Convert from a wide-form dataset for the Cross-sectional 
#to a long-form dataset for the panel model.
#-------------------------------------------------
#-------------------------------------------------

varList = c("MaxL_")
psm_Long <- BuildTimeSeries(dta=psm_Pairs,idField="GridID",varList_pre=varList,1982,2010,colYears=c("demend_y","apprend_y","regend_y"),
                            interpYears=c("Slope","Road_dist","Riv_Dist","UF","Elevation","terrai_are","Pop_","MeanT_","MeanP_","MaxT_","MaxP_","MinP_","MinT_","ntl_", "fedcon_dis", "stcon_dist", "log_dist", "mine_dist", "rail_dist","pre_trend_NDVI_max", "reu_id", "Id" ))
psm_Long$Year <- as.numeric(psm_Long$Year)

write.csv(psm_Long,file="/Users/rbtrichler/Documents/AidData/KFW Brazil Eval/GridDataProcessed/psm_Long.csv")

pModelMax_A <- "MaxL_ ~ TrtMnt_demend_y + factor(reu_id)"
pModelMax_B <- "MaxL_ ~ TrtMnt_demend_y + MeanT_ + MeanP_ + Pop_ + MaxT_ + MaxP_ + MinT_ + MinP_  + factor(reu_id) "
pModelMax_C <- "MaxL_ ~ TrtMnt_demend_y + MeanT_ + MeanP_ + Pop_ + MaxT_ + MaxP_ + MinT_ + MinP_  + factor(reu_id) + Year"
pModelMax_D <- "MaxL_ ~ TrtMnt_demend_y + MeanT_ + MeanP_ + Pop_ + MaxT_ + MaxP_ + MinT_ + MinP_ + Road_dist*TrtMnt_demend_y + factor(reu_id) + Year"

pModelMax_A_fit <- Stage2PSM(pModelMax_A ,psm_Long,type="cmreg", table_out=TRUE, opts=c("reu_id","Year"))
pModelMax_B_fit <- Stage2PSM(pModelMax_B ,psm_Long,type="cmreg", table_out=TRUE, opts=c("reu_id","Year"))
pModelMax_C_fit <- Stage2PSM(pModelMax_C ,psm_Long,type="cmreg", table_out=TRUE, opts=c("reu_id","Year"))
pModelMax_D_fit <- Stage2PSM(pModelMax_D ,psm_Long,type="cmreg", table_out=TRUE, opts=c("reu_id","Year"))


#------------------------------------------------------------------------
#------------------------------------------------------------------------

stargazer(pModelMax_A_fit $cmreg,pModelMax_B_fit $cmreg,pModelMax_C_fit $cmreg,type="html",align=TRUE,keep=c("TrtMnt","MeanT_","MeanP_","Pop_","MaxT_","MaxP_","MinT_","MinP_","Year"),
          covariate.labels=c("TrtMnt_regend_y","MeanT","MeanP","Pop","MaxT","MaxP","MinT","MinP","Year"),
          omit.stat=c("f","ser"),
          title="Regression Results",
          dep.var.labels=c("Max NDVI")
)

