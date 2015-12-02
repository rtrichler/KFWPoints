
##Join extracted covar data at pixel level

library(maptools)
library(reshape)
library(splitstackshape)
library(ggplot2)

library(devtools)
devtools::install_github("itpir/SCI@master")
library(SCI)
# library(stargazer)
# library(lmtest)
# library(multiwayvcov)
loadLibs()

#Obtain grid data with community level info
kfw_points = readShapePoints("/Users/rbtrichler/Documents/AidData/KFW Brazil Eval/KFW_Points/SethExtract_10k/kfw_10k_sample.shp")

#Drop Unused Variables at community level (NDVI, temp and precip because we're using at cell level); and pop, treatment info, and community identifiers because they are NA
kfw_grid@data <- kfw_grid@data[,-(8:744),drop=FALSE]

#Merge grid-level covariate files
#elevation
kfw_grid_elevation <- read.csv("/Users/rbtrichler/Documents/AidData/KFW Brazil Eval/Grid Data Extracts/KFW_Grids/extracted_data/elevation/extract.csv")
names(kfw_grid_elevation)[2] = "Elevation"
kfw_grid1= merge(kfw_grid, kfw_grid_elevation, by.x="GridID", by.y="Id")

#nighttime lights
kfw_grid_lights <- read.csv("/Users/rbtrichler/Documents/AidData/KFW Brazil Eval/Grid Data Extracts/KFW_Grids/extracted_data/v4avg_lights_x_pct/extract_merge.csv")
names(kfw_grid_lights)=gsub("ad","ntl",names(kfw_grid_lights), fixed=TRUE)
kfw_grid2= merge(kfw_grid1, kfw_grid_lights, by.x="GridID", by.y="Id")

#access (urban travel time, to urban centers with pop>50,000)
kfw_grid_access <- read.csv("/Users/rbtrichler/Documents/AidData/KFW Brazil Eval/Grid Data Extracts/KFW_Grids/extracted_data/access/extract.csv")
names(kfw_grid_access)[2]="urbtravtime"
kfw_grid3= merge (kfw_grid2, kfw_grid_access, by.x="GridID", by.y="Id")

#NDVI
kfw_grid_NDVI <- read.csv("/Users/rbtrichler/Documents/AidData/KFW Brazil Eval/Grid Data Extracts/KFW_Grids/extracted_data/ltdr_yearly_max_mask_lt6k/extract_merge.csv")

kfw_grid_NDVI_dec<- kfw_grid_NDVI[,2:35]/10000
kfw_grid_NDVI_all <- cbind(kfw_grid_NDVI[,c(-2:-35)], kfw_grid_NDVI_dec)
names(kfw_grid_NDVI_all)=gsub("ad", "MaxL", names(kfw_grid_NDVI_all), fixed=TRUE)
names(kfw_grid_NDVI_all)[1]="Id"
kfw_grid4=merge (kfw_grid3, kfw_grid_NDVI_all, by.x="GridID", by.y="Id")

#Distance to River
kfw_grid_riv <- read.csv("/Users/rbtrichler/Documents/AidData/KFW Brazil Eval/Grid Data Extracts/KFW_Grids/extracted_data/rivers_dist/extract.csv")
names(kfw_grid_riv)[2]="Riv_Dist"
kfw_grid5=merge (kfw_grid4, kfw_grid_riv, by.x="GridID", by.y="Id")

#Distance to Road
kfw_grid_road <- read.csv("/Users/rbtrichler/Documents/AidData/KFW Brazil Eval/Grid Data Extracts/KFW_Grids/extracted_data/roads_dist/extract.csv")
names(kfw_grid_road)[2]="Road_dist"
kfw_grid6=merge(kfw_grid5, kfw_grid_road, by.x="GridID", by.y="Id")

#Slope
kfw_grid_slope <- read.csv("/Users/rbtrichler/Documents/AidData/KFW Brazil Eval/Grid Data Extracts/KFW_Grids/extracted_data/slope/extract.csv")
names(kfw_grid_slope)[2]="Slope"
kfw_grid7=merge(kfw_grid6, kfw_grid_slope, by.x="GridID", by.y="Id")

#Temp
air_temp <- read.csv("/Users/rbtrichler/Documents/AidData/KFW Brazil Eval/Grid Data Extracts/updated_climate_data/temp_extract_merge.csv")

for (i in 2:length(air_temp))
{
  # splt <- strsplit(colnames(air_temp)[i],"_")
  # month = splt[[1]][3]
  # year = splt[[1]][2]
  year = substr(colnames(air_temp)[i], 6, 9)
  month = substr(colnames(air_temp)[i], 10, 11)
  dt = paste(year,"-",month,sep="")
  colnames(air_temp)[i] <- dt
}

air_temp_ts <- melt(air_temp,id="Id")
air_temp_ts <- cSplit(air_temp_ts, "variable", "-")
air_temp_ts_mean <- aggregate(value ~ variable_1 + Id, air_temp_ts, FUN=mean)
air_temp_ts_max <- aggregate(value ~ variable_1 + Id, air_temp_ts, FUN=max)
air_temp_ts_min <- aggregate(value ~ variable_1 + Id, air_temp_ts, FUN=min)

air_temp_mean <- reshape(air_temp_ts_mean, idvar=c("Id"), direction="wide", timevar="variable_1")
air_temp_max <- reshape(air_temp_ts_max, idvar=c("Id"), direction="wide", timevar="variable_1")
air_temp_min <- reshape(air_temp_ts_min, idvar=c("Id"), direction="wide", timevar="variable_1")

for (i in 2:length(air_temp_mean))
{
  colnames(air_temp_mean)[i] <- sub("value.","MeanT_",colnames(air_temp_mean)[i])
  colnames(air_temp_max)[i] <- sub("value.","MaxT_",colnames(air_temp_max)[i])
  colnames(air_temp_min)[i] <- sub("value.","MinT_",colnames(air_temp_min)[i])
}

air_temp_mean <- air_temp_mean[,-(35),drop=FALSE]
air_temp_max <- air_temp_max[,-(35),drop=FALSE]
air_temp_min <- air_temp_min[,-(35),drop=FALSE]

kfw_grid8=merge(kfw_grid7, air_temp_mean, by.x="GridID", by.y="Id")
kfw_grid9=merge(kfw_grid8, air_temp_max, by.x="GridID", by.y="Id")
kfw_grid10=merge(kfw_grid9, air_temp_min, by.x="GridID", by.y="Id")

#Precip
precip <- read.csv("/Users/rbtrichler/Documents/AidData/KFW Brazil Eval/Grid Data Extracts/updated_climate_data/precip_extract_merge.csv")

# splt splits columns when it sees _, e.g. ad_1982_02 yields 3 columns, "ad", "1982", "02" 
# month defines that the first row, represented as [[1]], and the 3rd column, [3], is the month
# year defines that the first row, [[1]], and the 2nd column, [2], is the year
# dt then renames the variable year-month, so 1982-02, rather than ad_1982_02

for (i in 2:length(precip))
{
  # splt <- strsplit(colnames(precip)[i],"_")
  # month = splt[[1]][3]
  # year = splt[[1]][2]
  year = substr(colnames(precip)[i], 6, 9)
  month = substr(colnames(precip)[i], 10, 11)
  dt = paste(year,"-",month,sep="")
  colnames(precip)[i] <- dt
}

precip_ts <- melt(precip,id="Id")
precip_ts <- cSplit(precip_ts, "variable", "-")
precip_ts_mean <- aggregate(value ~ variable_1 + Id, precip_ts, FUN=mean)
precip_ts_max <- aggregate(value ~ variable_1 + Id, precip_ts, FUN=max)
precip_ts_min <- aggregate(value ~ variable_1 + Id, precip_ts, FUN=min)
precip_mean <- reshape(precip_ts_mean, idvar=c("Id"), direction="wide", timevar="variable_1")
precip_max <- reshape(precip_ts_max, idvar=c("Id"), direction="wide", timevar="variable_1")
precip_min <- reshape(precip_ts_min, idvar=c("Id"), direction="wide", timevar="variable_1")

#Rename vars
for (i in 2:length(precip_mean))
{
  colnames(precip_mean)[i] <- sub("value.","MeanP_",colnames(precip_mean)[i])
  colnames(precip_max)[i] <- sub("value.","MaxP_",colnames(precip_max)[i])
  colnames(precip_min)[i] <- sub("value.","MinP_",colnames(precip_min)[i])
}

precip_mean <- precip_mean[,-(35),drop=FALSE]
precip_max <- precip_max[,-(35),drop=FALSE]
precip_min <- precip_min[,-(35),drop=FALSE]

kfw_grid11=merge(kfw_grid10, precip_mean, by.x="GridID", by.y="Id")
kfw_grid12=merge(kfw_grid11, precip_max, by.x="GridID", by.y="Id")
kfw_grid13=merge(kfw_grid12, precip_min, by.x="GridID", by.y="Id")

## Merge in high pressure covars (from Ash) at the community level: Vegetation, Soil Type Zones
# will drop out those to be merged in at grid level instead (distance to conservation units, logging, mining, railways)

kfw_grid_veg <- read.csv("/Users/rbtrichler/Google Drive/REU/KfW/Inputs/HighPressureCommCovars__Abbrev_Ash.csv")
kfw_grid_veg <- kfw_grid_veg[,-(10:27),drop=FALSE]
kfw_grid14=merge(kfw_grid13, kfw_grid_veg, by.x="Id", by.y="id")

## Merge in high pressure covars (from Ash) at the community level: Crop Value and Yield

kfw_grid_crop=readShapePoly("/Users/rbtrichler/Google Drive/REU/KfW/Inputs/HighPressureCovars_CropYield/Indig_ag.shp")
kfw_grid_crop@data<-kfw_grid_crop@data[,-(1:13),drop=FALSE]
kfw_grid_crop@data<-kfw_grid_crop@data[,-(2:21),drop=FALSE]
kfw_grid_crop@data<-kfw_grid_crop@data[,-(183:184),drop=FALSE]
kfw_grid15=merge(kfw_grid14, kfw_grid_crop@data, by.x="Id", by.y="id")

## Merge in high pressure covars (from Ash) at the GRID level: Distance to conservation units, mining, logging, railways
#Distance to Federal Conservation Unit
kfw_grid_fedcons<-read.csv("/Users/rbtrichler/Documents/AidData/KFW Brazil Eval/Grid Data Extracts/KFW_Grids/extracted_data/dist_from_conservation_federal_units/dfcf_e.csv")
names(kfw_grid_fedcons)[2]="fedcon_dist"
kfw_grid16=merge(kfw_grid15, kfw_grid_fedcons, by.x="GridID", by.y="Id")

#Distance to State Conservation Unit
kfw_grid_stcons<-read.csv("/Users/rbtrichler/Documents/AidData/KFW Brazil Eval/Grid Data Extracts/KFW_Grids/extracted_data/dist_from_conservation_state_units/dfcs_e.csv")
names(kfw_grid_stcons)[2]="stcon_dist"
kfw_grid17=merge(kfw_grid16, kfw_grid_stcons, by.x="GridID", by.y="Id")

#Distance to Logging Center
kfw_grid_log<-read.csv("/Users/rbtrichler/Documents/AidData/KFW Brazil Eval/Grid Data Extracts/KFW_Grids/extracted_data/dist_from_logging_center/dflc_e.csv")
names(kfw_grid_log)[2]="log_dist"
kfw_grid18=merge(kfw_grid17, kfw_grid_log, by.x="GridID", by.y="Id")

#Distance to Mining Activity
kfw_grid_mine<- read.csv("/Users/rbtrichler/Documents/AidData/KFW Brazil Eval/Grid Data Extracts/KFW_Grids/extracted_data/dist_from_mining_interests/dfmi_e.csv")
names(kfw_grid_mine)[2]="mine_dist"
kfw_grid19=merge(kfw_grid18, kfw_grid_mine, by.x="GridID", by.y="Id")

#Distance to Railway
kfw_grid_rail<-read.csv("/Users/rbtrichler/Documents/AidData/KFW Brazil Eval/Grid Data Extracts/KFW_Grids/extracted_data/dist_from_railways/dfrw_e.csv")
names(kfw_grid_rail)[2]="rail_dist"
kfw_grid20=merge(kfw_grid19, kfw_grid_rail, by.x="GridID", by.y="Id")

## Eliminate non-PPTAL communities

kfw_grid20$NA_check <- 0
kfw_grid20$NA_check[is.na(kfw_grid20$demend_y)] <- 1
kfw_grid21 <- kfw_grid20[kfw_grid20$NA_check != 1,]
kfw_grid20 <- kfw_grid21

## Create pre-trends
kfw_grid20$pre_trend_NDVI_max <- timeRangeTrend(kfw_grid20,"MaxL_[0-9][0-9][0-9][0-9]",1982,1995,"GridID")

kfw_grid20$pre_trend_temp_mean <- timeRangeTrend(kfw_grid20,"MeanT_[0-9][0-9][0-9][0-9]",1982,1995,"GridID")
kfw_grid20$pre_trend_temp_max <- timeRangeTrend(kfw_grid20,"MaxT_[0-9][0-9][0-9][0-9]",1982,1995,"GridID")
kfw_grid20$pre_trend_temp_min <- timeRangeTrend(kfw_grid20,"MinT_[0-9][0-9][0-9][0-9]",1982,1995,"GridID")

kfw_grid20$pre_trend_precip_mean <- timeRangeTrend(kfw_grid20,"MeanP_[0-9][0-9][0-9][0-9]",1982,1995,"GridID")
kfw_grid20$pre_trend_precip_max <- timeRangeTrend(kfw_grid20,"MaxP_[0-9][0-9][0-9][0-9]",1982,1995,"GridID")
kfw_grid20$pre_trend_precip_min <- timeRangeTrend(kfw_grid20,"MinP_[0-9][0-9][0-9][0-9]",1982,1995,"GridID")

kfw_grid20$pre_trend_ntl <- timeRangeTrend(kfw_grid20,"ntl_[0-9][0-9][0-9][0-9]",1992,1995,"GridID")

kfw_grid20$pre_trend_cv <- timeRangeTrend(kfw_grid20,"cv[0-9][0-9][0-9][0-9]",1994,1995,"GridID")
kfw_grid20$pre_trend_cy <- timeRangeTrend(kfw_grid20,"cy[0-9][0-9][0-9][0-9]",1991,1995,"GridID")
kfw_grid20$pre_trend_rv <- timeRangeTrend(kfw_grid20,"rv[0-9][0-9][0-9][0-9]",1994,1995,"GridID")
kfw_grid20$pre_trend_ry <- timeRangeTrend(kfw_grid20,"ry[0-9][0-9][0-9][0-9]",1991,1995,"GridID")
kfw_grid20$pre_trend_sov <- timeRangeTrend(kfw_grid20,"sov[0-9][0-9][0-9][0-9]",1994,1995,"GridID")
kfw_grid20$pre_trend_soy <- timeRangeTrend(kfw_grid20,"soy[0-9][0-9][0-9][0-9]",1991,1995,"GridID")
kfw_grid20$pre_trend_suv <- timeRangeTrend(kfw_grid20,"suv[0-9][0-9][0-9][0-9]",1994,1995,"GridID")
kfw_grid20$pre_trend_suy <- timeRangeTrend(kfw_grid20,"suy[0-9][0-9][0-9][0-9]",1991,1995,"GridID")
kfw_grid20$pre_trend_wv <- timeRangeTrend(kfw_grid20,"wv[0-9][0-9][0-9][0-9]",1994,1995,"GridID")


## Write Final Shapefile, with pre-trends
writePolyShape(kfw_grid20,"/Users/rbtrichler/Documents/AidData/KFW Brazil Eval/GridDataProcessed/OhFive_gridanalysis_inputs_wpretrends.shp")


