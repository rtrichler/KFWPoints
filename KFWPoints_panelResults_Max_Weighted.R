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
                            interpYears=c("Slope","Road_dist","Riv_Dist","UF","Elevation","urbtravtim","terrai_are","Pop_","MeanT_","MeanP_","MaxT_","MaxP_","MinP_","MinT_","ntl_","HubDist","HubName","reu_id"))
psm_Long$Year <- as.numeric(psm_Long$Year)

write.csv(psm_Long,file="/Users/rbtrichler/Documents/AidData/KFW Brazil Eval/KFW_Points/ProcessedData/psm_Long.csv")
psm_Long= read.csv("/Users/rbtrichler/Documents/AidData/KFW Brazil Eval/KFW_Points/ProcessedData/psm_Long.csv")

#create categorical variable for distance to boundary
psm_Long$HubDistCat<-0
psm_Long$HubDistCat[psm_Long$HubDist>5]<-1

#create arc of deforestation variable
psm_Long$arc<-0
psm_Long$arc[which(psm_Long$UF=="PA" | psm_Long$UF=="TO")] <- 1

#-------------------------------------------------
#-------------------------------------------------
#Run Panel Models
#-------------------------------------------------
#-------------------------------------------------

pModelMax_A = lm(MaxL_ ~ TrtMnt_demend_y + factor(reu_id),data=psm_Long, weights=terrai_are)
summary(pModelMax_A)
clusterA <- cluster.vcov(pModelMax_A,cbind(psm_Long$reu_id,psm_Long$Year),force_posdef=TRUE)
CMREG_A <- coeftest(pModelMax_A, clusterA)
print(CMREG_A)

pModelMax_B = lm(MaxL_ ~ TrtMnt_demend_y+ MeanT_ + MeanP_ + Pop_ + MaxT_ + MaxP_ + MinT_ + MinP_ + factor(reu_id),
                 data=psm_Long, weights=terrai_are)
summary(pModelMax_B)
clusterB <- cluster.vcov(pModelMax_B,cbind(psm_Long$reu_id,psm_Long$Year),force_posdef=TRUE)
CMREG_B <- coeftest(pModelMax_B, clusterB)
print(CMREG_B)

pModelMax_C = lm(MaxL_ ~ TrtMnt_demend_y+ MeanT_ + MeanP_ + Pop_ + MaxT_ + MaxP_ + MinT_ + MinP_ + Year + factor(reu_id),
                 data=psm_Long, weights=terrai_are)
summary(pModelMax_C)
clusterC <- cluster.vcov(pModelMax_C,cbind(psm_Long$reu_id,psm_Long$Year),force_posdef=TRUE)
CMREG_C <- coeftest(pModelMax_C, clusterC)
print(CMREG_C)

pModelMax_D = lm(MaxL_ ~ TrtMnt_demend_y+ MeanT_ + MeanP_ + Pop_ + MaxT_ + MaxP_ + MinT_ + MinP_ + Year + 
                   TrtMnt_demend_y*HubDist + factor(reu_id),
                 data=psm_Long, weights=terrai_are)
summary(pModelMax_D)
clusterD <- cluster.vcov(pModelMax_D,cbind(psm_Long$reu_id,psm_Long$Year),force_posdef=TRUE)
CMREG_D <- coeftest(pModelMax_D, clusterD)
print(CMREG_D)

pModelMax_E = lm(MaxL_ ~ TrtMnt_demend_y+ MeanT_ + MeanP_ + Pop_ + MaxT_ + MaxP_ + MinT_ + MinP_ + Year + 
                   TrtMnt_demend_y*HubDistCat + factor(reu_id),
                 data=psm_Long, weights=terrai_are)
summary(pModelMax_E)
clusterE <- cluster.vcov(pModelMax_E,cbind(psm_Long$reu_id,psm_Long$Year),force_posdef=TRUE)
CMREG_E <- coeftest(pModelMax_E, clusterE)
print(CMREG_E)

# pModelMax_F = lm(MaxL_ ~ TrtMnt_demend_y+ MeanT_ + MeanP_ + Pop_ + MaxT_ + MaxP_ + MinT_ + MinP_ + Year + 
#                    TrtMnt_demend_y*arc*HubDist + factor(reu_id),
#                  data=psm_Long, weights=terrai_are)
# summary(pModelMax_F)
# clusterF <- cluster.vcov(pModelMax_F,cbind(psm_Long$reu_id,psm_Long$Year),force_posdef=TRUE)
# CMREG_F <- coeftest(pModelMax_F, clusterF)
# print(CMREG_F)

#-----------------------------
#Look at Arc of Deforestation

table(psm_Long$reu_id, psm_Long$UF)
psm_Long_arc<- psm_Long[psm_Long$UF=="PA" | psm_Long$UF=="TO",]
plot(psm_Long_arc$MaxL_)
hist(psm_Long_arc$MaxL_)
plot(psm_Long_TO$Year, psm_Long_TO$MaxL_)

psm_Long_TO <- psm_Long[psm_Long$UF=="TO",]
psm_Long_TO <- psm_Long_TO[psm_Long_TO$HubDistCat==0,]

ggplot(data = psm_Long_TO, aes(x=Year, y=MaxL_,group=reu_id, colour=factor(UF))) + 
  #geom_point(size=.5) +
  geom_line(size=.5, linetype=2) +
  stat_summary(fun.y=mean,aes(x=Year, y=MaxL_,group=reu_id,colour=factor(UF)),data=psm_Long_TO,geom='line',size=1.5)+
  theme(axis.text.x=element_text(angle=90,hjust=1))

#------------------------------------------------------------------------
#Workspace
#------------------------------------------------------------------------

dta_Shp@data$MaxL_diff <- dta_Shp@data$MaxL_2001-dta_Shp@data$MaxL_2000
summary(dta_Shp@data$MaxL_diff)
sd(dta_Shp@data$MaxL_diff)

summary(dta_Shp@data$MaxL_2000)
sd(dta_Shp@data$MaxL_2000)

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

library(R2HTML)
library(stargazer)

red = runif(100,0.0,1.0)
green = runif(100,0.0,1.0)
blue = runif(100,0.0,1.0)

tDF <- data.frame(red, green, blue)

testModel <- lm(red~blue + green, data=tDF)
testModel2 <- lm(blue ~ green + red, data=tDF)

table_1<-stargazer(CMREG_A,CMREG_B,CMREG_C,CMREG_D,CMREG_E,
          type="html",align=TRUE,keep=c("TrtMnt","MeanT_","MeanP_","Pop_","MaxT_","MaxP_","MinT_","MinP_","Year"),
          #covariate.labels=c("TrtMnt_demend_y","MeanT","MeanP","Pop","MaxT","MaxP","MinT","MinP","Year"),
          omit.stat=c("f","ser"),
          title="Regression Results",
          dep.var.labels=c("Max NDVI")
)

#Put the directory and name you want.  Make sure the name ends
#in ".doc".
wordFile=file.path("/Users/rbtrichler/Documents/AidData/KFW Brazil Eval/KFW_Points","wordFile.docx")
#Don't ever change this.
cat("<html xmlns:o='urn:schemas-microsoft-com:office:office' xmlns:w='urn:schemas-microsoft-com:office:word' xmlns='http://www.w3.org/TR/REC-html40'>
<head><title>Microsoft Office HTML Example</title></head>
<body>", file=wordFile)

#Add any tables you want from stargazer.
cat(table_1, append=TRUE, file=wordFile)

#You can also add normal text and line splits if you want, i.e.:
text <- "The below table provides information on the BGR model, which has NULL results!:"
cat(text,  append=TRUE, file=testdoc)


#And, I can add the other table:
#cat(table_2, append=TRUE, file=testdoc)

cat("\n</body></html>", append=TRUE, file=wordFile)


