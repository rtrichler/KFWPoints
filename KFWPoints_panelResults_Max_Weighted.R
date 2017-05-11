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

#-----
# ** Build panel dataset - skip to reading it in if don't need to make any changes **

#-------------------------------------------------
#-------------------------------------------------
#Convert from a wide-form dataset for the Cross-sectional 
#to a long-form dataset for the panel model.
#-------------------------------------------------
#-------------------------------------------------

#drop out unsed time-varying vars
reshape <- dta_Shp
reshape<-reshape[,order(names(reshape))]
reshape1<-reshape[,-grep("(_198)",names(reshape))]
reshape2<-reshape1[,-grep("(_199)",names(reshape1))]
reshape3<-reshape2[,-grep("(ntl)",names(reshape2))]

#prep to convert to long form panel dataset
kfw_wide<-reshape3
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
psm_Long <- reshape(kfw_wide@data, varying=all_reshape, direction="long",idvar="ad_id",sep="_",timevar="Year")

## Add treatment and other vars

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

#create categorical variable for distance to boundary
psm_Long$HubDistCat<-0
psm_Long$HubDistCat[psm_Long$HubDist>5]<-1

#create arc of deforestation variable
psm_Long$arc<-0
psm_Long$arc[which(psm_Long$UF=="PA" | psm_Long$UF=="TO")] <- 1

#write.csv(psm_Long,file="/Users/rbtrichler/Documents/AidData/KFW Brazil Eval/KFW_Points/ProcessedData/psm_Long.csv")

#-----

## *READ IN PANEL DATASET* (use unless you need to rebuild panel)
psm_Long= read.csv("/Users/rbtrichler/Documents/AidData/KFW Brazil Eval/KFW_Points/ProcessedData/psm_Long.csv")


#-------------------------------------------------
#-------------------------------------------------
#Run Panel Models
#-------------------------------------------------
#-------------------------------------------------

pModelMax_A = lm(MaxL ~ trtdem + factor(reu_id),data=psm_Long, weights=terrai_are)
summary(pModelMax_A)
clusterA <- cluster.vcov(pModelMax_A,cbind(psm_Long$reu_id,psm_Long$Year),force_posdef=TRUE)
CMREG_A <- coeftest(pModelMax_A, clusterA)
#print(CMREG_A)

pModelMax_B = lm(MaxL ~ trtdem+ MeanT + MeanP  + MaxT + MaxP + MinT + MinP + Pop + factor(reu_id),
                 data=psm_Long, weights=terrai_are)
summary(pModelMax_B)
clusterB <- cluster.vcov(pModelMax_B,cbind(psm_Long$reu_id,psm_Long$Year),force_posdef=TRUE)
CMREG_B <- coeftest(pModelMax_B, clusterB)
#print(CMREG_B)

pModelMax_C = lm(MaxL ~ trtdem+ MeanT + MeanP  + MaxT + MaxP + MinT + MinP + Pop+Year + factor(reu_id),
                 data=psm_Long, weights=terrai_are)
summary(pModelMax_C)
clusterC <- cluster.vcov(pModelMax_C,cbind(psm_Long$reu_id,psm_Long$Year),force_posdef=TRUE)
CMREG_C <- coeftest(pModelMax_C, clusterC)
#print(CMREG_C)

pModelMax_D = lm(MaxL ~ trtdem+ MeanT + MeanP  + MaxT + MaxP + MinT + MinP + Pop+ factor(Year) + factor(reu_id),
                 data=psm_Long, weights=terrai_are)
summary(pModelMax_D)
clusterD <- cluster.vcov(pModelMax_D,cbind(psm_Long$reu_id,psm_Long$Year),force_posdef=TRUE)
CMREG_D <- coeftest(pModelMax_D, clusterD)
#print(CMREG_D)

pModelMax_E = lm(MaxL ~ trtdem+ MeanT + MeanP  + MaxT + MaxP + MinT + MinP + Pop+ 
                   trtdem*HubDist + factor(reu_id)+factor(Year),
                 data=psm_Long, weights=terrai_are)
summary(pModelMax_E)
clusterE <- cluster.vcov(pModelMax_E,cbind(psm_Long$reu_id,psm_Long$Year),force_posdef=TRUE)
CMREG_E <- coeftest(pModelMax_E, clusterE)
#print(CMREG_E)

pModelMax_F = lm(MaxL ~ trtdem+ MeanT + MeanP  + MaxT + MaxP + MinT + MinP + Pop + 
                   trtdem*HubDistCat + factor(reu_id)+ factor(Year),
                 data=psm_Long, weights=terrai_are)
summary(pModelMax_F)
clusterF <- cluster.vcov(pModelMax_F,cbind(psm_Long$reu_id,psm_Long$Year),force_posdef=TRUE)
CMREG_F <- coeftest(pModelMax_F, clusterF)
#print(CMREG_F)


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
#------------------------------------------------------------------------

##set stargazer options
stargazer(pModelMax_A_fit $cmreg,pModelMax_B_fit $cmreg,pModelMax_C_fit $cmreg,pModelMax_D_fit$cmreg,
          pModelMax_E_fit$cmreg,
          type="html",align=TRUE,keep=c("TrtMnt","MeanT_","MeanP_","Pop_","MaxT_","MaxP_","MinT_","MinP_","Year"),
          #covariate.labels=c("TrtMnt_demend_y","MeanT","MeanP","Pop","MaxT","MaxP","MinT","MinP","Year"),
          omit.stat=c("f","ser"),
          title="Regression Results",
          dep.var.labels=c("Max NDVI")
)

stargazer(CMREG_A,CMREG_B,CMREG_C,CMREG_D,CMREG_E,CMREG_F, CMREG_G, CMREG_H,
          type="html",align=TRUE,keep=c("TrtMnt","Pop_","MeanT_","MeanP_","MaxT_","MaxP_","MinT_","MinP_","Year"),
          #covariate.labels=c("Treatment (Demarcation)","Population","Mean Temp","Mean Precip",
          #                   "Max Temp","Max Precip","Min Temp","Min Precip","Year","Treatment*Boundary Distance",
          #                   "Treatment*Boundary Distance(Cat)"),
          add.lines=list(c("Observations","148,230","148,230","148,230","148,230","148,230","148,230","148,230","148,230"),
                         c("Year Fixed Effects?","No","No","No","No","No","Yes","Yes","Yes")),
          omit.stat=c("f","ser"),
          title="Regression Results",
          dep.var.labels=c("Max NDVI")
)

#Weighted Summary Stats
psm_Long$commwt <- 1/psm_Long$terrai_are

stat_vars<-c("MaxL","Slope","Riv_Dist","Road_dist","Elevation","terrai_are","Pop",
             "MeanT","MeanP","MinT","MinP","MaxT","MaxP","HubDist","commwt")
stats<-psm_Long[,(names(psm_Long) %in% stat_vars)]


#Used for JEEM submission
#Import html into Excel and modify within Excel (for JEEM 2nd resubmission)
stargazer(CMREG_A,CMREG_B,CMREG_C,CMREG_D,CMREG_E,CMREG_F,
                   type="html",align=TRUE,
                  omit=c("factor","Constant"), 
                  covariate.labels=c("Treatment (Demarcation)","Mean Temp","Mean Precip",
                                     "Max Temp","Max Precip","Min Temp","Min Precip","Population","Year",
                                     "Boundary Distance","Bounday Distance (Cat)",
                                     "Treatment (Dem) * Boundary Distance",
                                     "Treatment (Dem) * Boundary Distance (Cat)" ),
                   add.lines=list(c("Observations","148,230","148,230","148,230","148,230","148,230","148,230"),
                                  c("Community Fixed Effects?","Yes","Yes","Yes","Yes","Yes","Yes"),
                                  c("Year Fixed Effects?","No","No","No","Yes","Yes","Yes")),
                   omit.stat=c("f","ser"),
                   title="Regression Results",
                   star.cutoffs = c(0.05, 0.01, 0.001),
                   dep.var.labels=c("Max NDVI")
)


##output tables directly to word doc

library(R2HTML)
library(stargazer)

red = runif(100,0.0,1.0)
green = runif(100,0.0,1.0)
blue = runif(100,0.0,1.0)

tDF <- data.frame(red, green, blue)

testModel <- lm(red~blue + green, data=tDF)
testModel2 <- lm(blue ~ green + red, data=tDF)


table_1<-stargazer(CMREG_A,CMREG_B,CMREG_C,CMREG_D,CMREG_E,CMREG_F,
          type="html",align=TRUE,keep=c("TrtMnt","Pop_","MeanT_","MeanP_","MaxT_","MaxP_","MinT_","MinP_","Year"),
          covariate.labels=c("Treatment (Demarcation)","Population","Mean Temp","Mean Precip",
                             "Max Temp","Max Precip","Min Temp","Min Precip","Year"),
          add.lines=list(c("Observations","148,230","148,230","148,230","148,230","148,230","148,230"),
                         c("Community Fixed Effects?","Yes","Yes","Yes","Yes","Yes","Yes"),
                         c("Year Fixed Effects?","No","No","No","Yes","Yes","Yes")),
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


#----------------
# Scratch/Workspace/Archive
#----------------



pModelMax_F = lm(MaxL_ ~ TrtMnt_demend_y+ MeanT_ + MeanP_ + Pop_ + MaxT_ + MaxP_ + MinT_ + MinP_ + Year +
                   TrtMnt_demend_y*arc*HubDist + factor(reu_id),
                 data=psm_Long, weights=terrai_are)
summary(pModelMax_F)
clusterF <- cluster.vcov(pModelMax_F,cbind(psm_Long$reu_id,psm_Long$Year),force_posdef=TRUE)
CMREG_F <- coeftest(pModelMax_F, clusterF)
print(CMREG_F)


dta_Shp@data$MaxL_diff <- dta_Shp@data$MaxL_2001-dta_Shp@data$MaxL_2000
summary(dta_Shp@data$MaxL_diff)
sd(dta_Shp@data$MaxL_diff)

summary(dta_Shp@data$MaxL_2000)
sd(dta_Shp@data$MaxL_2000)

## !incorrect code for building panel dataset (but used for first two JEEM submissions)!
# varList = c("MaxL_")
# psm_Long <- BuildTimeSeries(dta=dta_Shp,idField="ad_id",varList_pre=varList,2000,2014,colYears=c("demend_y","apprend_y","regend_y"),
#                             interpYears=c("Slope","Road_dist","Riv_Dist","UF","Elevation","urbtravtim","terrai_are","Pop_","MeanT_","MeanP_","MaxT_","MaxP_","MinP_","MinT_","ntl_","HubDist","HubName","reu_id"))
# psm_Long$Year <- as.numeric(psm_Long$Year)
# 
# write.csv(psm_Long,file="/Users/rbtrichler/Documents/AidData/KFW Brazil Eval/KFW_Points/ProcessedData/psm_Long.csv")
# psm_Long= read.csv("/Users/rbtrichler/Documents/AidData/KFW Brazil Eval/KFW_Points/ProcessedData/psm_Long.csv")

