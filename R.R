Sys.setenv(TZ = "Etc/GMT-3")
library("openair", lib.loc="~/R/win-library/3.4")
library("reshape", lib.loc="~/R/win-library/3.4")
library("reshape2", lib.loc="~/R/win-library/3.4")
library("R.matlab", lib.loc="~/R/win-library/3.4")

# #READ METEOROLOGY
# # Import data from NCDC for AIRPORT (411710 and QU 411700)
# NCDC.Data<- read.csv("h:/MyDocs/@PROJECTS/PJ.AAE CONSTRUCTION PM/NCDC Aiport 710 QU 700.txt", sep="", na.strings=c("***", "*", "**","****", "*****","******","990"))
# NCDC<-subset(NCDC.Data,select=c(USAF, YR..MODAHRMN,DIR,SPD,TEMP))
# NCDC<-rename(NCDC,c(USAF="site",YR..MODAHRMN="date",DIR="wd",SPD="ws",TEMP="T"))
# NCDC$ws<-NCDC$ws*0.44047
# #windRose(NCDC,type="site",paddle=FALSE,breaks=c(0,1,2,3,4,5,6))
# NCDC$date <- as.POSIXct(strptime(NCDC$date, format = "%Y%m%d%H%M"))
# 
# # split Stations data
# NCDC.QU<-subset(NCDC,site==411700)
# NCDC.QU<-rename(NCDC.QU,c(ws='wsQ',wd='wdQ'))
# NCDC.DIA<-subset(NCDC,site==411710)
# NCDC.DIA<-rename(NCDC.DIA,c(ws='wsA',wd='wdA'))

#PREPARE BG DATA
fileN<-"h:/MyDocs/@PROJECTS/PJ.AAK C7290 NPRP 7-649-2-241 Fugitive/Data/AQERT 2 - June 1-5 OutDoor Home for Calibration.txt"
aqCalib<-import(fileN, date = "Datum", date.format = "%d.%m.%Y %H:%M",header.at=2,data.at=700,sep=";",dec=".",stringsAsFactors = FALSE)
drops<-c("AK", "NA")
aqCalib<-aqCalib[,!(names(aqCalib) %in% drops)]
aqCalib<-rename(aqCalib,c(W.Speed="ws",W.Direct="wd", PM10="PM10.obs",PM2.5="PM25.obs",PM1="PM1.obs"))
Conc<-aqCalib[,14:36]
Conc<-data.matrix(Conc)#particles / lt
PM<-aqCalib[,2:4]
PM<-data.matrix(PM)#ug/m3
writeMat(con="P:/Chemical Engineering/Air_Quality_Eng_R_Team/!GITHUB/PMCalibration/aqCalib.mat", aqCalibNUM=Conc,aqCalibPM=PM)



# Calculate Concentration
# Conc<-aqCalib[,14:36]
Conc<-data.matrix(Conc)#particles / lt
Diam<-c(0.265,0.290,0.325,0.375,0.425,0.475,0.540,0.615,0.675,0.750,
        0.900,1.150,1.450,1.800,2.250,2.750,3.250,3.750,4.500,5.750,7.000,8.000,9.250)
Vol<-(4/3)*pi*(Diam/2)^3
Mass<-Vol # *2500 #density in kg/m3
Conc<-sweep(Conc,MARGIN=2,Mass/1000000,'*') #�g/m3
aqCalib$PM1.mod<-rowSums (Conc[,1:11]*2750, na.rm = FALSE)
aqCalib$PM25.mod<-aqCalib$PM1.mod+rowSums (Conc[,12:15]*2850, na.rm = FALSE)
aqCalib$PM10.mod<-aqCalib$PM25.mod+rowSums (Conc[,1:23]*2180, na.rm = FALSE)

rm(Conc)
#write.csv(aqBG, file = "p:/Chemical Engineering/Air_Quality_Eng_R_Team/Papers/00 PM Construction Hala/aqBG.txt")

# # Merge with Meteorology
# aqBG.NCDC<-merge(aqBG,NCDC.QU, by ="date",all=TRUE)
# aqBG.NCDC$site<-'BackGround Site'
# aqBG.NCDC<-rename(aqBG.NCDC,c(wsQ='ws',wdQ='wd'))
# #aqBG.NCDC<-merge(aqBG.NCDC,NCDC.DIA, by ="date",all=TRUE)

# Filter data for study period
##aqBG.NCDC.Study<-subset(aqBG.NCDC,date<='2014-04-09 03:00:00')

#aqCalib.Study<-subset(aqCalib,date>='2017-05-31 12:00:00' & date<='2017-06-06 00:00:00')

# Get Matlab Data
aqCalibMod <- readMat('P:/Chemical Engineering/Air_Quality_Eng_R_Team/!GITHUB/PMCalibration/aqCalibMod.mat')
aq<-merge(aqCalib,aqCalibMod)

# Plot Data
polarPlot(aqCalib,pollutant='PM1.obs',statistic="mean", offset = 50, ws.int = 30, trans = TRUE,grid.line=2,k=10,uncertainty=FALSE,par.settings=list(fontsize=list(text=24)))                                                                           
# conditionalEval(aqCalib, obs = "PM10.obs", mod = "PM10.mod",statistic = "ws")
conditionalEval(aqCalib, obs = "PM10.obs", mod = "PM10.mod")
conditionalEval(aqCalib, obs = "PM25.obs", mod = "PM25.mod")
conditionalEval(aqCalib, obs = "PM1.obs", mod = "PM1.mod")
scatterPlot(aqCalib, x = "PM10.mod", y = "PM10.obs", method = "hexbin", col= "jet", linear=TRUE,mod.line = TRUE)
scatterPlot(aqCalib, x = "PM25.mod", y = "PM25.obs", method = "hexbin", col= "jet", linear=TRUE,mod.line = TRUE)
scatterPlot(aqCalib, x = "PM1.mod", y = "PM1.obs", method = "hexbin", col= "jet", linear=TRUE,mod.line = TRUE)
                
                
# #PREPARE CS DATA
# fileN2<-"p:/Chemical Engineering/Air_Quality_Eng_R_Team/Papers/00 PM Construction Hala/PRose-AQERT2.csv"
# aqCS<-import(fileN2, date = "date", file.type = "csv",date.format = "%Y-%m-%d %H:%M:%S",header.at=1,dec = ".",sep=";")
# #polarPlot(subset(aqCS, ws<18),pollutant='PM10',statistic="mean", grid.line=2,k=50,uncertainty=FALSE)

# Merge with Meteorology
aqCS.NCDC<-merge(aqCS,NCDC.QU, by ="date",all=TRUE)
aqCS.NCDC<-rename(aqCS.NCDC,c(PM10='PM10_calc'))
aqCS.NCDC$site<-'Construction Site'

# Filter data for study period
aqCS.NCDC.Study<-subset(aqCS.NCDC,date>='2014-04-30 11:00:00' & date<='2014-05-08 08:00:00')


# PREPARE FOR PLOTTING
pmLevel<-500

# Filtered histograms, Filtering of the data (as per Hala's excel)
dFilter2<-subset(aqCS,(wd>=345 | wd<=75))
dFilter<-subset(aqCS,(wd>=285 | wd<=75))
dFilterOut<-subset(aqCS,(wd>75 & wd<285))
dFilter2$filter<-'345-75'
dFilter$filter<-'285-75'
dFilterOut$filter<-'75-285'
pm10Filters<-rbind(dFilter2,dFilter,dFilterOut)

#ggplot(subset(pm10Filters,PM10<pmLevel),aes(PM10,fill=filter))+geom_density(alpha=0.2)

# Multiple plotting for results interpretting

#polarFreq(subset(aqCS.NCDC.Study,PM10_calc<pmLevel),pollutant='PM10_calc',statistic="weighted.mean", offset = 20, ws.int = 2, trans = FALSE,grid.line=2)
polarPlot(subset(aqCS.NCDC.Study,PM10_calc<3500 & ws<18),pollutant='PM10_calc',statistic="mean", offset = 50, ws.int = 30, trans = TRUE,grid.line=2,k=10,uncertainty=FALSE,par.settings=list(fontsize=list(text=24)))                                                                           
#polarCluster(subset(aqCS.NCDC.Study,PM10_calc<1500),pollutant='PM10_calc',n.clusters = 3,k=50)

#polarPlot(subset(aqBG.NCDC.Study,PM10_calc<15000 & ws<18),pollutant='PM10_calc',statistic="mean", grid.line=2,k=10,uncertainty=FALSE,min.bin=3,par.settings=list(fontsize=list(text=24)))
polarPlot(subset(aqBG.NCDC.Study,PM10_calc<500 & ws<18),pollutant='PM10_calc',statistic="mean", grid.line=2,min.bin=1,k=10,uncertainty=FALSE,par.settings=list(fontsize=list(text=24)))

#polarFreq(subset(aqBG.NCDC.Study,PM10_calc<5000),pollutant='PM10_calc',statistic="mean", offset = 50, ws.int = 30, trans = FALSE,grid.line=2)
#percentileRose(subset(aqBG.NCDC.Study,ws<8. & PM10_calc<170), pollutant = "PM10_calc",smooth = FALSE)

# join the two sites
aqCS.NCDC.Study<-rename(aqCS.NCDC.Study,c(ws='wsCS',wd='WdCS',wsQ='ws',wdQ='wd'))
t1<-subset(aqCS.NCDC.Study,select=c('ws','wd','site','PM10_calc'),na.rm=TRUE)
t2<-subset(aqBG.NCDC.Study,select=c('ws','wd','site','PM10_calc'),na.rm=TRUE)
aq.NCDC.study<-rbind(t1,t2)
rm(t1,t2)
#polarPlot(subset(aq.NCDC.study,PM10_calc<15000 & ws<18),pollutant='PM10_calc',statistic="mean", grid.line=2,k=10,uncertainty=FALSE,type='site')
#percentileRose(subset(aq.NCDC.study,ws<18. & PM10_calc<500), pollutant = "PM10_calc",smooth = FALSE,type='site',angle=11.25)
polarFreq(subset(aq.NCDC.study,PM10_calc<1500 & ws<18),pollutant='PM10_calc',ws.int = 2,statistic="weighted.mean", grid.line=2,type='site',par.settings=list(fontsize=list(text=18)))