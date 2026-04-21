library(EGRET)
#installed pakage.
siteNumber6 <- "02232400" #St. Johns River at Cocoa, FL
siteNumber5 <- "02234500" #St. Johns River at Sanford, FL
siteNumber4 <- "02236000" #St. Johns River at DeLand, FL
siteNumber3 <- "02244040" #St. Johns River at Satsuma, FL
siteNumber2 <- "02246500" #St. Johns River at St. Augustine, FL
siteNumber1 <- "02246500" #St. Johns River atJacksonville, FL

startDate6 <- "1990-04-25"
startDate5 <- "1990-01-12"
startDate4 <- "1998-09-15"
startDate3 <- "1996-04-09"
startDate2 <- "1990-03-13"
startDate1 <- "1999-01-26"
#Starting date of Data analysis
endDate6 <- "2023-5-17"
endDate5 <- "2023-11-08"
endDate4 <- "2024-03-11"
endDate3 <- "2023-11-16"
endDate2 <- "2014-06-25"
endDate1 <- "2023-11-28"
#ending date of data analysis
filePath <- "F:/St Johns DEM/Data Briona/WRTDS Run Data/"
#file path copy from the pc
fileName6 <- "TKN_Rockledge.CSV"
fileName5 <- "TKN_Sanford.CSV"
fileName4 <- "TKN_Astor.CSV"
fileName3 <- "TKN_CowCreek.CSV"
fileName2 <- "TKN_StAugustine.CSV"
fileName1 <- "TKN_Jacksonville.CSV"
#file name from the desire file
Sample6 <-readUserSample(filePath,fileName6,separator=",")
Sample5 <-readUserSample(filePath,fileName5,separator=",")
Sample4 <-readUserSample(filePath,fileName4,separator=",")
Sample3 <-readUserSample(filePath,fileName3,separator=",")
Sample2 <-readUserSample(filePath,fileName2,separator=",")
Sample1 <-readUserSample(filePath,fileName1,separator=",")
#sample will shown up with egret formart
Daily6 <- readNWISDaily(siteNumber6,"00060",startDate6,endDate6)
Daily5 <- readNWISDaily(siteNumber5,"00060",startDate5,endDate5)
Daily4 <- readNWISDaily(siteNumber4,"00060",startDate4,endDate4)
Daily3 <- readNWISDaily(siteNumber3,"00060",startDate3,endDate3)
Daily2 <- readNWISDaily(siteNumber2,"00060",startDate2,endDate2)
Daily1 <- readNWISDaily(siteNumber1,"00060",startDate1,endDate1)
#Daily should be with this kind of coding, and separator shoud use comma or "," "/t".
fileNamein6<-"TKN_Rockledge_Info.csv"
fileNamein5<-"TKN_Sanford_Info.csv"
fileNamein4<-"TKN_Astor_Info.csv"
fileNamein3<-"TKN_CowCreek_Info.csv"
fileNamein2<-"TKN_StAugustine_Info.csv"
fileNamein1<-"TKN_Jacksonville_Info.csv"
INFO6 <- readUserInfo(filePath, fileNamein6)
#station name Rockledge
Rockledge
Dwst
INFO5 <- readUserInfo(filePath, fileNamein5)
#station name Sanford
Sanford
Dwst
INFO4 <- readUserInfo(filePath, fileNamein4)
#station name Astor
Astor
Dwst
INFO3 <- readUserInfo(filePath, fileNamein3)
#station name Cow Creek
CowCreek
Dwst
INFO2 <- readUserInfo(filePath, fileNamein2)
#station name St Augustine
StAugustine
Upst
INFO1 <- readUserInfo(filePath, fileNamein1)
#station name Rockledge
Jacksonville
Upst
eList6 <- mergeReport(INFO6, Daily6,Sample6)
eList5 <- mergeReport(INFO5, Daily5,Sample5)
eList4 <- mergeReport(INFO4, Daily4,Sample4)
eList3 <- mergeReport(INFO3, Daily3,Sample3)
eList2 <- mergeReport(INFO2, Daily2,Sample2)
eList1 <- mergeReport(INFO1, Daily1,Sample1)
#eListNew <- as.egret(INFO, Daily, Sample, surfaces)
#INFO <- readUserInfo(filePath,fileName, separator=",",interactive=FALSE)
# Check sample data:
boxConcMonth(eList6)
boxConcMonth(eList5)
boxConcMonth(eList4)
boxConcMonth(eList3)
boxConcMonth(eList2)
boxConcMonth(eList1)

# Set up a 2-row, 3-column layout
par(mfrow = c(2, 3))  # 2 rows, 3 columns

# Plot box plots for each eList without y-axis
boxConcMonth(eList6, printTitle = FALSE, cex.axis = 1.5, cex.lab = 2.5, yaxp = c(0, 5, 5))
boxConcMonth(eList5, printTitle = FALSE, cex.axis = 1.5, cex.lab = 2.5, yaxp = c(0, 5, 5))
boxConcMonth(eList4, printTitle = FALSE, cex.axis = 1.5, cex.lab = 2.5, yaxp = c(0, 5, 5))
boxConcMonth(eList3, printTitle = FALSE, cex.axis = 1.5, cex.lab = 2.5, yaxp = c(0, 5, 5))
boxConcMonth(eList2, printTitle = FALSE, cex.axis = 1.5, cex.lab = 2.5, yaxp = c(0, 5, 5))
boxConcMonth(eList1, printTitle = FALSE, cex.axis = 1.5, cex.lab = 2.5, yaxp = c(0, 5, 5))

# Reset plotting layout
par(mfrow = c(1, 1))

# Close the device to save the image
dev.off()



boxQTwice(eList)
plotConcTime(eList)
plotConcQ(eList) 
# plotConcQ plots: censored values as a line connecting 0 and the value; not making good plots
multiPlotDataOverview(eList)
############################
############################
# Run WRTDS model:
eList <- modelEstimation(eList)

#Require Daily + INFO:
plotConcHist(eList,printTitle=FALSE)
plotFluxHist(eList,printTitle=FALSE)