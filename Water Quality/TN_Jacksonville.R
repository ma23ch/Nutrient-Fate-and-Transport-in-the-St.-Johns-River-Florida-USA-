# -----------------------------
# Load required packages
# -----------------------------
library(EGRET)
library(dplyr)

# -----------------------------
# File paths and names
# -----------------------------
filePath   <- "C:/Users/ma23ch/OneDrive - Florida State University/Desktop/WRTDS/Discharge/"
fileName   <- "Jacksonville_Final.CSV"

filePath1  <- "C:/Users/ma23ch/OneDrive - Florida State University/Desktop/WRTDS/Water Quality/"
fileName1  <- "TN_Jacksonville.CSV"

fileNamein <- "TN_Jacksonville_Info.csv"

# -----------------------------
# Step 1: Read sample and daily data
# -----------------------------
Sample <- readUserSample(filePath1, fileName1, separator = ",")
Daily  <- readUserDaily(filePath, fileName, qUnit = 2)

# -----------------------------
# Step 3: Read site info
# -----------------------------
INFO <- readUserInfo(filePath1, fileNamein)

Jack
JAK
# -----------------------------
# Step 4: Merge into EGRET eList
# -----------------------------
eList <- mergeReport(INFO, Daily, Sample)

# -----------------------------
# Step 5: Run EGRET example plots
# -----------------------------
boxConcMonth(eList)
boxQTwice(eList)
plotConcTime(eList)
plotConcQ(eList) 
# plotConcQ plots: censored values as a line connecting 0 and the value; not making good plots
multiPlotDataOverview(eList)
############################
############################
# Run WRTDS model:
eList <- modelEstimation(eList)

#error statistics
errorStats(eList)
# turn off scientific notation for axis labels
old_opts <- options(scipen = 999)  # large bias against scientific notation
fluxBiasMulti(eList)

#Require Daily + INFO:
plotConcHist(eList,printTitle=FALSE)
plotFluxHist(eList,printTitle=FALSE)

outdir <- "C:/Users/ma23ch/OneDrive - Florida State University/Desktop/WRTDS/Figure/"
if (!dir.exists(outdir)) dir.create(outdir, recursive = TRUE)

# ---- Concentration histogram ----
tiff(file.path(outdir, "TN_Jacksonville_ConcHist.tif"),
     units = "in", width = 6.5, height = 5.0, res = 600,
     compression = "lzw", pointsize = 14)  # base font size for device
par(mar = c(4.6, 5.2, 0.8, 0.8), mgp = c(2.6, 0.8, 0), tcl = -0.3,
    cex.axis = 1.3, cex.lab = 1.5, cex.main = 1.5, family = "Arial",
    col.lab = "white")
plotConcHist(eList, printTitle = FALSE)
dev.off()

# turn off scientific notation for axis labels
old_opts <- options(scipen = 999)  # large bias against scientific notation

# ---- Flux histogram ----
tiff(file.path(outdir, "TN_Jacksonville_FluxHist.tif"),
     units = "in", width = 6.5, height = 5.0, res = 600,
     compression = "lzw", pointsize = 14)
par(mar = c(4.6, 5.2, 0.8, 0.8), mgp = c(2.6, 0.8, 0), tcl = -0.3,
    cex.axis = 1.3, cex.lab = 1.5, cex.main = 1.5, family = "Arial",
    col.lab = "white")
plotFluxHist(eList, printTitle = FALSE, fluxUnit = 5)
dev.off()

#table for the summary
tableFlowChange(eList, istat = 2, qUnit = 2, yearPoints = c(1999, 2004,2009,2014, 2019, 2024))
tableChange(eList, fluxUnit = 5, yearPoints = c(1999, 2004,2009,2014, 2019, 2024))
