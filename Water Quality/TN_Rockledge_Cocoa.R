# -----------------------------
# Load required packages
# -----------------------------
library(EGRET)
library(dplyr)

# -----------------------------
# Define function to adjust low flows
# -----------------------------
adjustLowFlow <- function(Daily, Sample, minQprop) {
  
  # Extract Date and Q for adjustment
  DailyQ <- Daily %>%
    select(Date, Q)
  
  # Match sample dates to daily flow
  SampleQ <- Sample %>%
    left_join(DailyQ, by = join_by(Date))
  
  # Minimum allowable cfs based on sample days
  min_cfs <- min(SampleQ$Q[which(SampleQ$Q > 0)]) * minQprop
  
  # Loop through and adjust low flows
  for (i in 1:(nrow(DailyQ) - 1)) {
    if (DailyQ$Q[i] >= min_cfs) { next }
    increaseQ <- min_cfs - DailyQ$Q[i]
    DailyQ$Q[i] <- DailyQ$Q[i] + increaseQ
    DailyQ$Q[i + 1] <- DailyQ$Q[i + 1] - increaseQ
  }
  
  # Replace Q values in original Daily dataframe
  Daily$Q <- DailyQ$Q
  
  return(Daily)
}

# -----------------------------
# File paths and names
# -----------------------------
filePath   <- "C:/Users/ma23ch/OneDrive - Florida State University/Desktop/WRTDS/Discharge/"
fileName   <- "Cocoa_Final.CSV"

filePath1  <- "C:/Users/ma23ch/OneDrive - Florida State University/Desktop/WRTDS/Water Quality/"
fileName1  <- "TN_Rockledge.CSV"

fileNamein <- "TN_Rockledge_Info.csv"

# -----------------------------
# Step 1: Read sample and daily data
# -----------------------------
Sample <- readUserSample(filePath1, fileName1, separator = ",")
Daily  <- readUserDaily(filePath, fileName, qUnit = 2)

# -----------------------------
# Step 2: Adjust low flows
# -----------------------------
Daily <- adjustLowFlow(Daily, Sample, minQprop = 0.5)
# You can change minQprop (e.g., 0.2, 0.5, 1.0) depending on your needs

# -----------------------------
# Step 3: Read site info
# -----------------------------
INFO <- readUserInfo(filePath1, fileNamein)

RLC

RC

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
tiff(file.path(outdir, "TN_Rockledge_ConcHist.tif"),
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
tiff(file.path(outdir, "TN_Rockledge_FluxHist.tif"),
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
