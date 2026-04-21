# Load necessary libraries
library(WRTDStidal)
library(ggplot2)

# Define site details
siteNumber <- "02246500"  # St. Johns River at Jacksonville, FL
startDate <- "1999-01-26"
endDate <- "2023-11-28"
filePath <- "F:/St Johns DEM/Data Briona/WRTDS Run Data/"
fileName <- "TKN_Jacksonville.CSV"

# Read sample data (Ensure format: Date, Response, Flow, Detection Limit)
Sample <- read.csv(paste0(filePath, fileName), stringsAsFactors = FALSE)

# Check column names and rename if necessary
colnames(Sample) <- c("date", "res", "flo", "lim")  # Ensuring required format

# Convert date column to Date format
Sample$date <- as.Date(Sample$date)

# Load daily discharge data
Daily <- readNWISDaily(siteNumber, "00060", startDate, endDate)

# Remove negative discharge values (not supported in WRTDStidal)
Daily <- subset(Daily, Q >= 0)

# Merge Sample and Discharge Data
MergedData <- merge(Sample, Daily, by = "date", all.x = TRUE)

# Create tidal object
tidal_obj <- tidal(MergedData)

# Fit the WRTDS model using quantiles
tidfit <- modfit(tidal_obj, tau = c(0.1, 0.5, 0.9))

# Fit the WRTDS mean model
tidfitmean <- modfit(tidal_obj, resp_type = 'mean')

# Plot raw observed data
obsplot(tidal_obj)

# Generate fitted plot for median response
fitplot(tidfit, tau = 0.5)

# Generate seasonal trend plot
seasplot(tidfit)

# Generate year-wise seasonal variation
seasyrplot(tidfitmean, predicted = FALSE)

# Grid plot to visualize response over salinity/flow
gridplot(tidfit, month = "all")

# Normalized predicted results
prdnrmplot(tidfit)

# Weighting plot to assess regression weighting behavior
wtsplot(tidfit, ref = "2000-07-01")

# Summary of model performance
wrtdsperf(tidfit)

# Trend analysis using Seasonal Kendall Test
mobrks <- list(c(1, 2, 3), c(4, 5, 6), c(7, 8, 9), c(10, 11, 12))
yrbrks <- c(-Inf, 1985, 1994, 2003, Inf)
molabs <- c('JFM', 'AMJ', 'JAS', 'OND')
yrlabs <- c('1974-1985', '1986-1994', '1995-2003', '2004-2012')

wrtdstrnd_sk(tidfit, mobrks, yrbrks, molabs, yrlabs)

# End of script



