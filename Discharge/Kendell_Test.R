#-------------------------------------------------
# Load required packages
#-------------------------------------------------
library(EGRET)
library(dplyr)
library(lubridate)
library(rkt)   # For Seasonal Kendall test

#-------------------------------------------------
# File paths and file names
#-------------------------------------------------
filePath_Daily  <- "F:/St Johns DEM/Discharge/"
fileName_Daily  <- "Cocoa_test.CSV"

filePath_Sample <- "F:/St Johns DEM/Data Briona/WRTDS Run Data/"
fileName_Sample <- "TKN_Rockledge.CSV"

#-------------------------------------------------
# Read Sample and Daily data
#-------------------------------------------------
Sample <- readUserSample(filePath_Sample, fileName_Sample, separator = ",")
Daily  <- readUserDaily(filePath_Daily, fileName_Daily, qUnit = 2)

#-------------------------------------------------
# Handle negative flows: replace negative discharge with NA
#-------------------------------------------------
Daily <- Daily %>%
  mutate(Q = ifelse(Q < 0, NA, Q))

#-------------------------------------------------
# Join Sample with Daily flow data and subset only days with positive discharge and non-NA average concentration
#-------------------------------------------------
SampleQ <- Sample %>%
  left_join(Daily %>% select(Date, Q), by = "Date") %>%
  filter(!is.na(Q) & Q > 0 & !is.na(ConcAve))

#-------------------------------------------------
# Create seasonal blocks combining month and discharge sign
# (DischargeSign will all be "pos" here, but kept for completeness)
#-------------------------------------------------
SampleQ <- SampleQ %>%
  mutate(
    Month = lubridate::month(Date),
    DischargeSign = ifelse(Q > 0, "pos", "neg"),
    Block = as.integer(factor(paste0(Month, "_", DischargeSign)))  # convert to integer for rkt
  )

# Check number of observations per block
print(table(SampleQ$Block))

#-------------------------------------------------
# Convert Date to decimal year for rkt function (year + fraction of year)
#-------------------------------------------------
SampleQ <- SampleQ %>%
  mutate(
    Year = year(Date),
    DayOfYear = yday(Date),
    DecimalYear = Year + (DayOfYear - 1) / ifelse(leap_year(Year), 366, 365)
  )

#-------------------------------------------------
# Run Seasonal Kendall test
#-------------------------------------------------
sk_result <- rkt(
  SampleQ$DecimalYear,    # time: decimal year (positive)
  SampleQ$ConcAve,        # measurement
  SampleQ$Block           # season/block as integer
)

#-------------------------------------------------
# Show results
#-------------------------------------------------
print(sk_result)



      




