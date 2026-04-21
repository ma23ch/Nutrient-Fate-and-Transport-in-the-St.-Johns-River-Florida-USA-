# Load required libraries
library(readxl)
library(ggplot2)
library(tseries)
library(forecast)
library(urca)
library(lubridate)
library(openxlsx)
library(imputeTS) # for filling historical missing values

# ==== 1. Read Excel file ====
filePath <- "F:/St Johns DEM/Discharge"
fileName <- "Satsuma_Final.xlsx"
fullPath <- file.path(filePath, fileName)

df <- read_excel(fullPath)

# ==== 2. Order by date & filter from 1990-01-01 ====
df <- df[order(df$Date), ]
df <- df[df$Date >= as.Date("1992-10-02"), ]

# ==== 3. Create time series object ====
ts_data <- ts(df[[2]],
              start = c(year(min(df$Date)), yday(min(df$Date))),
              frequency = 365)

# ==== 4. Fill historical NAs using Kalman smoothing with ARIMA ====
ts_filled <- na_kalman(ts_data, model = "auto.arima")

# ==== 5. Fit ARIMA to filled series ====
fit <- auto.arima(ts_filled)
summary(fit)
checkresiduals(fit)

# ==== 6. Forecast future values ====
# Number of future days you want to forecast
future_steps <- 100 # change as needed
forecast_result <- forecast(fit, h = future_steps)

# ==== 7. Replace NAs in historical period with model estimates ====
final_filled <- ts_filled # already filled from step 4

# ==== 8. Combine filled historical + future forecast ====
final_series <- c(final_filled, forecast_result$mean)

# ==== 9. Save to Excel ====
output_df <- data.frame(
  Date = seq(from = min(df$Date),
             by = "day",
             length.out = length(final_filled) + future_steps),
  Qdaily = as.numeric(final_series)
)

write.xlsx(output_df, "F:/St Johns DEM/Discharge/Satsuma_forecast_all.xlsx")

# ==== 10. Plot results ====
autoplot(ts_filled, series = "Filled Historical") +
  autolayer(forecast_result$mean, series = "Forecast") +
  labs(title = "Filled Historical + Future Forecast",
       x = "Time", y = "Value") +
  theme_minimal()

