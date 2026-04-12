#=================================================
# --------Time series modelling---------------------
#==================================================
# Load specialized time-series libraries
library(forecast)
library(tseries)
library(dplyr)
library(ggplot2)

# 1. Prepare the Time Series Object
# We aggregate claims by month to see the 'pulse' of the data
monthly_data <- data %>%
  group_by(months_as_customer) %>%
  summarise(total_claims = n()) %>%
  arrange(months_as_customer)

# Convert to a Time Series (ts) object - assuming monthly data
ts_claims <- ts(monthly_data$total_claims, frequency = 12)

# 2. Classical Decomposition
# This answers: What is 'Normal' vs 'Trend' vs 'Random Noise'?
decomp <- decompose(ts_claims, type = "additive")
plot(decomp) # This produces the 'Decomposition of additive time series' graph

# 3. Moving Average (MA2)
# This helps smooth out the 'noise' to see the local direction
autoplot(ma(ts_claims, order = 2)) +
  labs(title = "2-Period Moving Average of Claims", x = "Time", y = "Smoothed Count")

# 4. Forecasting (ARIMA Model)
# This answers: Based on the past 24 months, what happens next?
fit_arima <- auto.arima(ts_claims)
forecast_arima <- forecast(fit_arima, h = 24)
plot(forecast_arima) # This produces the 'ARIMA Forecast' graph
