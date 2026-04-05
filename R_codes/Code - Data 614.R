# ----------------------------------------------------------------------------------
# ----------------------------------------------------------------------------------
# 1.	What time-series patterns exist in project spend and milestone completion rates across different project types and phases? 
# Load Libraries
library(dplyr)
library(ggplot2)
library(forecast)
library(zoo)

# Load Data
url <- "https://data.cityofnewyork.us/resource/fb86-vt7u.csv?$limit=50000"
df <- read.csv(url)

# Data Cleaning
df$fms_data_date <- as.Date(df$fms_data_date)
df$current_phase <- gsub("[()]", "", df$current_phase)

df <- df %>%
  filter(!is.na(total_budget), !is.na(spend_to_date))

# Fix Time Variable (use reporting_period instead of snapshot date)
df$reporting_period <- as.character(df$reporting_period)
df$YearMonth <- as.yearmon(df$reporting_period, "%Y%m")

# Aggregate Monthly Spend
ts_data <- df %>%
  group_by(YearMonth) %>%
  summarise(
    TotalSpend = sum(spend_to_date, na.rm = TRUE)
  ) %>%
  arrange(YearMonth)

# Create Complete Monthly Sequence (fill missing months)
all_months <- data.frame(
  YearMonth = seq(min(ts_data$YearMonth), max(ts_data$YearMonth), by = 1/12)
)

ts_data <- merge(all_months, ts_data, all.x = TRUE)
ts_data$TotalSpend[is.na(ts_data$TotalSpend)] <- 0

# Convert to Time Series Object
ts_spend <- ts(ts_data$TotalSpend, frequency = 12)

# STL Decomposition
ts_spend_clean <- na.omit(ts_spend)

if(length(ts_spend_clean) >= 24) {
  stl_decomp <- stl(ts_spend_clean, s.window = "periodic")
  plot(stl_decomp, main = "STL Decomposition of Project Spend")
} else {
  cat("Not enough data for STL decomposition\n")
}

# Rolling Statistics (6-month window)
ts_data <- ts_data %>%
  mutate(
    RollingMean = rollmean(TotalSpend, k = 6, fill = NA),
    RollingSD   = rollapply(TotalSpend, width = 6, FUN = sd, fill = NA)
  )

# Rolling Mean Plot
ggplot(ts_data, aes(x = YearMonth)) +
  geom_line(aes(y = TotalSpend), color = "gray") +
  geom_line(aes(y = RollingMean), color = "blue", size = 1) +
  ggtitle("Rolling Mean of Project Spend") +
  theme_minimal()

# ACF / PACF Analysis
acf(ts_spend, main = "ACF: Project Spend")
pacf(ts_spend, main = "PACF: Project Spend")

# Phase-Level Spend Analysis
phase_summary <- df %>%
  group_by(current_phase) %>%
  summarise(
    AvgSpend = mean(spend_to_date, na.rm = TRUE)
  )

ggplot(phase_summary, aes(x = reorder(current_phase, AvgSpend), y = AvgSpend)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  coord_flip() +
  ggtitle("Average Spend by Project Phase") +
  theme_minimal()

# Milestone / Phase Progression Over Time
phase_map <- c(
  "Pre-Design" = 1,
  "Design" = 2,
  "Procurement" = 3,
  "Construction" = 4,
  "Completed" = 5
)

df$PhaseNum <- phase_map[df$current_phase]

phase_ts <- df %>%
  group_by(YearMonth) %>%
  summarise(
    AvgPhase = mean(PhaseNum, na.rm = TRUE)
  )

ggplot(phase_ts, aes(x = YearMonth, y = AvgPhase)) +
  geom_line(color = "purple") +
  ggtitle("Average Project Phase Progression Over Time") +
  theme_minimal()

# ----------------------------------------------------------------------------------
# ----------------------------------------------------------------------------------
# Question 2: Can we forecast whether a project will exceed its budget or miss its deadline, and by how much? 
# Load Libraries
library(dplyr)
library(forecast)
library(ggplot2)

# Check the number of rows in the ts_data
cat("Number of rows in ts_data:", nrow(ts_data), "\n")

# Ensure the time series is monthly with a valid frequency
# Create time series objects (monthly frequency, 12 months per year)
ts_spend <- ts(ts_data$TotalSpend, frequency = 12)

# Check if ts_spend has enough data points for decomposition
cat("Length of ts_spend:", length(ts_spend), "\n")

# ARIMA Forecasting for Project Spend (with small data)
arima_model <- auto.arima(ts_spend)
forecast_arima <- forecast(arima_model, h = 12)

# Plot ARIMA forecast for Project Spend
plot(forecast_arima, main = "ARIMA Forecast: Project Spend")

# ETS Forecasting for Project Spend (with small data)
ets_model <- ets(ts_spend)
forecast_ets <- forecast(ets_model, h = 12)

# Plot ETS forecast for Project Spend
plot(forecast_ets, main = "ETS Forecast: Project Spend")

# Skip STL decomposition since there are less than 12 data points
cat("Skipping STL decomposition as there are fewer than 12 data points.\n")

# Forecasting Cost Overrun (%) using ARIMA
arima_overrun <- auto.arima(ts_data$AvgOverrun)
forecast_overrun <- forecast(arima_overrun, h = 12)

# Plot ARIMA forecast for Cost Overrun
plot(forecast_overrun, main = "ARIMA Forecast: Cost Overrun (%)")

# Output Forecast Values
cat("Next 12-Month Forecasted Spend (ARIMA):\n")
print(forecast_arima$mean)

cat("\nNext 12-Month Forecasted Cost Overrun (%):\n")
print(forecast_overrun$mean)

# ----------------------------------------------------------------------------------
# ----------------------------------------------------------------------------------
# Question 3: Which project-level factors most strongly predict cost overruns and schedule delays? 

# Load Library
library(dplyr)
library(ggplot2)
library(caret)

# Load data
url <- "https://data.cityofnewyork.us/resource/fb86-vt7u.csv"
df <- read.csv(url)

# Quick check
dim(df)
head(df)

# Data Cleaning
# Convert dates
df$fms_data_date <- as.Date(df$fms_data_date)

# Clean phase text
df$current_phase <- gsub("[()]", "", df$current_phase)

# Remove missing budget
df <- df %>% filter(!is.na(total_budget))

# Feature Engineering 
# Cost overrun proxy:
# If spend exceeds budget → overrun
df <- df %>%
  mutate(
    CostOverrunPct = (spend_to_date - total_budget) / total_budget * 100
  )

# Schedule delay proxy:
# Encode phases as numeric progression
phase_map <- c(
  "Pre-Design" = 1,
  "Design" = 2,
  "Procurement" = 3,
  "Construction" = 4,
  "Completed" = 5
)

df$PhaseNum <- phase_map[df$current_phase]

# Use reporting period as time indicator
df <- df %>%
  mutate(
    ScheduleDelay = PhaseNum * 10   # proxy: later phase = more time
  )

# Synthetic Variables 
set.seed(123)

df <- df %>%
  mutate(
    TeamSize = sample(5:50, n(), replace = TRUE),
    PermitComplexity = sample(1:5, n(), replace = TRUE),
    ChangeOrderCount = sample(0:20, n(), replace = TRUE)
  )

# Interaction term
df <- df %>%
  mutate(TeamSize_x_Permit = TeamSize * PermitComplexity)

# Remove NA rows
df <- na.omit(df)

# Train/Test 
set.seed(42)
trainIndex <- createDataPartition(df$CostOverrunPct, p = 0.8, list = FALSE)

train <- df[trainIndex, ]
test  <- df[-trainIndex, ]

# Regression Model
# Cost Model
model_cost <- lm(
  CostOverrunPct ~ TeamSize + PermitComplexity + ChangeOrderCount + TeamSize_x_Permit,
  data = train
)

summary(model_cost)

# Delay Model
model_delay <- lm(
  ScheduleDelay ~ TeamSize + PermitComplexity + ChangeOrderCount + TeamSize_x_Permit,
  data = train
)

summary(model_delay)

# Prediction
pred_cost <- predict(model_cost, test)
pred_delay <- predict(model_delay, test)

# Evalutation
rmse <- function(a, p) sqrt(mean((a - p)^2))
mae  <- function(a, p) mean(abs(a - p))

cat("Cost RMSE:", rmse(test$CostOverrunPct, pred_cost), "\n")
cat("Cost MAE :", mae(test$CostOverrunPct, pred_cost), "\n")

cat("Delay RMSE:", rmse(test$ScheduleDelay, pred_delay), "\n")
cat("Delay MAE :", mae(test$ScheduleDelay, pred_delay), "\n")

# Feature Importance
coef_cost <- data.frame(
  Feature = names(coef(model_cost)),
  Coefficient = coef(model_cost)
)

coef_delay <- data.frame(
  Feature = names(coef(model_delay)),
  Coefficient = coef(model_delay)
)

# Plots
# Cost Driver
ggplot(coef_cost[-1,], aes(x = reorder(Feature, Coefficient), y = Coefficient)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  coord_flip() +
  ggtitle("Top Drivers of Cost Overrun") +
  theme_minimal()

# Delay Driver
ggplot(coef_delay[-1,], aes(x = reorder(Feature, Coefficient), y = Coefficient)) +
  geom_bar(stat = "identity", fill = "darkred") +
  coord_flip() +
  ggtitle("Top Drivers of Schedule Delay") +
  theme_minimal()

# Actual vs Predicted 
# Cost
ggplot(data.frame(actual=test$CostOverrunPct, pred=pred_cost),
       aes(x=actual, y=pred)) +
  geom_point(alpha=0.6) +
  geom_abline(slope=1, intercept=0, color="red") +
  ggtitle("Actual vs Predicted Cost Overrun") +
  theme_minimal()

# Delay
ggplot(data.frame(actual=test$ScheduleDelay, pred=pred_delay),
       aes(x=actual, y=pred)) +
  geom_point(alpha=0.6) +
  geom_abline(slope=1, intercept=0, color="red") +
  ggtitle("Actual vs Predicted Schedule Delay") +
  theme_minimal()

# ----------------------------------------------------------------------------------
# ----------------------------------------------------------------------------------
# Question 4:	Can an AI automation layer extract structured risk features from a plain-text project brief and generate a real-time risk assessment? 

# Load libraries
library(stringr)  # for text parsing

# Example plain-text project brief
project_brief <- "
Project: New Community Center
Type: Construction
Estimated team: 25 people
Phases: Design, Procurement, Construction
Location: Brooklyn
Permit requirements: Moderate
Expected duration: 12 months
"

# Simple feature extraction (LLM or regex-based for demo)
# In practice, an LLM would parse free text. Here, we simulate extraction.

# Extract project type
project_type <- ifelse(str_detect(project_brief, regex("Construction", ignore_case = TRUE)), "Construction", "Other")

# Extract team size
team_size <- as.numeric(str_extract(project_brief, "(?<=team: )\\d+"))

# Count number of phases
phases <- str_extract_all(project_brief, "Design|Procurement|Construction|Pre-Design|Completed")[[1]]
phase_count <- length(phases)

# Extract permit complexity (1-5 scale)
permit_complexity <- ifelse(str_detect(project_brief, regex("Moderate", ignore_case = TRUE)), 3,
                            ifelse(str_detect(project_brief, regex("High", ignore_case = TRUE)), 5, 2))

# Interaction term
team_x_permit <- team_size * permit_complexity

# Create feature dataframe
feature_df <- data.frame(
  TeamSize = team_size,
  PermitComplexity = permit_complexity,
  ChangeOrderCount = 5,  # default/estimated
  TeamSize_x_Permit = team_x_permit
)

# Predict cost overrun & schedule delay using trained models
pred_cost <- predict(model_cost, feature_df)
pred_delay <- predict(model_delay, feature_df)

# Risk Assessment
# Convert to probability-style risk (demo example)
risk_cost <- min(max(pred_cost / 100, 0), 1)  # simple normalization
risk_delay <- min(max(pred_delay / 50, 0), 1)  # assuming max delay ~50 days

cat("AI Risk Assessment for Project:\n")
cat(sprintf("Predicted Cost Overrun: %.2f%%  | Estimated Risk: %.0f%%\n", pred_cost, risk_cost*100))
cat(sprintf("Predicted Schedule Delay: %.1f days | Estimated Risk: %.0f%%\n", pred_delay, risk_delay*100))

# ----------------------------------------------------------------------------------
# ----------------------------------------------------------------------------------
# Question 5: Can machine learning models improve prediction accuracy over classical forecasting? 
# Machine Learning Models
library(randomForest)
library(gbm)
library(reshape2)

# Random Forest
rf_cost <- randomForest(
  CostOverrunPct ~ TeamSize + PermitComplexity + ChangeOrderCount + TeamSize_x_Permit,
  data = train,
  ntree = 200
)

rf_delay <- randomForest(
  ScheduleDelay ~ TeamSize + PermitComplexity + ChangeOrderCount + TeamSize_x_Permit,
  data = train,
  ntree = 200
)

# Gradient Boosting
gbm_cost <- gbm(
  CostOverrunPct ~ TeamSize + PermitComplexity + ChangeOrderCount + TeamSize_x_Permit,
  data = train,
  distribution = "gaussian",
  n.trees = 200,
  interaction.depth = 3,
  shrinkage = 0.05,
  verbose = FALSE
)

gbm_delay <- gbm(
  ScheduleDelay ~ TeamSize + PermitComplexity + ChangeOrderCount + TeamSize_x_Permit,
  data = train,
  distribution = "gaussian",
  n.trees = 200,
  interaction.depth = 3,
  shrinkage = 0.05,
  verbose = FALSE
)

# Random Forest predictions
pred_rf_cost  <- predict(rf_cost, test)
pred_rf_delay <- predict(rf_delay, test)

# GBM predictions
pred_gbm_cost  <- predict(gbm_cost, test, n.trees = 200)
pred_gbm_delay <- predict(gbm_delay, test, n.trees = 200)

# Compare ALL models (Linear vs ML)
results <- data.frame(
  Model = c("Linear", "Random Forest", "Gradient Boosting"),
  
  Cost_RMSE = c(
    rmse(test$CostOverrunPct, pred_cost),
    rmse(test$CostOverrunPct, pred_rf_cost),
    rmse(test$CostOverrunPct, pred_gbm_cost)
  ),
  
  Cost_MAE = c(
    mae(test$CostOverrunPct, pred_cost),
    mae(test$CostOverrunPct, pred_rf_cost),
    mae(test$CostOverrunPct, pred_gbm_cost)
  ),
  
  Delay_RMSE = c(
    rmse(test$ScheduleDelay, pred_delay),
    rmse(test$ScheduleDelay, pred_rf_delay),
    rmse(test$ScheduleDelay, pred_gbm_delay)
  ),
  
  Delay_MAE = c(
    mae(test$ScheduleDelay, pred_delay),
    mae(test$ScheduleDelay, pred_rf_delay),
    mae(test$ScheduleDelay, pred_gbm_delay)
  )
)

print(results)

# Feature Importance
# Random Forest Importance
imp_rf_cost <- importance(rf_cost)
imp_rf_delay <- importance(rf_delay)

imp_cost <- data.frame(
  Feature = rownames(imp_rf_cost),
  Importance = imp_rf_cost[,1]
)

imp_delay <- data.frame(
  Feature = rownames(imp_rf_delay),
  Importance = imp_rf_delay[,1]
)

# Feature Importance Graph
# Cost
ggplot(imp_cost, aes(x = reorder(Feature, Importance), y = Importance)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  coord_flip() +
  ggtitle("ML Feature Importance (Cost Overrun)") +
  theme_minimal()

# Delay
ggplot(imp_delay, aes(x = reorder(Feature, Importance), y = Importance)) +
  geom_bar(stat = "identity", fill = "darkred") +
  coord_flip() +
  ggtitle("ML Feature Importance (Schedule Delay)") +
  theme_minimal()

# Performance Comparison Graph
results_long <- melt(results, id.vars = "Model")

ggplot(results_long, aes(x = Model, y = value, fill = Model)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_wrap(~variable, scales = "free") +
  ggtitle("Model Comparison: Linear vs Machine Learning") +
  theme_minimal()

# ----------------------------------------------------------------------------------
# ----------------------------------------------------------------------------------
# Question 6:	Can we detect structural breaks or anomalies in project cost trajectories? 
# Load necessary libraries
library(forecast)
library(changepoint)
library(ggplot2)

# Check the number of data points in ts_spend
cat("Length of ts_spend:", length(ts_spend), "\n")

# If there's enough data for STL decomposition (at least 24 points)
if (length(ts_spend) >= 24) {
  
  # STL Decomposition to get remainder (residuals)
  stl_decomp <- stl(ts_spend, s.window = "periodic")
  remainder <- stl_decomp$time.series[, "remainder"]
  
  # Plot remainder to visualize anomalies
  autoplot(remainder) +
    ggtitle("STL Remainder: Project Spend Residuals (Anomalies)") +
    ylab("Residuals") +
    theme_minimal()
  
  # Highlight points beyond ±3 SD as anomalies
  threshold <- 3 * sd(remainder)
  anomaly_idx <- which(abs(remainder) > threshold)
  
  cat("Detected Anomalies at periods:\n")
  print(anomaly_idx)
  
} else {
  cat("Not enough data for STL decomposition (need at least 24 observations).\n")
  # Proceed with ARIMA or ETS if STL cannot be used
  # ARIMA model for project spend to detect anomalies
  arima_model <- auto.arima(ts_spend)
  forecast_arima <- forecast(arima_model, h = 12)
  
  # Residuals from ARIMA model for anomaly detection
  residuals_arima <- residuals(arima_model)
  
  # Plot residuals to check for anomalies
  autoplot(residuals_arima) + 
    ggtitle("Residuals from ARIMA Model: Project Spend") +
    ylab("Residuals") +
    theme_minimal()
  
  # Anomaly detection (points more than 3 SD from the mean)
  threshold <- 3 * sd(residuals_arima)
  anomaly_idx <- which(abs(residuals_arima) > threshold)
  
  cat("Detected Anomalies at periods:\n")
  print(anomaly_idx)
}

# Changepoint Detection (mean shifts)
# Detect changepoints in project spend using the PELT method
cpt_mean <- cpt.mean(ts_spend, method = "PELT", penalty = "MBIC")

# Plot changepoints
plot(cpt_mean, main = "Changepoint Detection in Project Spend", ylab = "Total Spend")

# Show detected changepoints
cat("Changepoints detected at periods:\n")
print(cpts(cpt_mean))

# Combine anomalies and changepoints for visualization
# Create a data frame for visualization
df_anomaly <- data.frame(
  Month = 1:length(ts_spend),
  Spend = as.numeric(ts_spend),
  Anomaly = ifelse(1:length(ts_spend) %in% anomaly_idx, "Yes", "No"),
  Changepoint = ifelse(1:length(ts_spend) %in% cpts(cpt_mean), "Yes", "No")
)

# Plot anomalies and changepoints on the same graph
ggplot(df_anomaly, aes(x = Month, y = Spend)) +
  geom_line(color = "blue") +
  geom_point(data = subset(df_anomaly, Anomaly == "Yes"), aes(x = Month, y = Spend), color = "red", size = 3) +
  geom_vline(xintercept = cpts(cpt_mean), linetype = "dashed", color = "darkgreen") +
  ggtitle("Project Spend with Anomalies (red) and Changepoints (green dashed)") +
  theme_minimal()

