# Load libraries
library(readxl)
library(dplyr)
library(ggplot2)
library(lubridate)
library(caret)
library(randomForest)
library(corrplot)

# Load Excel dataset
data <- read_excel("insurance_claims.xlsx")

# Clean column names to make them valid R names
names(data) <- make.names(names(data))

# Convert target variable to binary: Y = 1, N = 0
data$fraud_reported <- ifelse(data$fraud_reported == "Y", 1, 0)

# Convert date columns to Date format
data$policy_bind_date <- as.Date(data$policy_bind_date)
data$incident_date <- as.Date(data$incident_date)

# Create new feature: time difference between policy start and incident (days)
data$time_gap <- as.numeric(data$incident_date - data$policy_bind_date)

# Convert all character columns to factors
data <- data %>%
  mutate(across(where(is.character), as.factor))

# Remove rows with missing values for simplicity
data <- na.omit(data)

# ----------------------------------------------------------------------------------
# ----------------------------------------------------------------------------------
# Question 1: What factors are most strongly associated with insurance fraud claims?

# Logistic Regression to identify key drivers of fraud
# Exclude ID and location columns
logit_model <- glm(fraud_reported ~ . -policy_number -incident_location, data = data, family = binomial)

# View summary to check significance of predictors
summary(logit_model)

# Convert coefficients to odds ratios
odds_ratios <- exp(coef(logit_model))
odds_ratios

# Compute confidence intervals for odds ratios
conf_intervals <- exp(confint(logit_model))
conf_intervals

# Random Forest model for feature importance
set.seed(123)
rf_model <- randomForest(as.factor(fraud_reported) ~ . -policy_number -incident_location,
                         data = data,
                         ntree = 300,
                         importance = TRUE)

# Show feature importance scores
importance(rf_model)

# Plot variable importance
varImpPlot(rf_model, main = "Random Forest Feature Importance")

# Correlation analysis for numeric variables
numeric_data <- data %>% select(where(is.numeric))
cor_matrix <- cor(numeric_data)
cor_matrix[, "fraud_reported"]  # Correlation of all numeric vars with fraud

# Visualize correlation matrix
corrplot(cor_matrix, method = "color", tl.cex = 0.7)

# Chi-square tests for selected categorical variables
chi_test <- function(var) {
  tbl <- table(data[[var]], data$fraud_reported)
  print(var)
  print(chisq.test(tbl))
}

# List of categorical variables to test
cat_vars <- c("incident_type",
              "collision_type",
              "incident_severity",
              "property_damage",
              "police_report_available",
              "insured_sex",
              "insured_education_level",
              "insured_occupation",
              "insured_relationship")

# Initialize empty dataframe to store results
chi_results <- data.frame(
  Variable = character(),
  Chi_Squared = numeric(),
  df = numeric(),
  p_value = numeric(),
  stringsAsFactors = FALSE
)

# Loop through variables and compute chi-square
for (var in cat_vars) {
  tbl <- table(data[[var]], data$fraud_reported)
  test <- chisq.test(tbl)
  chi_results <- rbind(chi_results, data.frame(
    Variable = var,
    Chi_Squared = round(test$statistic, 3),
    df = test$parameter,
    p_value = signif(test$p.value, 4)
  ))
}

# View combined results
chi_results

# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------
# Question 2: Can we accurately predict fraudulent insurance claims using machine learning models?

# Ensure target variable is a factor for classification
data$fraud_reported <- factor(data$fraud_reported, levels = c(0,1))

# Remove ID-like and location columns (if not already removed)
model_data <- data %>% select(-policy_number, -incident_location)

# Split dataset into training (70%) and testing (30%) sets
set.seed(42)
train_index <- createDataPartition(model_data$fraud_reported, p = 0.7, list = FALSE)
train_data <- model_data[train_index, ]
test_data  <- model_data[-train_index, ]

# Baseline Logistic Regression (Predictive)
logit_model_pred <- glm(fraud_reported ~ ., data = train_data, family = binomial)

# Predict probabilities on the test set
logit_probs <- predict(logit_model_pred, newdata = test_data, type = "response")

# Convert probabilities to class labels (threshold = 0.5)
logit_preds <- factor(ifelse(logit_probs > 0.5, 1, 0), levels = c(0,1))

# Compute confusion matrix and performance metrics
conf_logit <- confusionMatrix(logit_preds, test_data$fraud_reported, positive = "1")
conf_logit

# Extract key metrics
logit_accuracy  <- conf_logit$overall['Accuracy']
logit_precision <- conf_logit$byClass['Precision']
logit_recall    <- conf_logit$byClass['Recall']
logit_f1        <- conf_logit$byClass['F1']
logit_metrics <- data.frame(Model="Logistic Regression",
                            Accuracy=logit_accuracy,
                            Precision=logit_precision,
                            Recall=logit_recall,
                            F1=logit_f1)
logit_metrics

# Random Forest Model (Main Predictive Model)
set.seed(42)
rf_model_pred <- randomForest(fraud_reported ~ ., data = train_data, ntree = 300, importance = TRUE)

# Predict on test set
rf_preds <- predict(rf_model_pred, newdata = test_data)

# Confusion matrix and metrics
conf_rf <- confusionMatrix(rf_preds, test_data$fraud_reported, positive = "1")
conf_rf

# Extract key metrics
rf_accuracy  <- conf_rf$overall['Accuracy']
rf_precision <- conf_rf$byClass['Precision']
rf_recall    <- conf_rf$byClass['Recall']
rf_f1        <- conf_rf$byClass['F1']
rf_metrics <- data.frame(Model="Random Forest",
                         Accuracy=rf_accuracy,
                         Precision=rf_precision,
                         Recall=rf_recall,
                         F1=rf_f1)
rf_metrics

# Combine metrics for comparison
all_metrics <- rbind(logit_metrics, rf_metrics)
all_metrics

# Variable Importance from Random Forest
rf_var_imp <- importance(rf_model_pred)
varImpPlot(rf_model_pred, main = "Random Forest Feature Importance (Q2)")

# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------
# Question 3: How do fraud patterns evolve over time, and what temporal trends can be identified in insurance claims? (Time-Series / Data Wrangling Focus)

# Create time features
data$incident_year  <- year(data$incident_date)                      # Year of incident
data$incident_month <- month(data$incident_date)                     # Month of incident
data$month_year     <- floor_date(data$incident_date, "month")       # Month-Year for aggregation
data$policy_to_claim_days <- as.numeric(data$incident_date - data$policy_bind_date)

# Ensure fraud_reported is numeric 0/1 for aggregation
data$fraud_numeric <- as.numeric(as.character(data$fraud_reported))

# Aggregate monthly fraud rate
monthly_fraud <- data %>%
  group_by(month_year) %>%
  summarise(
    total_claims = n(),
    fraud_claims = sum(fraud_numeric, na.rm = TRUE),
    fraud_rate = fraud_claims / total_claims
  ) %>%
  ungroup()

# Monthly fraud rate plot
ggplot(monthly_fraud, aes(x = month_year, y = fraud_rate*100)) +
  geom_line(color = "firebrick", size = 1) +
  geom_point(color = "firebrick", size = 2) +
  scale_x_date(date_labels = "%b %Y", date_breaks = "2 months") +
  labs(title = "Monthly Insurance Fraud Rate Over Time",
       x = "Month-Year",
       y = "Fraud Rate (%)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Average policy-to-claim time by year
time_gap_year <- data %>%
  group_by(incident_year) %>%
  summarise(avg_policy_to_claim_days = mean(policy_to_claim_days, na.rm = TRUE))

# Plot average time gap trend
ggplot(time_gap_year, aes(x = incident_year, y = avg_policy_to_claim_days)) +
  geom_line(color = "darkgreen", size = 1) +
  geom_point(color = "darkgreen", size = 2) +
  labs(title = "Average Time Between Policy Start and Claim by Year",
       x = "Year",
       y = "Avg Days") +
  theme_minimal()
