# Load libraries
library(readxl)
library(dplyr)
library(ggplot2)
library(lubridate)
library(caret)
library(randomForest)
library(corrplot)
# Load additional specialized libraries
library(vcd)      # For Mosaic Plots
library(reshape2) # For data reshaping
library(tidyr)    # For cleaning

# Load Excel dataset
data <- read_excel("C:\\Users\\gurmo\\Downloads\\insurance claims.xlsx")

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

# --- NEW ANALYSIS 1: Claim Composition (The "Money" Analysis) ---
# Goal: Do fraudulent claims have higher 'paddings' in specific claim types?

claim_melt <- data %>%
  select(fraud_reported, injury_claim, property_claim, vehicle_claim) %>%
  pivot_longer(cols = -fraud_reported, names_to = "Claim_Type", values_to = "Amount")

ggplot(claim_melt, aes(x = Claim_Type, y = Amount, fill = fraud_reported)) +
  geom_boxplot(outlier.colour = "red", alpha = 0.7) +
  scale_fill_manual(values = c("0" = "#999999", "1" = "#E69F00"), 
                    labels = c("Genuine", "Fraud")) +
  labs(title = "Financial Padding: Fraud vs. Genuine Claims",
       subtitle = "Comparing Injury, Property, and Vehicle claim amounts",
       y = "Dollar Amount ($)", x = "Category") +
  theme_minimal()

# --- NEW ANALYSIS 2: The Geography of Risk ---
# Goal: Visualize which states 'over-contribute' to fraud using Mosaic Residuals

# Create a contingency table
geog_table <- structable(policy_state ~ fraud_reported, data = data)

# Plotting the Mosaic
mosaic(geog_table, shade = TRUE, legend = TRUE, 
       main = "Fraud Density by Policy State",
       labeling = labeling_border(rot_labels = c(0, 0, 0, 0)))

# INTERPRETATION: Blue boxes = significantly MORE fraud than expected. 
# Red boxes = significantly LESS fraud than expected.

names(train_data)

# --- NEW ANALYSIS 3: The Risk Scorecard & Watchlist ---
# Goal: Use the Random Forest to rank current claims by risk level.

# 1. Ensure the cleaned 'data' is what we use
# (Run your wrangling code first, then run this)

# 2. Convert target to FACTOR (Mandatory for Classification)
data$fraud_reported <- as.factor(data$fraud_reported)

# 3. Create a clean modeling dataframe (removing ID/Text columns)
# This ensures 'train_data' won't accidentally pull from the Boston dataset
model_df <- data %>% 
  select(-policy_number, -incident_location, -policy_bind_date, -incident_date)

# 4. Re-split the data
set.seed(42)
train_index <- createDataPartition(model_df$fraud_reported, p = 0.7, list = FALSE)
train_data  <- model_df[train_index, ]
test_data   <- model_df[-train_index, ]

# 5. Verify names (You should now see fraud_reported, months_as_customer, etc.)
print(names(train_data))

# 6. Train the Random Forest
set.seed(42)
rf_model_pred <- randomForest(fraud_reported ~ ., 
                              data = train_data, 
                              ntree = 300, 
                              importance = TRUE)

# 7. Generate the Risk Scorecard Probabilities
rf_probs <- predict(rf_model_pred, newdata = test_data, type = "prob")[, "1"]

# 2. Add probabilities back to the test set for a 'Watchlist'
test_watchlist <- test_data %>%
  mutate(Fraud_Probability = rf_probs) %>%
  mutate(Risk_Tier = case_when(
    Fraud_Probability > 0.8 ~ "CRITICAL (High Alert)",
    Fraud_Probability > 0.5 ~ "ELEVATED (Review)",
    TRUE ~ "LOW (Standard)"
  ))

# 3. View the Top 10 'Red Flag' cases for your presentation
top_red_flags <- test_watchlist %>%
  arrange(desc(Fraud_Probability)) %>%
  select(age, insured_occupation, total_claim_amount, Fraud_Probability, Risk_Tier) %>%
  head(10)

print(top_red_flags)

# --- NEW ANALYSIS 4: Hobby & Lifestyle Profiling ---

ggplot(data, aes(x = insured_hobbies, fill = incident_severity)) +
  geom_bar(position = "fill") +
  coord_flip() +
  labs(title = "Lifestyle vs. Incident Severity",
       y = "Proportion of Claims",
       x = "Policyholder Hobby") +
  theme_light() +
  scale_fill_brewer(palette = "RdYlGn", direction = -1)

# Look for hobbies like 'Chess' vs 'Crossfit' to see if severity changes.