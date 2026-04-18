
library(dplyr)
library(tidyverse)

# 1. Daily Aggregation
daily_data <- data %>%
  mutate(incident_day = as.Date(incident_date)) %>%
  group_by(incident_day) %>%
  summarise(
    claims_count = n(),
    fraud_count = sum(ifelse(fraud_reported == "1", 1, 0))
  )

# 2. Visualizing Daily Volatility 
# This shows the 'heartbeat' without needing years of data
ggplot(daily_data, aes(x = incident_day, y = claims_count)) +
  geom_line(color = "steelblue", size = 1) +
  geom_point(aes(color = as.factor(fraud_count > 0))) +
  labs(title = "Daily Incident Velocity",
       subtitle = "Identifying micro-spikes in claims reporting",
       x = "Calendar Date", y = "Number of Claims") +
  theme_minimal()
