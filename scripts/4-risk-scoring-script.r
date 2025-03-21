# ---------------------------------------------------------------
# Risk Scoring for Current Employees
# ---------------------------------------------------------------

# Load required libraries
library(tidyverse)
library(randomForest)

# Load cleaned data and models
hr_data_clean <- readRDS("data/hr_data_clean.rds")
log_model <- readRDS("data/logistic_model.rds")
rf_model <- readRDS("data/random_forest_model.rds")

# Create a function to calculate attrition risk scores
calculate_attrition_risk <- function(employee_data, model) {
  # Get prediction probabilities
  if (class(model)[1] == "randomForest") {
    # For random forest models, need to convert to probability
    risk_scores <- predict(model, newdata = employee_data, type = "prob")[,"Yes"]
  } else {
    # For logistic regression
    risk_scores <- predict(model, newdata = employee_data, type = "response")
  }
  
  # Add scores to employee data
  employee_data$AttritionRiskScore <- round(risk_scores * 100, 1)
  employee_data$RiskCategory <- case_when(
    employee_data$AttritionRiskScore < 15 ~ "Low",
    employee_data$AttritionRiskScore < 30 ~ "Medium",
    employee_data$AttritionRiskScore < 50 ~ "High",
    TRUE ~ "Very High"
  )
  
  return(employee_data)
}

# Filter for current employees (those who haven't left yet)
current_employees <- hr_data_clean %>%
  filter(Attrition == "No")

cat("Analyzing attrition risk for", nrow(current_employees), "current employees\n")

# Calculate risk scores using logistic regression
current_with_risk <- calculate_attrition_risk(current_employees, log_model)

# View employees at highest risk
high_risk_employees <- current_with_risk %>%
  arrange(desc(AttritionRiskScore)) %>%
  select(Department, JobRole, Age, JobLevel, MonthlyIncome, OverTime, 
         JobSatisfaction, WorkLifeBalance, AttritionRiskScore, RiskCategory) %>%
  head(20)

cat("\nTop 20 employees at highest risk of attrition:\n")
print(high_risk_employees)

# Summarize risk distribution
risk_distribution <- current_with_risk %>%
  group_by(RiskCategory) %>%
  summarise(
    Count = n(),
    Percentage = round(n() / nrow(current_with_risk) * 100, 1)
  )

cat("\nRisk category distribution:\n")
print(risk_distribution)

# Analyze risk patterns
risk_by_department <- current_with_risk %>%
  group_by(Department) %>%
  summarise(
    EmployeeCount = n(),
    AvgRiskScore = round(mean(AttritionRiskScore), 1),
    HighRiskCount = sum(RiskCategory %in% c("High", "Very High")),
    HighRiskPercentage = round(sum(RiskCategory %in% c("High", "Very High")) / n() * 100, 1)
  ) %>%
  arrange(desc(HighRiskPercentage))

cat("\nAttrition risk by department:\n")
print(risk_by_department)

risk_by_role <- current_with_risk %>%
  group_by(JobRole) %>%
  summarise(
    EmployeeCount = n(),
    AvgRiskScore = round(mean(AttritionRiskScore), 1),
    HighRiskCount = sum(RiskCategory %in% c("High", "Very High")),
    HighRiskPercentage = round(sum(RiskCategory %in% c("High", "Very High")) / n() * 100, 1)
  ) %>%
  arrange(desc(HighRiskPercentage))

cat("\nAttrition risk by job role:\n")
print(risk_by_role)

# Create visualization of risk distribution
risk_dist_plot <- ggplot(current_with_risk, aes(x = AttritionRiskScore, fill = RiskCategory)) +
  geom_histogram(bins = 30) +
  scale_fill_manual(values = c("Low" = "green", "Medium" = "yellow", "High" = "orange", "Very High" = "red")) +
  theme_minimal() +
  labs(title = "Distribution of Attrition Risk Scores",
       x = "Attrition Risk Score (%)",
       y = "Number of Employees")

print(risk_dist_plot)
ggsave("visualizations/risk_distribution.png", plot = risk_dist_plot, width = 10, height = 6, dpi = 300)

# Save results for reporting
write.csv(high_risk_employees, "data/high_risk_employees.csv", row.names = FALSE)
saveRDS(current_with_risk, "data/employees_with_risk.rds")
