# ===============================================================
# HR Employee Attrition Analysis - Complete R Script
# ===============================================================

# Load all required libraries
library(tidyverse)
library(corrplot)
library(caret)
library(randomForest)

# ---------------------------------------------------------------
# 1. Data Preparation
# ---------------------------------------------------------------

# Import data
hr_data <- read.csv("WA_Fn-UseC_-HR-Employee-Attrition.csv")

# Explore data structure
dim(hr_data)  # Check dimensions
glimpse(hr_data)  # View structure
head(hr_data, 5)  # View first 5 rows
sum(is.na(hr_data))  # Check for missing values

# Data cleaning and preparation
hr_data_clean <- hr_data %>%
  # Convert categorical variables to factors
  mutate(
    Attrition = factor(Attrition),
    BusinessTravel = factor(BusinessTravel),
    Department = factor(Department),
    EducationField = factor(EducationField),
    Gender = factor(Gender),
    JobRole = factor(JobRole),
    MaritalStatus = factor(MaritalStatus),
    OverTime = factor(OverTime)
  ) %>%
  # Remove unnecessary columns
  select(-EmployeeCount, -EmployeeNumber, -Over18, -StandardHours) %>%
  # Create age and tenure groups for visualization
  mutate(
    AgeGroup = case_when(
      Age < 30 ~ "Under 30",
      Age < 40 ~ "30-39",
      Age < 50 ~ "40-49",
      TRUE ~ "50+"
    ),
    TenureGroup = case_when(
      YearsAtCompany < 2 ~ "0-1 years",
      YearsAtCompany < 5 ~ "2-4 years",
      YearsAtCompany < 10 ~ "5-9 years",
      TRUE ~ "10+ years"
    ),
    SalaryGroup = case_when(
      MonthlyIncome < 3000 ~ "Low",
      MonthlyIncome < 6000 ~ "Medium",
      MonthlyIncome < 10000 ~ "High",
      TRUE ~ "Very High"
    )
  )

# ---------------------------------------------------------------
# 2. Exploratory Data Analysis
# ---------------------------------------------------------------

# Calculate overall attrition rate
attrition_count <- sum(hr_data_clean$Attrition == "Yes")
total_count <- nrow(hr_data_clean)
attrition_rate <- (attrition_count / total_count) * 100
print(paste("Overall Attrition Rate:", round(attrition_rate, 2), "%"))

# ----------------------------------------------
# Create Attrition Heatmaps
# ----------------------------------------------

# Department vs Job Role heatmap
dept_role_attrition <- hr_data_clean %>%
  group_by(Department, JobRole) %>%
  summarise(
    Count = n(),
    Attrition = sum(Attrition == "Yes"),
    AttritionRate = round(Attrition / Count * 100, 1),
    .groups = "drop"
  ) %>%
  filter(Count >= 3)  # Filter out very small groups

# Create and save the heatmap
dept_role_plot <- ggplot(dept_role_attrition, aes(x = JobRole, y = Department, fill = AttritionRate)) +
  geom_tile(color = "white") +
  geom_text(aes(label = paste0(AttritionRate, "%")), color = "black", size = 3) +
  scale_fill_gradient(low = "white", high = "red") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Attrition Rate Heatmap by Department and Job Role",
       x = "Job Role", y = "Department", fill = "Attrition %")

print(dept_role_plot)
# Uncomment to save the plot
# ggsave("dept_role_heatmap.png", plot = dept_role_plot, width = 12, height = 6, dpi = 300)

# Overtime vs Age Group heatmap
overtime_age_attrition <- hr_data_clean %>%
  group_by(OverTime, AgeGroup) %>%
  summarise(
    Count = n(),
    Attrition = sum(Attrition == "Yes"),
    AttritionRate = round(Attrition / Count * 100, 1),
    .groups = "drop"
  )

overtime_age_plot <- ggplot(overtime_age_attrition, aes(x = AgeGroup, y = OverTime, fill = AttritionRate)) +
  geom_tile(color = "white") +
  geom_text(aes(label = paste0(AttritionRate, "%")), color = "black", size = 3) +
  scale_fill_gradient(low = "white", high = "red") +
  theme_minimal() +
  labs(title = "Attrition Rate Heatmap by Overtime Status and Age Group",
       x = "Age Group", y = "Overtime", fill = "Attrition %")

print(overtime_age_plot)
# Uncomment to save the plot
# ggsave("overtime_age_heatmap.png", plot = overtime_age_plot, width = 10, height = 4, dpi = 300)

# ----------------------------------------------
# Multi-factor Attrition Dashboard
# ----------------------------------------------

factors <- c("Department", "JobRole", "AgeGroup", "TenureGroup", "SalaryGroup", "OverTime", "MaritalStatus")

attrition_long <- map_dfr(factors, function(factor) {
  hr_data_clean %>%
    group_by(!!sym(factor)) %>%
    summarise(
      Count = n(),
      Attrition = sum(Attrition == "Yes"),
      AttritionRate = round(Attrition / Count * 100, 1),
      .groups = "drop"
    ) %>%
    mutate(Factor = factor, 
           Category = as.character(!!sym(factor)))
})

# Plot multi-factor attrition dashboard
attrition_dashboard <- ggplot(attrition_long, aes(x = reorder(Category, AttritionRate), y = AttritionRate, fill = AttritionRate)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = paste0(AttritionRate, "%\n(", Count, ")")), 
            hjust = -0.1, size = 3) +
  scale_fill_gradient(low = "green", high = "red") +
  facet_wrap(~ Factor, scales = "free_x") +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    strip.background = element_rect(fill = "gray90")
  ) +
  labs(title = "Attrition Hotspots Across Multiple Factors",
       x = NULL, y = "Attrition Rate (%)", fill = "Attrition %") +
  coord_flip()

print(attrition_dashboard)
# Uncomment to save the plot
# ggsave("attrition_dashboard.png", plot = attrition_dashboard, width = 14, height = 10, dpi = 300)

# ----------------------------------------------
# Correlation Analysis
# ----------------------------------------------

correlation_vars <- hr_data_clean %>%
  select(Age, DailyRate, DistanceFromHome, Education, EnvironmentSatisfaction,
         HourlyRate, JobInvolvement, JobLevel, JobSatisfaction, MonthlyIncome,
         MonthlyRate, NumCompaniesWorked, PercentSalaryHike, RelationshipSatisfaction,
         TotalWorkingYears, TrainingTimesLastYear, WorkLifeBalance, YearsAtCompany,
         YearsInCurrentRole, YearsSinceLastPromotion, YearsWithCurrManager)

correlation_matrix <- cor(correlation_vars)
corrplot(correlation_matrix, method = "circle", type = "upper", 
         tl.col = "black", tl.srt = 45, tl.cex = 0.7)

# Print key correlations
key_correlations <- correlation_matrix["MonthlyIncome", ]
top_corr <- sort(abs(key_correlations), decreasing = TRUE)[1:5]
cat("\nTop correlations with Monthly Income:\n")
print(key_correlations[names(top_corr)])

# ----------------------------------------------
# Analyze Key Demographics and Job Factors
# ----------------------------------------------

# Analyze attrition by age
age_attrition <- hr_data_clean %>%
  group_by(AgeGroup) %>%
  summarise(
    Count = n(),
    AttritionCount = sum(Attrition == "Yes"),
    AttritionRate = round(AttritionCount / Count * 100, 1)
  )

cat("\nAttrition by Age Group:\n")
print(age_attrition)

# Analyze attrition by gender
gender_attrition <- hr_data_clean %>%
  group_by(Gender) %>%
  summarise(
    Count = n(),
    AttritionCount = sum(Attrition == "Yes"),
    AttritionRate = round(AttritionCount / Count * 100, 1)
  )

cat("\nAttrition by Gender:\n")
print(gender_attrition)

# Analyze job satisfaction and attrition
satisfaction_attrition <- hr_data_clean %>%
  group_by(JobSatisfaction) %>%
  summarise(
    Count = n(),
    AttritionCount = sum(Attrition == "Yes"),
    AttritionRate = round(AttritionCount / Count * 100, 1)
  )

cat("\nAttrition by Job Satisfaction Level:\n")
print(satisfaction_attrition)

# Analyze work-life balance and attrition
wlb_attrition <- hr_data_clean %>%
  group_by(WorkLifeBalance) %>%
  summarise(
    Count = n(),
    AttritionCount = sum(Attrition == "Yes"),
    AttritionRate = round(AttritionCount / Count * 100, 1)
  )

cat("\nAttrition by Work-Life Balance Level:\n")
print(wlb_attrition)

# ---------------------------------------------------------------
# 3. Predictive Modeling
# ---------------------------------------------------------------

# Prepare data for modeling
set.seed(123) # For reproducibility

# Split data into training and testing sets
training_index <- createDataPartition(hr_data_clean$Attrition, p = 0.7, list = FALSE)
train_data <- hr_data_clean[training_index, ]
test_data <- hr_data_clean[-training_index, ]

# Check class balance
train_balance <- table(train_data$Attrition)
train_balance_pct <- prop.table(train_balance) * 100
cat("\nTraining data class balance:\n")
print(train_balance)
cat("Percentage:", round(train_balance_pct, 1), "%\n")

# ----------------------------------------------
# Logistic Regression Model
# ----------------------------------------------

# Build a logistic regression model
log_model <- glm(Attrition ~ ., data = train_data, family = "binomial")

# Make predictions on test data
log_predictions <- predict(log_model, newdata = test_data, type = "response")
log_pred_class <- ifelse(log_predictions > 0.5, "Yes", "No")

# Evaluate logistic regression model
log_conf_matrix <- confusionMatrix(factor(log_pred_class, levels = levels(test_data$Attrition)), 
                                  test_data$Attrition)

cat("\nLogistic Regression Model Performance:\n")
print(log_conf_matrix)

# ----------------------------------------------
# Random Forest Model
# ----------------------------------------------

# Build a random forest model
rf_model <- randomForest(Attrition ~ ., data = train_data, ntree = 100, importance = TRUE)

# Make predictions with random forest
rf_predictions <- predict(rf_model, newdata = test_data)

# Evaluate random forest performance
rf_conf_matrix <- confusionMatrix(rf_predictions, test_data$Attrition)

cat("\nRandom Forest Model Performance:\n")
print(rf_conf_matrix)

# ----------------------------------------------
# Feature Importance Analysis
# ----------------------------------------------

# Extract and analyze feature importance
importance_df <- as.data.frame(importance(rf_model))
importance_df$Variable <- rownames(importance_df)

# Sort by importance
importance_df <- importance_df %>%
  arrange(desc(MeanDecreaseGini))

# Plot variable importance
importance_plot <- ggplot(importance_df[1:15,], aes(x = reorder(Variable, MeanDecreaseGini), y = MeanDecreaseGini)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  coord_flip() +
  theme_minimal() +
  labs(title = "Top 15 Variables by Importance for Attrition Prediction",
       x = "Features",
       y = "Importance (Mean Decrease in Gini)")

print(importance_plot)
# Uncomment to save the plot
# ggsave("feature_importance.png", plot = importance_plot, width = 10, height = 6, dpi = 300)

cat("\nTop 10 most important features:\n")
print(importance_df[1:10, c("Variable", "MeanDecreaseGini")])

# ---------------------------------------------------------------
# 4. Risk Scoring for Current Employees
# ---------------------------------------------------------------

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

cat("\nAnalyzing attrition risk for", nrow(current_employees), "current employees\n")

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
# Uncomment to save the plot
# ggsave("risk_distribution.png", plot = risk_dist_plot, width = 10, height = 6, dpi = 300)

# ---------------------------------------------------------------
# 5. Summary Insights and Recommendations
# ---------------------------------------------------------------

cat("\n\n===============================================================\n")
cat("HR ATTRITION ANALYSIS: KEY FINDINGS AND RECOMMENDATIONS\n")
cat("===============================================================\n\n")

cat("OVERALL ATTRITION METRICS:\n")
cat("- Overall attrition rate:", round(attrition_rate, 1), "%\n")
cat("- Highest attrition by department:", risk_by_department$Department[1], "at", 
    risk_by_department$HighRiskPercentage[1], "% high risk\n")
cat("- Highest attrition by job role:", risk_by_role$JobRole[1], "at", 
    risk_by_role$HighRiskPercentage[1], "% high risk\n")
cat("- Overtime status increases attrition by", 
    round(overtime_age_attrition$AttritionRate[overtime_age_attrition$OverTime == "Yes"][1] - 
            overtime_age_attrition$AttritionRate[overtime_age_attrition$OverTime == "No"][1], 1), 
    "percentage points\n\n")

cat("TOP ATTRITION PREDICTORS:\n")
cat("1.", importance_df$Variable[1], "\n")
cat("2.", importance_df$Variable[2], "\n")
cat("3.", importance_df$Variable[3], "\n")
cat("4.", importance_df$Variable[4], "\n")
cat("5.", importance_df$Variable[5], "\n\n")

cat("MODEL PERFORMANCE:\n")
cat("- Random Forest accuracy:", round(rf_conf_matrix$overall["Accuracy"] * 100, 1), "%\n")
cat("- Logistic Regression accuracy:", round(log_conf_matrix$overall["Accuracy"] * 100, 1), "%\n\n")

cat("KEY RECOMMENDATIONS:\n")
cat("1. Address overtime policies, especially in", risk_by_department$Department[1], "department\n")
cat("2. Review compensation for", risk_by_role$JobRole[1], "roles\n")
cat("3. Implement targeted retention program for employees with <2 years tenure\n")
cat("4. Improve work-life balance measures for high-risk departments\n")
cat("5. Deploy regular attrition risk assessments using the predictive model\n\n")

cat("CURRENT RISK ASSESSMENT:\n")
cat("- Total high/very high risk employees:", sum(risk_distribution$Count[risk_distribution$RiskCategory %in% c("High", "Very High")]), "\n")
cat("- Percentage of workforce at high risk:", 
    sum(risk_distribution$Percentage[risk_distribution$RiskCategory %in% c("High", "Very High")]), "%\n")

cat("\nAnalysis complete.\n")

# End of script
