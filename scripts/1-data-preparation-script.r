# ---------------------------------------------------------------
# Data Preparation for HR Attrition Analysis
# ---------------------------------------------------------------

# Load required libraries
library(tidyverse)

# Import data
hr_data <- read.csv("data/WA_Fn-UseC_-HR-Employee-Attrition.csv")

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

# Save cleaned data for later use
saveRDS(hr_data_clean, "data/hr_data_clean.rds")

# Output summary of cleaned data
cat("Data cleaning complete. Dataset contains", nrow(hr_data_clean), "rows and", ncol(hr_data_clean), "columns.\n")
cat("Attrition rate:", round(mean(hr_data_clean$Attrition == "Yes") * 100, 1), "%\n")
