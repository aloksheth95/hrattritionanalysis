# ---------------------------------------------------------------
# Exploratory Analysis of HR Attrition Data
# ---------------------------------------------------------------

# Load required libraries
library(tidyverse)
library(corrplot)

# Load cleaned data (run 01_data_preparation.R first)
hr_data_clean <- readRDS("data/hr_data_clean.rds")

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
ggsave("visualizations/dept_role_heatmap.png", plot = dept_role_plot, width = 12, height = 6, dpi = 300)

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
ggsave("visualizations/overtime_age_heatmap.png", plot = overtime_age_plot, width = 10, height = 4, dpi = 300)

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
ggsave("visualizations/attrition_dashboard.png", plot = attrition_dashboard, width = 14, height = 10, dpi = 300)

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
png("visualizations/correlation_matrix.png", width = 1000, height = 800)
corrplot(correlation_matrix, method = "circle", type = "upper", 
         tl.col = "black", tl.srt = 45, tl.cex = 0.7,
         title = "Correlation Matrix of HR Variables")
dev.off()

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

# ----------------------------------------------
# Save Results for Further Analysis
# ----------------------------------------------

# Save key analysis data frames
saveRDS(attrition_long, "data/attrition_analysis.rds")
write.csv(dept_role_attrition, "data/dept_role_attrition.csv", row.names = FALSE)
write.csv(overtime_age_attrition, "data/overtime_age_attrition.csv", row.names = FALSE)

# Print completion message
cat("\nExploratory analysis complete. Visualizations saved to 'visualizations' folder.\n")
