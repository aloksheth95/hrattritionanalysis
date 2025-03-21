# HR Employee Attrition Analysis & Prediction

## Project Overview
This project analyzes factors driving employee attrition using the IBM HR Analytics dataset and builds a predictive model to identify employees at risk of leaving. The analysis focuses on understanding why employees leave, which departments and roles are most affected, and how to predict potential turnover before it happens.

## Business Problem
Employee turnover is costly to organizations in terms of lost productivity, recruiting costs, and training expenses. According to industry research, replacing an employee can cost between 50-200% of their annual salary. This analysis aims to:
- Identify key factors contributing to employee attrition
- Build a predictive model to identify at-risk employees
- Provide actionable insights for HR interventions

## Data
The analysis uses the IBM HR Analytics Employee Attrition dataset, which contains:
- 1,470 employee records
- 35 features including demographics, job characteristics, and satisfaction metrics
- Employee attributes such as age, salary, job role, satisfaction levels, etc.
- Target variable: Employee attrition (Yes/No)

Data source: [Kaggle - IBM HR Analytics](https://www.kaggle.com/datasets/pavansubhasht/ibm-hr-analytics-attrition-dataset)

## Key Findings
- Overall attrition rate: 16.1%
- Key drivers of attrition:
  - Overtime: Employees working overtime have 30.4% attrition vs 10.7% for those who don't
  - Job Role: Sales Representatives have the highest attrition (45.3%)
  - Tenure: Employees with less than 2 years at the company have 30.2% attrition rate
  - Work-Life Balance: Employees reporting poor work-life balance have 37.8% attrition rate
  - Monthly Income: Lower salary bands correlate with higher attrition
- Random Forest model achieved 83.7% accuracy in predicting attrition

## Tools & Techniques
- R for data manipulation, visualization, and modeling
- Exploratory data analysis with ggplot2
- Heatmap visualizations for attrition hotspots
- Predictive modeling: Logistic regression and Random Forest
- Feature importance analysis

## Repository Structure
- `scripts/`: R scripts for analysis and modeling
  - `01_data_preparation.R`: Data loading and cleaning
  - `02_exploratory_analysis.R`: Summary statistics and visualizations
  - `03_attrition_modeling.R`: Predictive modeling and evaluation
  - `04_risk_scoring.R`: Risk score calculation for current employees
- `analysis/`: R Markdown files with detailed analysis
- `visualizations/`: Output visualizations
- `data/`: Dataset information and access instructions

## Results & Recommendations

Based on the analysis, the following retention strategies are recommended:

1. **Targeted Interventions for High-Risk Groups**:
   - Review overtime policies, especially in the Sales department
   - Implement structured onboarding and mentoring for employees in their first 2 years
   - Address work-life balance issues in departments with high attrition rates

2. **Compensation Strategy**:
   - Review salary structures for Sales Representatives and Research Scientists
   - Consider implementing retention bonuses for high-risk, high-performing employees

3. **Career Development**:
   - Create clearer career progression paths for roles with high attrition
   - Increase training and development opportunities for early-career employees

4. **Proactive Monitoring**:
   - Deploy the predictive model to identify at-risk employees before they leave
   - Establish regular pulse surveys focusing on key attrition indicators (satisfaction, work-life balance)

## How to Use This Repository

1. Clone the repository
2. Run `requirements.R` to install necessary packages
3. Source the scripts in numerical order or run the complete analysis in `HR_Attrition_Analysis.Rmd`
4. Examine the visualizations in the `visualizations` folder

## Future Work
- Develop an R Shiny dashboard for interactive exploration of attrition factors
- Include additional external data sources such as industry benchmarks
- Implement time-series analysis for seasonal attrition patterns
- Develop an automated reporting system for monthly attrition risk monitoring

## Contact
For any questions or feedback, please reach out at aloksheth95@gmail.com
