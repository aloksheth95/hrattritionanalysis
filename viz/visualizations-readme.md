# Visualizations Directory

This directory contains all visualizations generated during the HR Attrition Analysis.

## Key Visualizations

1. **dept_role_heatmap.png** - Heatmap showing attrition rates by department and job role
2. **overtime_age_heatmap.png** - Heatmap showing attrition rates by overtime status and age group
3. **attrition_dashboard.png** - Comprehensive dashboard of attrition rates across multiple factors
4. **correlation_matrix.png** - Correlation plot of numerical variables
5. **feature_importance.png** - Bar chart showing the importance of variables in predicting attrition
6. **risk_distribution.png** - Histogram showing the distribution of attrition risk scores

## How These Visualizations Help HR Decision Making

- **Heatmaps** provide an intuitive view of "hotspots" where attrition is concentrated
- **Correlation matrix** helps identify relationships between employee metrics
- **Feature importance** chart reveals which factors most strongly predict attrition
- **Risk distribution** visualization shows how many employees fall into each risk category

## How to Re-Generate Visualizations

Run the R scripts in the `scripts/` directory in sequence:
1. `01_data_preparation.R`
2. `02_exploratory_analysis.R`
3. `03_attrition_modeling.R`
4. `04_risk_scoring.R`

Or generate the complete report by knitting the R Markdown document:
`Rscript -e "rmarkdown::render('analysis/HR_Attrition_Analysis.Rmd')"`
