# ---------------------------------------------------------------
# Requirements for HR Attrition Analysis Project
# ---------------------------------------------------------------

# List of required packages
required_packages <- c(
  "tidyverse",  # For data manipulation and visualization
  "corrplot",   # For correlation visualization
  "caret",      # For machine learning workflows
  "randomForest", # For random forest modeling
  "knitr",      # For R Markdown documents
  "kableExtra", # For better tables in R Markdown
  "rmarkdown"   # For generating reports
)

# Function to install missing packages
install_if_missing <- function(packages) {
  new_packages <- packages[!(packages %in% installed.packages()[,"Package"])]
  if(length(new_packages)) {
    cat("Installing missing packages:", paste(new_packages, collapse = ", "), "\n")
    install.packages(new_packages)
  } else {
    cat("All required packages are already installed.\n")
  }
}

# Check and install missing packages
install_if_missing(required_packages)

# Create required directories if they don't exist
dirs <- c("data", "scripts", "analysis", "visualizations")
for(dir in dirs) {
  if(!dir.exists(dir)) {
    cat("Creating directory:", dir, "\n")
    dir.create(dir)
  }
}

# Display environment information
cat("\nR version information:\n")
print(R.version.string)

cat("\nRequired packages:\n")
for(pkg in required_packages) {
  if(pkg %in% installed.packages()[,"Package"]) {
    pkg_version <- packageVersion(pkg)
    cat(sprintf("- %s (version %s): Installed\n", pkg, pkg_version))
  } else {
    cat(sprintf("- %s: Not installed\n", pkg))
  }
}

cat("\nSetup complete. You're ready to begin the HR Attrition Analysis!\n")
