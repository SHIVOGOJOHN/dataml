# Quick Installation Guide for Shiny Applications Suite

## Option 1: Install from R Console (Recommended)

Open R or RStudio and run:

```r
# Set working directory
setwd("c:/Users/adm/.vscode/R")

# Run the installation script
source("install_packages.R")
```

## Option 2: Install Manually

Copy and paste this into your R console:

```r
# Install all required packages
install.packages(c(
  # Core Shiny
  "shiny", "shinydashboard", "bslib", "shinyWidgets",
  
  # Data manipulation
  "dplyr", "tidyr", "lubridate",
  
  # Data import
  "readr", "readxl", "haven",
  
  # Visualization
  "ggplot2", "plotly", "DT", "dygraphs", "corrplot",
  
  # Statistical analysis
  "pwr", "broom", "skimr", "qcc",
  
  # Machine learning
  "randomForest", "caret",
  
  # Financial analysis
  "quantmod", "PerformanceAnalytics", "forecast", "xts",
  
  # Geospatial
  "leaflet", "sf", "viridis",
  
  # Text analysis
  "tidytext", "wordcloud2",
  
  # Utilities
  "scales"
), dependencies = TRUE)
```

## Option 3: Install by Category

If you want to install packages for specific apps only:

### For Business Analytics & Data Explorer:
```r
install.packages(c("shiny", "shinydashboard", "bslib", "plotly", "DT", 
                   "dplyr", "tidyr", "readr", "readxl", "ggplot2", "skimr", "corrplot"))
```

### For Statistical Lab:
```r
install.packages(c("shiny", "bslib", "plotly", "pwr", "broom", "ggplot2"))
```

### For Model Deployment:
```r
install.packages(c("shiny", "bslib", "plotly", "randomForest", "caret", "ggplot2"))
```

### For Financial Analysis:
```r
install.packages(c("shiny", "bslib", "plotly", "quantmod", "PerformanceAnalytics", 
                   "forecast", "dygraphs", "xts"))
```

### For Geospatial Explorer:
```r
install.packages(c("shiny", "bslib", "plotly", "leaflet", "sf", "viridis", "dplyr"))
```

### For Quality Control:
```r
install.packages(c("shiny", "bslib", "plotly", "qcc", "ggplot2", "dplyr", "lubridate"))
```

### For Survey Analyzer:
```r
install.packages(c("shiny", "bslib", "plotly", "DT", "readr", "readxl", "haven", 
                   "dplyr", "tidyr", "tidytext", "wordcloud2"))
```

## Verify Installation

After installation, verify by running:

```r
# Load critical packages
library(shiny)
library(bslib)
library(plotly)
library(dplyr)

# If no errors, you're ready!
cat("✓ All critical packages loaded successfully!\n")
```

## Launch Applications

Once packages are installed:

```r
# Launch the launcher
shiny::runApp("c:/Users/adm/.vscode/R/launcher.R")

# Or launch individual apps
shiny::runApp("c:/Users/adm/.vscode/R/apps/01_business_analytics")
shiny::runApp("c:/Users/adm/.vscode/R/apps/02_data_explorer")
shiny::runApp("c:/Users/adm/.vscode/R/apps/03_statistical_lab")
# ... etc
```

## Troubleshooting

### If a package fails to install:

1. **Check R version**: Some packages require R >= 4.0
   ```r
   R.version.string
   ```

2. **Install from source** (if binary not available):
   ```r
   install.packages("package_name", type = "source")
   ```

3. **Install dependencies first**:
   ```r
   install.packages("package_name", dependencies = TRUE)
   ```

4. **Update existing packages**:
   ```r
   update.packages(ask = FALSE)
   ```

### Common Issues:

- **sf package**: May require additional system libraries on Linux/Mac
- **quantmod**: Requires internet connection for financial data
- **wordcloud2**: May need htmlwidgets package first

## Minimal Installation (Core Apps Only)

If you want to start with just the essential packages:

```r
install.packages(c("shiny", "bslib", "plotly", "DT", "dplyr", "ggplot2"))
```

This will allow you to run most apps with reduced functionality.
