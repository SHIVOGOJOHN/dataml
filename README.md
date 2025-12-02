# Shiny Applications Suite

A comprehensive collection of 8 interactive R Shiny applications for data analysis, visualization, and decision-making. Each application is deployed and accessible via web browser.

![R](https://img.shields.io/badge/R-276DC3?style=for-the-badge&logo=r&logoColor=white)
![Shiny](https://img.shields.io/badge/Shiny-blue?style=for-the-badge&logo=RStudio&logoColor=white)
![License](https://img.shields.io/badge/license-MIT-green?style=for-the-badge)

## 🚀 Live Applications

All applications are deployed and ready to use:

| # | Application | Description | Live Demo |
|---|-------------|-------------|-----------|
| 1 | **Business Analytics Dashboard** | Track sales, KPIs, and customer churn | [Launch App](https://shivogojohn.shinyapps.io/business-analytics-dashboard/) |
| 2 | **Data Explorer** | Upload and explore datasets with visualizations | [Launch App](https://shivogojohn.shinyapps.io/data-explorer/) |
| 3 | **Statistical Lab** | Interactive statistical tools and simulations | [Launch App](https://shivogojohn.shinyapps.io/statistical-lab/) |
| 4 | **Model Deployment Hub** | Deploy and test ML models | [Launch App](https://datamlproject.shinyapps.io/model-deployment-hub/) |
| 5 | **Financial Analysis Suite** | Portfolio optimization and forecasting | [Launch App](https://shivogojohn.shinyapps.io/financial-analysis-suite/) |
| 6 | **Geospatial Explorer** | Interactive maps of Kenya | [Launch App](https://shivogojohn.shinyapps.io/geospatial-explorer-kenya/) |
| 7 | **Quality Control Monitor** | SPC charts and process monitoring | [Launch App](https://datamlproject.shinyapps.io/quality-control-monitor/) |
| 8 | **Survey Analyzer** | Analyze surveys with NPS metrics | [Launch App](https://datamlproject.shinyapps.io/survey-analyzer/) |

### 🎯 Launcher App
Access all applications from a single hub: [**Shiny Apps Launcher**](https://datamlproject.shinyapps.io/shiny-apps-launcher/)

## 📋 What Each App Does

### 1. Business Analytics Dashboard
**What it does:** Helps you understand your business performance
- See total sales, profits, and orders at a glance
- Track how sales change over time
- Identify which products sell best
- Analyze customer churn (who's leaving and why)
- Filter data by date, region, or category

**Best for:** Business owners, sales managers, analysts

---

### 2. Data Explorer
**What it does:** Lets you upload and explore your own data
- Upload CSV, Excel, or other data files
- See summary statistics automatically
- Create charts and graphs with a few clicks
- Filter and search through your data
- Download cleaned data

**Best for:** Anyone who works with data and wants quick insights

---

### 3. Statistical Lab
**What it does:** Interactive tools for statistics and testing
- **Confidence Intervals:** See how sample size affects accuracy
- **A/B Testing:** Calculate if your test results are significant
- **Distributions:** Explore normal, t, chi-square distributions
- **Regression:** Check if your model assumptions are met
- **Central Limit Theorem:** Visualize this important concept

**Best for:** Students, researchers, data scientists learning statistics

---

### 4. Model Deployment Hub
**What it does:** Test machine learning models with your own data
- **Random Forest Model:** Predict iris flower species
- **Linear Regression:** Predict car fuel efficiency
- See how accurate the models are
- Understand which features matter most
- Get instant predictions

**Best for:** Data scientists, ML practitioners, students

---

### 5. Financial Analysis Suite
**What it does:** Analyze stocks and optimize investment portfolios
- **Forecasting:** Predict future stock prices using ARIMA/ETS
- **Portfolio Optimization:** Find the best mix of stocks
- **Risk Analysis:** Calculate Value at Risk (VaR)
- Interactive charts for financial data
- Download analysis results

**Best for:** Investors, financial analysts, portfolio managers

---

### 6. Geospatial Explorer (Kenya)
**What it does:** Create interactive maps of Kenya
- Plot points on a map of Kenya
- Create heatmaps to show density
- Cluster nearby points automatically
- Color-code regions by values
- Filter and analyze spatial data

**Best for:** Researchers, NGOs, businesses analyzing geographic data in Kenya

---

### 7. Quality Control Monitor
**What it does:** Monitor manufacturing quality and processes
- **Control Charts:** Track if your process is stable
- **Capability Analysis:** See if you meet specifications (Cp, Cpk)
- **Real-time Monitoring:** Add new measurements and see results
- Identify out-of-control points
- Calculate defect rates (PPM)

**Best for:** Manufacturing engineers, quality managers, Six Sigma practitioners

---

### 8. Survey Analyzer
**What it does:** Analyze survey responses and calculate NPS
- Upload survey data (CSV, Excel, SPSS)
- Calculate Net Promoter Score (NPS) automatically
- See satisfaction ratings and distributions
- Cross-tabulate responses
- Generate downloadable reports

**Best for:** Market researchers, customer success teams, HR departments

## 🛠️ Technologies Used

- **R Shiny** - Interactive web framework
- **bslib** - Modern Bootstrap 5 theming
- **plotly** - Interactive visualizations
- **DT** - Interactive data tables
- **leaflet** - Interactive maps
- **dplyr** - Data manipulation
- **ggplot2** - Static visualizations

## 📁 Project Structure

```
R/
├── apps/                          # Individual applications
│   ├── 01_business_analytics/
│   ├── 02_data_explorer/
│   ├── 03_statistical_lab/
│   ├── 04_model_deployment/
│   ├── 05_financial_analysis/
│   ├── 06_geospatial_explorer/
│   ├── 07_quality_control/
│   └── 08_survey_analyzer/
├── shared/                        # Shared utilities
│   ├── theme.R                    # UI theming
│   ├── ui_components.R            # Reusable components
│   └── data_utils.R               # Data generation
├── launcher.R                     # Main launcher app
├── README.md                      # This file
└── APPS_GUIDE.md                  # Detailed app guide
```

## 🚀 Running Locally

### Prerequisites
- R (version 4.0 or higher)
- RStudio (recommended)

### Installation

1. **Clone the repository**
```bash
git clone https://github.com/SHIVOGOJOHN/dataml.git
cd dataml
```

2. **Install required packages**
```r
source("install_packages.R")
```

Or install manually:
```r
install.packages(c(
  "shiny", "shinydashboard", "bslib", "plotly", "DT", 
  "dplyr", "ggplot2", "readr", "readxl", "leaflet",
  "quantmod", "forecast", "randomForest", "caret",
  "qcc", "haven", "shinyjs"
))
```

3. **Run the launcher**
```r
shiny::runApp("launcher.R")
```

Or run individual apps:
```r
shiny::runApp("apps/01_business_analytics")
```

## 📊 Features

### Modern UI Design
- Clean, professional interface using Bootstrap 5
- Responsive design works on desktop and mobile
- Consistent theming across all applications
- Interactive charts and visualizations

### User-Friendly
- Intuitive navigation
- Clear instructions and tooltips
- Sample data included for testing
- Download capabilities for results

### Production-Ready
- Deployed on shinyapps.io
- Optimized performance
- Error handling
- Input validation

## 📖 Documentation

- **[APPS_GUIDE.md](APPS_GUIDE.md)** - Detailed guide for each application
- **[README.md](README.md)** - This file
- **[INSTALL.md](INSTALL.md)** - Installation instructions

## 🤝 Contributing

Contributions are welcome! Please feel free to submit a Pull Request.

## 📄 License

This project is licensed under the MIT License.

## 👤 Author

**John Shivogo**
- GitHub: [@SHIVOGOJOHN](https://github.com/SHIVOGOJOHN)
- Portfolio: [Add your portfolio link]

## 🌟 Acknowledgments

- Built with R Shiny
- Deployed on shinyapps.io
- Uses modern web design principles
- Inspired by real-world business needs

## 📞 Support

For questions or issues, please open an issue on GitHub or contact me directly.

---

**⭐ If you find this project useful, please consider giving it a star!**
