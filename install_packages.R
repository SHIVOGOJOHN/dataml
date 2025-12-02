# Install Required Packages for Shiny Applications Suite
# Run this script to install all dependencies

cat("Installing Shiny Applications Suite Dependencies\n")
cat("================================================\n\n")

# List of all required packages
packages <- c(
    # Core Shiny packages
    "shiny",
    "shinydashboard",
    "bslib",
    "shinyWidgets",

    # Data manipulation
    "dplyr",
    "tidyr",
    "lubridate",

    # Data import
    "readr",
    "readxl",
    "haven",

    # Visualization
    "ggplot2",
    "plotly",
    "DT",
    "dygraphs",
    "corrplot",

    # Statistical analysis
    "pwr",
    "broom",
    "skimr",
    "qcc",

    # Machine learning
    "randomForest",
    "caret",

    # Financial analysis
    "quantmod",
    "PerformanceAnalytics",
    "forecast",
    "xts",

    # Geospatial
    "leaflet",
    "sf",
    "viridis",
    "rnaturalearth",

    # Text analysis
    "tidytext",
    "wordcloud2",

    # Utilities
    "scales"
)

# Function to install packages if not already installed
install_if_missing <- function(pkg) {
    if (!require(pkg, character.only = TRUE, quietly = TRUE)) {
        cat(sprintf("Installing %s...\n", pkg))
        install.packages(pkg, dependencies = TRUE, repos = "https://cloud.r-project.org/")
        return(TRUE)
    } else {
        cat(sprintf("✓ %s already installed\n", pkg))
        return(FALSE)
    }
}

# Install packages
cat("Checking and installing packages...\n\n")
newly_installed <- 0
already_installed <- 0

for (pkg in packages) {
    if (install_if_missing(pkg)) {
        newly_installed <- newly_installed + 1
    } else {
        already_installed <- already_installed + 1
    }
}

cat("\n================================================\n")
cat("Installation Summary:\n")
cat(sprintf("  Newly installed: %d packages\n", newly_installed))
cat(sprintf("  Already installed: %d packages\n", already_installed))
cat(sprintf("  Total packages: %d\n", length(packages)))
cat("================================================\n\n")

# Verify critical packages
cat("Verifying critical packages...\n")
critical_packages <- c("shiny", "bslib", "plotly", "DT", "dplyr")
all_ok <- TRUE

for (pkg in critical_packages) {
    if (require(pkg, character.only = TRUE, quietly = TRUE)) {
        cat(sprintf("✓ %s loaded successfully\n", pkg))
    } else {
        cat(sprintf("✗ %s failed to load\n", pkg))
        all_ok <- FALSE
    }
}

if (all_ok) {
    cat("\n✓ All critical packages verified!\n")
    cat("\nYou can now run the applications:\n")
    cat("  shiny::runApp('launcher.R')\n")
    cat("  shiny::runApp('apps/01_business_analytics')\n")
} else {
    cat("\n✗ Some packages failed to load. Please check error messages above.\n")
}
