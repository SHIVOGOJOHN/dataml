# Shared Theme Utilities for Shiny Applications
# Modern UI styling using bslib

library(bslib)

# Main theme for all apps
app_theme <- bs_theme(
  version = 5,
  bg = "#FFFFFF",
  fg = "#212529",
  primary = "#0d6efd",
  secondary = "#6c757d",
  success = "#198754",
  info = "#0dcaf0",
  warning = "#ffc107",
  danger = "#dc3545",
  base_font = font_google("Inter"),
  heading_font = font_google("Outfit"),
  code_font = font_google("Fira Code")
)

# Dark theme variant
app_theme_dark <- bs_theme(
  version = 5,
  bg = "#1a1d23",
  fg = "#e9ecef",
  primary = "#4dabf7",
  secondary = "#868e96",
  success = "#51cf66",
  info = "#4dabf7",
  warning = "#ffd43b",
  danger = "#ff6b6b",
  base_font = font_google("Inter"),
  heading_font = font_google("Outfit"),
  code_font = font_google("Fira Code")
)

# Custom CSS for enhanced styling
custom_css <- "
  /* Card styling */
  .info-card {
    background: linear-gradient(135deg, #667eea 0%, #764ba2 100%);
    color: white;
    border-radius: 12px;
    padding: 20px;
    margin: 10px 0;
    box-shadow: 0 4px 6px rgba(0,0,0,0.1);
    transition: transform 0.2s ease;
  }
  
  .info-card:hover {
    transform: translateY(-2px);
    box-shadow: 0 6px 12px rgba(0,0,0,0.15);
  }
  
  .metric-card {
    background: white;
    border-radius: 8px;
    padding: 20px;
    border-left: 4px solid #0d6efd;
    box-shadow: 0 2px 4px rgba(0,0,0,0.08);
  }
  
  .metric-value {
    font-size: 2.5rem;
    font-weight: 700;
    color: #0d6efd;
    margin: 10px 0;
  }
  
  .metric-label {
    font-size: 0.9rem;
    color: #6c757d;
    text-transform: uppercase;
    letter-spacing: 1px;
  }
  
  /* Button enhancements */
  .btn-custom {
    border-radius: 8px;
    padding: 10px 24px;
    font-weight: 500;
    transition: all 0.3s ease;
  }
  
  .btn-custom:hover {
    transform: translateY(-1px);
    box-shadow: 0 4px 8px rgba(0,0,0,0.15);
  }
  
  /* Plot container */
  .plot-container {
    background: white;
    border-radius: 12px;
    padding: 20px;
    box-shadow: 0 2px 8px rgba(0,0,0,0.08);
    margin: 15px 0;
  }
  
  /* Header styling */
  .app-header {
    background: linear-gradient(90deg, #667eea 0%, #764ba2 100%);
    color: white;
    padding: 30px;
    border-radius: 0 0 20px 20px;
    margin-bottom: 30px;
    box-shadow: 0 4px 12px rgba(0,0,0,0.1);
  }
  
  .app-title {
    font-size: 2rem;
    font-weight: 700;
    margin: 0;
  }
  
  .app-subtitle {
    font-size: 1.1rem;
    opacity: 0.9;
    margin-top: 8px;
  }
  
  /* Sidebar styling */
  .well {
    background: #f8f9fa;
    border-radius: 12px;
    border: none;
    box-shadow: 0 2px 4px rgba(0,0,0,0.05);
  }
  
  /* Table enhancements */
  .dataTables_wrapper {
    padding: 20px;
  }
  
  /* Loading animation */
  .loading-spinner {
    border: 4px solid #f3f3f3;
    border-top: 4px solid #0d6efd;
    border-radius: 50%;
    width: 40px;
    height: 40px;
    animation: spin 1s linear infinite;
    margin: 20px auto;
  }
  
  @keyframes spin {
    0% { transform: rotate(0deg); }
    100% { transform: rotate(360deg); }
  }
  
  /* Success/Error messages */
  .alert {
    border-radius: 8px;
    border: none;
    padding: 16px 20px;
  }
  
  /* Input styling */
  .form-control, .form-select {
    border-radius: 8px;
    border: 1px solid #dee2e6;
    transition: border-color 0.2s ease;
  }
  
  .form-control:focus, .form-select:focus {
    border-color: #0d6efd;
    box-shadow: 0 0 0 3px rgba(13, 110, 253, 0.1);
  }
"

# Function to apply theme to a Shiny UI
apply_theme <- function(ui, dark_mode = FALSE) {
  theme <- if(dark_mode) app_theme_dark else app_theme
  
  tagList(
    ui,
    tags$head(
      tags$style(HTML(custom_css))
    )
  )
}

# Gradient backgrounds for different app types
get_gradient <- function(type = "default") {
  gradients <- list(
    default = "linear-gradient(135deg, #667eea 0%, #764ba2 100%)",
    business = "linear-gradient(135deg, #f093fb 0%, #f5576c 100%)",
    analytics = "linear-gradient(135deg, #4facfe 0%, #00f2fe 100%)",
    finance = "linear-gradient(135deg, #43e97b 0%, #38f9d7 100%)",
    stats = "linear-gradient(135deg, #fa709a 0%, #fee140 100%)",
    geo = "linear-gradient(135deg, #30cfd0 0%, #330867 100%)",
    quality = "linear-gradient(135deg, #a8edea 0%, #fed6e3 100%)",
    survey = "linear-gradient(135deg, #ff9a9e 0%, #fecfef 100%)"
  )
  
  gradients[[type]] %||% gradients$default
}
