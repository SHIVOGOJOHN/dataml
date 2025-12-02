# Shiny Applications Launcher
# Central hub to access all applications

library(shiny)
library(bslib)
library(shinyjs)

# App metadata with deployment URLs
apps <- list(
  list(
    id = "01_business_analytics",
    name = "Business Analytics",
    description = "Real-time sales dashboard with KPI monitoring and customer churn analysis",
    icon = "chart-line",
    color = "linear-gradient(135deg, #f093fb 0%, #f5576c 100%)",
    tags = c("Business", "Analytics", "Sales", "KPI"),
    url = "https://shivogojohn.shinyapps.io/business-analytics-dashboard/"
  ),
  list(
    id = "02_data_explorer",
    name = "Data Explorer",
    description = "Upload and explore datasets with dynamic visualizations and filtering",
    icon = "search",
    color = "linear-gradient(135deg, #4facfe 0%, #00f2fe 100%)",
    tags = c("Data", "Exploration", "Visualization"),
    url = "https://shivogojohn.shinyapps.io/data-explorer/"
  ),
  list(
    id = "03_statistical_lab",
    name = "Statistical Lab",
    description = "Interactive tools for CI simulation, A/B testing, distributions, and regression",
    icon = "flask",
    color = "linear-gradient(135deg, #fa709a 0%, #fee140 100%)",
    tags = c("Statistics", "Education", "Simulation"),
    url = "https://shivogojohn.shinyapps.io/statistical-lab/"
  ),
  list(
    id = "04_model_deployment",
    name = "Model Deployment Hub",
    description = "Deploy and test machine learning models with interactive predictions",
    icon = "robot",
    color = "linear-gradient(135deg, #667eea 0%, #764ba2 100%)",
    tags = c("ML", "Prediction", "Model"),
    url = "https://datamlproject.shinyapps.io/model-deployment-hub/"
  ),
  list(
    id = "05_financial_analysis",
    name = "Financial Analysis",
    description = "Portfolio optimization, time series forecasting, and VaR analysis",
    icon = "dollar-sign",
    color = "linear-gradient(135deg, #43e97b 0%, #38f9d7 100%)",
    tags = c("Finance", "Portfolio", "Risk"),
    url = "https://shivogojohn.shinyapps.io/financial-analysis-suite/"
  ),
  list(
    id = "06_geospatial_explorer",
    name = "Geospatial Explorer",
    description = "Interactive maps with points, heatmaps, clusters, and choropleth",
    icon = "map",
    color = "linear-gradient(135deg, #30cfd0 0%, #330867 100%)",
    tags = c("Maps", "Geographic", "Spatial"),
    url = "https://shivogojohn.shinyapps.io/geospatial-explorer-kenya/"
  ),
  list(
    id = "07_quality_control",
    name = "Quality Control Monitor",
    description = "SPC charts, process capability analysis, and real-time monitoring",
    icon = "check-square",
    color = "linear-gradient(135deg, #a8edea 0%, #fed6e3 100%)",
    tags = c("QC", "SPC", "Manufacturing"),
    url = "https://datamlproject.shinyapps.io/quality-control-monitor/"
  ),
  list(
    id = "08_survey_analyzer",
    name = "Survey Analyzer",
    description = "Analyze survey data with NPS, satisfaction metrics, and reporting",
    icon = "clipboard-list",
    color = "linear-gradient(135deg, #ff9a9e 0%, #fecfef 100%)",
    tags = c("Survey", "NPS", "Analysis"),
    url = "https://datamlproject.shinyapps.io/survey-analyzer/"
  )
)

# UI
ui <- page_fillable(
  theme = bs_theme(
    version = 5,
    bg = "#f8f9fa",
    fg = "#212529",
    primary = "#0d6efd",
    base_font = font_google("Inter"),
    heading_font = font_google("Outfit")
  ),
  tags$head(
    tags$style(HTML("
      .app-header {
        background: linear-gradient(90deg, #667eea 0%, #764ba2 100%);
        color: white;
        padding: 60px 40px;
        border-radius: 0 0 30px 30px;
        margin-bottom: 40px;
        box-shadow: 0 6px 20px rgba(0,0,0,0.15);
      }

      .app-title {
        font-size: 3rem;
        font-weight: 700;
        margin: 0;
        text-shadow: 2px 2px 4px rgba(0,0,0,0.2);
      }

      .app-subtitle {
        font-size: 1.3rem;
        opacity: 0.95;
        margin-top: 15px;
      }

      .app-card {
        background: white;
        border-radius: 16px;
        padding: 30px;
        margin: 15px;
        box-shadow: 0 4px 12px rgba(0,0,0,0.08);
        transition: all 0.3s ease;
        cursor: pointer;
        position: relative;
        overflow: hidden;
      }

      .app-card:hover {
        transform: translateY(-8px);
        box-shadow: 0 8px 24px rgba(0,0,0,0.15);
      }

      .app-card-header {
        height: 80px;
        border-radius: 12px;
        display: flex;
        align-items: center;
        justify-content: center;
        margin-bottom: 20px;
        color: white;
        font-size: 2.5rem;
      }

      .app-card-title {
        font-size: 1.5rem;
        font-weight: 600;
        margin: 15px 0 10px 0;
        color: #212529;
      }

      .app-card-description {
        color: #6c757d;
        font-size: 0.95rem;
        line-height: 1.5;
        margin-bottom: 15px;
      }

      .app-tag {
        display: inline-block;
        background: #e9ecef;
        color: #495057;
        padding: 4px 12px;
        border-radius: 12px;
        font-size: 0.75rem;
        margin: 3px;
        font-weight: 500;
      }

      .launch-btn {
        width: 100%;
        background: linear-gradient(90deg, #667eea 0%, #764ba2 100%);
        color: white;
        border: none;
        padding: 12px;
        border-radius: 8px;
        font-weight: 600;
        font-size: 1rem;
        cursor: pointer;
        transition: all 0.3s ease;
        margin-top: 15px;
      }

      .launch-btn:hover {
        transform: translateY(-2px);
        box-shadow: 0 6px 16px rgba(102, 126, 234, 0.4);
      }

      .search-box {
        max-width: 600px;
        margin: 0 auto 30px auto;
      }

      .footer {
        text-align: center;
        padding: 40px 20px;
        color: #6c757d;
        margin-top: 60px;
        border-top: 1px solid #dee2e6;
      }
    "))
  ),
  useShinyjs(), # Enable shinyjs for JavaScript redirects
  div(
    class = "app-header",
    h1(class = "app-title", "Shiny Applications Suite"),
    p(class = "app-subtitle", "A comprehensive collection of interactive R Shiny applications for data analysis, visualization, and decision-making")
  ),
  div(
    class = "container",
    style = "max-width: 1400px; margin: 0 auto; padding: 0 20px;",
    div(
      class = "search-box",
      textInput("search", "",
        placeholder = "Search applications...",
        width = "100%"
      )
    ),
    uiOutput("apps_grid"),
    div(
      class = "footer",
      p("Built with R Shiny | All applications use modern bslib theming and interactive visualizations"),
      p("Total Applications: 8 | Click any card to view application details")
    )
  )
)

# Server
server <- function(input, output, session) {
  # Filter apps based on search query
  filtered_apps <- reactive({
    query <- tolower(input$search)

    if (is.null(query) || query == "") {
      return(apps)
    }

    # Filter apps by name, description, or tags
    Filter(function(app) {
      grepl(query, tolower(app$name)) ||
        grepl(query, tolower(app$description)) ||
        any(sapply(app$tags, function(tag) grepl(query, tolower(tag))))
    }, apps)
  })

  # Render filtered apps grid
  output$apps_grid <- renderUI({
    filtered <- filtered_apps()

    if (length(filtered) == 0) {
      div(
        style = "text-align: center; padding: 60px; color: #6c757d;",
        icon("search", style = "font-size: 4rem; opacity: 0.3; margin-bottom: 20px;"),
        h3("No applications found"),
        p("Try a different search term")
      )
    } else {
      div(
        id = "apps-grid",
        style = "display: grid; grid-template-columns: repeat(auto-fill, minmax(350px, 1fr)); gap: 20px;",
        lapply(filtered, function(app) {
          div(
            class = "app-card",
            onclick = sprintf("Shiny.setInputValue('launch_app', '%s', {priority: 'event'})", app$id),
            div(
              class = "app-card-header",
              style = sprintf("background: %s;", app$color),
              icon(app$icon)
            ),
            div(
              class = "app-card-title",
              app$name
            ),
            div(
              class = "app-card-description",
              app$description
            ),
            div(
              lapply(app$tags, function(tag) {
                span(class = "app-tag", tag)
              })
            ),
            tags$button(
              class = "launch-btn",
              "Launch Application"
            )
          )
        })
      )
    }
  })

  # Launch app - redirect to deployed URL
  observeEvent(input$launch_app, {
    app_info <- apps[[which(sapply(apps, function(a) a$id == input$launch_app))]]

    # Open deployed app in new browser tab
    runjs(sprintf('window.open("%s", "_blank");', app_info$url))
  })
}

# Run the application
shinyApp(ui = ui, server = server)
