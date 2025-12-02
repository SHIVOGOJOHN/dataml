# Financial Analysis Suite
# Portfolio optimization, time series forecasting, and risk analysis

library(shiny)
library(bslib)
library(quantmod)
library(PerformanceAnalytics)
library(forecast)
library(dygraphs)
library(xts)
library(plotly)
library(dplyr)

# Source shared utilities
source("shared/theme.R")
source("shared/ui_components.R")
source("shared/data_utils.R")

# UI
ui <- page_navbar(
    title = "Financial Analysis Suite",
    theme = app_theme,
    tags$head(
        tags$style(HTML(custom_css))
    ),

    # Time Series Explorer
    nav_panel(
        "Time Series",
        icon = icon("chart-line"),
        layout_sidebar(
            sidebar = sidebar(
                h4("Data Settings"),
                radioButtons("ts_source", "Data Source:",
                    choices = c("Generate Sample" = "sample", "Upload CSV" = "upload")
                ),
                conditionalPanel(
                    condition = "input.ts_source == 'sample'",
                    sliderInput("ts_n", "Number of Observations:", min = 50, max = 500, value = 200),
                    selectInput("ts_pattern", "Pattern Type:",
                        choices = c("Trend + Seasonal" = "both", "Trend Only" = "trend", "Seasonal Only" = "seasonal")
                    )
                ),
                hr(),
                h4("Forecast Settings"),
                sliderInput("forecast_horizon", "Forecast Horizon:", min = 5, max = 50, value = 20),
                selectInput("forecast_method", "Method:",
                    choices = c("ARIMA" = "arima", "ETS" = "ets", "Simple Moving Avg" = "sma")
                ),
                actionButton("run_forecast", "Run Forecast", icon = icon("chart-line"), class = "btn-primary")
            ),
            h2("Time Series Forecasting"),
            card_container(
                title = "Interactive Time Series Plot",
                dygraphOutput("ts_dygraph", height = 400)
            ),
            fluidRow(
                column(6, uiOutput("ts_mean_card")),
                column(6, uiOutput("ts_trend_card"))
            ),
            card_container(
                title = "Forecast Results",
                plotlyOutput("forecast_plot", height = 350)
            ),
            card_container(
                title = "Model Diagnostics",
                plotOutput("acf_plot", height = 250)
            )
        )
    ),

    # Portfolio Analysis
    nav_panel(
        "Portfolio",
        icon = icon("briefcase"),
        layout_sidebar(
            sidebar = sidebar(
                h4("Portfolio Composition"),
                sliderInput("weight_1", "Asset 1 Weight:", min = 0, max = 1, value = 0.4, step = 0.05),
                sliderInput("weight_2", "Asset 2 Weight:", min = 0, max = 1, value = 0.3, step = 0.05),
                sliderInput("weight_3", "Asset 3 Weight:", min = 0, max = 1, value = 0.3, step = 0.05),
                textOutput("weight_check"),
                hr(),
                h4("Risk Parameters"),
                sliderInput("risk_free_rate", "Risk-Free Rate (%):", min = 0, max = 5, value = 2, step = 0.1),
                actionButton("calc_portfolio", "Calculate Metrics", icon = icon("calculator"), class = "btn-success")
            ),
            h2("Portfolio Optimization"),
            fluidRow(
                column(4, uiOutput("port_return_card")),
                column(4, uiOutput("port_risk_card")),
                column(4, uiOutput("sharpe_card"))
            ),
            card_container(
                title = "Efficient Frontier",
                plotlyOutput("efficient_frontier_plot", height = 400)
            ),
            card_container(
                title = "Asset Returns",
                plotlyOutput("returns_plot", height = 300)
            )
        )
    ),

    # VaR Analysis
    nav_panel(
        "Risk (VaR)",
        icon = icon("exclamation-triangle"),
        layout_sidebar(
            sidebar = sidebar(
                h4("VaR Parameters"),
                sliderInput("var_confidence", "Confidence Level:", min = 0.90, max = 0.99, value = 0.95, step = 0.01),
                numericInput("var_investment", "Investment Amount ($):", value = 100000, min = 1000),
                sliderInput("var_horizon", "Time Horizon (days):", min = 1, max = 30, value = 10),
                selectInput("var_method", "VaR Method:",
                    choices = c("Historical" = "historical", "Parametric" = "parametric", "Monte Carlo" = "monte_carlo")
                ),
                actionButton("calc_var", "Calculate VaR", icon = icon("shield-alt"), class = "btn-danger")
            ),
            h2("Value at Risk (VaR) Analysis"),
            fluidRow(
                column(6, uiOutput("var_card")),
                column(6, uiOutput("cvar_card"))
            ),
            card_container(
                title = "VaR Visualization",
                plotlyOutput("var_plot", height = 400)
            ),
            card_container(
                title = "Return Distribution",
                plotlyOutput("returns_hist", height = 300)
            ),
            card_container(
                title = "Interpretation",
                uiOutput("var_interpretation")
            )
        )
    )
)

# Server
server <- function(input, output, session) {
    # Generate sample time series
    ts_data <- reactive({
        set.seed(42)
        n <- input$ts_n
        time <- 1:n

        if (input$ts_pattern == "both") {
            trend <- seq(100, 150, length.out = n)
            seasonal <- 10 * sin(2 * pi * time / 30)
            noise <- rnorm(n, 0, 5)
            values <- trend + seasonal + noise
        } else if (input$ts_pattern == "trend") {
            trend <- seq(100, 150, length.out = n)
            noise <- rnorm(n, 0, 5)
            values <- trend + noise
        } else {
            seasonal <- 20 * sin(2 * pi * time / 30) + 100
            noise <- rnorm(n, 0, 5)
            values <- seasonal + noise
        }

        ts(values, frequency = 12)
    })

    # Interactive time series plot
    output$ts_dygraph <- renderDygraph({
        ts_xts <- as.xts(ts_data())
        dygraph(ts_xts, main = "Time Series Data") %>%
            dyRangeSelector() %>%
            dyOptions(colors = "#667eea", strokeWidth = 2)
    })

    output$ts_mean_card <- renderUI({
        metric_card("Mean", round(mean(ts_data()), 2), "chart-bar", "#0d6efd")
    })

    output$ts_trend_card <- renderUI({
        # Simple linear trend
        time <- 1:length(ts_data())
        trend_coef <- coef(lm(as.numeric(ts_data()) ~ time))[2]
        direction <- if (trend_coef > 0) "↗ Increasing" else "↘ Decreasing"
        metric_card("Trend", direction, "arrow-trend-up", "#198754")
    })

    forecast_result <- reactiveVal(NULL)

    observeEvent(input$run_forecast, {
        ts_obj <- ts_data()

        fc <- switch(input$forecast_method,
            "arima" = forecast(auto.arima(ts_obj), h = input$forecast_horizon),
            "ets" = forecast(ets(ts_obj), h = input$forecast_horizon),
            "sma" = {
                n <- length(ts_obj)
                last_10 <- tail(ts_obj, 10)
                mean_val <- mean(last_10)
                forecast_obj <- list(
                    mean = rep(mean_val, input$forecast_horizon),
                    lower = matrix(rep(mean_val - 10, input$forecast_horizon), ncol = 2),
                    upper = matrix(rep(mean_val + 10, input$forecast_horizon), ncol = 2)
                )
                class(forecast_obj) <- "forecast"
                forecast_obj
            }
        )

        forecast_result(fc)
    })

    output$forecast_plot <- renderPlotly({
        req(forecast_result())

        fc <- forecast_result()
        historical <- as.numeric(ts_data())
        n <- length(historical)
        time_hist <- 1:n
        time_fc <- (n + 1):(n + input$forecast_horizon)

        plot_ly() %>%
            add_trace(
                x = time_hist, y = historical, name = "Historical",
                type = "scatter", mode = "lines", line = list(color = "#0d6efd")
            ) %>%
            add_trace(
                x = time_fc, y = fc$mean, name = "Forecast",
                type = "scatter", mode = "lines", line = list(color = "red", dash = "dash")
            ) %>%
            add_ribbons(
                x = time_fc,
                ymin = fc$lower[, 2], ymax = fc$upper[, 2],
                name = "95% CI", fillcolor = "rgba(255, 0, 0, 0.2)",
                line = list(color = "transparent")
            ) %>%
            layout(
                xaxis = list(title = "Time"),
                yaxis = list(title = "Value"),
                hovermode = "x unified"
            )
    })

    output$acf_plot <- renderPlot({
        par(mfrow = c(1, 2))
        acf(ts_data(), main = "ACF")
        pacf(ts_data(), main = "PACF")
    })

    # Portfolio Analysis
    returns_data <- reactive({
        set.seed(42)
        n <- 252 # Trading days
        # Generate correlated returns
        returns <- matrix(rnorm(n * 3, 0, 0.02), ncol = 3)
        returns[, 2] <- returns[, 2] + 0.3 * returns[, 1] # Correlation
        returns[, 3] <- returns[, 3] + 0.2 * returns[, 1]

        colnames(returns) <- c("Asset 1", "Asset 2", "Asset 3")
        returns
    })

    output$weight_check <- renderText({
        total <- input$weight_1 + input$weight_2 + input$weight_3
        if (abs(total - 1) > 0.01) {
            paste("⚠ Weights sum to", round(total, 2), "- should equal 1.0")
        } else {
            "✓ Weights sum to 1.0"
        }
    })

    portfolio_metrics <- reactiveVal(NULL)

    observeEvent(input$calc_portfolio, {
        weights <- c(input$weight_1, input$weight_2, input$weight_3)
        weights <- weights / sum(weights) # Normalize

        returns <- returns_data()
        mean_returns <- colMeans(returns) * 252 # Annualize
        cov_matrix <- cov(returns) * 252

        port_return <- sum(weights * mean_returns)
        port_risk <- sqrt(t(weights) %*% cov_matrix %*% weights)
        sharpe <- (port_return - input$risk_free_rate / 100) / port_risk

        portfolio_metrics(list(
            return = port_return,
            risk = port_risk,
            sharpe = as.numeric(sharpe)
        ))
    })

    output$port_return_card <- renderUI({
        req(portfolio_metrics())
        ret <- portfolio_metrics()$return * 100
        metric_card("Expected Return", paste0(round(ret, 2), "%"), "chart-line", "#198754")
    })

    output$port_risk_card <- renderUI({
        req(portfolio_metrics())
        risk <- portfolio_metrics()$risk * 100
        metric_card("Portfolio Risk", paste0(round(risk, 2), "%"), "exclamation-circle", "#ffc107")
    })

    output$sharpe_card <- renderUI({
        req(portfolio_metrics())
        sharpe <- portfolio_metrics()$sharpe
        metric_card("Sharpe Ratio", round(sharpe, 3), "star", "#0d6efd")
    })

    output$efficient_frontier_plot <- renderPlotly({
        returns <- returns_data()
        mean_returns <- colMeans(returns) * 252
        cov_matrix <- cov(returns) * 252

        # Generate random portfolios
        n_portfolios <- 1000
        port_returns <- numeric(n_portfolios)
        port_risks <- numeric(n_portfolios)

        for (i in 1:n_portfolios) {
            weights <- runif(3)
            weights <- weights / sum(weights)
            port_returns[i] <- sum(weights * mean_returns)
            port_risks[i] <- sqrt(t(weights) %*% cov_matrix %*% weights)
        }

        plot_ly(
            x = port_risks * 100, y = port_returns * 100,
            type = "scatter", mode = "markers",
            marker = list(
                size = 5, color = port_returns, colorscale = "Viridis",
                showscale = TRUE, colorbar = list(title = "Return")
            )
        ) %>%
            layout(
                xaxis = list(title = "Risk (%)"),
                yaxis = list(title = "Return (%)"),
                title = "Efficient Frontier"
            )
    })

    output$returns_plot <- renderPlotly({
        returns <- returns_data()
        cumulative <- apply(returns, 2, function(x) cumprod(1 + x))

        plot_ly() %>%
            add_trace(y = cumulative[, 1], name = "Asset 1", type = "scatter", mode = "lines") %>%
            add_trace(y = cumulative[, 2], name = "Asset 2", type = "scatter", mode = "lines") %>%
            add_trace(y = cumulative[, 3], name = "Asset 3", type = "scatter", mode = "lines") %>%
            layout(
                xaxis = list(title = "Trading Day"),
                yaxis = list(title = "Cumulative Growth"),
                hovermode = "x unified"
            )
    })

    # VaR Analysis
    var_results <- reactiveVal(NULL)

    observeEvent(input$calc_var, {
        set.seed(42)
        # Simulate portfolio returns
        returns <- rnorm(1000, 0.0005, 0.02) # Daily returns

        if (input$var_method == "historical") {
            var_percentile <- quantile(returns, 1 - input$var_confidence)
            var <- -var_percentile * input$var_investment * sqrt(input$var_horizon)
            cvar <- -mean(returns[returns <= var_percentile]) * input$var_investment * sqrt(input$var_horizon)
        } else if (input$var_method == "parametric") {
            mu <- mean(returns)
            sigma <- sd(returns)
            z <- qnorm(1 - input$var_confidence)
            var <- -(mu + z * sigma) * input$var_investment * sqrt(input$var_horizon)
            # CVaR for normal distribution
            cvar <- input$var_investment * sigma * dnorm(z) / (1 - input$var_confidence) * sqrt(input$var_horizon)
        } else {
            # Monte Carlo
            n_sim <- 10000
            mc_returns <- rnorm(n_sim, mean(returns), sd(returns))
            var_percentile <- quantile(mc_returns, 1 - input$var_confidence)
            var <- -var_percentile * input$var_investment * sqrt(input$var_horizon)
            cvar <- -mean(mc_returns[mc_returns <= var_percentile]) * input$var_investment * sqrt(input$var_horizon)
        }

        var_results(list(
            var = var,
            cvar = cvar,
            returns = returns
        ))
    })

    output$var_card <- renderUI({
        req(var_results())
        var_val <- var_results()$var
        info_box(
            "Value at Risk (VaR)",
            paste0("$", format_number(round(var_val, 0))),
            icon = "exclamation-triangle",
            gradient = get_gradient("finance")
        )
    })

    output$cvar_card <- renderUI({
        req(var_results())
        cvar_val <- var_results()$cvar
        metric_card("CVaR (Expected Shortfall)", paste0("$", format_number(round(cvar_val, 0))), "ban", "#dc3545")
    })

    output$var_plot <- renderPlotly({
        req(var_results())

        returns <- var_results()$returns
        var_percentile <- quantile(returns, 1 - input$var_confidence)

        plot_ly(
            x = returns * 100, type = "histogram", nbinsx = 50,
            marker = list(color = "#667eea")
        ) %>%
            add_trace(
                x = c(var_percentile * 100, var_percentile * 100),
                y = c(0, 100), mode = "lines", type = "scatter",
                name = paste0("VaR (", input$var_confidence * 100, "%)"),
                line = list(color = "red", width = 3, dash = "dash")
            ) %>%
            layout(
                xaxis = list(title = "Daily Return (%)"),
                yaxis = list(title = "Frequency"),
                showlegend = TRUE
            )
    })

    output$returns_hist <- renderPlotly({
        req(var_results())

        plot_ly(
            x = var_results()$returns * 100, type = "histogram",
            marker = list(color = "#4facfe"), nbinsx = 40
        ) %>%
            layout(
                xaxis = list(title = "Return (%)"),
                yaxis = list(title = "Frequency")
            )
    })

    output$var_interpretation <- renderUI({
        req(var_results())

        tagList(
            p(
                strong("Value at Risk (VaR):"),
                sprintf(
                    "With %d%% confidence, the maximum expected loss over %d days is $%s.",
                    input$var_confidence * 100,
                    input$var_horizon,
                    format_number(round(var_results()$var, 0))
                )
            ),
            p(
                strong("Conditional VaR (CVaR):"),
                sprintf(
                    "If losses exceed VaR, the expected loss is $%s.",
                    format_number(round(var_results()$cvar, 0))
                )
            ),
            p(em("Note: This is a simplified demonstration. Real-world VaR calculations should account for market conditions, correlations, and other factors."))
        )
    })
}

# Run the application
shinyApp(ui = ui, server = server)
