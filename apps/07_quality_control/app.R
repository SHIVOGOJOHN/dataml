# Quality Control Monitor
# Statistical process control and monitoring dashboard

library(shiny)
library(bslib)
library(qcc)
library(ggplot2)
library(plotly)
library(dplyr)
library(lubridate)
library(DT)

# Source shared utilities
source("shared/theme.R")
source("shared/ui_components.R")
source("shared/data_utils.R")

# UI
ui <- page_sidebar(
    title = "Quality Control Monitor",
    theme = app_theme,
    sidebar = sidebar(
        width = 300,
        app_header(
            "Quality Control",
            "SPC charts and process monitoring",
            gradient = get_gradient("quality")
        ),
        hr(),
        h4("Data Settings"),
        radioButtons("qc_source", "Data Source:",
            choices = c("Generate Sample" = "sample", "Live Simulation" = "live")
        ),
        sliderInput("qc_n", "Number of Samples:", min = 50, max = 500, value = 200),
        numericInput("qc_target", "Target Value:", value = 100),
        sliderInput("qc_sigma", "Process SD:", min = 1, max = 10, value = 2, step = 0.5),
        checkboxInput("add_outliers", "Add Out-of-Control Points", FALSE),
        hr(),
        h4("Control Chart Settings"),
        selectInput("chart_type", "Chart Type:",
            choices = c(
                "X-bar (Individual)" = "xbar",
                "R Chart (Range)" = "r",
                "S Chart (StdDev)" = "s",
                "P Chart (Proportion)" = "p"
            )
        ),
        sliderInput("control_limits", "Control Limits (σ):", min = 1, max = 4, value = 3, step = 0.5),
        actionButton("generate_qc", "Generate Data", icon = icon("sync"), class = "btn-primary")
    ),
    tags$head(
        tags$style(HTML(custom_css))
    ),
    navset_card_tab(
        nav_panel(
            "Control Charts",
            icon = icon("chart-line"),
            h3("Statistical Process Control"),
            fluidRow(
                column(3, uiOutput("in_control_card")),
                column(3, uiOutput("cp_card")),
                column(3, uiOutput("cpk_card")),
                column(3, uiOutput("sigma_level_card"))
            ),
            card_container(
                title = "Control Chart",
                plotlyOutput("control_chart", height = 400)
            ),
            fluidRow(
                column(
                    6,
                    card_container(
                        title = "Process Histogram",
                        plotlyOutput("process_histogram", height = 300)
                    )
                ),
                column(
                    6,
                    card_container(
                        title = "Run Chart",
                        plotlyOutput("run_chart", height = 300)
                    )
                )
            ),
            card_container(
                title = "Out-of-Control Points",
                DTOutput("ooc_table")
            )
        ),
        nav_panel(
            "Process Capability",
            icon = icon("tachometer-alt"),
            h3("Process Capability Analysis"),
            fluidRow(
                column(4, uiOutput("cp_detail_card")),
                column(4, uiOutput("cpk_detail_card")),
                column(4, uiOutput("ppm_card"))
            ),
            card_container(
                title = "Capability Analysis",
                plotOutput("capability_plot", height = 400)
            ),
            card_container(
                title = "Specification Limits",
                uiOutput("spec_limits_ui"),
                actionButton("update_specs", "Update Limits", icon = icon("check"), class = "btn-success")
            ),
            card_container(
                title = "Process Summary",
                verbatimTextOutput("process_summary")
            )
        ),
        nav_panel(
            "Real-time Monitor",
            icon = icon("clock"),
            h3("Live Process Monitoring"),
            fluidRow(
                column(6, uiOutput("current_value_card")),
                column(6, uiOutput("status_card"))
            ),
            card_container(
                title = "Real-time Chart",
                plotlyOutput("realtime_chart", height = 400)
            ),
            actionButton("add_sample", "Add Sample Point", icon = icon("plus"), class = "btn-info"),
            actionButton("reset_monitor", "Reset", icon = icon("redo"), class = "btn-warning")
        )
    )
)

# Server
server <- function(input, output, session) {
    # Generate QC data
    qc_data <- reactive({
        input$generate_qc # Trigger

        set.seed(sample.int(10000, 1))
        data <- generate_qc_data(n = input$qc_n, seed = NULL)

        # Adjust to user settings
        data$measurement <- (data$measurement - 100) / 2 * input$qc_sigma + input$qc_target

        if (input$add_outliers) {
            # Add some out-of-control points
            outlier_idx <- sample(1:nrow(data), min(10, nrow(data) %/% 20))
            data$measurement[outlier_idx] <- data$measurement[outlier_idx] +
                sample(c(-1, 1), length(outlier_idx), replace = TRUE) * input$qc_sigma * 4
        }

        # Recalculate control limits
        data$ucl <- input$qc_target + 3 * input$qc_sigma
        data$lcl <- input$qc_target - 3 * input$qc_sigma
        data$out_of_control <- data$measurement > data$ucl | data$measurement < data$lcl

        data
    })

    # Calculate process capability
    process_capability <- reactive({
        req(qc_data())

        data <- qc_data()
        USL <- input$qc_target + 3 * input$qc_sigma
        LSL <- input$qc_target - 3 * input$qc_sigma

        mean_val <- mean(data$measurement)
        sd_val <- sd(data$measurement)

        Cp <- (USL - LSL) / (6 * sd_val)
        Cpk <- min((USL - mean_val) / (3 * sd_val), (mean_val - LSL) / (3 * sd_val))

        # PPM defective
        ppm <- (pnorm(LSL, mean_val, sd_val) + (1 - pnorm(USL, mean_val, sd_val))) * 1e6

        list(Cp = Cp, Cpk = Cpk, ppm = ppm, mean = mean_val, sd = sd_val)
    })

    # Info cards
    output$in_control_card <- renderUI({
        req(qc_data())
        pct_in_control <- mean(!qc_data()$out_of_control) * 100
        metric_card("In Control", paste0(round(pct_in_control, 1), "%"), "check-circle", "#198754")
    })

    output$cp_card <- renderUI({
        req(process_capability())
        cp <- process_capability()$Cp
        color <- if (cp >= 1.33) "#198754" else if (cp >= 1.0) "#ffc107" else "#dc3545"
        metric_card("Cp", round(cp, 3), "arrows-alt-h", color)
    })

    output$cpk_card <- renderUI({
        req(process_capability())
        cpk <- process_capability()$Cpk
        color <- if (cpk >= 1.33) "#198754" else if (cpk >= 1.0) "#ffc107" else "#dc3545"
        metric_card("Cpk", round(cpk, 3), "arrow-right", color)
    })

    output$sigma_level_card <- renderUI({
        req(process_capability())
        cpk <- process_capability()$Cpk
        sigma_level <- cpk * 3
        metric_card("Sigma Level", round(sigma_level, 2), "star", "#0d6efd")
    })

    # Control chart
    output$control_chart <- renderPlotly({
        req(qc_data())

        data <- qc_data()

        plot_ly(data, x = ~sample_id, y = ~measurement) %>%
            add_trace(
                type = "scatter", mode = "lines+markers",
                marker = list(size = 6, color = ~ ifelse(out_of_control, "red", "blue")),
                line = list(color = "lightblue"),
                name = "Measurement"
            ) %>%
            add_trace(
                x = c(1, nrow(data)), y = c(input$qc_target, input$qc_target),
                type = "scatter", mode = "lines",
                line = list(color = "green", width = 2, dash = "solid"),
                name = "Target"
            ) %>%
            add_trace(
                x = c(1, nrow(data)), y = c(data$ucl[1], data$ucl[1]),
                type = "scatter", mode = "lines",
                line = list(color = "red", width = 1, dash = "dash"),
                name = "UCL"
            ) %>%
            add_trace(
                x = c(1, nrow(data)), y = c(data$lcl[1], data$lcl[1]),
                type = "scatter", mode = "lines",
                line = list(color = "red", width = 1, dash = "dash"),
                name = "LCL"
            ) %>%
            layout(
                xaxis = list(title = "Sample Number"),
                yaxis = list(title = "Measurement"),
                hovermode = "x unified",
                showlegend = TRUE
            )
    })

    # Process histogram
    output$process_histogram <- renderPlotly({
        req(qc_data())

        plot_ly(qc_data(),
            x = ~measurement, type = "histogram",
            marker = list(color = "#667eea"), nbinsx = 30
        ) %>%
            add_trace(
                x = c(input$qc_target, input$qc_target), y = c(0, input$qc_n / 10),
                type = "scatter", mode = "lines",
                line = list(color = "green", width = 2),
                name = "Target"
            ) %>%
            layout(
                xaxis = list(title = "Measurement"),
                yaxis = list(title = "Frequency"),
                showlegend = TRUE
            )
    })

    # Run chart
    output$run_chart <- renderPlotly({
        req(qc_data())

        data <- qc_data()

        plot_ly(data,
            x = ~timestamp, y = ~measurement,
            type = "scatter", mode = "lines+markers",
            marker = list(size = 5, color = "#4facfe"),
            line = list(color = "#4facfe")
        ) %>%
            layout(
                xaxis = list(title = "Time"),
                yaxis = list(title = "Measurement")
            )
    })

    # Out-of-control table
    output$ooc_table <- renderDT({
        req(qc_data())

        ooc_points <- qc_data() %>%
            filter(out_of_control) %>%
            select(sample_id, timestamp, measurement, machine, operator) %>%
            mutate(
                timestamp = format(timestamp, "%Y-%m-%d %H:%M"),
                measurement = round(measurement, 2)
            )

        if (nrow(ooc_points) == 0) {
            data.frame(Message = "No out-of-control points detected")
        } else {
            datatable(ooc_points, options = list(pageLength = 10), rownames = FALSE)
        }
    })

    # Capability cards
    output$cp_detail_card <- renderUI({
        req(process_capability())
        cp <- process_capability()$Cp
        interpretation <- if (cp >= 1.33) "Capable" else if (cp >= 1.0) "Marginally Capable" else "Not Capable"
        info_box(
            "Process Capability (Cp)",
            paste0(round(cp, 3), " - ", interpretation),
            icon = "arrows-alt-h",
            gradient = get_gradient("quality")
        )
    })

    output$cpk_detail_card <- renderUI({
        req(process_capability())
        cpk <- process_capability()$Cpk
        metric_card("Cpk Index", round(cpk, 3), "arrow-right", "#0d6efd")
    })

    output$ppm_card <- renderUI({
        req(process_capability())
        ppm <- process_capability()$ppm
        metric_card("Defects (PPM)", format_number(round(ppm, 0)), "exclamation-triangle", "#dc3545")
    })

    # Capability plot
    output$capability_plot <- renderPlot({
        req(qc_data(), process_capability())

        data <- qc_data()
        cap <- process_capability()

        USL <- input$qc_target + 3 * input$qc_sigma
        LSL <- input$qc_target - 3 * input$qc_sigma

        # Create histogram
        hist(data$measurement,
            breaks = 30, col = rgb(0.4, 0.5, 0.9, 0.6),
            main = "Process Capability Histogram",
            xlab = "Measurement", ylab = "Frequency",
            xlim = c(LSL - input$qc_sigma, USL + input$qc_sigma)
        )

        # Add normal curve
        x <- seq(min(data$measurement), max(data$measurement), length.out = 100)
        y <- dnorm(x, cap$mean, cap$sd) * length(data$measurement) * diff(range(data$measurement)) / 30
        lines(x, y, col = "blue", lwd = 2)

        # Add specification limits
        abline(v = USL, col = "red", lwd = 2, lty = 2)
        abline(v = LSL, col = "red", lwd = 2, lty = 2)
        abline(v = input$qc_target, col = "green", lwd = 2)

        legend("topright",
            legend = c("Target", "USL/LSL", "Normal Fit"),
            col = c("green", "red", "blue"),
            lwd = 2, lty = c(1, 2, 1)
        )
    })

    # Spec limits UI
    output$spec_limits_ui <- renderUI({
        tagList(
            sliderInput("usl_input", "Upper Spec Limit (USL):",
                min = input$qc_target, max = input$qc_target + 10 * input$qc_sigma,
                value = input$qc_target + 3 * input$qc_sigma
            ),
            sliderInput("lsl_input", "Lower Spec Limit (LSL):",
                min = input$qc_target - 10 * input$qc_sigma, max = input$qc_target,
                value = input$qc_target - 3 * input$qc_sigma
            )
        )
    })

    # Process summary
    output$process_summary <- renderPrint({
        req(process_capability())

        cap <- process_capability()

        cat("Process Capability Analysis\n")
        cat("===========================\n\n")
        cat("Process Mean:", round(cap$mean, 2), "\n")
        cat("Process SD:", round(cap$sd, 2), "\n")
        cat("Target:", input$qc_target, "\n\n")
        cat("Capability Indices:\n")
        cat("  Cp  =", round(cap$Cp, 3), "\n")
        cat("  Cpk =", round(cap$Cpk, 3), "\n\n")
        cat("Expected Defects:", round(cap$ppm, 0), "PPM\n")
    })

    # Real-time monitoring
    realtime_data <- reactiveValues(
        measurements = numeric(0),
        timestamps = as.POSIXct(character(0))
    )

    observeEvent(input$add_sample, {
        new_val <- rnorm(1, input$qc_target, input$qc_sigma)
        realtime_data$measurements <- c(realtime_data$measurements, new_val)
        realtime_data$timestamps <- c(realtime_data$timestamps, Sys.time())
    })

    observeEvent(input$reset_monitor, {
        realtime_data$measurements <- numeric(0)
        realtime_data$timestamps <- as.POSIXct(character(0))
    })

    output$current_value_card <- renderUI({
        if (length(realtime_data$measurements) == 0) {
            metric_card("Current Value", "N/A", "hourglass", "#6c757d")
        } else {
            current <- tail(realtime_data$measurements, 1)
            metric_card("Current Value", round(current, 2), "tachometer-alt", "#0d6efd")
        }
    })

    output$status_card <- renderUI({
        if (length(realtime_data$measurements) == 0) {
            info_box("Status", "Waiting for data...",
                icon = "clock",
                gradient = get_gradient("quality")
            )
        } else {
            current <- tail(realtime_data$measurements, 1)
            ucl <- input$qc_target + 3 * input$qc_sigma
            lcl <- input$qc_target - 3 * input$qc_sigma

            if (current > ucl || current < lcl) {
                info_box("Status", "OUT OF CONTROL",
                    icon = "times-circle",
                    gradient = "linear-gradient(135deg, #dc3545 0%, #c82333 100%)"
                )
            } else {
                info_box("Status", "IN CONTROL",
                    icon = "check-circle",
                    gradient = "linear-gradient(135deg, #198754 0%, #157347 100%)"
                )
            }
        }
    })

    output$realtime_chart <- renderPlotly({
        if (length(realtime_data$measurements) == 0) {
            plot_ly() %>%
                layout(
                    title = "No data yet - click 'Add Sample Point'",
                    xaxis = list(title = "Sample"),
                    yaxis = list(title = "Measurement")
                )
        } else {
            ucl <- input$qc_target + 3 * input$qc_sigma
            lcl <- input$qc_target - 3 * input$qc_sigma

            plot_ly(
                x = 1:length(realtime_data$measurements),
                y = realtime_data$measurements,
                type = "scatter", mode = "lines+markers",
                marker = list(size = 8, color = "#667eea"),
                line = list(color = "#667eea", width = 2)
            ) %>%
                add_trace(
                    x = c(1, length(realtime_data$measurements)),
                    y = c(input$qc_target, input$qc_target),
                    mode = "lines", name = "Target",
                    line = list(color = "green", width = 2)
                ) %>%
                add_trace(
                    x = c(1, length(realtime_data$measurements)),
                    y = c(ucl, ucl),
                    mode = "lines", name = "UCL",
                    line = list(color = "red", width = 1, dash = "dash")
                ) %>%
                add_trace(
                    x = c(1, length(realtime_data$measurements)),
                    y = c(lcl, lcl),
                    mode = "lines", name = "LCL",
                    line = list(color = "red", width = 1, dash = "dash")
                ) %>%
                layout(
                    xaxis = list(title = "Sample"),
                    yaxis = list(title = "Measurement"),
                    showlegend = TRUE
                )
        }
    })
}

# Run the application
shinyApp(ui = ui, server = server)
