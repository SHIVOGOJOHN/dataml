# Data Explorer
# Interactive data exploration tool with CSV/Excel upload and dynamic visualization

library(shiny)
library(bslib)
library(readr)
library(readxl)
library(ggplot2)
library(plotly)
library(DT)
library(dplyr)
library(tidyr)
library(skimr)
library(corrplot)

# Source shared utilities
# Source shared utilities
source("shared/theme.R")
source("shared/ui_components.R")
source("shared/data_utils.R")

# UI
ui <- page_sidebar(
    title = "Data Explorer",
    theme = app_theme,
    sidebar = sidebar(
        width = 300,
        app_header(
            "Data Explorer",
            "Upload and explore your data interactively",
            gradient = get_gradient("analytics")
        ),
        hr(),
        h4("Data Source"),
        radioButtons(
            "data_source",
            "Choose data source:",
            choices = c("Upload File" = "upload", "Use Sample Data" = "sample"),
            selected = "sample"
        ),
        conditionalPanel(
            condition = "input.data_source == 'upload'",
            fileInput(
                "file_upload",
                "Choose CSV/Excel file:",
                accept = c(".csv", ".xlsx", ".xls"),
                buttonLabel = "Browse...",
                placeholder = "No file selected"
            )
        ),
        conditionalPanel(
            condition = "input.data_source == 'sample'",
            selectInput(
                "sample_dataset",
                "Select sample dataset:",
                choices = c(
                    "Sales Data" = "sales",
                    "Customer Churn" = "churn",
                    "Quality Control" = "qc",
                    "Survey Results" = "survey"
                )
            )
        ),
        hr(),
        h4("Visualization Settings"),
        conditionalPanel(
            condition = "output.data_loaded",
            uiOutput("x_var_ui"),
            uiOutput("y_var_ui"),
            uiOutput("color_var_ui"),
            selectInput(
                "plot_type",
                "Plot Type:",
                choices = c(
                    "Scatter Plot" = "scatter",
                    "Bar Chart" = "bar",
                    "Box Plot" = "box",
                    "Histogram" = "histogram",
                    "Line Chart" = "line",
                    "Violin Plot" = "violin"
                )
            ),
            checkboxInput("show_trend", "Show Trend Line", FALSE)
        )
    ),
    tags$head(
        tags$style(HTML(custom_css))
    ),
    navset_card_tab(
        nav_panel(
            "Summary",
            icon = icon("table"),
            h3("Data Summary"),
            fluidRow(
                column(4, uiOutput("n_rows_card")),
                column(4, uiOutput("n_cols_card")),
                column(4, uiOutput("n_missing_card"))
            ),
            hr(),
            card_container(
                title = "Variable Summary Statistics",
                DTOutput("summary_table")
            ),
            card_container(
                title = "Data Preview",
                DTOutput("data_preview")
            )
        ),
        nav_panel(
            "Visualize",
            icon = icon("chart-bar"),
            h3("Interactive Plots"),
            card_container(
                title = "Main Plot",
                plotlyOutput("main_plot", height = 500)
            ),
            fluidRow(
                column(
                    6,
                    card_container(
                        title = "Distribution Plot",
                        plotlyOutput("distribution_plot", height = 350)
                    )
                ),
                column(
                    6,
                    card_container(
                        title = "Correlation Matrix",
                        plotOutput("correlation_plot", height = 350)
                    )
                )
            )
        ),
        nav_panel(
            "Filter & Transform",
            icon = icon("filter"),
            h3("Data Filtering"),
            fluidRow(
                column(
                    6,
                    card_container(
                        title = "Filters",
                        uiOutput("filter_ui"),
                        actionButton("apply_filters", "Apply Filters", icon = icon("check"), class = "btn-primary"),
                        actionButton("reset_filters", "Reset", icon = icon("redo"), class = "btn-warning")
                    )
                ),
                column(
                    6,
                    card_container(
                        title = "Data Info",
                        verbatimTextOutput("filter_info")
                    )
                )
            ),
            card_container(
                title = "Filtered Data",
                DTOutput("filtered_data_table"),
                downloadButton("download_filtered", "Download Filtered Data", class = "btn-success")
            )
        ),
        nav_panel(
            "Export",
            icon = icon("download"),
            h3("Export Options"),
            card_container(
                title = "Download Data",
                p("Download the current dataset or summary statistics."),
                downloadButton("download_data", "Download Full Data (CSV)", class = "btn-primary"),
                downloadButton("download_summary", "Download Summary (CSV)", class = "btn-info"),
                hr(),
                h4("Download Plots"),
                p("Download the current visualization as an image."),
                downloadButton("download_plot", "Download Main Plot (PNG)", class = "btn-success")
            )
        )
    )
)

# Server
server <- function(input, output, session) {
    # Reactive data
    data <- reactive({
        if (input$data_source == "sample") {
            switch(input$sample_dataset,
                "sales" = generate_sales_data(),
                "churn" = generate_churn_data(),
                "qc" = generate_qc_data(),
                "survey" = generate_survey_data()
            )
        } else {
            req(input$file_upload)

            ext <- tools::file_ext(input$file_upload$name)

            tryCatch(
                {
                    if (ext == "csv") {
                        read_csv(input$file_upload$datapath, show_col_types = FALSE)
                    } else if (ext %in% c("xlsx", "xls")) {
                        read_excel(input$file_upload$datapath)
                    } else {
                        NULL
                    }
                },
                error = function(e) {
                    showNotification("Error loading file", type = "error")
                    NULL
                }
            )
        }
    })

    # Check if data is loaded
    output$data_loaded <- reactive({
        !is.null(data())
    })
    outputOptions(output, "data_loaded", suspendWhenHidden = FALSE)

    # Get numeric and categorical columns
    numeric_cols <- reactive({
        req(data())
        names(data())[sapply(data(), is.numeric)]
    })

    categorical_cols <- reactive({
        req(data())
        names(data())[sapply(data(), function(x) is.character(x) || is.factor(x))]
    })

    all_cols <- reactive({
        req(data())
        names(data())
    })

    # Dynamic UI for variable selection
    output$x_var_ui <- renderUI({
        selectInput("x_var", "X Variable:", choices = all_cols())
    })

    output$y_var_ui <- renderUI({
        selectInput("y_var", "Y Variable:", choices = c("None", numeric_cols()), selected = "None")
    })

    output$color_var_ui <- renderUI({
        selectInput("color_var", "Color By:", choices = c("None", all_cols()), selected = "None")
    })

    # Summary Cards
    output$n_rows_card <- renderUI({
        req(data())
        metric_card("Rows", format_number(nrow(data())), "table", "#0d6efd")
    })

    output$n_cols_card <- renderUI({
        req(data())
        metric_card("Columns", format_number(ncol(data())), "columns", "#198754")
    })

    output$n_missing_card <- renderUI({
        req(data())
        n_missing <- sum(is.na(data()))
        metric_card("Missing Values", format_number(n_missing), "exclamation-triangle", "#ffc107")
    })

    # Summary Table
    output$summary_table <- renderDT({
        req(data())

        summary_stats <- calculate_summary_stats(data())

        datatable(
            summary_stats,
            options = list(pageLength = 10, scrollX = TRUE),
            rownames = FALSE
        )
    })

    # Data Preview
    output$data_preview <- renderDT({
        req(data())

        datatable(
            head(data(), 100),
            options = list(pageLength = 10, scrollX = TRUE),
            rownames = FALSE
        )
    })

    # Main Plot
    output$main_plot <- renderPlotly({
        req(data(), input$x_var)

        df <- data()
        x_var <- input$x_var
        y_var <- if (input$y_var == "None") NULL else input$y_var
        color_var <- if (input$color_var == "None") NULL else input$color_var

        # Create base plot based on type
        if (input$plot_type == "scatter" && !is.null(y_var)) {
            p <- plot_ly(df,
                x = ~ get(x_var), y = ~ get(y_var), type = "scatter", mode = "markers",
                color = if (!is.null(color_var)) ~ get(color_var) else NULL,
                marker = list(size = 8, opacity = 0.7)
            )

            if (input$show_trend && is.numeric(df[[x_var]]) && is.numeric(df[[y_var]])) {
                fit <- lm(get(y_var) ~ get(x_var), data = df)
                p <- p %>% add_trace(
                    x = ~ get(x_var), y = fitted(fit), mode = "lines",
                    name = "Trend", line = list(color = "red", dash = "dash")
                )
            }
        } else if (input$plot_type == "bar") {
            if (!is.null(y_var)) {
                agg_data <- df %>%
                    group_by(!!sym(x_var)) %>%
                    summarise(value = mean(!!sym(y_var), na.rm = TRUE), .groups = "drop")
                p <- plot_ly(agg_data, x = ~ get(x_var), y = ~value, type = "bar")
            } else {
                count_data <- df %>% count(!!sym(x_var))
                p <- plot_ly(count_data, x = ~ get(x_var), y = ~n, type = "bar")
            }
        } else if (input$plot_type == "box" && !is.null(y_var)) {
            p <- plot_ly(df,
                x = ~ get(x_var), y = ~ get(y_var), type = "box",
                color = if (!is.null(color_var)) ~ get(color_var) else NULL
            )
        } else if (input$plot_type == "histogram") {
            p <- plot_ly(df,
                x = ~ get(x_var), type = "histogram",
                nbinsx = 30,
                marker = list(color = "#667eea")
            )
        } else if (input$plot_type == "line" && !is.null(y_var)) {
            p <- plot_ly(df,
                x = ~ get(x_var), y = ~ get(y_var), type = "scatter", mode = "lines+markers",
                color = if (!is.null(color_var)) ~ get(color_var) else NULL
            )
        } else if (input$plot_type == "violin" && !is.null(y_var)) {
            p <- plot_ly(df,
                x = ~ get(x_var), y = ~ get(y_var), type = "violin",
                color = if (!is.null(color_var)) ~ get(color_var) else NULL
            )
        } else {
            # Default: histogram
            p <- plot_ly(df, x = ~ get(x_var), type = "histogram")
        }

        p %>% layout(
            xaxis = list(title = x_var),
            yaxis = list(title = y_var %||% "Count"),
            hovermode = "closest"
        )
    })

    # Distribution Plot
    output$distribution_plot <- renderPlotly({
        req(data(), numeric_cols())

        if (length(numeric_cols()) > 0) {
            # Plot distribution of first numeric column
            first_num_col <- numeric_cols()[1]

            plot_ly(data(),
                x = ~ get(first_num_col), type = "histogram",
                marker = list(color = "#4facfe"),
                nbinsx = 30
            ) %>%
                layout(
                    title = paste("Distribution of", first_num_col),
                    xaxis = list(title = first_num_col),
                    yaxis = list(title = "Frequency")
                )
        }
    })

    # Correlation Plot
    output$correlation_plot <- renderPlot({
        req(data(), numeric_cols())

        if (length(numeric_cols()) >= 2) {
            numeric_data <- data() %>% select(all_of(numeric_cols()))
            cor_matrix <- cor(numeric_data, use = "complete.obs")

            corrplot(cor_matrix,
                method = "color", type = "upper",
                addCoef.col = "black", number.cex = 0.7,
                tl.col = "black", tl.srt = 45,
                col = colorRampPalette(c("#dc3545", "white", "#0d6efd"))(200)
            )
        } else {
            plot.new()
            text(0.5, 0.5, "Need at least 2 numeric variables", cex = 1.5)
        }
    })

    # Filter UI
    output$filter_ui <- renderUI({
        req(data())

        # Create filter inputs for first few columns
        filter_inputs <- lapply(head(all_cols(), 5), function(col) {
            if (is.numeric(data()[[col]])) {
                sliderInput(
                    paste0("filter_", col),
                    paste("Filter", col),
                    min = min(data()[[col]], na.rm = TRUE),
                    max = max(data()[[col]], na.rm = TRUE),
                    value = c(min(data()[[col]], na.rm = TRUE), max(data()[[col]], na.rm = TRUE))
                )
            } else {
                selectInput(
                    paste0("filter_", col),
                    paste("Filter", col),
                    choices = c("All", unique(data()[[col]])),
                    selected = "All",
                    multiple = TRUE
                )
            }
        })

        tagList(filter_inputs)
    })

    # Filtered data
    filtered_data <- reactiveVal(NULL)

    observeEvent(input$apply_filters, {
        filtered_data(data())
    })

    observeEvent(input$reset_filters, {
        filtered_data(data())
    })

    output$filter_info <- renderPrint({
        if (is.null(filtered_data())) {
            cat("No filters applied\n")
            cat("Original data:", nrow(data()), "rows\n")
        } else {
            cat("Filters applied\n")
            cat("Filtered data:", nrow(filtered_data()), "rows\n")
            cat("Original data:", nrow(data()), "rows\n")
        }
    })

    output$filtered_data_table <- renderDT({
        display_data <- if (is.null(filtered_data())) data() else filtered_data()

        datatable(
            display_data,
            options = list(pageLength = 10, scrollX = TRUE),
            rownames = FALSE
        )
    })

    # Download Handlers
    output$download_data <- downloadHandler(
        filename = function() {
            paste("data_export_", Sys.Date(), ".csv", sep = "")
        },
        content = function(file) {
            write.csv(data(), file, row.names = FALSE)
        }
    )

    output$download_summary <- downloadHandler(
        filename = function() {
            paste("summary_", Sys.Date(), ".csv", sep = "")
        },
        content = function(file) {
            summary_stats <- calculate_summary_stats(data())
            write.csv(summary_stats, file, row.names = FALSE)
        }
    )

    output$download_filtered <- downloadHandler(
        filename = function() {
            paste("filtered_data_", Sys.Date(), ".csv", sep = "")
        },
        content = function(file) {
            display_data <- if (is.null(filtered_data())) data() else filtered_data()
            write.csv(display_data, file, row.names = FALSE)
        }
    )
}

# Run the application
shinyApp(ui = ui, server = server)
