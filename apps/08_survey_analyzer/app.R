# Survey Analyzer
# Upload, analyze, and visualize survey data

library(shiny)
library(bslib)
library(readr)
library(haven)
library(readxl)
library(dplyr)
library(ggplot2)
library(plotly)
library(DT)
library(tidyr)
library(wordcloud2)
library(scales)

# Source shared utilities
source("shared/theme.R")
source("shared/ui_components.R")
source("shared/data_utils.R")

# UI
ui <- page_sidebar(
    title = "Survey Analyzer",
    theme = app_theme,
    sidebar = sidebar(
        width = 300,
        app_header(
            "Survey Analyzer",
            "Analyze and visualize survey responses",
            gradient = get_gradient("survey")
        ),
        hr(),
        h4("Data Source"),
        radioButtons("survey_source", "Choose source:",
            choices = c("Upload File" = "upload", "Use Sample" = "sample")
        ),
        conditionalPanel(
            condition = "input.survey_source == 'upload'",
            fileInput("survey_file", "Upload survey data:",
                accept = c(".csv", ".xlsx", ".xls", ".sav"),
                buttonLabel = "Browse...",
                placeholder = "No file selected"
            )
        ),
        hr(),
        h4("Analysis Options"),
        uiOutput("question_select_ui"),
        checkboxInput("show_percentages", "Show Percentages", TRUE),
        selectInput("chart_style", "Chart Style:",
            choices = c("Bar Chart" = "bar", "Pie Chart" = "pie", "Donut Chart" = "donut")
        )
    ),
    tags$head(
        tags$style(HTML(custom_css))
    ),
    navset_card_tab(
        nav_panel(
            "Overview",
            icon = icon("chart-pie"),
            h3("Survey Overview"),
            fluidRow(
                column(3, uiOutput("total_responses_card")),
                column(3, uiOutput("avg_satisfaction_card")),
                column(3, uiOutput("nps_score_card")),
                column(3, uiOutput("completion_rate_card"))
            ),
            fluidRow(
                column(
                    6,
                    card_container(
                        title = "Response Distribution",
                        plotlyOutput("response_dist_plot", height = 300)
                    )
                ),
                column(
                    6,
                    card_container(
                        title = "Demographics",
                        plotlyOutput("demographics_plot", height = 300)
                    )
                )
            ),
            card_container(
                title = "Key Insights",
                uiOutput("key_insights")
            )
        ),
        nav_panel(
            "Question Analysis",
            icon = icon("question-circle"),
            h3("Individual Question Analysis"),
            card_container(
                title = "Question Response",
                plotlyOutput("question_plot", height = 400)
            ),
            fluidRow(
                column(
                    6,
                    card_container(
                        title = "Cross-tabulation",
                        uiOutput("crosstab_ui"),
                        DTOutput("crosstab_table")
                    )
                ),
                column(
                    6,
                    card_container(
                        title = "Frequency Table",
                        DTOutput("frequency_table")
                    )
                )
            )
        ),
        nav_panel(
            "Satisfaction Analysis",
            icon = icon("smile"),
            h3("Satisfaction & NPS Analysis"),
            fluidRow(
                column(
                    6,
                    card_container(
                        title = "Satisfaction Distribution",
                        plotlyOutput("satisfaction_dist", height = 300)
                    )
                ),
                column(
                    6,
                    card_container(
                        title = "NPS Breakdown",
                        plotlyOutput("nps_breakdown", height = 300)
                    )
                )
            ),
            card_container(
                title = "Satisfaction by Segment",
                plotlyOutput("satisfaction_segment", height = 350)
            ),
            card_container(
                title = "Recommendation Analysis",
                plotlyOutput("recommendation_plot", height = 300)
            )
        ),
        nav_panel(
            "Data Table",
            icon = icon("table"),
            h3("Survey Data"),
            card_container(
                DTOutput("survey_data_table"),
                downloadButton("download_survey", "Download Data (CSV)", class = "btn-success")
            )
        ),
        nav_panel(
            "Report",
            icon = icon("file-alt"),
            h3("Survey Report"),
            card_container(
                title = "Executive Summary",
                uiOutput("executive_summary")
            ),
            card_container(
                title = "Detailed Analysis",
                verbatimTextOutput("detailed_analysis")
            ),
            downloadButton("download_report", "Download Report (TXT)", icon = icon("file-alt"), class = "btn-primary")
        )
    )
)

# Server
server <- function(input, output, session) {
    # Load survey data
    survey_data <- reactive({
        if (input$survey_source == "sample") {
            generate_survey_data(n = 200)
        } else {
            req(input$survey_file)

            ext <- tools::file_ext(input$survey_file$name)

            tryCatch(
                {
                    if (ext == "csv") {
                        read_csv(input$survey_file$datapath, show_col_types = FALSE)
                    } else if (ext %in% c("xlsx", "xls")) {
                        read_excel(input$survey_file$datapath)
                    } else if (ext == "sav") {
                        read_sav(input$survey_file$datapath)
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

    # Calculate NPS
    nps_score <- reactive({
        req(survey_data())

        if ("nps_score" %in% names(survey_data())) {
            scores <- survey_data()$nps_score
            promoters <- sum(scores >= 9) / length(scores)
            detractors <- sum(scores <= 6) / length(scores)
            (promoters - detractors) * 100
        } else {
            NA
        }
    })

    # Overview cards
    output$total_responses_card <- renderUI({
        req(survey_data())
        metric_card("Total Responses", nrow(survey_data()), "users", "#0d6efd")
    })

    output$avg_satisfaction_card <- renderUI({
        req(survey_data())

        if ("satisfaction" %in% names(survey_data())) {
            avg <- mean(survey_data()$satisfaction, na.rm = TRUE)
            metric_card("Avg Satisfaction", paste0(round(avg, 2), "/5"), "star", "#ffc107")
        } else {
            metric_card("Avg Satisfaction", "N/A", "star", "#6c757d")
        }
    })

    output$nps_score_card <- renderUI({
        score <- nps_score()

        if (!is.na(score)) {
            color <- if (score > 50) "#198754" else if (score > 0) "#ffc107" else "#dc3545"
            metric_card("NPS Score", round(score, 1), "thumbs-up", color)
        } else {
            metric_card("NPS Score", "N/A", "thumbs-up", "#6c757d")
        }
    })

    output$completion_rate_card <- renderUI({
        req(survey_data())
        # Calculate completion based on missing values
        complete <- sum(complete.cases(survey_data())) / nrow(survey_data()) * 100
        metric_card("Completion Rate", paste0(round(complete, 1), "%"), "check-circle", "#198754")
    })

    # Response distribution
    output$response_dist_plot <- renderPlotly({
        req(survey_data())

        if ("age_group" %in% names(survey_data())) {
            age_counts <- survey_data() %>%
                count(age_group) %>%
                arrange(desc(n))

            plot_ly(age_counts,
                x = ~age_group, y = ~n, type = "bar",
                marker = list(color = "#667eea")
            ) %>%
                layout(
                    xaxis = list(title = "Age Group"),
                    yaxis = list(title = "Count"),
                    margin = list(b = 100)
                )
        }
    })

    # Demographics plot
    output$demographics_plot <- renderPlotly({
        req(survey_data())

        if ("gender" %in% names(survey_data())) {
            gender_counts <- survey_data() %>%
                count(gender)

            plot_ly(gender_counts,
                labels = ~gender, values = ~n, type = "pie",
                textinfo = "label+percent",
                marker = list(colors = c("#667eea", "#764ba2", "#f093fb", "#f5576c"))
            ) %>%
                layout(showlegend = TRUE)
        }
    })

    # Key insights
    output$key_insights <- renderUI({
        req(survey_data())

        tagList(
            h4("Summary"),
            tags$ul(
                tags$li(strong("Total Respondents:"), nrow(survey_data())),
                if ("satisfaction" %in% names(survey_data())) {
                    satisfied <- mean(survey_data()$satisfaction >= 4, na.rm = TRUE) * 100
                    tags$li(strong("Satisfied Customers:"), paste0(round(satisfied, 1), "% (rating 4-5)"))
                },
                if (!is.na(nps_score())) {
                    tags$li(
                        strong("NPS Classification:"),
                        if (nps_score() > 50) "Excellent" else if (nps_score() > 0) "Good" else "Needs Improvement"
                    )
                },
                if ("would_recommend" %in% names(survey_data())) {
                    recommend <- mean(survey_data()$would_recommend == "Yes", na.rm = TRUE) * 100
                    tags$li(strong("Would Recommend:"), paste0(round(recommend, 1), "%"))
                }
            )
        )
    })

    # Question selector
    output$question_select_ui <- renderUI({
        req(survey_data())

        # Get categorical/ordinal columns
        questions <- names(survey_data())[sapply(survey_data(), function(x) {
            is.character(x) || is.factor(x) || is.numeric(x)
        })]

        selectInput("selected_question", "Select Question:",
            choices = questions,
            selected = questions[1]
        )
    })

    # Question analysis plot
    output$question_plot <- renderPlotly({
        req(survey_data(), input$selected_question)

        data <- survey_data()
        question <- input$selected_question

        if (is.numeric(data[[question]])) {
            # For numeric questions, show histogram
            plot_ly(
                x = data[[question]], type = "histogram",
                marker = list(color = "#667eea"), nbinsx = 20
            ) %>%
                layout(
                    xaxis = list(title = question),
                    yaxis = list(title = "Frequency")
                )
        } else {
            # For categorical, show bar chart
            counts <- data %>%
                count(!!sym(question)) %>%
                arrange(desc(n))

            if (input$chart_style == "bar") {
                plot_ly(counts,
                    x = ~ get(question), y = ~n, type = "bar",
                    marker = list(color = "#667eea")
                ) %>%
                    layout(
                        xaxis = list(title = question),
                        yaxis = list(title = "Count"),
                        margin = list(b = 100)
                    )
            } else {
                plot_ly(counts,
                    labels = ~ get(question), values = ~n,
                    type = "pie",
                    hole = if (input$chart_style == "donut") 0.4 else 0,
                    textinfo = "label+percent"
                ) %>%
                    layout(showlegend = TRUE)
            }
        }
    })

    # Frequency table
    output$frequency_table <- renderDT({
        req(survey_data(), input$selected_question)

        freq <- survey_data() %>%
            count(!!sym(input$selected_question)) %>%
            arrange(desc(n)) %>%
            mutate(
                Percentage = round(n / sum(n) * 100, 1)
            )

        names(freq)[1] <- input$selected_question

        datatable(freq, options = list(pageLength = 10), rownames = FALSE)
    })

    # Crosstab UI
    output$crosstab_ui <- renderUI({
        req(survey_data())

        cat_cols <- names(survey_data())[sapply(survey_data(), function(x) {
            is.character(x) || is.factor(x)
        })]

        if (length(cat_cols) >= 2) {
            selectInput("crosstab_var", "Cross-tabulate with:",
                choices = cat_cols[cat_cols != input$selected_question]
            )
        } else {
            p("Not enough categorical variables for cross-tabulation")
        }
    })

    # Crosstab table
    output$crosstab_table <- renderDT({
        req(survey_data(), input$selected_question, input$crosstab_var)

        ct <- survey_data() %>%
            count(!!sym(input$selected_question), !!sym(input$crosstab_var)) %>%
            pivot_wider(names_from = !!sym(input$crosstab_var), values_from = n, values_fill = 0)

        datatable(ct, options = list(pageLength = 10, scrollX = TRUE), rownames = FALSE)
    })

    # Satisfaction distribution
    output$satisfaction_dist <- renderPlotly({
        req(survey_data())

        if ("satisfaction" %in% names(survey_data())) {
            counts <- survey_data() %>%
                count(satisfaction) %>%
                mutate(satisfaction = factor(satisfaction, levels = 1:5))

            plot_ly(counts,
                x = ~satisfaction, y = ~n, type = "bar",
                marker = list(color = c("#dc3545", "#ffc107", "#6c757d", "#0dcaf0", "#198754"))
            ) %>%
                layout(
                    xaxis = list(title = "Satisfaction Rating"),
                    yaxis = list(title = "Count")
                )
        }
    })

    # NPS breakdown
    output$nps_breakdown <- renderPlotly({
        req(survey_data())

        if ("nps_score" %in% names(survey_data())) {
            nps_data <- survey_data() %>%
                mutate(
                    nps_category = case_when(
                        nps_score >= 9 ~ "Promoters",
                        nps_score >= 7 ~ "Passives",
                        TRUE ~ "Detractors"
                    )
                ) %>%
                count(nps_category)

            plot_ly(nps_data,
                labels = ~nps_category, values = ~n,
                type = "pie",
                marker = list(colors = c("#dc3545", "#ffc107", "#198754")),
                textinfo = "label+percent"
            ) %>%
                layout(title = "NPS Categories")
        }
    })

    # Satisfaction by segment
    output$satisfaction_segment <- renderPlotly({
        req(survey_data())

        if ("satisfaction" %in% names(survey_data()) && "age_group" %in% names(survey_data())) {
            segment_data <- survey_data() %>%
                group_by(age_group) %>%
                summarise(avg_satisfaction = mean(satisfaction, na.rm = TRUE), .groups = "drop")

            plot_ly(segment_data,
                x = ~age_group, y = ~avg_satisfaction,
                type = "bar", marker = list(color = "#4facfe")
            ) %>%
                layout(
                    xaxis = list(title = "Age Group"),
                    yaxis = list(title = "Average Satisfaction", range = c(0, 5)),
                    margin = list(b = 100)
                )
        }
    })

    # Recommendation plot
    output$recommendation_plot <- renderPlotly({
        req(survey_data())

        if ("would_recommend" %in% names(survey_data())) {
            rec_counts <- survey_data() %>%
                count(would_recommend)

            plot_ly(rec_counts,
                x = ~would_recommend, y = ~n, type = "bar",
                marker = list(color = c("#ffc107", "#dc3545", "#198754"))
            ) %>%
                layout(
                    xaxis = list(title = "Would Recommend"),
                    yaxis = list(title = "Count")
                )
        }
    })

    # Survey data table
    output$survey_data_table <- renderDT({
        req(survey_data())

        datatable(
            survey_data(),
            options = list(pageLength = 10, scrollX = TRUE),
            rownames = FALSE
        )
    })

    # Download handler
    output$download_survey <- downloadHandler(
        filename = function() {
            paste("survey_data_", Sys.Date(), ".csv", sep = "")
        },
        content = function(file) {
            write.csv(survey_data(), file, row.names = FALSE)
        }
    )

    # Executive summary
    output$executive_summary <- renderUI({
        req(survey_data())

        tagList(
            h4("Survey Analysis Report"),
            p(strong("Report Date:"), format(Sys.Date(), "%B %d, %Y")),
            p(strong("Total Responses:"), nrow(survey_data())),
            hr(),
            h5("Key Findings:"),
            tags$ol(
                if ("satisfaction" %in% names(survey_data())) {
                    avg_sat <- mean(survey_data()$satisfaction, na.rm = TRUE)
                    tags$li(sprintf("Average satisfaction rating is %.2f out of 5", avg_sat))
                },
                if (!is.na(nps_score())) {
                    tags$li(sprintf("Net Promoter Score (NPS) is %.1f", nps_score()))
                },
                if ("would_recommend" %in% names(survey_data())) {
                    rec_pct <- mean(survey_data()$would_recommend == "Yes", na.rm = TRUE) * 100
                    tags$li(sprintf("%.1f%% of respondents would recommend", rec_pct))
                }
            )
        )
    })

    # Detailed analysis
    output$detailed_analysis <- renderPrint({
        req(survey_data())

        cat("Survey Data Summary\n")
        cat("===================\n\n")

        cat("Dimensions:", nrow(survey_data()), "responses x", ncol(survey_data()), "variables\n\n")

        cat("Variable Summary:\n")
        print(summary(survey_data()))
    })

    # Download report handler
    output$download_report <- downloadHandler(
        filename = function() {
            paste("survey_report_", Sys.Date(), ".txt", sep = "")
        },
        content = function(file) {
            req(survey_data())

            # Create comprehensive text report
            report <- c(
                "========================================",
                "       SURVEY ANALYSIS REPORT",
                "========================================",
                "",
                paste("Report Generated:", format(Sys.time(), "%Y-%m-%d %H:%M:%S")),
                paste("Total Responses:", nrow(survey_data())),
                "",
                "========================================",
                "EXECUTIVE SUMMARY",
                "========================================",
                ""
            )

            # Add satisfaction metrics
            if ("satisfaction" %in% names(survey_data())) {
                avg_sat <- mean(survey_data()$satisfaction, na.rm = TRUE)
                satisfied_pct <- mean(survey_data()$satisfaction >= 4, na.rm = TRUE) * 100
                report <- c(
                    report,
                    paste("Average Satisfaction:", round(avg_sat, 2), "out of 5"),
                    paste("Satisfied Customers:", round(satisfied_pct, 1), "% (rating 4-5)"),
                    ""
                )
            }

            # Add NPS
            if (!is.na(nps_score())) {
                nps_val <- nps_score()
                nps_class <- if (nps_val > 50) "Excellent" else if (nps_val > 0) "Good" else "Needs Improvement"
                report <- c(
                    report,
                    paste("Net Promoter Score (NPS):", round(nps_val, 1)),
                    paste("NPS Classification:", nps_class),
                    ""
                )
            }

            # Add recommendation
            if ("would_recommend" %in% names(survey_data())) {
                rec_pct <- mean(survey_data()$would_recommend == "Yes", na.rm = TRUE) * 100
                report <- c(
                    report,
                    paste("Would Recommend:", round(rec_pct, 1), "%"),
                    ""
                )
            }

            # Add completion rate
            complete_pct <- sum(complete.cases(survey_data())) / nrow(survey_data()) * 100
            report <- c(
                report,
                paste("Survey Completion Rate:", round(complete_pct, 1), "%"),
                "",
                "========================================",
                "DETAILED STATISTICS",
                "========================================",
                ""
            )

            # Add data dimensions
            report <- c(
                report,
                paste("Dimensions:", nrow(survey_data()), "responses x", ncol(survey_data()), "variables"),
                "",
                "Variable Summary:",
                "----------------"
            )

            # Add summary statistics
            summary_text <- capture.output(print(summary(survey_data())))
            report <- c(report, summary_text, "")

            # Add demographics if available
            if ("age_group" %in% names(survey_data())) {
                report <- c(
                    report,
                    "========================================",
                    "DEMOGRAPHICS - AGE DISTRIBUTION",
                    "========================================",
                    ""
                )
                age_dist <- table(survey_data()$age_group)
                for (i in seq_along(age_dist)) {
                    pct <- age_dist[i] / sum(age_dist) * 100
                    report <- c(
                        report,
                        sprintf("  %s: %d (%.1f%%)", names(age_dist)[i], age_dist[i], pct)
                    )
                }
                report <- c(report, "")
            }

            if ("gender" %in% names(survey_data())) {
                report <- c(
                    report,
                    "========================================",
                    "DEMOGRAPHICS - GENDER DISTRIBUTION",
                    "========================================",
                    ""
                )
                gender_dist <- table(survey_data()$gender)
                for (i in seq_along(gender_dist)) {
                    pct <- gender_dist[i] / sum(gender_dist) * 100
                    report <- c(
                        report,
                        sprintf("  %s: %d (%.1f%%)", names(gender_dist)[i], gender_dist[i], pct)
                    )
                }
                report <- c(report, "")
            }

            # Footer
            report <- c(
                report,
                "========================================",
                "END OF REPORT",
                "========================================"
            )

            # Write to file
            writeLines(report, file)
        }
    )
}

# Run the application
shinyApp(ui = ui, server = server)
