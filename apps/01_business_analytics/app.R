# Business Analytics Dashboard
# Real-time dashboard with sales performance, KPIs, and customer churn analysis

library(shiny)
library(shinydashboard)
library(plotly)
library(DT)
library(dplyr)
library(lubridate)
library(scales)

# Source shared utilities
source("shared/theme.R")
source("shared/ui_components.R")
source("shared/data_utils.R")

# Generate sample data
sales_data <- generate_sales_data(n = 1000)
churn_data <- generate_churn_data(n = 500)

# UI
ui <- dashboardPage(
    skin = "blue",
    dashboardHeader(title = "Business Analytics Dashboard"),
    dashboardSidebar(
        sidebarMenu(
            menuItem("Sales Performance", tabName = "sales", icon = icon("chart-line")),
            menuItem("Customer Churn", tabName = "churn", icon = icon("users")),
            menuItem("KPI Monitoring", tabName = "kpi", icon = icon("tachometer-alt"))
        ),
        hr(),

        # Filters
        h4("Filters", style = "padding-left: 15px;"),
        dateRangeInput(
            "date_range",
            "Date Range:",
            start = min(sales_data$date),
            end = max(sales_data$date),
            min = min(sales_data$date),
            max = max(sales_data$date)
        ),
        selectInput(
            "region_filter",
            "Region:",
            choices = c("All", unique(sales_data$region)),
            selected = "All"
        ),
        selectInput(
            "category_filter",
            "Category:",
            choices = c("All", unique(sales_data$category)),
            selected = "All"
        ),
        actionButton("reset_filters", "Reset Filters",
            icon = icon("redo"),
            class = "btn-warning", style = "margin: 15px;"
        )
    ),
    dashboardBody(
        tags$head(
            tags$style(HTML(custom_css))
        ),
        tabItems(
            # Sales Performance Tab
            tabItem(
                tabName = "sales",
                h2("Sales Performance Dashboard"),

                # KPI Boxes
                fluidRow(
                    valueBoxOutput("total_sales_box", width = 3),
                    valueBoxOutput("total_profit_box", width = 3),
                    valueBoxOutput("avg_margin_box", width = 3),
                    valueBoxOutput("total_orders_box", width = 3)
                ),

                # Charts Row 1
                fluidRow(
                    box(
                        title = "Sales Trend Over Time",
                        status = "primary",
                        solidHeader = TRUE,
                        width = 8,
                        plotlyOutput("sales_trend_plot", height = 300)
                    ),
                    box(
                        title = "Sales by Category",
                        status = "info",
                        solidHeader = TRUE,
                        width = 4,
                        plotlyOutput("category_pie_plot", height = 300)
                    )
                ),

                # Charts Row 2
                fluidRow(
                    box(
                        title = "Top Products by Sales",
                        status = "success",
                        solidHeader = TRUE,
                        width = 6,
                        plotlyOutput("top_products_plot", height = 300)
                    ),
                    box(
                        title = "Regional Performance",
                        status = "warning",
                        solidHeader = TRUE,
                        width = 6,
                        plotlyOutput("regional_plot", height = 300)
                    )
                ),

                # Data Table
                fluidRow(
                    box(
                        title = "Detailed Sales Data",
                        status = "primary",
                        solidHeader = TRUE,
                        width = 12,
                        DTOutput("sales_table"),
                        downloadButton("download_sales", "Download Data", class = "btn-success")
                    )
                )
            ),

            # Customer Churn Tab
            tabItem(
                tabName = "churn",
                h2("Customer Churn Analysis"),
                fluidRow(
                    valueBoxOutput("churn_rate_box", width = 3),
                    valueBoxOutput("avg_tenure_box", width = 3),
                    valueBoxOutput("avg_charges_box", width = 3),
                    valueBoxOutput("total_customers_box", width = 3)
                ),
                fluidRow(
                    box(
                        title = "Churn by Contract Type",
                        status = "danger",
                        solidHeader = TRUE,
                        width = 6,
                        plotlyOutput("churn_contract_plot", height = 300)
                    ),
                    box(
                        title = "Churn by Payment Method",
                        status = "warning",
                        solidHeader = TRUE,
                        width = 6,
                        plotlyOutput("churn_payment_plot", height = 300)
                    )
                ),
                fluidRow(
                    box(
                        title = "Tenure vs Monthly Charges (Churn Analysis)",
                        status = "info",
                        solidHeader = TRUE,
                        width = 12,
                        plotlyOutput("churn_scatter_plot", height = 400)
                    )
                ),
                fluidRow(
                    box(
                        title = "Customer Data",
                        status = "primary",
                        solidHeader = TRUE,
                        width = 12,
                        DTOutput("churn_table")
                    )
                )
            ),

            # KPI Monitoring Tab
            tabItem(
                tabName = "kpi",
                h2("Key Performance Indicators"),
                fluidRow(
                    column(
                        width = 6,
                        card_container(
                            title = "Financial Metrics",
                            metric_card("Total Revenue", textOutput("kpi_revenue", inline = TRUE), "dollar-sign", "#28a745"),
                            metric_card("Total Profit", textOutput("kpi_profit", inline = TRUE), "chart-line", "#17a2b8"),
                            metric_card("Profit Margin", textOutput("kpi_margin", inline = TRUE), "percentage", "#ffc107")
                        )
                    ),
                    column(
                        width = 6,
                        card_container(
                            title = "Operational Metrics",
                            metric_card("Total Orders", textOutput("kpi_orders", inline = TRUE), "shopping-cart", "#6f42c1"),
                            metric_card("Avg Order Value", textOutput("kpi_aov", inline = TRUE), "money-bill", "#fd7e14"),
                            metric_card("Items Sold", textOutput("kpi_items", inline = TRUE), "boxes", "#e83e8c")
                        )
                    )
                ),
                fluidRow(
                    box(
                        title = "Monthly Performance Trends",
                        status = "primary",
                        solidHeader = TRUE,
                        width = 12,
                        plotlyOutput("kpi_trend_plot", height = 400)
                    )
                ),
                fluidRow(
                    box(
                        title = "Performance Heatmap",
                        status = "info",
                        solidHeader = TRUE,
                        width = 12,
                        plotlyOutput("performance_heatmap", height = 400)
                    )
                )
            )
        )
    )
)

# Server
server <- function(input, output, session) {
    # Reactive filtered data
    filtered_sales <- reactive({
        data <- sales_data %>%
            filter(date >= input$date_range[1] & date <= input$date_range[2])

        if (input$region_filter != "All") {
            data <- data %>% filter(region == input$region_filter)
        }

        if (input$category_filter != "All") {
            data <- data %>% filter(category == input$category_filter)
        }

        data
    })

    # Reset filters
    observeEvent(input$reset_filters, {
        updateDateRangeInput(session, "date_range",
            start = min(sales_data$date),
            end = max(sales_data$date)
        )
        updateSelectInput(session, "region_filter", selected = "All")
        updateSelectInput(session, "category_filter", selected = "All")
    })

    # Sales Performance Value Boxes
    output$total_sales_box <- renderValueBox({
        total <- sum(filtered_sales()$sales)
        valueBox(
            format_currency(total),
            "Total Sales",
            icon = icon("dollar-sign"),
            color = "green"
        )
    })

    output$total_profit_box <- renderValueBox({
        total <- sum(filtered_sales()$profit)
        valueBox(
            format_currency(total),
            "Total Profit",
            icon = icon("chart-line"),
            color = "blue"
        )
    })

    output$avg_margin_box <- renderValueBox({
        avg <- mean(filtered_sales()$profit_margin, na.rm = TRUE)
        valueBox(
            format_percentage(avg),
            "Avg Profit Margin",
            icon = icon("percentage"),
            color = "yellow"
        )
    })

    output$total_orders_box <- renderValueBox({
        total <- nrow(filtered_sales())
        valueBox(
            format_number(total),
            "Total Orders",
            icon = icon("shopping-cart"),
            color = "purple"
        )
    })

    # Sales Trend Plot
    output$sales_trend_plot <- renderPlotly({
        trend_data <- filtered_sales() %>%
            mutate(month = floor_date(date, "month")) %>%
            group_by(month) %>%
            summarise(
                sales = sum(sales),
                profit = sum(profit),
                .groups = "drop"
            )

        plot_ly(trend_data, x = ~month) %>%
            add_trace(
                y = ~sales, name = "Sales", type = "scatter", mode = "lines+markers",
                line = list(color = "#0d6efd", width = 3),
                marker = list(size = 8)
            ) %>%
            add_trace(
                y = ~profit, name = "Profit", type = "scatter", mode = "lines+markers",
                line = list(color = "#198754", width = 3),
                marker = list(size = 8)
            ) %>%
            layout(
                xaxis = list(title = "Month"),
                yaxis = list(title = "Amount ($)"),
                hovermode = "x unified",
                legend = list(orientation = "h", y = -0.2)
            )
    })

    # Category Pie Chart
    output$category_pie_plot <- renderPlotly({
        cat_data <- filtered_sales() %>%
            group_by(category) %>%
            summarise(sales = sum(sales), .groups = "drop")

        plot_ly(cat_data,
            labels = ~category, values = ~sales, type = "pie",
            textinfo = "label+percent",
            marker = list(colors = c("#667eea", "#764ba2", "#f093fb", "#f5576c", "#4facfe"))
        ) %>%
            layout(showlegend = TRUE)
    })

    # Top Products Bar Chart
    output$top_products_plot <- renderPlotly({
        top_products <- filtered_sales() %>%
            group_by(product) %>%
            summarise(sales = sum(sales), .groups = "drop") %>%
            arrange(desc(sales)) %>%
            head(10)

        plot_ly(top_products,
            x = ~ reorder(product, sales), y = ~sales,
            type = "bar", marker = list(color = "#667eea")
        ) %>%
            layout(
                xaxis = list(title = "Product"),
                yaxis = list(title = "Total Sales ($)"),
                margin = list(b = 100)
            )
    })

    # Regional Performance
    output$regional_plot <- renderPlotly({
        regional_data <- filtered_sales() %>%
            group_by(region) %>%
            summarise(
                sales = sum(sales),
                profit = sum(profit),
                .groups = "drop"
            )

        plot_ly(regional_data,
            x = ~region, y = ~sales, name = "Sales",
            type = "bar", marker = list(color = "#0d6efd")
        ) %>%
            add_trace(y = ~profit, name = "Profit", marker = list(color = "#198754")) %>%
            layout(
                xaxis = list(title = "Region"),
                yaxis = list(title = "Amount ($)"),
                barmode = "group"
            )
    })

    # Sales Table
    output$sales_table <- renderDT({
        datatable(
            filtered_sales() %>%
                select(date, product, category, region, sales, quantity, profit, profit_margin) %>%
                mutate(
                    sales = format_currency(sales),
                    profit = format_currency(profit),
                    profit_margin = format_percentage(profit_margin)
                ),
            options = list(pageLength = 10, scrollX = TRUE),
            rownames = FALSE
        )
    })

    # Download handler
    output$download_sales <- downloadHandler(
        filename = function() {
            paste("sales_data_", Sys.Date(), ".csv", sep = "")
        },
        content = function(file) {
            write.csv(filtered_sales(), file, row.names = FALSE)
        }
    )

    # Churn Analysis Value Boxes
    output$churn_rate_box <- renderValueBox({
        rate <- mean(churn_data$churn) * 100
        valueBox(
            format_percentage(rate),
            "Churn Rate",
            icon = icon("user-times"),
            color = "red"
        )
    })

    output$avg_tenure_box <- renderValueBox({
        avg <- mean(churn_data$tenure_months)
        valueBox(
            paste(round(avg, 1), "months"),
            "Avg Tenure",
            icon = icon("calendar"),
            color = "blue"
        )
    })

    output$avg_charges_box <- renderValueBox({
        avg <- mean(churn_data$monthly_charges)
        valueBox(
            format_currency(avg),
            "Avg Monthly Charge",
            icon = icon("dollar-sign"),
            color = "green"
        )
    })

    output$total_customers_box <- renderValueBox({
        total <- nrow(churn_data)
        valueBox(
            format_number(total),
            "Total Customers",
            icon = icon("users"),
            color = "purple"
        )
    })

    # Churn by Contract Type
    output$churn_contract_plot <- renderPlotly({
        churn_contract <- churn_data %>%
            group_by(contract_type, churn) %>%
            summarise(count = n(), .groups = "drop") %>%
            mutate(churn_label = ifelse(churn == 1, "Churned", "Retained"))

        plot_ly(churn_contract,
            x = ~contract_type, y = ~count, color = ~churn_label,
            type = "bar", colors = c("#198754", "#dc3545")
        ) %>%
            layout(
                xaxis = list(title = "Contract Type"),
                yaxis = list(title = "Number of Customers"),
                barmode = "stack"
            )
    })

    # Churn by Payment Method
    output$churn_payment_plot <- renderPlotly({
        churn_payment <- churn_data %>%
            group_by(payment_method) %>%
            summarise(churn_rate = mean(churn) * 100, .groups = "drop") %>%
            arrange(desc(churn_rate))

        plot_ly(churn_payment,
            x = ~ reorder(payment_method, churn_rate), y = ~churn_rate,
            type = "bar", marker = list(color = "#ffc107")
        ) %>%
            layout(
                xaxis = list(title = "Payment Method"),
                yaxis = list(title = "Churn Rate (%)"),
                margin = list(b = 100)
            )
    })

    # Churn Scatter Plot
    output$churn_scatter_plot <- renderPlotly({
        plot_data <- churn_data %>%
            mutate(churn_label = ifelse(churn == 1, "Churned", "Retained"))

        plot_ly(plot_data,
            x = ~tenure_months, y = ~monthly_charges, color = ~churn_label,
            type = "scatter", mode = "markers",
            colors = c("#198754", "#dc3545"),
            marker = list(size = 8, opacity = 0.6)
        ) %>%
            layout(
                xaxis = list(title = "Tenure (months)"),
                yaxis = list(title = "Monthly Charges ($)"),
                hovermode = "closest"
            )
    })

    # Churn Table
    output$churn_table <- renderDT({
        datatable(
            churn_data %>%
                mutate(
                    churn = ifelse(churn == 1, "Yes", "No"),
                    monthly_charges = format_currency(monthly_charges),
                    total_charges = format_currency(total_charges)
                ),
            options = list(pageLength = 10, scrollX = TRUE),
            rownames = FALSE
        )
    })

    # KPI Outputs
    output$kpi_revenue <- renderText({
        format_currency(sum(filtered_sales()$sales))
    })

    output$kpi_profit <- renderText({
        format_currency(sum(filtered_sales()$profit))
    })

    output$kpi_margin <- renderText({
        format_percentage(mean(filtered_sales()$profit_margin, na.rm = TRUE))
    })

    output$kpi_orders <- renderText({
        format_number(nrow(filtered_sales()))
    })

    output$kpi_aov <- renderText({
        format_currency(mean(filtered_sales()$sales))
    })

    output$kpi_items <- renderText({
        format_number(sum(filtered_sales()$quantity))
    })

    # KPI Trend Plot
    output$kpi_trend_plot <- renderPlotly({
        monthly_kpis <- filtered_sales() %>%
            mutate(month = floor_date(date, "month")) %>%
            group_by(month) %>%
            summarise(
                revenue = sum(sales),
                profit = sum(profit),
                orders = n(),
                items = sum(quantity),
                .groups = "drop"
            )

        plot_ly(monthly_kpis, x = ~month) %>%
            add_trace(
                y = ~revenue, name = "Revenue", type = "scatter", mode = "lines+markers",
                yaxis = "y", line = list(color = "#0d6efd")
            ) %>%
            add_trace(
                y = ~profit, name = "Profit", type = "scatter", mode = "lines+markers",
                yaxis = "y", line = list(color = "#198754")
            ) %>%
            add_trace(
                y = ~orders, name = "Orders", type = "scatter", mode = "lines+markers",
                yaxis = "y2", line = list(color = "#ffc107")
            ) %>%
            layout(
                xaxis = list(title = "Month"),
                yaxis = list(title = "Revenue / Profit ($)", side = "left"),
                yaxis2 = list(title = "Orders", overlaying = "y", side = "right"),
                hovermode = "x unified"
            )
    })

    # Performance Heatmap
    output$performance_heatmap <- renderPlotly({
        heatmap_data <- filtered_sales() %>%
            mutate(
                month = format(date, "%b"),
                week = week(date)
            ) %>%
            group_by(month, region) %>%
            summarise(sales = sum(sales), .groups = "drop")

        heatmap_matrix <- heatmap_data %>%
            tidyr::pivot_wider(names_from = region, values_from = sales, values_fill = 0)

        plot_ly(
            x = colnames(heatmap_matrix)[-1],
            y = heatmap_matrix$month,
            z = as.matrix(heatmap_matrix[, -1]),
            type = "heatmap",
            colorscale = "Blues"
        ) %>%
            layout(
                xaxis = list(title = "Region"),
                yaxis = list(title = "Month")
            )
    })
}

# Run the application
shinyApp(ui = ui, server = server)
