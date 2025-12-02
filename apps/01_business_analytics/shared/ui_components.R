# Shared UI Components for Shiny Applications
# Reusable components for consistent design

library(shiny)
library(shinyWidgets)

# Metric Card Component
metric_card <- function(label, value, icon = NULL, color = "#0d6efd") {
    div(
        class = "metric-card",
        style = sprintf("border-left-color: %s;", color),
        if (!is.null(icon)) {
            div(
                style = "float: right; font-size: 2rem; opacity: 0.3;",
                icon(icon)
            )
        },
        div(class = "metric-label", label),
        div(class = "metric-value", value)
    )
}

# Info Box with Icon
info_box <- function(title, subtitle = NULL, icon = NULL, gradient = NULL) {
    style_attr <- if (!is.null(gradient)) {
        sprintf("background: %s;", gradient)
    } else {
        ""
    }

    div(
        class = "info-card",
        style = style_attr,
        if (!is.null(icon)) {
            div(
                style = "float: right; font-size: 3rem; opacity: 0.4;",
                icon(icon)
            )
        },
        h3(style = "margin-top: 0;", title),
        if (!is.null(subtitle)) p(subtitle)
    )
}

# App Header Component
app_header <- function(title, subtitle = NULL, gradient = NULL) {
    style_attr <- if (!is.null(gradient)) {
        sprintf("background: %s;", gradient)
    } else {
        ""
    }

    div(
        class = "app-header",
        style = style_attr,
        h1(class = "app-title", title),
        if (!is.null(subtitle)) {
            p(class = "app-subtitle", subtitle)
        }
    )
}

# Plot Container with Title
plot_container <- function(plot_output, title = NULL) {
    div(
        class = "plot-container",
        if (!is.null(title)) {
            h4(style = "margin-top: 0; color: #495057;", title)
        },
        plot_output
    )
}

# Stats Grid Layout (for multiple metrics)
stats_grid <- function(..., cols = 3) {
    items <- list(...)

    fluidRow(
        lapply(items, function(item) {
            column(12 / cols, item)
        })
    )
}

# Custom Action Button
custom_button <- function(inputId, label, icon = NULL, color = "primary") {
    actionButton(
        inputId,
        label,
        icon = icon,
        class = sprintf("btn-custom btn-%s", color)
    )
}

# Download Button Styled
styled_download_button <- function(outputId, label, icon = icon("download")) {
    downloadButton(
        outputId,
        label,
        class = "btn-custom btn-success",
        icon = icon
    )
}

# File Upload Area
file_upload_area <- function(inputId, label = "Choose File", accept = NULL) {
    div(
        style = "background: #f8f9fa; border: 2px dashed #dee2e6; border-radius: 12px; padding: 30px; text-align: center; margin: 20px 0;",
        icon("cloud-upload", style = "font-size: 3rem; color: #6c757d; margin-bottom: 15px;"),
        fileInput(
            inputId,
            label,
            accept = accept,
            buttonLabel = "Browse",
            placeholder = "No file selected"
        )
    )
}

# Loading Spinner
loading_spinner <- function() {
    div(
        class = "loading-spinner"
    )
}

# Empty State Message
empty_state <- function(message, icon = "inbox") {
    div(
        style = "text-align: center; padding: 60px 20px; color: #6c757d;",
        icon(icon, style = "font-size: 4rem; opacity: 0.3; margin-bottom: 20px;"),
        h4(message),
        p("No data to display")
    )
}

# Alert Box
alert_box <- function(message, type = "info") {
    div(
        class = sprintf("alert alert-%s", type),
        role = "alert",
        message
    )
}

# Card Container
card_container <- function(..., title = NULL) {
    div(
        style = "background: white; border-radius: 12px; padding: 24px; box-shadow: 0 2px 8px rgba(0,0,0,0.08); margin: 15px 0;",
        if (!is.null(title)) {
            h4(style = "margin-top: 0; padding-bottom: 15px; border-bottom: 2px solid #f0f0f0;", title)
        },
        ...
    )
}

# Sidebar with Custom Styling
styled_sidebar <- function(...) {
    sidebarPanel(
        class = "well",
        style = "background: #f8f9fa; border-radius: 12px;",
        ...
    )
}

# Main Panel with Custom Styling
styled_main_panel <- function(...) {
    mainPanel(
        style = "padding: 0 20px;",
        ...
    )
}

# Tab Panel with Icon
tab_with_icon <- function(title, ..., icon = NULL) {
    tab_title <- if (!is.null(icon)) {
        tagList(icon(icon), " ", title)
    } else {
        title
    }

    tabPanel(tab_title, ...)
}

# Value Box (similar to shinydashboard)
value_box <- function(value, subtitle, icon = NULL, color = "#0d6efd", width = 4) {
    column(
        width = width,
        div(
            style = sprintf(
                "background: %s; color: white; border-radius: 12px; padding: 25px; margin: 10px 0; box-shadow: 0 4px 6px rgba(0,0,0,0.1);",
                color
            ),
            if (!is.null(icon)) {
                div(
                    style = "float: right; font-size: 2.5rem; opacity: 0.5;",
                    icon(icon)
                )
            },
            div(
                style = "font-size: 2rem; font-weight: 700; margin-bottom: 5px;",
                value
            ),
            div(
                style = "font-size: 0.9rem; opacity: 0.9;",
                subtitle
            )
        )
    )
}
