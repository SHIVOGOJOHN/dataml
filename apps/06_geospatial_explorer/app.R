# Geospatial Explorer
# Interactive mapping and geographic visualization with leaflet

library(shiny)
library(bslib)
library(leaflet)
library(sf)
library(dplyr)
library(plotly)
library(viridis)
library(DT)

# Source shared utilities
source("shared/theme.R")
source("shared/ui_components.R")
source("shared/data_utils.R")

# UI
ui <- page_sidebar(
    title = "Geospatial Explorer",
    theme = app_theme,
    sidebar = sidebar(
        width = 300,
        app_header(
            "Geospatial Explorer",
            "Interactive maps and geographic analysis",
            gradient = get_gradient("geo")
        ),
        hr(),
        h4("Map Settings"),
        selectInput("map_type", "Map Type:",
            choices = c(
                "Points" = "points",
                "Heatmap" = "heatmap",
                "Choropleth" = "choropleth",
                "Clusters" = "clusters"
            )
        ),
        sliderInput("n_points", "Number of Points:", min = 10, max = 500, value = 100),
        selectInput("color_scheme", "Color Scheme:",
            choices = c(
                "Viridis" = "viridis",
                "Plasma" = "plasma",
                "Inferno" = "inferno",
                "Blues" = "Blues",
                "Reds" = "Reds"
            )
        ),
        hr(),
        h4("Filters"),
        uiOutput("value_filter_ui"),
        actionButton("generate_data", "Generate New Data", icon = icon("sync"), class = "btn-primary"),
        actionButton("reset_view", "Reset Map View", icon = icon("redo"), class = "btn-warning")
    ),
    tags$head(
        tags$style(HTML(custom_css))
    ),
    navset_card_tab(
        nav_panel(
            "Map",
            icon = icon("map"),
            h3("Interactive Map"),
            fluidRow(
                column(4, uiOutput("total_points_card")),
                column(4, uiOutput("avg_value_card")),
                column(4, uiOutput("area_card"))
            ),
            card_container(
                leafletOutput("main_map", height = 600)
            )
        ),
        nav_panel(
            "Analysis",
            icon = icon("chart-area"),
            h3("Spatial Analysis"),
            fluidRow(
                column(
                    6,
                    card_container(
                        title = "Value Distribution by Region",
                        plotlyOutput("region_plot", height = 350)
                    )
                ),
                column(
                    6,
                    card_container(
                        title = "Spatial Density",
                        plotlyOutput("density_plot", height = 350)
                    )
                )
            ),
            card_container(
                title = "Data Summary",
                DTOutput("geo_table")
            )
        ),
        nav_panel(
            "Info",
            icon = icon("info-circle"),
            h3("About This Application"),
            card_container(
                title = "Geospatial Features",
                tagList(
                    h4("Interactive Mapping"),
                    p("Explore geographic data with interactive markers, popups, and zoom controls."),
                    h4("Visualization Types"),
                    tags$ul(
                        tags$li(strong("Points:"), "Individual markers with values"),
                        tags$li(strong("Heatmap:"), "Density visualization"),
                        tags$li(strong("Choropleth:"), "Regional color-coded maps"),
                        tags$li(strong("Clusters:"), "Grouped markers for large datasets")
                    ),
                    h4("Use Cases"),
                    tags$ul(
                        tags$li("Disease spread tracking"),
                        tags$li("Property price visualization"),
                        tags$li("Customer location analysis"),
                        tags$li("Environmental monitoring")
                    )
                )
            )
        )
    )
)

# Server
server <- function(input, output, session) {
    # Generate geospatial data
    geo_data <- reactive({
        input$generate_data # Trigger regeneration

        set.seed(sample.int(10000, 1))
        generate_geo_data(n = input$n_points)
    })

    # Filter UI
    output$value_filter_ui <- renderUI({
        req(geo_data())
        sliderInput("value_filter", "Filter by Value:",
            min = floor(min(geo_data()$value)),
            max = ceiling(max(geo_data()$value)),
            value = c(floor(min(geo_data()$value)), ceiling(max(geo_data()$value)))
        )
    })

    # Filtered data
    filtered_data <- reactive({
        req(geo_data(), input$value_filter)
        geo_data() %>%
            filter(value >= input$value_filter[1] & value <= input$value_filter[2])
    })

    # Info cards
    output$total_points_card <- renderUI({
        req(filtered_data())
        metric_card("Total Points", nrow(filtered_data()), "map-marker-alt", "#0d6efd")
    })

    output$avg_value_card <- renderUI({
        req(filtered_data())
        avg <- mean(filtered_data()$value)
        metric_card("Average Value", round(avg, 1), "chart-line", "#198754")
    })

    output$area_card <- renderUI({
        req(filtered_data())
        # Approximate area covered
        lat_range <- diff(range(filtered_data()$latitude))
        lon_range <- diff(range(filtered_data()$longitude))
        area <- round(lat_range * lon_range, 1)
        metric_card("Coverage Area", paste0(area, "°²"), "expand", "#ffc107")
    })

    # Main map
    output$main_map <- renderLeaflet({
        req(filtered_data())

        data <- filtered_data()

        # Color palette
        pal <- colorNumeric(
            palette = input$color_scheme,
            domain = data$value
        )

        # Base map
        map <- leaflet(data) %>%
            addProviderTiles(providers$CartoDB.Positron) %>%
            fitBounds(
                lng1 = min(data$longitude), lat1 = min(data$latitude),
                lng2 = max(data$longitude), lat2 = max(data$latitude)
            )

        # Add layers based on map type
        if (input$map_type == "points") {
            map <- map %>%
                addCircleMarkers(
                    lng = ~longitude, lat = ~latitude,
                    radius = 8,
                    color = ~ pal(value),
                    fillOpacity = 0.7,
                    stroke = TRUE,
                    weight = 1,
                    popup = ~ paste0(
                        "<strong>", label, "</strong><br/>",
                        "Value: ", round(value, 2), "<br/>",
                        "Category: ", category
                    )
                ) %>%
                addLegend("bottomright",
                    pal = pal, values = ~value,
                    title = "Value", opacity = 0.7
                )
        } else if (input$map_type == "heatmap") {
            map <- map %>%
                addHeatmap(
                    lng = ~longitude, lat = ~latitude,
                    intensity = ~ value / max(value),
                    blur = 20, max = 0.5, radius = 15
                )
        } else if (input$map_type == "clusters") {
            map <- map %>%
                addCircleMarkers(
                    lng = ~longitude, lat = ~latitude,
                    radius = 8,
                    color = ~ pal(value),
                    fillOpacity = 0.7,
                    clusterOptions = markerClusterOptions(),
                    popup = ~ paste0(
                        "<strong>", label, "</strong><br/>",
                        "Value: ", round(value, 2)
                    )
                ) %>%
                addLegend("bottomright",
                    pal = pal, values = ~value,
                    title = "Value", opacity = 0.7
                )
        } else if (input$map_type == "choropleth") {
            # Create grid-based regions
            map <- map %>%
                addCircleMarkers(
                    lng = ~longitude, lat = ~latitude,
                    radius = 15,
                    color = ~ pal(value),
                    fillOpacity = 0.6,
                    stroke = FALSE
                ) %>%
                addLegend("bottomright",
                    pal = pal, values = ~value,
                    title = "Value", opacity = 0.7
                )
        }

        map
    })

    # Reset map view
    observeEvent(input$reset_view, {
        req(filtered_data())
        data <- filtered_data()

        leafletProxy("main_map") %>%
            fitBounds(
                lng1 = min(data$longitude), lat1 = min(data$latitude),
                lng2 = max(data$longitude), lat2 = max(data$latitude)
            )
    })

    # Region plot
    output$region_plot <- renderPlotly({
        req(filtered_data())

        region_summary <- filtered_data() %>%
            group_by(category) %>%
            summarise(
                mean_value = mean(value),
                count = n(),
                .groups = "drop"
            )

        plot_ly(region_summary,
            x = ~category, y = ~mean_value,
            type = "bar", marker = list(color = "#667eea"),
            text = ~ paste("Count:", count),
            hoverinfo = "text"
        ) %>%
            layout(
                xaxis = list(title = "Category"),
                yaxis = list(title = "Average Value"),
                margin = list(b = 100)
            )
    })

    # Density plot
    output$density_plot <- renderPlotly({
        req(filtered_data())

        plot_ly(filtered_data(),
            x = ~latitude, y = ~longitude,
            type = "histogram2d", colorscale = "Blues"
        ) %>%
            layout(
                xaxis = list(title = "Latitude"),
                yaxis = list(title = "Longitude"),
                title = "Spatial Density Heatmap"
            )
    })

    # Data table
    output$geo_table <- renderDT({
        req(filtered_data())

        datatable(
            filtered_data() %>%
                select(label, category, latitude, longitude, value) %>%
                mutate(
                    latitude = round(latitude, 4),
                    longitude = round(longitude, 4),
                    value = round(value, 2)
                ),
            options = list(pageLength = 10, scrollX = TRUE),
            rownames = FALSE
        )
    })
}

# Run the application
shinyApp(ui = ui, server = server)
