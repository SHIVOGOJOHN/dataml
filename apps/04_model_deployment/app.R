# Model Deployment Hub
# Interactive interface for deploying and demonstrating machine learning models

library(shiny)
library(bslib)
library(randomForest)
library(caret)
library(ggplot2)
library(plotly)
library(dplyr)
library(DT)

# Source shared utilities
source("shared/theme.R")
source("shared/ui_components.R")
source("shared/data_utils.R")

# Train sample models on app load
set.seed(42)
# Simple classification model
iris_rf <- randomForest(Species ~ ., data = iris, ntree = 100)

# Regression model
mtcars_lm <- lm(mpg ~ wt + hp + cyl, data = mtcars)

# UI
ui <- page_sidebar(
    title = "Model Deployment Hub",
    theme = app_theme,
    sidebar = sidebar(
        width = 300,
        app_header(
            "Model Hub",
            "Deploy and test machine learning models",
            gradient = get_gradient("stats")
        ),
        hr(),
        h4("Model Selection"),
        selectInput(
            "model_type",
            "Choose Model:",
            choices = c(
                "Iris Classification (Random Forest)" = "iris_rf",
                "MPG Prediction (Linear Regression)" = "mtcars_lm"
            )
        ),
        hr(),
        h4("Model Inputs"),
        conditionalPanel(
            condition = "input.model_type == 'iris_rf'",
            sliderInput("iris_sepal_length", "Sepal Length:", min = 4, max = 8, value = 5.8, step = 0.1),
            sliderInput("iris_sepal_width", "Sepal Width:", min = 2, max = 5, value = 3.0, step = 0.1),
            sliderInput("iris_petal_length", "Petal Length:", min = 1, max = 7, value = 4.0, step = 0.1),
            sliderInput("iris_petal_width", "Petal Width:", min = 0.1, max = 3, value = 1.3, step = 0.1)
        ),
        conditionalPanel(
            condition = "input.model_type == 'mtcars_lm'",
            sliderInput("mtcars_wt", "Weight (1000 lbs):", min = 1.5, max = 5.5, value = 3.2, step = 0.1),
            sliderInput("mtcars_hp", "Horsepower:", min = 50, max = 350, value = 150, step = 10),
            sliderInput("mtcars_cyl", "Cylinders:", min = 4, max = 8, value = 6, step = 2)
        ),
        actionButton("predict_btn", "Generate Prediction", icon = icon("robot"), class = "btn-primary btn-lg")
    ),
    tags$head(
        tags$style(HTML(custom_css))
    ),
    navset_card_tab(
        nav_panel(
            "Prediction",
            icon = icon("bullseye"),
            h3("Model Prediction Results"),
            fluidRow(
                column(
                    6,
                    uiOutput("prediction_card")
                ),
                column(
                    6,
                    uiOutput("confidence_card")
                )
            ),
            conditionalPanel(
                condition = "input.model_type == 'iris_rf'",
                card_container(
                    title = "Class Probabilities",
                    plotlyOutput("class_probs_plot", height = 300)
                )
            ),
            card_container(
                title = "Prediction Details",
                verbatimTextOutput("prediction_details")
            ),
            card_container(
                title = "Feature Importance",
                plotlyOutput("feature_importance_plot", height = 350)
            )
        ),
        nav_panel(
            "Model Performance",
            icon = icon("chart-line"),
            h3("Model Evaluation Metrics"),
            conditionalPanel(
                condition = "input.model_type == 'iris_rf'",
                fluidRow(
                    column(4, uiOutput("accuracy_card")),
                    column(4, uiOutput("oob_error_card")),
                    column(4, uiOutput("n_trees_card"))
                ),
                card_container(
                    title = "Confusion Matrix",
                    plotlyOutput("confusion_matrix_plot", height = 400)
                ),
                card_container(
                    title = "Error Rate by Class",
                    plotlyOutput("error_rate_plot", height = 300)
                )
            ),
            conditionalPanel(
                condition = "input.model_type == 'mtcars_lm'",
                fluidRow(
                    column(3, uiOutput("r2_card")),
                    column(3, uiOutput("rmse_card")),
                    column(3, uiOutput("mae_card")),
                    column(3, uiOutput("adj_r2_card"))
                ),
                fluidRow(
                    column(
                        6,
                        card_container(
                            title = "Actual vs Predicted",
                            plotlyOutput("actual_pred_plot", height = 350)
                        )
                    ),
                    column(
                        6,
                        card_container(
                            title = "Residuals Plot",
                            plotlyOutput("residuals_scatter_plot", height = 350)
                        )
                    )
                ),
                card_container(
                    title = "Model Summary",
                    verbatimTextOutput("lm_summary")
                )
            )
        ),
        nav_panel(
            "Model Info",
            icon = icon("info-circle"),
            h3("Model Information"),
            card_container(
                title = "Model Description",
                uiOutput("model_description")
            ),
            card_container(
                title = "Training Data Info",
                DTOutput("training_data_table")
            ),
            card_container(
                title = "Model Parameters",
                verbatimTextOutput("model_params")
            )
        )
    )
)

# Server
server <- function(input, output, session) {
    # Reactive prediction
    prediction <- reactiveVal(NULL)

    observeEvent(input$predict_btn, {
        if (input$model_type == "iris_rf") {
            new_data <- data.frame(
                Sepal.Length = input$iris_sepal_length,
                Sepal.Width = input$iris_sepal_width,
                Petal.Length = input$iris_petal_length,
                Petal.Width = input$iris_petal_width
            )

            pred <- predict(iris_rf, new_data, type = "response")
            pred_prob <- predict(iris_rf, new_data, type = "prob")

            prediction(list(
                class = as.character(pred),
                probabilities = pred_prob[1, ],
                confidence = max(pred_prob[1, ])
            ))
        } else if (input$model_type == "mtcars_lm") {
            new_data <- data.frame(
                wt = input$mtcars_wt,
                hp = input$mtcars_hp,
                cyl = input$mtcars_cyl
            )

            pred <- predict(mtcars_lm, new_data, interval = "prediction")

            prediction(list(
                value = pred[1, "fit"],
                lower = pred[1, "lwr"],
                upper = pred[1, "upr"]
            ))
        }
    })

    # Prediction Cards
    output$prediction_card <- renderUI({
        req(prediction())

        if (input$model_type == "iris_rf") {
            info_box(
                title = "Predicted Class",
                subtitle = prediction()$class,
                icon = "seedling",
                gradient = get_gradient("stats")
            )
        } else {
            info_box(
                title = "Predicted MPG",
                subtitle = paste0(round(prediction()$value, 2), " mpg"),
                icon = "gas-pump",
                gradient = get_gradient("stats")
            )
        }
    })

    output$confidence_card <- renderUI({
        req(prediction())

        if (input$model_type == "iris_rf") {
            conf <- prediction()$confidence * 100
            metric_card("Confidence", paste0(round(conf, 1), "%"), "check-circle", "#198754")
        } else {
            interval_width <- prediction()$upper - prediction()$lower
            metric_card("95% PI Width", paste0("±", round(interval_width / 2, 2)), "arrows-alt-h", "#0d6efd")
        }
    })

    output$class_probs_plot <- renderPlotly({
        req(prediction(), input$model_type == "iris_rf")

        probs <- data.frame(
            Class = names(prediction()$probabilities),
            Probability = as.numeric(prediction()$probabilities)
        )

        plot_ly(probs,
            x = ~Class, y = ~Probability, type = "bar",
            marker = list(color = c("#667eea", "#764ba2", "#f093fb"))
        ) %>%
            layout(
                xaxis = list(title = "Species"),
                yaxis = list(title = "Probability", range = c(0, 1)),
                title = "Class Probabilities"
            )
    })

    output$prediction_details <- renderPrint({
        req(prediction())

        if (input$model_type == "iris_rf") {
            cat("Prediction Details\n")
            cat("==================\n\n")
            cat("Predicted Class:", prediction()$class, "\n")
            cat("Confidence:", round(prediction()$confidence * 100, 2), "%\n\n")
            cat("Class Probabilities:\n")
            print(round(prediction()$probabilities, 4))
        } else {
            cat("Prediction Details\n")
            cat("==================\n\n")
            cat("Point Estimate:", round(prediction()$value, 2), "mpg\n")
            cat("95% Prediction Interval:\n")
            cat("  Lower:", round(prediction()$lower, 2), "mpg\n")
            cat("  Upper:", round(prediction()$upper, 2), "mpg\n")
        }
    })

    output$feature_importance_plot <- renderPlotly({
        if (input$model_type == "iris_rf") {
            importance <- importance(iris_rf)
            imp_df <- data.frame(
                Feature = rownames(importance),
                Importance = importance[, 1]
            ) %>% arrange(desc(Importance))

            plot_ly(imp_df,
                x = ~ reorder(Feature, Importance), y = ~Importance,
                type = "bar", marker = list(color = "#4facfe")
            ) %>%
                layout(
                    xaxis = list(title = "Feature"),
                    yaxis = list(title = "Mean Decrease Gini"),
                    margin = list(b = 100)
                )
        } else {
            # For linear model, show coefficient magnitudes
            coefs <- abs(coef(mtcars_lm)[-1]) # Exclude intercept
            imp_df <- data.frame(
                Feature = names(coefs),
                Coefficient = coefs
            )

            plot_ly(imp_df,
                x = ~ reorder(Feature, Coefficient), y = ~Coefficient,
                type = "bar", marker = list(color = "#4facfe")
            ) %>%
                layout(
                    xaxis = list(title = "Feature"),
                    yaxis = list(title = "|Coefficient|"),
                    margin = list(b = 100)
                )
        }
    })

    # Model Performance - Classification
    output$accuracy_card <- renderUI({
        # Use OOB predictions for accuracy
        oob_pred <- iris_rf$predicted
        accuracy <- mean(oob_pred == iris$Species) * 100
        metric_card("Accuracy", paste0(round(accuracy, 1), "%"), "bullseye", "#198754")
    })

    output$oob_error_card <- renderUI({
        oob_error <- iris_rf$err.rate[nrow(iris_rf$err.rate), "OOB"] * 100
        metric_card("OOB Error", paste0(round(oob_error, 1), "%"), "exclamation-triangle", "#dc3545")
    })

    output$n_trees_card <- renderUI({
        metric_card("Trees", iris_rf$ntree, "tree", "#0d6efd")
    })

    output$confusion_matrix_plot <- renderPlotly({
        cm <- table(Predicted = iris_rf$predicted, Actual = iris$Species)

        # Convert to data frame for plotly
        cm_df <- as.data.frame(cm)

        plot_ly(
            x = cm_df$Actual,
            y = cm_df$Predicted,
            z = cm_df$Freq,
            type = "heatmap",
            colorscale = "Blues",
            text = cm_df$Freq,
            texttemplate = "%{text}",
            showscale = FALSE
        ) %>%
            layout(
                xaxis = list(title = "Actual"),
                yaxis = list(title = "Predicted"),
                title = "Confusion Matrix"
            )
    })

    output$error_rate_plot <- renderPlotly({
        err_rate <- iris_rf$err.rate
        err_df <- data.frame(
            Trees = 1:nrow(err_rate),
            OOB = err_rate[, "OOB"],
            setosa = err_rate[, "setosa"],
            versicolor = err_rate[, "versicolor"],
            virginica = err_rate[, "virginica"]
        )

        plot_ly(err_df, x = ~Trees) %>%
            add_trace(
                y = ~OOB, name = "OOB", type = "scatter", mode = "lines",
                line = list(color = "#0d6efd", width = 2)
            ) %>%
            add_trace(
                y = ~setosa, name = "setosa", type = "scatter", mode = "lines",
                line = list(color = "#198754")
            ) %>%
            add_trace(
                y = ~versicolor, name = "versicolor", type = "scatter", mode = "lines",
                line = list(color = "#ffc107")
            ) %>%
            add_trace(
                y = ~virginica, name = "virginica", type = "scatter", mode = "lines",
                line = list(color = "#dc3545")
            ) %>%
            layout(
                xaxis = list(title = "Number of Trees"),
                yaxis = list(title = "Error Rate"),
                hovermode = "x unified"
            )
    })

    # Model Performance - Regression
    output$r2_card <- renderUI({
        r2 <- summary(mtcars_lm)$r.squared * 100
        metric_card("R²", paste0(round(r2, 1), "%"), "chart-area", "#0d6efd")
    })

    output$rmse_card <- renderUI({
        rmse <- sqrt(mean(residuals(mtcars_lm)^2))
        metric_card("RMSE", round(rmse, 2), "square-root-alt", "#dc3545")
    })

    output$mae_card <- renderUI({
        mae <- mean(abs(residuals(mtcars_lm)))
        metric_card("MAE", round(mae, 2), "ruler", "#ffc107")
    })

    output$adj_r2_card <- renderUI({
        adj_r2 <- summary(mtcars_lm)$adj.r.squared * 100
        metric_card("Adj R²", paste0(round(adj_r2, 1), "%"), "adjust", "#198754")
    })

    output$actual_pred_plot <- renderPlotly({
        actual <- mtcars$mpg
        predicted <- fitted(mtcars_lm)

        plot_ly(
            x = actual, y = predicted, type = "scatter", mode = "markers",
            marker = list(size = 10, color = "#667eea", opacity = 0.6)
        ) %>%
            add_trace(
                x = c(min(actual), max(actual)),
                y = c(min(actual), max(actual)),
                mode = "lines", name = "Perfect Fit",
                line = list(color = "red", dash = "dash")
            ) %>%
            layout(
                xaxis = list(title = "Actual MPG"),
                yaxis = list(title = "Predicted MPG"),
                showlegend = TRUE
            )
    })

    output$residuals_scatter_plot <- renderPlotly({
        fitted_vals <- fitted(mtcars_lm)
        residuals_vals <- residuals(mtcars_lm)

        plot_ly(
            x = fitted_vals, y = residuals_vals, type = "scatter", mode = "markers",
            marker = list(size = 10, color = "#764ba2", opacity = 0.6)
        ) %>%
            add_trace(
                x = range(fitted_vals), y = c(0, 0),
                mode = "lines", name = "Zero Line",
                line = list(color = "red", dash = "dash")
            ) %>%
            layout(
                xaxis = list(title = "Fitted Values"),
                yaxis = list(title = "Residuals"),
                showlegend = TRUE
            )
    })

    output$lm_summary <- renderPrint({
        summary(mtcars_lm)
    })

    # Model Info
    output$model_description <- renderUI({
        if (input$model_type == "iris_rf") {
            tagList(
                h4("Random Forest Classifier"),
                p("This model predicts the species of iris flowers based on sepal and petal measurements."),
                p(strong("Algorithm:"), "Random Forest with 100 trees"),
                p(strong("Target Variable:"), "Species (setosa, versicolor, virginica)"),
                p(strong("Features:"), "Sepal Length, Sepal Width, Petal Length, Petal Width"),
                p(strong("Training Data:"), "Fisher's Iris dataset (150 samples)")
            )
        } else {
            tagList(
                h4("Multiple Linear Regression"),
                p("This model predicts fuel efficiency (MPG) based on vehicle characteristics."),
                p(strong("Algorithm:"), "Ordinary Least Squares (OLS) Regression"),
                p(strong("Target Variable:"), "Miles Per Gallon (MPG)"),
                p(strong("Features:"), "Weight, Horsepower, Cylinders"),
                p(strong("Training Data:"), "mtcars dataset (32 vehicles)")
            )
        }
    })

    output$training_data_table <- renderDT({
        if (input$model_type == "iris_rf") {
            datatable(
                head(iris, 20),
                options = list(pageLength = 10, scrollX = TRUE),
                rownames = FALSE
            )
        } else {
            datatable(
                head(mtcars[, c("mpg", "wt", "hp", "cyl")], 20),
                options = list(pageLength = 10, scrollX = TRUE),
                rownames = TRUE
            )
        }
    })

    output$model_params <- renderPrint({
        if (input$model_type == "iris_rf") {
            cat("Random Forest Parameters\n")
            cat("========================\n\n")
            cat("Number of trees:", iris_rf$ntree, "\n")
            cat("Variables tried at each split:", iris_rf$mtry, "\n")
            cat("Type:", iris_rf$type, "\n")
        } else {
            cat("Linear Regression Parameters\n")
            cat("============================\n\n")
            cat("Formula:", deparse(formula(mtcars_lm)), "\n")
            cat("Number of observations:", nrow(mtcars), "\n")
            cat("Degrees of freedom:", mtcars_lm$df.residual, "\n")
        }
    })
}

# Run the application
shinyApp(ui = ui, server = server)
