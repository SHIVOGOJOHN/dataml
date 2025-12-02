# Statistical Lab
# Interactive statistical calculators, simulations, and educational tools

library(shiny)
library(bslib)
library(ggplot2)
library(plotly)
library(pwr)
library(broom)

# Source shared utilities
source("shared/theme.R")
source("shared/ui_components.R")
source("shared/data_utils.R")

# UI
ui <- page_navbar(
    title = "Statistical Lab",
    theme = app_theme,
    tags$head(
        tags$style(HTML(custom_css))
    ),

    # Confidence Interval Simulator
    nav_panel(
        "CI Simulator",
        icon = icon("chart-area"),
        layout_sidebar(
            sidebar = sidebar(
                h4("Simulation Parameters"),
                sliderInput("ci_n", "Sample Size (n):", min = 10, max = 200, value = 30),
                sliderInput("ci_conf_level", "Confidence Level:", min = 0.8, max = 0.99, value = 0.95, step = 0.01),
                numericInput("ci_pop_mean", "True Population Mean:", value = 100),
                numericInput("ci_pop_sd", "Population SD:", value = 15, min = 1),
                sliderInput("ci_n_samples", "Number of Samples:", min = 10, max = 200, value = 100),
                actionButton("run_ci_sim", "Run Simulation", icon = icon("play"), class = "btn-primary")
            ),
            h2("Confidence Interval Simulator"),
            p("This tool demonstrates the concept of confidence intervals by repeatedly sampling from a population and calculating 95% CIs."),
            fluidRow(
                column(4, uiOutput("ci_coverage_card")),
                column(4, uiOutput("ci_expected_card")),
                column(4, uiOutput("ci_samples_card"))
            ),
            card_container(
                title = "Confidence Intervals Visualization",
                plotlyOutput("ci_plot", height = 500)
            ),
            card_container(
                title = "Interpretation",
                p(strong("Coverage Rate:"), "The proportion of confidence intervals that contain the true population mean."),
                p(strong("Expected:"), textOutput("ci_interpretation", inline = TRUE))
            )
        )
    ),

    # A/B Testing Calculator
    nav_panel(
        "A/B Testing",
        icon = icon("flask"),
        layout_sidebar(
            sidebar = sidebar(
                h4("Test Parameters"),
                numericInput("ab_baseline", "Baseline Conversion Rate:", value = 0.10, min = 0, max = 1, step = 0.01),
                numericInput("ab_effect", "Minimum Detectable Effect:", value = 0.02, min = 0.001, max = 0.5, step = 0.01),
                sliderInput("ab_power", "Statistical Power:", min = 0.7, max = 0.99, value = 0.8, step = 0.01),
                sliderInput("ab_alpha", "Significance Level (α):", min = 0.01, max = 0.1, value = 0.05, step = 0.01),
                radioButtons("ab_test_type", "Test Type:", choices = c("Two-sided" = "two", "One-sided" = "one")),
                actionButton("calc_ab", "Calculate", icon = icon("calculator"), class = "btn-primary")
            ),
            h2("A/B Testing Power Calculator"),
            p("Calculate the required sample size for your A/B test or the power of your current design."),
            fluidRow(
                column(6, uiOutput("ab_sample_size_card")),
                column(6, uiOutput("ab_power_card"))
            ),
            card_container(
                title = "Power Curve",
                plotlyOutput("power_curve_plot", height = 400)
            ),
            card_container(
                title = "Sample Size Sensitivity",
                p("How sample size changes with different effect sizes:"),
                plotlyOutput("sample_size_plot", height = 300)
            )
        )
    ),

    # Distribution Explorer
    nav_panel(
        "Distributions",
        icon = icon("bell-curve"),
        layout_sidebar(
            sidebar = sidebar(
                h4("Distribution Settings"),
                selectInput("dist_type", "Select Distribution:",
                    choices = c(
                        "Normal" = "norm",
                        "T-distribution" = "t",
                        "Chi-squared" = "chisq",
                        "F-distribution" = "f",
                        "Binomial" = "binom",
                        "Poisson" = "pois"
                    )
                ),
                conditionalPanel(
                    condition = "input.dist_type == 'norm'",
                    numericInput("norm_mean", "Mean:", value = 0),
                    numericInput("norm_sd", "SD:", value = 1, min = 0.1)
                ),
                conditionalPanel(
                    condition = "input.dist_type == 't'",
                    sliderInput("t_df", "Degrees of Freedom:", min = 1, max = 30, value = 5)
                ),
                conditionalPanel(
                    condition = "input.dist_type == 'chisq'",
                    sliderInput("chisq_df", "Degrees of Freedom:", min = 1, max = 30, value = 5)
                ),
                conditionalPanel(
                    condition = "input.dist_type == 'f'",
                    sliderInput("f_df1", "DF1:", min = 1, max = 30, value = 5),
                    sliderInput("f_df2", "DF2:", min = 1, max = 30, value = 10)
                ),
                conditionalPanel(
                    condition = "input.dist_type == 'binom'",
                    sliderInput("binom_n", "Trials (n):", min = 1, max = 100, value = 20),
                    sliderInput("binom_p", "Probability (p):", min = 0, max = 1, value = 0.5, step = 0.01)
                ),
                conditionalPanel(
                    condition = "input.dist_type == 'pois'",
                    numericInput("pois_lambda", "Lambda (λ):", value = 5, min = 0.1)
                ),
                hr(),
                h4("Probability Calculator"),
                numericInput("prob_x", "Calculate P(X ≤ x):", value = 0),
                actionButton("calc_prob", "Calculate", icon = icon("calculator"), class = "btn-info")
            ),
            h2("Probability Distribution Playground"),
            p("Explore and visualize different probability distributions."),
            fluidRow(
                column(4, uiOutput("dist_mean_card")),
                column(4, uiOutput("dist_var_card")),
                column(4, uiOutput("dist_prob_card"))
            ),
            card_container(
                title = "Distribution Visualization",
                plotlyOutput("dist_plot", height = 400)
            ),
            card_container(
                title = "Quantile Plot",
                plotlyOutput("quantile_plot", height = 300)
            )
        )
    ),

    # Regression Diagnostics
    nav_panel(
        "Regression",
        icon = icon("chart-line"),
        layout_sidebar(
            sidebar = sidebar(
                h4("Regression Settings"),
                sliderInput("reg_n", "Sample Size:", min = 50, max = 500, value = 100),
                sliderInput("reg_slope", "True Slope:", min = -5, max = 5, value = 2, step = 0.1),
                sliderInput("reg_intercept", "True Intercept:", min = -10, max = 10, value = 5),
                sliderInput("reg_noise", "Noise Level:", min = 0.1, max = 10, value = 2),
                checkboxInput("reg_outliers", "Add Outliers", FALSE),
                conditionalPanel(
                    condition = "input.reg_outliers",
                    sliderInput("reg_n_outliers", "Number of Outliers:", min = 1, max = 10, value = 3)
                ),
                actionButton("gen_reg_data", "Generate Data", icon = icon("sync"), class = "btn-primary")
            ),
            h2("Linear Regression Explorer"),
            p("Visualize regression assumptions and diagnostic plots."),
            fluidRow(
                column(3, uiOutput("reg_r2_card")),
                column(3, uiOutput("reg_slope_card")),
                column(3, uiOutput("reg_pvalue_card")),
                column(3, uiOutput("reg_rmse_card"))
            ),
            fluidRow(
                column(
                    6,
                    card_container(
                        title = "Regression Plot",
                        plotlyOutput("reg_plot", height = 350)
                    )
                ),
                column(
                    6,
                    card_container(
                        title = "Residuals vs Fitted",
                        plotOutput("residuals_plot", height = 350)
                    )
                )
            ),
            fluidRow(
                column(
                    6,
                    card_container(
                        title = "Q-Q Plot",
                        plotOutput("qq_plot", height = 300)
                    )
                ),
                column(
                    6,
                    card_container(
                        title = "Scale-Location Plot",
                        plotOutput("scale_location_plot", height = 300)
                    )
                )
            ),
            card_container(
                title = "Regression Summary",
                verbatimTextOutput("reg_summary")
            )
        )
    ),

    # Sampling Distribution
    nav_panel(
        "Sampling",
        icon = icon("random"),
        layout_sidebar(
            sidebar = sidebar(
                h4("Population Parameters"),
                selectInput("pop_dist", "Population Distribution:",
                    choices = c("Normal" = "norm", "Uniform" = "unif", "Exponential" = "exp", "Skewed" = "skewed")
                ),
                sliderInput("samp_size", "Sample Size (n):", min = 5, max = 100, value = 30),
                sliderInput("n_samples", "Number of Samples:", min = 100, max = 2000, value = 1000, step = 100),
                actionButton("run_sampling", "Run Simulation", icon = icon("play"), class = "btn-primary")
            ),
            h2("Central Limit Theorem Explorer"),
            p("Demonstrate the Central Limit Theorem by sampling from various distributions."),
            fluidRow(
                column(4, uiOutput("pop_mean_card")),
                column(4, uiOutput("sample_mean_card")),
                column(4, uiOutput("se_card"))
            ),
            fluidRow(
                column(
                    6,
                    card_container(
                        title = "Population Distribution",
                        plotlyOutput("pop_dist_plot", height = 300)
                    )
                ),
                column(
                    6,
                    card_container(
                        title = "Sampling Distribution of Mean",
                        plotlyOutput("sampling_dist_plot", height = 300)
                    )
                )
            ),
            card_container(
                title = "Sample Means Distribution",
                plotlyOutput("means_histogram", height = 400)
            )
        )
    )
)

# Server
server <- function(input, output, session) {
    # CI Simulator
    ci_results <- reactiveVal(NULL)

    observeEvent(input$run_ci_sim, {
        set.seed(42)

        n <- input$ci_n
        conf_level <- input$ci_conf_level
        pop_mean <- input$ci_pop_mean
        pop_sd <- input$ci_pop_sd
        n_samples <- input$ci_n_samples

        results <- data.frame(
            sample_id = 1:n_samples,
            sample_mean = numeric(n_samples),
            ci_lower = numeric(n_samples),
            ci_upper = numeric(n_samples),
            contains_mean = logical(n_samples)
        )

        for (i in 1:n_samples) {
            sample_data <- rnorm(n, pop_mean, pop_sd)
            sample_mean <- mean(sample_data)
            se <- sd(sample_data) / sqrt(n)
            margin <- qt((1 + conf_level) / 2, n - 1) * se

            results$sample_mean[i] <- sample_mean
            results$ci_lower[i] <- sample_mean - margin
            results$ci_upper[i] <- sample_mean + margin
            results$contains_mean[i] <- (pop_mean >= results$ci_lower[i]) & (pop_mean <= results$ci_upper[i])
        }

        ci_results(results)
    })

    output$ci_coverage_card <- renderUI({
        req(ci_results())
        coverage <- mean(ci_results()$contains_mean) * 100
        metric_card("Coverage Rate", paste0(round(coverage, 1), "%"), "bullseye", "#0d6efd")
    })

    output$ci_expected_card <- renderUI({
        expected <- input$ci_conf_level * 100
        metric_card("Expected", paste0(expected, "%"), "check", "#198754")
    })

    output$ci_samples_card <- renderUI({
        req(ci_results())
        metric_card("Samples", nrow(ci_results()), "vial", "#ffc107")
    })

    output$ci_plot <- renderPlotly({
        req(ci_results())

        results <- ci_results()
        results$color <- ifelse(results$contains_mean, "Contains Mean", "Misses Mean")

        # Show first 50 for visibility
        plot_data <- head(results, 50)

        plot_ly(plot_data,
            x = ~sample_id, y = ~sample_mean,
            color = ~color, colors = c("#198754", "#dc3545"),
            type = "scatter", mode = "markers",
            error_y = list(
                type = "data",
                symmetric = FALSE,
                array = ~ (ci_upper - sample_mean),
                arrayminus = ~ (sample_mean - ci_lower)
            )
        ) %>%
            add_trace(
                x = c(0, 51), y = c(input$ci_pop_mean, input$ci_pop_mean),
                mode = "lines", name = "True Mean",
                line = list(color = "black", dash = "dash"),
                showlegend = TRUE
            ) %>%
            layout(
                xaxis = list(title = "Sample Number"),
                yaxis = list(title = "Sample Mean"),
                title = paste0("First 50 Confidence Intervals (", input$ci_conf_level * 100, "% level)")
            )
    })

    output$ci_interpretation <- renderText({
        req(ci_results())
        coverage <- mean(ci_results()$contains_mean) * 100
        paste0(
            "With ", input$ci_conf_level * 100, "% confidence intervals, we expect about ",
            input$ci_conf_level * 100, "% to contain the true mean. ",
            "In this simulation, ", round(coverage, 1), "% of intervals contained the true mean."
        )
    })

    # A/B Testing
    ab_results <- reactiveVal(NULL)

    observeEvent(input$calc_ab, {
        h <- input$ab_test_type
        n <- pwr.2p.test(
            h = ES.h(input$ab_baseline, input$ab_baseline + input$ab_effect),
            sig.level = input$ab_alpha,
            power = input$ab_power,
            alternative = if (h == "two") "two.sided" else "greater"
        )$n

        ab_results(list(
            n_per_group = ceiling(n),
            total_n = ceiling(n * 2)
        ))
    })

    output$ab_sample_size_card <- renderUI({
        req(ab_results())
        metric_card("Sample Size per Group", format_number(ab_results()$n_per_group), "users", "#0d6efd")
    })

    output$ab_power_card <- renderUI({
        metric_card("Target Power", paste0(input$ab_power * 100, "%"), "bolt", "#198754")
    })

    output$power_curve_plot <- renderPlotly({
        req(ab_results())

        effect_sizes <- seq(0.001, 0.1, length.out = 50)
        powers <- sapply(effect_sizes, function(e) {
            pwr.2p.test(
                h = ES.h(input$ab_baseline, input$ab_baseline + e),
                n = ab_results()$n_per_group,
                sig.level = input$ab_alpha,
                alternative = if (input$ab_test_type == "two") "two.sided" else "greater"
            )$power
        })

        plot_ly(
            x = effect_sizes * 100, y = powers, type = "scatter", mode = "lines",
            line = list(color = "#667eea", width = 3)
        ) %>%
            add_trace(
                x = c(input$ab_effect * 100), y = c(input$ab_power),
                mode = "markers", marker = list(size = 12, color = "red"),
                name = "Target"
            ) %>%
            layout(
                xaxis = list(title = "Effect Size (%)"),
                yaxis = list(title = "Statistical Power"),
                title = "Power Curve"
            )
    })

    output$sample_size_plot <- renderPlotly({
        effect_sizes <- seq(0.005, 0.1, length.out = 20)
        sample_sizes <- sapply(effect_sizes, function(e) {
            ceiling(pwr.2p.test(
                h = ES.h(input$ab_baseline, input$ab_baseline + e),
                sig.level = input$ab_alpha,
                power = input$ab_power
            )$n)
        })

        plot_ly(
            x = effect_sizes * 100, y = sample_sizes, type = "bar",
            marker = list(color = "#4facfe")
        ) %>%
            layout(
                xaxis = list(title = "Effect Size (%)"),
                yaxis = list(title = "Required Sample Size"),
                title = "Sample Size vs Effect Size"
            )
    })

    # Distribution Explorer
    output$dist_plot <- renderPlotly({
        x_range <- switch(input$dist_type,
            "norm" = seq(input$norm_mean - 4 * input$norm_sd, input$norm_mean + 4 * input$norm_sd, length.out = 200),
            "t" = seq(-5, 5, length.out = 200),
            "chisq" = seq(0, input$chisq_df * 3, length.out = 200),
            "f" = seq(0, 6, length.out = 200),
            "binom" = 0:input$binom_n,
            "pois" = 0:(input$pois_lambda * 4)
        )

        y_vals <- switch(input$dist_type,
            "norm" = dnorm(x_range, input$norm_mean, input$norm_sd),
            "t" = dt(x_range, input$t_df),
            "chisq" = dchisq(x_range, input$chisq_df),
            "f" = df(x_range, input$f_df1, input$f_df2),
            "binom" = dbinom(x_range, input$binom_n, input$binom_p),
            "pois" = dpois(x_range, input$pois_lambda)
        )

        plot_type <- if (input$dist_type %in% c("binom", "pois")) "bar" else "scatter"

        plot_ly(
            x = x_range, y = y_vals, type = plot_type,
            mode = if (plot_type == "scatter") "lines" else NULL,
            marker = if (plot_type == "bar") list(color = "#667eea") else NULL,
            line = if (plot_type == "scatter") list(color = "#667eea", width = 3) else NULL
        ) %>%
            layout(
                xaxis = list(title = "x"),
                yaxis = list(title = "Density/Probability"),
                title = "Probability Distribution"
            )
    })

    output$dist_mean_card <- renderUI({
        mean_val <- switch(input$dist_type,
            "norm" = input$norm_mean,
            "t" = 0,
            "chisq" = input$chisq_df,
            "f" = input$f_df2 / (input$f_df2 - 2),
            "binom" = input$binom_n * input$binom_p,
            "pois" = input$pois_lambda
        )

        metric_card("Mean", round(mean_val, 2), "chart-line", "#0d6efd")
    })

    output$dist_var_card <- renderUI({
        var_val <- switch(input$dist_type,
            "norm" = input$norm_sd^2,
            "t" = if (input$t_df > 2) input$t_df / (input$t_df - 2) else NA,
            "chisq" = 2 * input$chisq_df,
            "f" = NA,
            "binom" = input$binom_n * input$binom_p * (1 - input$binom_p),
            "pois" = input$pois_lambda
        )

        metric_card("Variance", round(var_val, 2), "square-root-alt", "#198754")
    })

    dist_prob <- reactiveVal(NULL)

    observeEvent(input$calc_prob, {
        prob <- switch(input$dist_type,
            "norm" = pnorm(input$prob_x, input$norm_mean, input$norm_sd),
            "t" = pt(input$prob_x, input$t_df),
            "chisq" = pchisq(input$prob_x, input$chisq_df),
            "f" = pf(input$prob_x, input$f_df1, input$f_df2),
            "binom" = pbinom(input$prob_x, input$binom_n, input$binom_p),
            "pois" = ppois(input$prob_x, input$pois_lambda)
        )

        dist_prob(prob)
    })

    output$dist_prob_card <- renderUI({
        req(dist_prob())
        metric_card("P(X ≤ x)", round(dist_prob(), 4), "calculator", "#ffc107")
    })

    # Regression Explorer
    reg_data <- reactiveVal(NULL)

    observeEvent(input$gen_reg_data,
        {
            set.seed(42)
            n <- input$reg_n
            x <- runif(n, 0, 10)
            y <- input$reg_intercept + input$reg_slope * x + rnorm(n, 0, input$reg_noise)

            if (input$reg_outliers) {
                outlier_idx <- sample(1:n, input$reg_n_outliers)
                y[outlier_idx] <- y[outlier_idx] + rnorm(input$reg_n_outliers, 0, input$reg_noise * 5)
            }

            reg_data(data.frame(x = x, y = y))
        },
        ignoreNULL = FALSE
    )

    reg_model <- reactive({
        req(reg_data())
        lm(y ~ x, data = reg_data())
    })

    output$reg_plot <- renderPlotly({
        req(reg_data())

        plot_ly(reg_data(),
            x = ~x, y = ~y, type = "scatter", mode = "markers",
            marker = list(size = 8, color = "#667eea", opacity = 0.6)
        ) %>%
            add_trace(
                x = ~x, y = fitted(reg_model()), mode = "lines",
                name = "Fitted Line", line = list(color = "red", width = 2)
            ) %>%
            layout(
                xaxis = list(title = "X"),
                yaxis = list(title = "Y"),
                showlegend = TRUE
            )
    })

    output$reg_r2_card <- renderUI({
        req(reg_model())
        r2 <- summary(reg_model())$r.squared
        metric_card("R²", round(r2, 3), "chart-area", "#0d6efd")
    })

    output$reg_slope_card <- renderUI({
        req(reg_model())
        slope <- coef(reg_model())[2]
        metric_card("Slope", round(slope, 3), "arrow-trend-up", "#198754")
    })

    output$reg_pvalue_card <- renderUI({
        req(reg_model())
        pval <- summary(reg_model())$coefficients[2, 4]
        metric_card("P-value", format(pval, digits = 4, scientific = TRUE), "flask", "#ffc107")
    })

    output$reg_rmse_card <- renderUI({
        req(reg_model())
        rmse <- sqrt(mean(residuals(reg_model())^2))
        metric_card("RMSE", round(rmse, 3), "exclamation-triangle", "#dc3545")
    })

    output$residuals_plot <- renderPlot({
        req(reg_model())

        plot(fitted(reg_model()), residuals(reg_model()),
            xlab = "Fitted Values", ylab = "Residuals",
            main = "Residuals vs Fitted",
            pch = 19, col = rgb(0.4, 0.5, 0.9, 0.6)
        )
        abline(h = 0, col = "red", lwd = 2, lty = 2)
        lines(lowess(fitted(reg_model()), residuals(reg_model())), col = "blue", lwd = 2)
    })

    output$qq_plot <- renderPlot({
        req(reg_model())
        qqnorm(residuals(reg_model()),
            pch = 19, col = rgb(0.4, 0.5, 0.9, 0.6),
            main = "Normal Q-Q Plot"
        )
        qqline(residuals(reg_model()), col = "red", lwd = 2)
    })

    output$scale_location_plot <- renderPlot({
        req(reg_model())
        plot(fitted(reg_model()), sqrt(abs(residuals(reg_model()))),
            xlab = "Fitted Values", ylab = "√|Residuals|",
            main = "Scale-Location Plot",
            pch = 19, col = rgb(0.4, 0.5, 0.9, 0.6)
        )
        lines(lowess(fitted(reg_model()), sqrt(abs(residuals(reg_model())))), col = "red", lwd = 2)
    })

    output$reg_summary <- renderPrint({
        req(reg_model())
        summary(reg_model())
    })

    # Sampling Distribution
    sampling_results <- reactiveVal(NULL)

    observeEvent(input$run_sampling,
        {
            set.seed(42)

            pop_data <- switch(input$pop_dist,
                "norm" = rnorm(10000, 100, 15),
                "unif" = runif(10000, 50, 150),
                "exp" = rexp(10000, 1 / 100),
                "skewed" = rbeta(10000, 2, 5) * 100
            )

            sample_means <- replicate(input$n_samples, mean(sample(pop_data, input$samp_size)))

            sampling_results(list(
                pop_data = pop_data,
                sample_means = sample_means
            ))
        },
        ignoreNULL = FALSE
    )

    output$pop_mean_card <- renderUI({
        req(sampling_results())
        pop_mean <- mean(sampling_results()$pop_data)
        metric_card("Population Mean", round(pop_mean, 2), "globe", "#0d6efd")
    })

    output$sample_mean_card <- renderUI({
        req(sampling_results())
        sample_mean <- mean(sampling_results()$sample_means)
        metric_card("Mean of Sample Means", round(sample_mean, 2), "chart-bar", "#198754")
    })

    output$se_card <- renderUI({
        req(sampling_results())
        se <- sd(sampling_results()$sample_means)
        metric_card("SE of Sample Means", round(se, 2), "wave-square", "#ffc107")
    })

    output$pop_dist_plot <- renderPlotly({
        req(sampling_results())

        plot_ly(
            x = sampling_results()$pop_data, type = "histogram",
            marker = list(color = "#667eea"), nbinsx = 50
        ) %>%
            layout(
                xaxis = list(title = "Value"),
                yaxis = list(title = "Frequency"),
                title = "Population Distribution"
            )
    })

    output$sampling_dist_plot <- renderPlotly({
        req(sampling_results())

        # Theoretical normal overlay
        means <- sampling_results()$sample_means
        x_range <- seq(min(means), max(means), length.out = 100)
        theoretical <- dnorm(x_range, mean(means), sd(means)) * length(means) * diff(range(means)) / 30

        plot_ly(
            x = means, type = "histogram", name = "Observed",
            marker = list(color = "#4facfe"), nbinsx = 30
        ) %>%
            add_trace(
                x = x_range, y = theoretical, type = "scatter", mode = "lines",
                name = "Theoretical Normal", line = list(color = "red", width = 3)
            ) %>%
            layout(
                xaxis = list(title = "Sample Mean"),
                yaxis = list(title = "Frequency"),
                title = "Sampling Distribution (CLT)"
            )
    })

    output$means_histogram <- renderPlotly({
        req(sampling_results())

        plot_ly(
            x = sampling_results()$sample_means, type = "histogram",
            marker = list(color = "#764ba2"), nbinsx = 40
        ) %>%
            layout(
                xaxis = list(title = "Sample Mean"),
                yaxis = list(title = "Frequency"),
                title = paste0("Distribution of ", input$n_samples, " Sample Means (n=", input$samp_size, ")")
            )
    })
}

# Run the application
shinyApp(ui = ui, server = server)
