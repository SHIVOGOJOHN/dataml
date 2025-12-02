# Shared Data Utilities for Shiny Applications
# Data loading, validation, and sample data generation

library(dplyr)
library(lubridate)

# Generate sample sales data
generate_sales_data <- function(n = 1000, seed = 123) {
    set.seed(seed)

    data.frame(
        date = seq.Date(Sys.Date() - 365, Sys.Date(), length.out = n),
        product = sample(c("Product A", "Product B", "Product C", "Product D", "Product E"), n, replace = TRUE),
        category = sample(c("Electronics", "Clothing", "Food", "Home", "Sports"), n, replace = TRUE),
        region = sample(c("North", "South", "East", "West", "Central"), n, replace = TRUE),
        sales = round(rnorm(n, 5000, 2000), 2),
        quantity = sample(1:50, n, replace = TRUE),
        cost = round(rnorm(n, 3000, 1500), 2)
    ) %>%
        mutate(
            sales = pmax(sales, 0),
            cost = pmax(cost, 0),
            profit = sales - cost,
            profit_margin = round((profit / sales) * 100, 2)
        )
}

# Generate sample customer churn data
generate_churn_data <- function(n = 500, seed = 123) {
    set.seed(seed)

    data.frame(
        customer_id = 1:n,
        tenure_months = sample(1:72, n, replace = TRUE),
        monthly_charges = round(rnorm(n, 70, 30), 2),
        total_charges = 0,
        contract_type = sample(c("Month-to-month", "One year", "Two year"), n, replace = TRUE, prob = c(0.5, 0.3, 0.2)),
        payment_method = sample(c("Electronic check", "Mailed check", "Bank transfer", "Credit card"), n, replace = TRUE),
        churn = sample(c(0, 1), n, replace = TRUE, prob = c(0.73, 0.27))
    ) %>%
        mutate(
            total_charges = monthly_charges * tenure_months,
            monthly_charges = pmax(monthly_charges, 0)
        )
}

# Generate sample time series data
generate_timeseries_data <- function(n = 365, seed = 123) {
    set.seed(seed)

    dates <- seq.Date(Sys.Date() - n, Sys.Date() - 1, by = "day")
    trend <- seq(100, 200, length.out = n)
    seasonal <- 20 * sin(2 * pi * seq_along(dates) / 30)
    noise <- rnorm(n, 0, 10)

    data.frame(
        date = dates,
        value = trend + seasonal + noise,
        trend = trend,
        seasonal = seasonal
    )
}

# Generate sample survey data
generate_survey_data <- function(n = 200, seed = 123) {
    set.seed(seed)

    data.frame(
        respondent_id = 1:n,
        age_group = sample(c("18-25", "26-35", "36-45", "46-55", "56+"), n, replace = TRUE),
        gender = sample(c("Male", "Female", "Other", "Prefer not to say"), n, replace = TRUE),
        satisfaction = sample(1:5, n, replace = TRUE, prob = c(0.05, 0.1, 0.2, 0.35, 0.3)),
        nps_score = sample(0:10, n, replace = TRUE),
        product_quality = sample(1:5, n, replace = TRUE, prob = c(0.03, 0.07, 0.2, 0.4, 0.3)),
        customer_service = sample(1:5, n, replace = TRUE, prob = c(0.05, 0.1, 0.25, 0.35, 0.25)),
        would_recommend = sample(c("Yes", "No", "Maybe"), n, replace = TRUE, prob = c(0.6, 0.15, 0.25))
    )
}

# Generate sample quality control data
generate_qc_data <- function(n = 200, seed = 123) {
    set.seed(seed)

    target <- 100

    data.frame(
        sample_id = 1:n,
        timestamp = seq.POSIXt(Sys.time() - n * 3600, Sys.time(), length.out = n),
        measurement = rnorm(n, target, 2),
        machine = sample(c("Machine A", "Machine B", "Machine C"), n, replace = TRUE),
        operator = sample(c("Operator 1", "Operator 2", "Operator 3"), n, replace = TRUE),
        shift = sample(c("Day", "Night"), n, replace = TRUE)
    ) %>%
        mutate(
            ucl = target + 3 * 2, # Upper Control Limit
            lcl = target - 3 * 2, # Lower Control Limit
            out_of_control = measurement > ucl | measurement < lcl
        )
}

# Generate sample financial/stock data
generate_stock_data <- function(symbol = "STOCK", days = 365, seed = 123) {
    set.seed(seed)

    dates <- seq.Date(Sys.Date() - days, Sys.Date(), by = "day")
    # Remove weekends
    dates <- dates[!weekdays(dates) %in% c("Saturday", "Sunday")]

    n <- length(dates)
    returns <- rnorm(n, 0.0005, 0.02)
    price <- 100 * cumprod(1 + returns)

    data.frame(
        date = dates,
        open = price * (1 + rnorm(n, 0, 0.01)),
        high = price * (1 + abs(rnorm(n, 0.01, 0.01))),
        low = price * (1 - abs(rnorm(n, 0.01, 0.01))),
        close = price,
        volume = round(rnorm(n, 1000000, 300000))
    ) %>%
        mutate(
            volume = pmax(volume, 0)
        )
}

# Generate sample geospatial data - KENYA FOCUSED
generate_geo_data <- function(n = 100, seed = 123) {
    set.seed(seed)

    # Focus on Kenya
    # Kenya coordinates: Latitude -4.68 to 5.03, Longitude 33.91 to 41.91
    data.frame(
        location_id = 1:n,
        latitude = runif(n, -4.68, 5.03), # Kenya latitude range
        longitude = runif(n, 33.91, 41.91), # Kenya longitude range
        value = round(rnorm(n, 100, 30), 2),
        category = sample(c("Nairobi", "Mombasa", "Kisumu", "Nakuru", "Eldoret"), n, replace = TRUE),
        label = sample(c(
            "Nairobi", "Mombasa", "Kisumu", "Nakuru", "Eldoret",
            "Thika", "Malindi", "Kitale", "Garissa", "Kakamega",
            "Nyeri", "Machakos", "Meru", "Kericho", "Embu"
        ), n, replace = TRUE)
    )
}

# Data validation helper
validate_uploaded_data <- function(data, required_cols = NULL) {
    errors <- character()

    if (!is.data.frame(data)) {
        errors <- c(errors, "Data must be a data frame")
        return(list(valid = FALSE, errors = errors))
    }

    if (nrow(data) == 0) {
        errors <- c(errors, "Data frame is empty")
    }

    if (!is.null(required_cols)) {
        missing_cols <- setdiff(required_cols, names(data))
        if (length(missing_cols) > 0) {
            errors <- c(errors, paste("Missing required columns:", paste(missing_cols, collapse = ", ")))
        }
    }

    list(
        valid = length(errors) == 0,
        errors = errors,
        n_rows = nrow(data),
        n_cols = ncol(data),
        col_names = names(data)
    )
}

# Calculate summary statistics
calculate_summary_stats <- function(data) {
    numeric_cols <- names(data)[sapply(data, is.numeric)]

    if (length(numeric_cols) == 0) {
        return(data.frame(message = "No numeric columns found"))
    }

    summary_list <- lapply(numeric_cols, function(col) {
        x <- data[[col]]
        data.frame(
            Column = col,
            Mean = round(mean(x, na.rm = TRUE), 2),
            Median = round(median(x, na.rm = TRUE), 2),
            SD = round(sd(x, na.rm = TRUE), 2),
            Min = round(min(x, na.rm = TRUE), 2),
            Max = round(max(x, na.rm = TRUE), 2),
            Missing = sum(is.na(x))
        )
    })

    do.call(rbind, summary_list)
}

# Format currency
format_currency <- function(x) {
    paste0("$", formatC(x, format = "f", big.mark = ",", digits = 2))
}

# Format percentage
format_percentage <- function(x) {
    paste0(round(x, 2), "%")
}

# Format large numbers
format_number <- function(x) {
    formatC(x, format = "f", big.mark = ",", digits = 0)
}
