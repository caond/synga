
' compare_dist Function: Compares marginal distributions of data_converted and synthetic data
#'
#' This function generates visual comparisons for categorical, datetime,
#' and numerical columns by plotting bar plots and histograms.
#' It automatically adjusts the number of plots per page and pauses
#' when necessary to allow sequential viewing.
#'
#' @param data_model A list containing the data_converted dataset and metadata.
#'        It should have:
#'        - `data_model$data_converted`: The data_converted dataset (data frame).
#'        - `data_model$metadata`: Metadata containing column names and types.
#' @param synthetic_data A data frame containing the synthetic dataset,
#'        structured similarly to `data_model$data_converted`.
#'
#' @return The function does not return a value but generates multiple plots
#'         for comparison.
#'
#' @details
#' - **Categorical variables (`factor`)**: Uses `plot_categorical_comparison()` to compare category proportions.
#' - **Datetime variables (`datetime`)**: Generates three plots:
#'     - Month distribution
#'     - Day of the week distribution
#'     - Hour distribution
#' - **Numeric variables (`integer`, `numeric`)**: Uses histograms for density comparison.
#'
#' @examples
#' # Assuming `data_model` and `synthetic_data` are predefined:
#' compare_dist(data_model, synthetic_data)
#'
#' @export
compare_dist <- function(data_model, synthetic_data) {

  col_names <- data_model$metadata$col_names
  col_types <- data_model$metadata$col_type

  for (j in seq_along(col_names)) {
    col <- col_names[j]
    col_type <- col_types[j]
    cat('\nComparing',col, ', please wait ...')
    if (!(col_type %in% c("key", "text"))) {

      # Open new plot window if not in RStudio
      if (.Platform$GUI != "RStudio") dev.new()

      # Generate and print plot
      if (col_type == "factor") {
        print(plot_categorical_comparison(data_model$data_converted[[col]], synthetic_data[[col]], col))
      }

      if (col_type == "datetime") {
        data_converted_dt <- as.POSIXct(data_model$data_converted[[col]], tryFormats = c("%Y-%m-%d %H:%M:%OS", "%d/%m/%Y %H:%M:%OS"))
        synthetic_dt <- as.POSIXct(synthetic_data[[col]], tryFormats = c("%Y-%m-%d %H:%M:%OS", "%d/%m/%Y %H:%M:%OS"))

        print(plot_categorical_comparison(
          factor(substr(months(data_converted_dt), 1, 3), levels = month.abb),
          factor(substr(months(synthetic_dt), 1, 3), levels = month.abb),
          paste0("Month-", col)
        ))

        readline("Press [Enter] to continue..."); if (.Platform$GUI != "RStudio") dev.new()

        print(plot_categorical_comparison(
          factor(substr(weekdays(data_converted_dt), 1, 3), levels = c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun")),
          factor(substr(weekdays(synthetic_dt), 1, 3), levels = c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun")),
          paste0("Day-", col)
        ))

        readline("Press [Enter] to continue..."); if (.Platform$GUI != "RStudio") dev.new()

        print(plot_numeric_histogram_comparison(
          as.integer(format(data_converted_dt, "%H")),
          as.integer(format(synthetic_dt, "%H")),
          paste0("Hour-", col)
        ))
        readline("Press [Enter] to continue..."); if (.Platform$GUI != "RStudio") dev.new()
      }

      if (col_type == "date") {
        data_converted_dt <- as.POSIXct(data_model$data_converted[[col]], tryFormats = c("%Y-%m-%d", "%d/%m/%Y"))
        synthetic_dt <- as.POSIXct(synthetic_data[[col]], tryFormats = c("%Y-%m-%d", "%d/%m/%Y"))

        print(plot_categorical_comparison(
          factor(substr(months(data_converted_dt), 1, 3), levels = month.abb),
          factor(substr(months(synthetic_dt), 1, 3), levels = month.abb),
          paste0("Month-", col)
        ))

        readline("Press [Enter] to continue..."); if (.Platform$GUI != "RStudio") dev.new()

        print(plot_categorical_comparison(
          factor(substr(weekdays(data_converted_dt), 1, 3), levels = c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun")),
          factor(substr(weekdays(synthetic_dt), 1, 3), levels = c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun")),
          paste0("Day-", col)
        ))

        readline("Press [Enter] to continue..."); if (.Platform$GUI != "RStudio") dev.new()

        print(plot_numeric_histogram_comparison(
          lubridate::year(data_converted_dt),
          lubridate::year(synthetic_dt),
          paste0("Year of ", col)
        ))
        readline("Press [Enter] to continue..."); if (.Platform$GUI != "RStudio") dev.new()
      }

      if (col_type %in% c("integer", "numeric")) {
        print(plot_numeric_histogram_comparison(
          data_model$data_converted[[col]],
          synthetic_data[[col]],
          col
        ))
      }

      # Wait for user input before moving to next plot (if not already done)
      if (col_type %in% c("factor", "integer", "numeric")) {
        readline("Press [Enter] to continue...")
      }
    }
  }

}
