
' compare_dist Function: Compares marginal distributions of original and synthetic data
#'
#' This function generates visual comparisons for categorical, datetime,
#' and numerical columns by plotting bar plots and histograms.
#' It automatically adjusts the number of plots per page and pauses
#' when necessary to allow sequential viewing.
#'
#' @param data_model A list containing the original dataset and metadata.
#'        It should have:
#'        - `data_model$original`: The original dataset (data frame).
#'        - `data_model$metadata`: Metadata containing column names and types.
#' @param synthetic_data A data frame containing the synthetic dataset,
#'        structured similarly to `data_model$original`.
#'
#' @return The function does not return a value but generates multiple plots
#'         for comparison.
#'
#' @details
#' - **Categorical variables (`factor`)**: Uses `bar_plot()` to compare category proportions.
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
  col_types <- data_model$metadata$col_type  # Get column types in advance
  idx <- 1  # Counter for plots per page

  for (j in seq_along(col_names)) {
    col <- col_names[j]
    col_type <- col_types[j]

    if (! (col_type %in% c('key','text'))) {
      # Adjust layout dynamically based on column type
      plots_per_page <- ifelse(col_type %in% c("date", "datetime"), 6, 4)

      # Reset plotting window if necessary
      if (idx == 1 || idx > plots_per_page) {
        if (idx > 1) readline(prompt = "Press [Enter] to continue...")  # Pause for user
        if (.Platform$GUI != "RStudio") dev.new()  # Open a new plotting window if not in RStudio
        par(mfrow = c(ceiling(plots_per_page / 2), 2))  # Set grid dynamically
        idx <- 1  # Reset counter
      }

      if (col_type == "factor") {
        bar_plot(data_model$original[[col]], synthetic_data[[col]], col)
        idx <- idx + 1
      }

      if (col_type == "datetime") {
        original_dt <- as.POSIXct(data_model$original[[col]], tryFormats = c("%Y-%m-%d %H:%M:%OS", "%d/%m/%Y %H:%M:%OS"))
        synthetic_dt <- as.POSIXct(synthetic_data[[col]], tryFormats = c("%Y-%m-%d %H:%M:%OS", "%d/%m/%Y %H:%M:%OS"))
        month_names <- month.abb
        bar_plot(factor(substr(months(original_dt), 1, 3), levels = month_names),
                 factor(substr(months(synthetic_dt), 1, 3), levels = month_names), paste0('Month-', col))
        day_names <- c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun")
        bar_plot(factor(substr(weekdays(original_dt), 1, 3), levels = day_names),
                 factor(substr(weekdays(synthetic_dt), 1, 3), levels = day_names), paste0('Day-', col))
        bar_plot(as.integer(format(original_dt, "%H")), as.integer(format(synthetic_dt, "%H")), paste0('Hour-', col))
        idx <- idx + 3  # 3 plots used
      }

      if (col_type == "date") {
        original_dt <- as.POSIXct(data_model$original[[col]], tryFormats = c("%Y-%m-%d", "%d/%m/%Y"))
        synthetic_dt <- as.POSIXct(synthetic_data[[col]], tryFormats = c("%Y-%m-%d", "%d/%m/%Y"))
        month_names <- month.abb
        bar_plot(factor(substr(months(original_dt), 1, 3), levels = month_names),
                 factor(substr(months(synthetic_dt), 1, 3), levels = month_names), paste0('Month-', col))
        day_names <- c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun")
        bar_plot(factor(substr(weekdays(original_dt), 1, 3), levels = day_names),
                 factor(substr(weekdays(synthetic_dt), 1, 3), levels = day_names), paste0('Day-', col))
        hist(year(original_dt),
             main = paste0("Original Year ", col), xlab = col, col = "blue", freq = FALSE)
        hist(year(synthetic_dt),
             main = paste0("Synthetic Year ", col), xlab = col, col = "red", freq = FALSE)
        idx <- idx + 3  # 3 plots used
      }

      if (col_type %in% c('integer', 'numeric')) {
        hist(data_model$original[[col]], main = paste0("Original ", col), xlab = col, col = "blue", freq = FALSE)
        hist(synthetic_data[[col]], main = paste0("Synthetic ", col), xlab = col, col = "red", freq = FALSE)
        idx <- idx + 1
      }
    }
  }


}
