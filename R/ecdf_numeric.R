#' Compute ECDF for Numeric Data With NA values
#'
#' This function calculates the empirical cumulative distribution function (ECDF)
#' for numerical data. It returns a function that can be used to obtain ECDF
#' values for new data points, as well as an inverse ECDF function.
#'
#' @param df A vector representing numerical data.
#' @return A function that computes ECDF values for categorical inputs.
#'         The function also has two attributes:
#'         - `"lookup"`: A named vector containing the cumulative probabilities of each category.
#'         - `"inverse"`: A function that retrieves the category for a given probability.
#' @examples
#' x <- c(1.2, 2.5, NA, 3.0, 5.1, NA, 4.4)
#' ecdf_num <- ecdf_numeric(x)
#' u <- ecdf_num(x)
#' x_recovered <- attr(ecdf_num, "inverse")(u)
#' mean(is.na(x_recovered))  # Should be ~equal to mean(is.na(x))
#' @keywords internal

ecdf_numeric <- function(x) {
  # Split into non-NA and NA
  x_non_na <- x[!is.na(x)]
  p_na <- mean(is.na(x))

  # Base ECDF function using non-NA values
  ecdf_fun <- ecdf(x_non_na)

  # Forward transformation: numeric → [0, 1]
  ecdf_function <- function(x_input) {
    sapply(x_input, function(xi) {
      if (is.na(xi)) {
        return(p_na)  # Map NA to its dedicated probability mass
      } else {
        # Scale ECDF result into [p_na, 1]
        return(p_na + (1 - p_na) * ecdf_fun(xi))
      }
    })
  }

  # Inverse transformation: [0, 1] → numeric or NA
  inverse_ecdf_function <- function(p_input) {
    sapply(p_input, function(pi) {
      if (is.na(pi) || pi < 0 || pi > 1) return(NA)
      if (pi < p_na) return(NA)
      # Rescale back to [0,1] for quantile
      p_rescaled <- (pi - p_na) / (1 - p_na)
      return(quantile(x_non_na, probs = p_rescaled, type = 8, na.rm = TRUE))
    })
  }

  # Add attributes (just like ecdf_categorical)
  attr(ecdf_function, "inverse") <- inverse_ecdf_function
  attr(ecdf_function, "p_na") <- p_na
  attr(ecdf_function, "lookup") <- NULL  # Numeric ECDF doesn’t have a lookup table

  return(ecdf_function)
}
