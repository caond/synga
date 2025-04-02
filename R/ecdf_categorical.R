#' Compute ECDF for Categorical Data
#'
#' This function calculates the empirical cumulative distribution function (ECDF)
#' for categorical data. It returns a function that can be used to obtain ECDF
#' values for new data points, as well as an inverse ECDF function.
#'
#' @param df A vector or factor representing categorical data.
#' @return A function that computes ECDF values for categorical inputs.
#'         The function also has two attributes:
#'         - `"lookup"`: A named vector containing the cumulative probabilities of each category.
#'         - `"inverse"`: A function that retrieves the category for a given probability.
#' @examples
#' data <- c("A", "B", "A", "C", "B", "B", "A", "C", "C", "C")
#' ecdf_func <- ecdf_categorical(data)
#' ecdf_values <- ecdf_func(c("A", "B", "C"))  # Get ECDF values
#' inverse_values <- attr(ecdf_func, "inverse")(c(0.2, 0.5, 0.8))  # Get categories from probabilities
#' @keywords internal
ecdf_categorical <- function(df) {
  df_non_na <- df[!is.na(df)]

  # 1. Compute NA rate
  p_na <- mean(is.na(df))

  # 2. Calculate frequencies and probabilities (excluding NAs)
  frequency <- table(df_non_na)
  probability <- frequency / length(df_non_na)
  cumulative_prob <- cumsum(probability)

  # 3. Adjust cumulative probabilities to account for NA mass
  cumulative_prob <- p_na + (1 - p_na) * cumulative_prob

  # 4. Define forward ECDF (category → [0,1])
  ecdf_function <- function(x) {
    sapply(x, function(val) {
      if (is.na(val)) return(p_na)
      val <- as.character(val)
      if (val %in% names(cumulative_prob)) {
        return(cumulative_prob[val])
      } else {
        return(NA)
      }
    })
  }

  # 5. Define inverse ECDF ([0,1] → category or NA)
  inverse_ecdf_function <- function(p) {
    sapply(p, function(prob) {
      if (is.na(prob) || prob < 0 || prob > 1) return(NA)
      if (prob < p_na) return(NA)  # This probability maps to NA
      # Find category where adjusted cumulative prob >= p
      match_index <- which(cumulative_prob >= prob)[1]
      if (!is.na(match_index)) {
        return(names(cumulative_prob)[match_index])
      } else {
        return(NA)
      }
    })
  }

  # Attach attributes
  attr(ecdf_function, "lookup") <- cumulative_prob
  attr(ecdf_function, "inverse") <- inverse_ecdf_function
  attr(ecdf_function, "p_na") <- p_na

  # Return the ECDF function
  return(ecdf_function)
}
