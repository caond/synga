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
  # Calculate the frequency of each unique category
  frequency <- table(df)
  
  # Convert frequencies to probabilities (relative frequency of each category)
  probability <- frequency / length(df)
  
  # Compute cumulative sum of probabilities (ECDF values)
  cumulative_prob <- cumsum(probability)  # Cumulative probability distribution
  
  # Define a function to retrieve ECDF values for a vector of categorical data
  ecdf_function <- function(df) {
    as.vector(sapply(df, function(x) {
      
      if (x %in% names(cumulative_prob)) {
        cumulative_prob[as.character(x)]
      } else {
        NA  # Return NA for unknown categories
      }
      
    }))
  }
  
  # Define the inverse ECDF function (finds the category for a given probability)
  inverse_ecdf_function <- function(prob_values) {
    as.vector(sapply(prob_values, function(prob) {
      if (prob < 0 || prob > 1) return(NA)  # Return NA for probabilities outside [0, 1]
      # Find the first category where the cumulative probability is greater than or equal to the input probability
      match_index <- which(cumulative_prob >= prob)[1]  
      # Return the corresponding category if found, otherwise return NA
      if (!is.na(match_index)) {
        return(names(cumulative_prob)[match_index])  
      } else {
        return(NA)  
      }
    }))
  }
  
  # Attach the cumulative probability lookup table as an attribute to the function
  attr(ecdf_function, "lookup") <- cumulative_prob
  
  # Attach the inverse ECDF function as an attribute to the function
  attr(ecdf_function, "inverse") <- inverse_ecdf_function
  
  # Return the ECDF function
  return(ecdf_function)
}
