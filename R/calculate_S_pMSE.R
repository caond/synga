#' Calculate Standardized pMSE (S_pMSE) for Categorical Variables
#'
#' This function computes the Propensity Mean Squared Error (pMSE) and its standardized version (S_pMSE),
#' which quantify how distinguishable synthetic data is from original data. It fits a logistic regression
#' model to classify observations as synthetic or original based on a categorical variable.
#'
#' The standardized pMSE (S_pMSE) is defined as:
#' \deqn{S_{pMSE} = \frac{pMSE}{p(1 - p)}}
#' where \code{p} is the proportion of synthetic observations.
#'
#' @param org A vector of original categorical values.
#' @param syn A vector of synthetic categorical values (same domain as \code{org}).
#'
#' @return A list with two components:
#' \describe{
#'   \item{\code{pMSE}}{The raw propensity mean squared error.}
#'   \item{\code{S_pMSE}}{The standardized pMSE, comparable across settings.}
#' }
#'
#' @details This function removes rows with \code{NA} values using \code{complete.cases()} before modeling.
#' It uses logistic regression with one-hot encoding (via \code{factor}) to predict source labels from the input variable.
#'
#' @seealso \code{\link{glm}}, \code{\link{predict}}, \code{\link{calculate_S_pMSE_numeric}}
#'
#' @examples
#' org <- c("A", "B", "A", "C", NA)
#' syn <- c("A", "B", "C", "C", "A")
#' calculate_S_pMSE(org, syn)
#'
#' @export


calculate_S_pMSE <- function(org, syn) {

  # Combine original and synthetic into one data frame
  data <- data.frame(
    value = c(org, syn),
    source = c(rep(0, length(org)), rep(1, length(syn)))
  )

   # Remove rows with any missing values
  data <- data[complete.cases(data), ]

  # Ensure categorical treatment
  data$value <- factor(data$value)

  # Fit logistic regression
  model <- glm(source ~ value, data = data, family = "binomial")

  # Predict probabilities
  probs <- predict(model, type = "response")

  # Compute pMSE
  y <- data$source
  pmse <- mean((probs - y)^2)

  # Compute standardized S_pMSE
  p <- mean(y)
  s_pmse <- pmse / (p * (1 - p))

  return(s_pmse)
}
