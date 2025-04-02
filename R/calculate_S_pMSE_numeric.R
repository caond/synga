#' Calculate Standardized pMSE (S_pMSE) for Numeric Variables
#'
#' This function computes the Propensity Mean Squared Error (pMSE) and its standardized version (S_pMSE),
#' to assess how distinguishable synthetic numeric data is from original numeric data.
#' It fits a logistic regression model to classify observations as synthetic or original based on numeric values.
#'
#' The standardized pMSE (S_pMSE) is defined as:
#' \deqn{S_{pMSE} = \frac{pMSE}{p(1 - p)}}
#' where \code{p} is the proportion of synthetic observations.
#'
#' @param org A numeric vector of original values.
#' @param syn A numeric vector of synthetic values (ideally same distribution domain as \code{org}).
#'
#' @return A list with two components:
#' \describe{
#'   \item{\code{pMSE}}{The raw propensity mean squared error.}
#'   \item{\code{S_pMSE}}{The standardized pMSE, normalized by the null expectation.}
#' }
#'
#' @details The function removes rows with \code{NA} using \code{complete.cases()} before fitting the model.
#' It uses logistic regression to predict whether an observation is synthetic or original based on the numeric values.
#'
#' @seealso \code{\link{glm}}, \code{\link{predict}}, \code{\link{calculate_S_pMSE}}
#'
#' @examples
#' org <- rnorm(1000)
#' syn <- rnorm(1000, mean = 0.5)
#' calculate_S_pMSE_numeric(org, syn)
#'
#' @export

calculate_S_pMSE_numeric <- function(org, syn) {
  # Combine data and add source label
  data <- data.frame(
    value = c(org, syn),
    source = c(rep(0, length(org)), rep(1, length(syn)))
  )

  # Drop rows with missing values
  data <- data[complete.cases(data), ]

  # Fit logistic regression model (numeric predictor)
  model <- glm(source ~ value, data = data, family = "binomial")

  # Predict probabilities
  probs <- predict(model, type = "response")

  # True labels
  y <- data$source

  # Compute pMSE
  pmse <- mean((probs - y)^2)

  # Compute standardized S_pMSE
  p <- mean(y)
  s_pmse <- pmse / (p * (1 - p))

  return(s_pmse)

}
