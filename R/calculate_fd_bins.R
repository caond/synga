#' Calculate Optimal Number of Histogram Bins using Freedman–Diaconis Rule
#'
#' This function computes the optimal number of histogram bins using the
#' Freedman–Diaconis rule, which adapts the bin width based on the data’s
#' interquartile range (IQR) and sample size.
#'
#' The Freedman–Diaconis bin width is calculated as:
#' \deqn{h = 2 \cdot \text{IQR}(x) / n^{1/3}}
#' where \code{n} is the number of observations (non-NA), and the number of bins is:
#' \deqn{\text{bins} = \lceil(\max(x) - \min(x)) / h\rceil}
#'
#' @param x A numeric vector from which to calculate the number of bins.
#'
#' @return An integer giving the recommended number of histogram bins.
#'
#' @details This rule is robust to outliers and adapts better than Sturges' rule for non-normal data.
#' A minimum of 2 observations is required; otherwise the function returns a fallback value of 10.
#'
#' @seealso \code{\link{geom_histogram}}, \code{\link[graphics]{hist}}
#'
#' @examples
#' x <- rnorm(100)
#' bins <- calculate_fd_bins(x)
#' hist(x, breaks = bins)
#'
#' @export


calculate_fd_bins <- function(x) {
  x <- x[!is.na(x)]
  n <- length(x)
  if (n < 2) return(10)  # fallback if too few values

  iqr <- IQR(x)
  bin_width <- 2 * iqr / n^(1/3)
  bin_width <- max(bin_width, .Machine$double.eps)  # avoid divide by zero
  bins <- ceiling((max(x) - min(x)) / bin_width)
  return(bins)
}
