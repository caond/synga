#' Transform a Numeric Vector to a Uniform Distribution
#'
#' This function fits a given numeric vector to multiple probability distributions
#' (Beta, Normal, and Gamma) and transforms the data to a uniform scale [0,1].
#' The best transformation is selected based on the Kolmogorov-Smirnov (KS) statistic.
#' If no parametric model fits well, the empirical cumulative distribution function (ECDF) is used.
#'
#' @param x A numeric vector to be transformed.
#' @return A list containing:
#'   - `method`: The selected transformation method ("beta", "normal", "gamma", or "ecdf").
#'   - `params`: Parameters of the selected distribution (for inverse transformation).
#'   - `transformed`: The transformed data in [0,1] scale.
#'   - `aic`: The Akaike Information Criterion (AIC) of the fit (NA for ECDF).
#'   - `ks`: The KS statistic measuring the uniformity of the transformation.
#'
#' @examples
#' x <- rgamma(100, shape = 2, rate = 1)  # Generate example data
#' best_fit <- transform_to_uniform(x)
#' print(best_fit$method)  # Output: "gamma", "beta", "normal", or "ecdf"
#' @importFrom fitdistrplus fitdist
find_best_transform <- function(x) {
  eps <- 1e-6  # Small constant to avoid division by zero
  methods <- list(
    beta = function(x) {

      x_scaled <- (x - min(x)) / (max(x) - min(x) + eps)  # Scale to (0,1)

      fit <- try(fitdistrplus::fitdist(x_scaled, "beta"), silent = TRUE)
      if (inherits(fit, "try-error")) return(NULL)
      transformed<- pbeta(x_scaled, fit$estimate["shape1"], fit$estimate["shape2"])
      ks_stat <- ks.test(transformed, "punif")$statistic

      list(
        method = "beta",
        params = list(shape1 = fit$estimate["shape1"], shape2 = fit$estimate["shape2"],
                      min_x = min(x), max_x = max(x)),  # Store original min/max for inverse transform
        transformed=transformed,
        aic = fit$aic,
        ks=ks_stat
      )
    },
    normal = function(x) {
      fit <- try(fitdistrplus::fitdist(x, "norm"), silent = TRUE)
      if (inherits(fit, "try-error")) return(NULL)
      transformed <- pnorm(x, fit$estimate["mean"], fit$estimate["sd"])
      ks_stat <- ks.test(transformed, "punif")$statistic

      list(
        method = "normal",
        params = list(mean = fit$estimate["mean"], sd = fit$estimate["sd"]),
        transformed = transformed,
        aic = fit$aic,
        ks = ks_stat
      )
    },
    gamma = function(x) {
      if (any(x <= 0)) {  # Shift if data contains non-positive values
        shift <- abs(min(x)) + eps  # Small shift to make all values positive
        x_shifted <- x + shift
      } else {
        shift <- 0
        x_shifted <- x
      }

      fit <- try(fitdistrplus::fitdist(x_shifted, "gamma"), silent = TRUE)
      if (inherits(fit, "try-error")) return(NULL)

      transformed <- pgamma(x_shifted, fit$estimate["shape"], fit$estimate["rate"])
      ks_stat <- ks.test(transformed, "punif")$statistic

      list(
        method = "gamma",
        params = list(shape = fit$estimate["shape"], rate = fit$estimate["rate"], shift = shift),
        transformed = transformed,
        aic = fit$aic,
        ks = ks_stat
      )
    }
  )

  best_fit <- NULL
  best_ks <- Inf
  best_aic <- Inf

  for (m in names(methods)) {
    res <- methods[[m]](x)
    if (is.null(res)) next  # Skip if fitting failed

    # Prioritize lowest KS statistic, use AIC as a tiebreaker
    if (res$ks < best_ks || (res$ks == best_ks && res$aic < best_aic)) {
      best_ks <- res$ks
      best_aic <- res$aic
      best_fit <- res
    }
  }


  # Fit ECDF and compute KS statistic
  ecdf_fit <- ecdf(x)
  ecdf_transformed <- ecdf_fit(x)
  ecdf_ks <- ks.test(ecdf_transformed, "punif")$statistic


  # If no parametric model fits well, use ECDF
  if (is.null(best_fit)|| ecdf_ks < best_ks ) {
    best_fit <- list(
      method = "ecdf",
      params = ecdf_fit,
      transformed =ecdf_transformed,
      ks = ecdf_ks,
      aic = NA
    )
  }

  return(best_fit)

}
