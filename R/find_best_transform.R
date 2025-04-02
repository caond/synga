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

  methods <- list(
    beta = function(x) {

      idx <- !is.na(x)
      x_non_na <- x[idx]

      # Basic checks
      if (length(x_non_na) < 5) return(NULL)

      min_x <- min(x_non_na)
      max_x <- max(x_non_na)
      range_x <- max_x - min_x
      if (range_x < EPS) return(NULL)

      # Scale to (0, 1)
      x_scaled <- (x_non_na - min_x) / (range_x + EPS)

      # Add artificial endpoints to help qbeta stay close to min/max
      x_scaled_fit <- c(0, 1, x_scaled)

      # Fit Beta distribution
      fit <- try(fitdistrplus::fitdist(x_scaled_fit, "beta"), silent = TRUE)
      if (inherits(fit, "try-error")) return(NULL)

      shape1 <- fit$estimate["shape1"]
      shape2 <- fit$estimate["shape2"]
      p_na <- mean(is.na(x))

      # Transform non-NA values
      transformed <- rep(NA_real_, length(x))
      transformed[idx] <- p_na + (1 - p_na) * pbeta(x_scaled, shape1, shape2)

      # Encode NA as exactly p_na in uniform space (fixed, reserved slice)
      transformed[!idx] <- p_na

      # Return model info and transform
      list(
        method = "beta",
        transformed = transformed,
        params = list(
          shape1 = shape1,
          shape2 = shape2,
          min_x = min_x,
          max_x = max_x,
          p_na   = p_na
        ),
        aic = fit$aic,
        ks  = ks.test(pbeta(x_scaled, shape1, shape2), "punif")$statistic
      )

    },
    normal = function(x) {
      idx <- !is.na(x)
      x_non_na <- x[idx]

      # Basic checks
      if (length(x_non_na) < 5) return(NULL)

      mean_x <- mean(x_non_na)
      sd_x   <- sd(x_non_na)
      if (sd_x < EPS) return(NULL)  # Flat data

      p_na <- mean(is.na(x))

      # Transform
      transformed <- rep(NA_real_, length(x))
      transformed[idx] <- p_na + (1 - p_na) * pnorm(x[idx], mean_x, sd_x)
      transformed[!idx] <- p_na  # encode missing at exact p_na

      # Return model and metadata
      list(
        method = "normal",
        transformed = transformed,
        params = list(
          mean = mean_x,
          sd   = sd_x,
          p_na = p_na
        ),
        aic = NA,  # optional: not using AIC here
        ks  = ks.test(pnorm(x_non_na, mean_x, sd_x), "punif")$statistic
      )
    },
    gamma = function(x) {
      idx <- !is.na(x)
      x_non_na <- x[idx]
      p_na <- mean(is.na(x))

      if (length(x_non_na) < 5) return(NULL)

      # Gamma requires positive values → shift if needed
      shift <- 0
      if (min(x_non_na) <= 0) {
        shift <- abs(min(x_non_na)) + EPS
        x_non_na <- x_non_na + shift
      }

      # Fit gamma distribution
      fit <- try(fitdistrplus::fitdist(x_non_na, "gamma"), silent = TRUE)
      if (inherits(fit, "try-error")) return(NULL)

      shape <- fit$estimate["shape"]
      rate  <- fit$estimate["rate"]

      # Transform
      transformed <- rep(NA_real_, length(x))
      x_shifted <- x + shift  # apply shift to full x

      transformed[idx] <- p_na + (1 - p_na) * pgamma(x_shifted[idx], shape, rate)
      transformed[!idx] <- p_na  # encode NA at fixed point

      list(
        method = "gamma",
        transformed = transformed,
        params = list(
          shape = shape,
          rate  = rate,
          shift = shift,
          p_na  = p_na
        ),
        aic = fit$aic,
        ks  = ks.test(pgamma(x_non_na, shape, rate), "punif")$statistic
      )
    }
  )

  best_fit <- NULL
  best_ks <- Inf
  best_aic <- Inf
  x_non_na <- x[!is.na(x)]
  if (length(x_non_na)>5){
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
  }

  # Fit ECDF and compute KS statistic
  fun <- ecdf_numeric(x)
  ecdf_transformed <- fun(x)
  ecdf_ks <- ks.test(ecdf_transformed, "punif")$statistic


  # If no parametric model fits well, use ECDF
  if (length(x_non_na)/length(x)<0.7 || length(x_non_na)<30 || is.null(best_fit)|| ecdf_ks < best_ks ) {
    best_fit <- list(
      method = "ecdf_numeric",
      params = fun,
      transformed =ecdf_transformed,
      ks = ecdf_ks,
      aic = NA
    )
  }

  return(best_fit)

}
