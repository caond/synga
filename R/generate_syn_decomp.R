#' Generate Synthetic Data Using Decomposition Methods
#'
#' This function generates synthetic data from a given data model using either
#' Cholesky or eigen decomposition, depending on numerical stability conditions.
#'
#' @param data_model A list containing the original data, transformed uniform data,
#'   and metadata. The uniform data should be in [0,1] scale.
#' @param n_samples Integer. Number of synthetic samples to generate. If NA, defaults
#'   to the number of rows in `data_model$original`.
#'
#' @return A data frame containing synthetic data in [0,1] uniform scale.
#'
#' @details
#' The function follows these steps:
#' \enumerate{
#'   \item Converts input data to a normal space using the quantile function.
#'   \item Computes the correlation matrix of the transformed data.
#'   \item Ensures the correlation matrix is positive definite using `nearPD` if necessary.
#'   \item Selects the decomposition method:
#'     \itemize{
#'       \item Eigen decomposition is used if the correlation matrix has a high condition
#'             number, is large, close to singular, or still not positive definite.
#'       \item Otherwise, Cholesky decomposition is used.
#'     }
#'   \item Generates synthetic data using random noise and the chosen decomposition method.
#'   \item Transforms synthetic data back to a uniform scale using the normal CDF.
#' }
#'
#' @note This function applies `nearPD` to adjust the correlation matrix if it is not
#'   positive definite, ensuring numerical stability. If eigen decomposition is used,
#'   non-negative eigenvalues are enforced.
#'
#' @examples
#' \dontrun{
#'   synthetic_data <- generate_syn_decomp(data_model)
#' }
#'
#' @export
generate_syn_decomp <- function(data_model, n_samples = NA) {
  if (is.na(n_samples)) n_samples <- NROW(data_model$original)
  # Detect columns with 0 variance (only 1 unique value after removing NAs)
  is_constant <- apply(data_model$data_uniform, 2, function(col) {
    length(unique(na.omit(col))) == 1
  })

  data_uniform_reduced <- data_model$data_uniform[, !is_constant, drop = FALSE]

  cdf_data <- pmin(pmax(as.matrix(data_uniform_reduced), EPS), 1 - EPS)
  z_data <- apply(cdf_data, 2, qnorm)
  cor_norm <- cor(z_data, use = "pairwise.complete.obs")

  if (!matrixcalc::is.positive.definite(cor_norm)) {
    cor_norm <- as.matrix(Matrix::nearPD(cor_norm)$mat)
    warning("Correlation matrix was not positive definite. NearPD correction applied.")
  }

  sds <- sqrt(diag(var(z_data, na.rm = TRUE)))
  cats <- data_model$metadata$col_names[data_model$metadata$col_type %in% c('factor','integer','date')]
  sds[cats] <- 1
  means <- colMeans(z_data, na.rm = TRUE)
  means[cats] <- 0
  random_noise <- matrix(rnorm(n_samples * ncol(data_uniform_reduced)), n_samples, ncol(data_uniform_reduced))

  # Checks for conditions to use eigen decomposition
  use_eigen <- FALSE

  # Better matrix diagnostics
  if (kappa(cor_norm) > 1e6) {
    use_eigen <- TRUE
    warning("High condition number")
  } else if (nrow(cor_norm) > 500) {
    use_eigen <- TRUE
    warning("Large correlation matrix")
  } else if (!matrixcalc::is.positive.definite(cor_norm)) {
    use_eigen <- TRUE
    warning("Correlation matrix is not positive definite")
  }

  if (use_eigen) {
    # Eigen decomposition
    eigen_decomp <- eigen(cor_norm)
    eigenvalues <- eigen_decomp$values
    eigenvectors <- eigen_decomp$vectors

    # Ensure eigenvalues are non-negative
    eigenvalues <- pmax(eigenvalues, 0)
    synthetic_z_data <- t(eigenvectors %*% diag(sqrt(eigenvalues)) %*% t(random_noise) * sds + means)
  } else {
    # Cholesky decomposition
    m_chol <- chol(cor_norm)
    synthetic_z_data <- t(t(m_chol) %*% t(random_noise) * sds + means)
  }

  const_values <- sapply(data_model$data_uniform[, is_constant, drop = FALSE], function(col) unique(na.omit(col)))
  const_matrix <- as.data.frame(matrix(rep(const_values, each = n_samples), nrow = n_samples))
  colnames(const_matrix) <- names(const_values)

  synthetic_uniform <- apply(synthetic_z_data, 2, pnorm)
  synthetic_uniform <- as.data.frame(synthetic_uniform)
  colnames(synthetic_uniform) <- colnames(data_uniform_reduced)
  synthetic_uniform_full <- cbind(synthetic_uniform, const_matrix)
  synthetic_uniform_full <- synthetic_uniform_full[, colnames(data_model$data_uniform)]

  return(convert_back_original(synthetic_uniform_full))
}
