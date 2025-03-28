#' Internal List of Distance Functions
#'
#' This list contains various distance functions used in the package.
#' It is intended for internal use.
#'
#' @keywords internal
.onLoad <- function(libname, pkgname) {
  #library(dplyr)

  distance_functions <<- list(
    wasserstein = wasserstein_distance_costm,
    frobenius = frobenius_distance,
    auc = auc_distance
  )
}
