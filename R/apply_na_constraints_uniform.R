#' Apply Conditional NA Rules in Uniform Space Before Inverse Transformation
#'
#' This function enforces structural NA constraints (e.g., if A is NA, then B must also be NA)
#' within the uniform [0,1] space of copula-based synthetic data before the inverse CDF transformation.
#' It uses `data_model$rules` to detect which synthetic values of column \code{a} fall below the \code{p_na} threshold
#' and sets the corresponding entries in column \code{b} to \code{NA}.
#'
#' @param synthetic_uniform A data.frame of synthetic values in uniform [0,1] scale (pre-inverse transformation).
#' @param data_model The original data model object used for synthesis, containing:
#' \itemize{
#'   \item \code{rules}: A list of NA constraint rules with \code{if_column} and \code{then_column}.
#'   \item \code{transformed}: A list with transformation metadata including \code{p_na} for each variable.
#' }
#'
#' @return The modified \code{synthetic_uniform} data.frame with NA values enforced based on constraint rules.
#'
#' @details
#' This logic should be applied just after generating uniform synthetic samples but before applying
#' any inverse transformation (e.g., empirical quantile mapping or ECDF back-conversion).
#' It ensures conditional NA dependencies are respected during synthetic generation without distorting the marginals.
#'
#' @examples
#' \dontrun{
#' # Enforce rules like: if Age_of_death is NA ⇒ Cause_of_death is NA
#' synthetic_uniform <- apply_na_constraints_uniform(synthetic_uniform, data_model)
#' }
#'
#' @export

apply_na_constraints_uniform <- function(synthetic_uniform, data_model) {

  for (r in 1:length(data_model$rules)) {
    cat('\n',r)
    rule<-data_model$rules[[r]]
    a <- rule$if_column
    b <- rule$then_column


    # Handle only if both columns exist in the synthetic uniform data
    if (a %in% names(synthetic_uniform) && b %in% names(synthetic_uniform)) {
      a_uniform_vals <- synthetic_uniform[[a]]
      a_will_be_na <- a_uniform_vals < attr(data_model$transformed[[a]]$params,'p_na')
      # Enforce: if A will be NA, then B must also be NA
      synthetic_uniform[[b]][a_will_be_na] <- NA
    }
  }

  return(synthetic_uniform)
}
