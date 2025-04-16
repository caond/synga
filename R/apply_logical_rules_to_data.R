#' Detect Logical Implication Rules Between Columns
#'
#' This function identifies logical implication rules of the form:
#' \code{if A == x (or A <= x) ⇒ B == y}, or \code{if is.na(A) ⇒ B == y}.
#' It supports both categorical and numeric predictors and can detect
#' high-confidence (default 100\%) conditional relationships between columns
#' in a data.frame.
#'
#' @param df A data.frame containing the input dataset.
#' @param n_core Number of CPU cores to use for parallel rule detection. Default is 1 (no parallelism).
#'
#' @return A list of detected logical rules. Each rule is represented as a list with the following fields:
#' \describe{
#'   \item{\code{if_column}}{The conditioning variable (left-hand side of the rule).}
#'   \item{\code{if_value}}{The specific value or threshold for \code{if_column}.}
#'   \item{\code{if_operator}}{Logical operator used: \code{"=="} for categorical, \code{"<="} for numeric, or \code{"is.na"}.}
#'   \item{\code{then_column}}{The dependent variable (right-hand side of the rule).}
#'   \item{\code{then_value}}{The most frequent value of \code{then_column} under the condition.}
#'   \item{\code{confidence}}{The proportion of cases where the rule holds. Rules are only returned if confidence == 1.}
#'   \item{\code{support}}{The number of rows that satisfy the condition in \code{if_column}.}
#' }
#'
#' @details
#' For numeric columns, the function generates thresholds based on decile quantiles (10\%, ..., 90\%) to evaluate
#' rules like \code{A <= x ⇒ B == y}. For categorical columns, all unique values are tested. If the rule holds
#' with 100\% confidence and at least 10 supporting rows, it is returned.
#'
#' @examples
#' \dontrun{
#' rules <- detect_all_logical_rules(df)
#' str(rules[[1]])
#' }
#'
#' @export

apply_logical_rules_to_data <- function(df, rules) {
  for (rule in rules) {
    a <- rule$if_column
    a_val <- rule$if_value
    op <- if (!is.null(rule$if_operator)) rule$if_operator else "=="
    b <- rule$then_column
    b_val <- rule$then_value

    # Identify rows where condition is TRUE
    idx <- rep(FALSE, nrow(df))
    if (op == "==") {
      if (is.na(a_val)) {
        idx <- is.na(df[[a]])
      } else {
        idx <- df[[a]] == a_val
      }
    } else if (op == "<=") {
      idx <- !is.na(df[[a]]) & df[[a]] <= a_val
    }

    # Apply the rule to then_column
    if (is.na(b_val)) {
      df[[b]][idx] <- NA
    } else {
      df[[b]][idx] <- b_val
    }
  }

  return(df)
}
