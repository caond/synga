#' Detect NA Propagation Rules Between Columns
#'
#' This function scans a data frame and detects logical rules of the form:
#' \code{if is.na(A) ⇒ is.na(B)}. These rules help identify structural dependencies
#' where the missingness in one column implies missingness in another.
#'
#' @param df A data.frame to analyze for NA propagation rules.
#' @param n_core Number of CPU cores to use for parallel processing. Default is 1 (no parallelism).
#' @param MIN_SUPPORT_PCT Minimum proportion of rows that must satisfy \code{is.na(A)} for a rule to be considered. Default is 0.1 (10\%).
#'
#' @return A list of detected rules. Each rule is a list with elements:
#' \describe{
#'   \item{\code{if_column}}{The column in which \code{NA} implies missingness elsewhere.}
#'   \item{\code{then_column}}{The dependent column that is also \code{NA} whenever \code{if_column} is \code{NA}.}
#'   \item{\code{confidence}}{The confidence of the rule, i.e., fraction of cases where \code{then_column} is also \code{NA}.}
#'   \item{\code{support}}{The number of rows where \code{if_column} is \code{NA}.}
#'   \item{\code{support_pct}}{Proportion of the data with \code{NA} in \code{if_column}.}
#' }
#'
#' @details
#' Rules are returned only if:
#' \itemize{
#'   \item The number of \code{NA} values in \code{if_column} exceeds \code{MIN_SUPPORT_PCT}
#'   \item The rule holds with \code{100\% confidence} (\code{is.na(B)} whenever \code{is.na(A)})
#' }
#'
#' This function is especially useful for maintaining NA structure in synthetic data generation pipelines.
#'
#' @examples
#' \dontrun{
#' rules <- detect_na_implications(df, MIN_SUPPORT_PCT = 0.05)
#' }
#'
#' @export

detect_na_implications<- function(df, n_core = 1, MIN_SUPPORT_PCT = 0.1) {
  CONFIDENCE <- 1
  df <- as.data.frame(df)
  columns <- names(df)
  total_rows <- nrow(df)
  MIN_SUPPORT <- ceiling(MIN_SUPPORT_PCT * total_rows)

  rule_finder <- function(a) {
    local_rules <- list()

    for (b in setdiff(columns, a)) {
      mask <- is.na(df[[a]])
      total <- sum(mask)

      if (total >= MIN_SUPPORT) {
        conf <- sum(is.na(df[[b]][mask])) / total
        if (conf >= CONFIDENCE) {
          local_rules[[length(local_rules) + 1]] <- list(
            if_column = a,
            then_column = b,
            confidence = round(conf, 3),
            support = total,
            support_pct = round(total / total_rows, 3)
          )
        }
      }
    }

    return(local_rules)
  }

  if (n_core > 1) {
    results <- foreach(a = columns, .combine = c, .packages = "base") %dopar% rule_finder(a)
  } else {
    results <- unlist(lapply(columns, rule_finder), recursive = FALSE)
  }

  return(results)
}

