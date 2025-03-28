#' Example Dataset: ED Data for synga Package
#'
#' This dataset contains example synthetic data for testing and demonstrating
#' the functionality of the synga package.
#'
#' @format A data frame with 10000 rows and 9 variables:
#' \describe{
#'   \item{establishment_code}{factor variable}
#'   \item{arrival_datetime}{datetime variable}
#'   \item{sex}{factor variable}
#'   \item{ethnicity}{factor variable}
#'   \item{triage_category}{factor variable}
#'   \item{mode_of_arrival}{factor variable}
#'   \item{presenting_complaint}{factor variable}
#'   \item{admission}{factor variable}
#'   \item{attendance_length_of_episode}{integer variable}
#' }
#' @source DOH ED Synthetic data
#'
#' @examples
#' data(ed)
#' head(ed)
"ed"
