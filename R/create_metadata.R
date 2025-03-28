
#' Create a Data Frame Structure Definition
#'
#' This function generates a structure definition for a given data frame (`df`).
#' It automatically detects column types and allows for manual overrides.
#'
#' @param df A data frame for which the structure is to be created.
#' @param overrides A named list of column types to override default detections (optional).
#'
#' @return A data frame with two columns: `col_names` (column names) and `col_type` (detected or overridden data types).
#'
#' @examples
#' ed_struct <- create_metadata(ed,
#'   overrides = list(
#'     establishment_code = "factor",
#'     arrival_datetime = "datetime",
#'     sex = "factor",
#'     ethnicity = "factor",
#'     triage_category = "factor",
#'     mode_of_arrival = "factor",
#'     presenting_complaint = "factor",
#'     admission = "factor",
#'     attendance_length_of_episode = "integer"
#'   )
#' )
#' @keywords internal
create_metadata <- function(df, overrides = list()) {
  # Create initial structure by detecting column types
  metadata <- data.frame(
    col_names = colnames(df),
    col_type = apply(df, 2, typeof),
    stringsAsFactors = FALSE
  )

  # Apply overrides where specified
  for (col in names(overrides)) {
    if (col %in% metadata$col_names) {
      metadata[metadata$col_names == col, "col_type"] <- overrides[[col]]
    }
  }

  return(metadata)
}
