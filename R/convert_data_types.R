
#' Convert Data Types in a Data Frame
#'
#' This function ensures that the columns in a given data frame (`df`) are converted
#' to the appropriate data types based on a reference structure (`metadata`).
#'
#' @param df A data frame whose columns data type need to be converted
#' @param metadata A reference data frame structure containing column names (`col_names`)
#' and their corresponding data types (`col_type`).
#'
#' @return A data frame with standardized column data types.
#'
#' @examples
#' df <- data.frame(
#'   category = c("A", "B", "A", "C"),
#'   timestamp = c("01/01/2023 12:00:00", "02/01/2023 14:30:00",
#'                 "03/01/2023 09:45:00", "04/01/2023 16:20:00")
#' )
#' metadata <- data.frame(
#'   col_names = c("category", "timestamp"),
#'   col_type = c("factor", "datetime")
#' )
#' df_converted <- convert_data_types (df, metadata)
#'
#' @keywords internal
convert_data_types <- function(df, metadata) {
  # Loop through each column defined in metadata
  metadata_for_syn<-metadata %>% filter( !(col_type %in% c('text','key') ))
  for (i in seq_along(metadata_for_syn$col_names)) {
    col_name <- metadata_for_syn$col_names[i]  # Extract column name
    col_type <- metadata_for_syn$col_type[i]   # Extract expected data type

    # Convert columns to factors if their type is 'factor'
    if (col_type =="factor") {
      df[[col_name]] <- as.factor(df[[col_name]])
    }

    # Convert datetime columns to numeric POSIX timestamp
    if (col_type == "datetime") {
      #df[[col_name]] <- as.numeric(as.POSIXct(df[[col_name]], format = "%d/%m/%Y %H:%M:%OS"))
      df[[col_name]] <-as.POSIXct(df[[col_name]], tryFormats = c("%d/%m/%Y %H:%M:%OS","%Y-%m-%d %H:%M:%OS"))
    }
    if (col_type == "date") {
      #df[[col_name]] <- as.numeric(as.POSIXct(df[[col_name]], format = "%d/%m/%Y %H:%M:%OS"))
      df[[col_name]] <-as.POSIXct(df[[col_name]], tryFormats = c("%d/%m/%Y", "%Y-%m-%d"))
    }
  }

  # Return the modified data frame with standardized data types
  return(df[,metadata_for_syn$col_names])
}
