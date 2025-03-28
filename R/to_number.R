#' to_number Function: Convert factors and datetime columns to numeric
#'
#' This function takes a dataframe and converts columns of type 'factor'
#' to numeric values and columns of type 'datetime' to POSIX numeric timestamps.
#' It uses the `data_model` metadata to identify the column types.
#'
#' @param data_model A list containing the metadata of the dataset, including column names
#'        and their corresponding types. The metadata should be in the form of a dataframe with
#'        at least two columns: 'col_names' (character vector of column names) and 'col_type'
#'        (character vector of column types).
#' @param df A dataframe where columns will be converted based on the column types specified
#'           in `data_model$metadata`.
#'
#' @return A modified dataframe with 'factor' columns converted to numeric and 'datetime' columns
#'         converted to POSIX numeric timestamps.
#'
#' @details
#' - **Factors**: Converted to numeric values using `as.factor` and `as.numeric`.
#' - **Datetime**: Converted to POSIX timestamps using `as.POSIXct` and then to numeric.
#'
#' @examples
#' # Assuming `data_model` and `df` are predefined:
#' df_modified <- to_number(data_model, df)
#'
#' @keywords internal
to_number <- function(data_model,df) {
  df<-df[,colnames(data_model$data_converted)]
  # Iterate over all column names in the dataframe
  for (col in data_model$metadata$col_names) {

    # If the column type is 'factor', convert it to numeric
    if (data_model$metadata[col, 'col_type'] == 'factor') {
      df[[col]] <- as.numeric(as.factor(df[[col]]))
    }

    # If the column type is 'datetime', convert it to numeric POSIX timestamp
    if (data_model$metadata[col, 'col_type'] == 'datetime') {
      df[[col]] <- as.numeric(as.POSIXct(df[[col]], format = "%d/%m/%Y %H:%M:%OS"))
    }

    if (data_model$metadata[col, 'col_type'] == 'date') {
      df[[col]] <- as.numeric(as.POSIXct(df[[col]], format = "%d/%m/%Y"))
    }
  }

  # Return the modified dataframe
  return(df)
}
