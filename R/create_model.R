#' Create a Data Transformation Model
#'
#' This function takes a data frame (`df`) and applies a series of transformations
#' to prepare the data for modeling. It generates metadata, converts data types,
#' fits empirical CDFs, and transforms the data into a uniform [0, 1] space.
#'
#' @param df A data frame containing the original dataset to be processed.
#' @param data_type A named list specifying column data type overrides: factor, datetime, date, integer, numeric, text, key
#'
#' @return A list containing the following components:
#'   - `original`: The original input data frame.
#'   - `metadata`: A data frame describing column types after applying overrides.
#'   - `data_converted`: The input data frame with converted data types according to metadata.
#'   - `transformed`: A list of distribution functions for each column.
#'   - `data_uniform`: The data transformed into a uniform [0, 1] space based on transform functions.
#'
#' @examples
#' library(synga)
#' ed_model <- create_model(ed,
#'   data_type<-list(
#'            event_id="key",
#'            patient_name="text",
#'            establishment_code = "factor",
#'            arrival_datetime = "datetime",
#'            sex = "factor",
#'            ethnicity = "factor",
#'            triage_category = "factor",
#'            mode_of_arrival = "factor",
#'            presenting_complaint = "factor",
#'            admission = "factor",
#'            attendance_length_of_episode = "integer"
#'         )
#' )
#'
#' @export
create_model <- function(df, data_type,na.rm=FALSE,na.rules=TRUE,n_core=1) {

  allowed_types <- c("factor", "date","datetime", "date", "integer", "numeric", "text", "key")
  invalid_types <- unlist(data_type)[!unlist(unlist(data_type)) %in% allowed_types]
  if (length(invalid_types) > 0) {
      stop(paste("Error: data types should be one of the followings:", paste(allowed_types, collapse = ", ")))
  }


  if (na.rm)
  {
    na.rules<-FALSE
    # make sure the data is complete
    df <- df[complete.cases(df), ]
  }

  # Step 1: Create metadata describing column types, with user-specified overrides.
  metadata <- create_metadata(df, overrides = data_type)

  # remove key,text from metadata
  #metadata_for_syn<-metadata %>% filter( !(col_type %in% c('text','key') ))

  # Step 2: Convert data types based on the generated metadata.
  data_converted <- convert_data_types(df, metadata)

  # Step 3: checksum
  #checksums <- apply(data_converted, 1, function(row) digest::digest(paste(row, collapse = ""), algo = "md5"))

  # Step 4: find best distribution or empirical cumulative distribution functions (ECDF) for each column.
  transformed <- transform_model(data_converted, metadata)


  # Step 5: collect data
  data_uniform <- as.data.frame(setNames(lapply(names(transformed), function(col_name) {
                    col <- transformed[[col_name]]$transformed
                  }), names(transformed)))  # Assign original column names to the transformed data frame

  if (na.rules)
  {
    rules<-detect_na_implications(data_converted,n_core)
  }else{
    rules<-NA
  }



  # Return a structured list containing all transformation results.
  return(list(
    original = df,
    metadata = metadata,
    data_converted = data_converted,
    transformed = transformed,
    rules=rules,
    data_uniform = data_uniform
  ))
}
