#' Transform a DataFrame Using ECDF or Best-Fit Distributions
#'
#' This function transforms each numeric column in a dataframe using the best-fit
#' parametric distribution (Beta, Normal, Gamma) or ECDF if no parametric model fits well.
#' Categorical columns are transformed using a custom ECDF function.
#'
#' @param df A dataframe containing the data to be transformed.
#' @param metadata A dataframe containing column metadata, including:
#'   - `col_names`: Names of the columns to transform.
#'   - `col_type`: Type of each column ("numeric" or "factor").
#' @return A named list of transformations for each column. Each entry contains:
#'   - `method`: Chosen transformation method ("beta", "normal", "gamma", "ecdf", or "ecdf_category").
#'   - `params`: Parameters of the selected distribution (for inverse transformation).
#'   - `transformed`: The transformed data in [0,1] scale.
#'   - `aic`: Akaike Information Criterion (for parametric fits, NA for ECDF).
#'   - `ks`: Kolmogorov-Smirnov statistic measuring the uniformity of the transformation.
#'
#' @examples
#' df <- data.frame(A = rnorm(100), B = rgamma(100, shape = 2))
#' metadata <- data.frame(col_names = c("A", "B"), col_type = c("numeric", "numeric"))
#' transformed_data <- transform_model(df, metadata)
#' print(transformed_data$A$method)  # Output: "normal", "gamma", "beta", or "ecdf"

transform_model <- function(df, metadata) {
  metadata_for_syn<-metadata %>% filter( !(col_type %in% c('text','key') ))


  setNames(lapply(metadata_for_syn$col_names, function(col_name) {

    # Determine the column type from metadata
    col_type <- metadata_for_syn[col_name, 'col_type']

    # Extract the column data
    col <- df[[col_name]]

    # Apply ECDF transformation based on column type
    if (col_type != "factor") {
      best_fit <- find_best_transform(as.numeric(col))  # fit the best distribution or Standard ECDF for numerical data
    } else {
      fun <- ecdf_categorical(col)  # Custom ECDF for categorical data
      # Apply the ECDF function to transform data into a uniform distribution [0,1]
      transformed<-fun(col)
      best_fit<-list(method="ecdf_category",fun=fun,transformed=transformed)
    }

    # Store the transform function
    return(best_fit)

  }), metadata_for_syn$col_names)  # Assign column names to the output list
}

