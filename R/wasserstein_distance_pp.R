#' Compute Wasserstein Distance using Point Patterns
#'
#' This function calculates the **Wasserstein distance** between two datasets (`df1` and `df2`) using **point patterns**. The function converts the data into point patterns and computes the Wasserstein distance between their respective distributions. The Wasserstein distance measures the minimal "cost" of transforming one point pattern distribution into another.
#'
#' @param df1 A numeric data frame representing the first dataset.
#' @param df2 A numeric data frame representing the second dataset.
#'
#' @return A numeric value representing the **Wasserstein distance** between the point patterns of the two datasets.
#'
#' @details
#' The function calculates the **Wasserstein distance** by:
#' 1. **Resampling the datasets**: If one dataset has more rows than the other, the larger dataset is randomly resampled (without replacement) to match the number of rows in the smaller dataset.
#' 2. **Converting data to point patterns**: The function uses the `pp()` function to convert the datasets (`df1` and `df2`) into point patterns. This process involves treating the data as spatial points in a coordinate space.
#' 3. **Computing the Wasserstein distance**: The **Wasserstein distance** is then calculated between the two point patterns using the `wasserstein()` function, which measures the minimal cost required to transform one point pattern into another.
#'
#' The **Wasserstein distance** is commonly used in spatial statistics and topological data analysis to compare point distributions or spatial patterns.
#'
#' @keywords internal
wasserstein_distance_pp<-function(df1,df2){
  # Convert data frames to matrices
  if (nrow(df1) < nrow(df2)) {
    df2_resampled <- df2[sample(1:nrow(df2), nrow(df1), replace = FALSE), ]
    pp1 <- pp(df1)
    pp2 <- pp(df2_resampled)
  } else {
    df1_resampled <- df1[sample(1:nrow(df1), nrow(df2), replace = FALSE), ]
    pp1 <- pp(df1_resampled)
    pp2 <- pp(df2)
  }
  wd<-wasserstein(pp1, pp2)
  return(wd)
}
