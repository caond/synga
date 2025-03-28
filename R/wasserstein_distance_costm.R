#' Compute Wasserstein Distance between Two Datasets
#'
#' This function calculates the **Wasserstein distance** (or **Earth Mover's Distance**) between two datasets (`df1` and `df2`) based on their pairwise Euclidean distances. The Wasserstein distance is a measure of the "cost" required to transform one distribution into another. This function is commonly used to compare two datasets in terms of their distributions.
#'
#' @param df1 A numeric data frame representing the first dataset.
#' @param df2 A numeric data frame representing the second dataset.
#'
#' @return A numeric value representing the **Wasserstein distance** between the two datasets.
#'
#' @details
#' The function calculates the **Wasserstein distance** between the two datasets by:
#' 1. **Converting the datasets to matrices** (`df1` and `df2`) for compatibility with distance functions.
#' 2. **Computing the pairwise cost matrix** using the **Euclidean distance** between all pairs of observations from `df1` and `df2`.
#' 3. Using the **Wasserstein** transport method from the `transport` package, the function calculates the optimal "flow" needed to transform the distribution of `df1` into the distribution of `df2` based on the pairwise distances.
#'
#' The **Wasserstein distance** is a useful metric in many applications like comparing distributions, generating synthetic data, and evaluating how well one dataset approximates another.
#'
#' @keywords internal
wasserstein_distance_costm<-function(df1,df2){
  # Convert data frames to matrices
  mat1 <- as.matrix(df1)
  mat2 <- as.matrix(df2)
  # Compute the pairwise cost matrix using Euclidean distance
  cost_matrix <- as.matrix(dist(rbind(mat1, mat2)))[1:nrow(mat1), (nrow(mat1) + 1):(nrow(mat1) + nrow(mat2))]
  # Compute Wasserstein distance
  wd <- transport::wasserstein(rep(1/nrow(mat1), nrow(mat1)), rep(1/nrow(mat2), nrow(mat2)), costm = cost_matrix, p = 2)
  return(wd)
}
