
#' Compute Frobenius Distance Between Two DataFrames
#'
#' This function calculates the **Frobenius distance** between the correlation matrices of two datasets (`df1` and `df2`). The Frobenius distance is a measure of the difference between two matrices, computed as the norm of the difference between their correlation matrices.
#'
#' @param df1 A numeric data frame representing the first dataset.
#' @param df2 A numeric data frame representing the second dataset.
#'
#' @return A numeric value representing the **Frobenius distance** between the correlation matrices of the two datasets.
#'
#' @details
#' The function performs the following steps:
#' 1. **Compute the correlation matrices** for both `df1` and `df2` using pairwise complete observations.
#' 2. **Calculate the Frobenius distance**: The Frobenius distance between the two correlation matrices is computed as the **Frobenius norm** of their difference, which is the square root of the sum of the absolute squares of the elements in the difference matrix.
#' 
#' The Frobenius distance is commonly used to measure how similar or different two matrices are. In this case, it is used to measure the difference between the correlation structures of the two datasets.
#'
#' @importFrom stats cor
frobenius_distance<-function(df1,df2)
{
  
  cor_df1 <- cor(df1,use = "pairwise.complete.obs")
  cor_df2 <- cor(df2,use = "pairwise.complete.obs")
  diff <- norm(cor_df1 - cor_df2, type = "F") 
  return(diff)
  
}