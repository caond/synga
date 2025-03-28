#' benchmark_cor Function: Compares the correlation structures between two datasets
#'
#' This function compares the correlation structures of two datasets by computing the correlation matrices and visualizing their differences.
#' It calculates the Frobenius norm difference, Wasserstein distance, logistic AUC, and the absolute correlation differences,
#' visualizing them in a heatmap with statistical summaries.
#'
#' @param data_model A list containing metadata for the original dataset, including the converted data (`data_converted`).
#' @param df A dataset to compare against the original dataset, typically a synthetic or altered version.
#'
#' @return The function does not return a value. It generates a heatmap displaying the correlation differences between the two datasets,
#' along with Frobenius norm, Wasserstein distance, and logistic AUC values as text annotations.
#'
#' @details
#' - **Frobenius norm**: Measures the difference between the correlation matrices using a matrix norm.
#' - **Wasserstein distance**: A measure of the distance between the joint distributions of the two datasets.
#' - **Logistic AUC**: The Area Under the Curve (AUC) of the logistic regression model used to compare the datasets.
#' - **Heatmap**: Displays the absolute difference between the correlation matrices, with statistics overlaid.
#'
#' @examples
#' # Assuming data_model is a list with metadata and converted data, and df is a synthetic dataset:
#' @seealso \code{\link{create_model}}
#' benchmark_cor(data_model, df)
#' @export
benchmark_cor <- function(data_model, df) {

  # get original data

  df1<-to_number(data_model,data_model$data_converted)
  df2<-to_number(data_model,df)

  # Compute correlation matrices for both datasets
  cor1 <- cor(df1, use = "pairwise.complete.obs")
  cor2 <- cor(df2, use = "pairwise.complete.obs")

  # calculating synthetic checksum
  synthetic_checksum<-  apply(df, 1, function(row) digest::digest(paste(row, collapse = ""), algo = "md5"))
  #check if any rows in the original data are found in the synthetic
  identical_rows<-sum(data_model$checksum %in% synthetic_checksum)
  # Compute joint distribution metrics (e.g., Wasserstein distance, logistic AUC)
  auc <- auc_distance(df1,df2)
  frobenius <- frobenius_distance(df1,df2)

  # Compute the absolute difference between correlation matrices
  cor_diff <- abs(cor1 - cor2)

  # Remove the lower triangle of the matrix (including the diagonal) for visualization
  cor_diff[lower.tri(x = cor_diff, diag = TRUE)] <- NA

  # Set up plot parameters
  par(mar = c(2, 2, 2, 2), bg = "white", mfcol = c(1, 1), cex = 0.50)

  # Generate a heatmap to visualize correlation differences
  heatmap(cor_diff, Rowv = NA, Colv = NA, revC = TRUE, symm = TRUE, col = rev(heat.colors(9)))

  # Add a legend displaying min, median, and max correlation differences
  legend(
    x = "top",
    legend = c(
      paste(round(min(cor_diff, na.rm = TRUE), 2), "(min)"),
      paste(round(median(cor_diff, na.rm = TRUE), 2), "(median)"),
      paste(round(max(cor_diff, na.rm = TRUE), 2), "(max)")
    ),
    fill = rev(heat.colors(9))[c(1, 5, 9)],
    cex = 0.70
  )

  # Get current plot coordinates
  usr <- par("usr")

  # Draw a rectangular box in the plot for summary statistics
  rect(
    usr[1] + 0.4 * (usr[2] - usr[1]),
    usr[3] + 0.75 * (usr[4] - usr[3]),
    usr[1] + 0.6 * (usr[2] - usr[1]),
    usr[3] + 0.85 * (usr[4] - usr[3]),
    border = "black", lwd = 1
  )

  # Add text annotations for Frobenius norm, Wasserstein distance, and logistic AUC
  text(
    x = usr[1] + 0.5 * (usr[2] - usr[1]),
    y = usr[3] + 0.83 * (usr[4] - usr[3]),
    labels = paste("Frobenius:", round(frobenius, 2)),
    cex = 0.7
  )
  text(
    x = usr[1] + 0.5 * (usr[2] - usr[1]),
    y = usr[3] + 0.80 * (usr[4] - usr[3]),
    labels = paste("AUC:", round(auc, 2)),
    cex = 0.7
  )
  text(
    x = usr[1] + 0.5 * (usr[2] - usr[1]),
    y = usr[3] + 0.77 * (usr[4] - usr[3]),
    labels = paste("Identical rows:", identical_rows),
    cex = 0.7
  )
}
