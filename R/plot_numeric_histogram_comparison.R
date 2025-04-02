#' Plot Overlaid Histograms for Original and Synthetic Numeric Data
#'
#' This function generates an overlaid histogram using `ggplot2` to visually compare the distribution
#' of a numeric column in the original and synthetic datasets. The histograms are plotted as density
#' estimates (`freq = FALSE` equivalent), and the original/synthetic data are color-coded for clarity.
#'
#' @param org A numeric variable from the original dataset (vector).
#' @param syn A numeric variable from the synthetic dataset (vector).
#' @param col A string representing the column name being plotted.
#'
#' @return A `ggplot` object containing the overlaid histogram plot. This can be printed or saved like any standard ggplot.
#'
#' @details The function combines the original and synthetic values into a single data frame with a `source` column
#' indicating the origin of each value. It then plots both histograms using semi-transparent fill to allow comparison.
#'
#' @seealso \code{\link[ggplot2]{geom_histogram}}, \code{\link[ggplot2]{ggplot}}, \code{\link{calculate_S_pMSE_numeric}}
#'
#' @examples
#' \dontrun{
#' # Assuming you have org and syn available:
#' dev.new()
#' print(plot_numeric_histogram_comparison(org, syn, "age"))
#' }
#'
#' @export

plot_numeric_histogram_comparison <- function(org, syn, col) {

  # Combine into one data frame
  df <- data.frame(
    value = c(org, syn),
    source = factor(c(
      rep("Original", length(org)),
      rep("Synthetic", length(syn))
    ))
  )
  spmse <- round(calculate_S_pMSE_numeric(org, syn), 3)
  bins <- calculate_fd_bins(df$value)
  x_breaks <- pretty(df$value, n = 10)
  # Create ggplot histogram
  p <- ggplot2::ggplot(df, ggplot2::aes(x = value, fill = source)) +
    ggplot2::geom_histogram(ggplot2::aes(y = ..density..), position =  ggplot2::position_dodge(), alpha = 1, bins = bins) +
    ggplot2::scale_fill_manual(values = c("steelblue", "tomato")) +
    ggplot2::scale_x_continuous(breaks = x_breaks)+
    ggplot2::labs(
      title = paste0(col, ": S_pMSE = ", spmse),
      x = col, y = "Density"
    ) +
    ggplot2::theme_minimal(base_size = 13)

  return(p)
}
