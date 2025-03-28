#' bar_plot Function: Plots bar charts for categorical data
#'
#' This function generates two bar plots side by side:
#' one for the original dataset and one for the synthetic dataset.
#' It normalizes the frequency counts to proportions and adds a legend for categories.
#'
#' @param org A categorical variable from the original dataset (vector).
#' @param syn A categorical variable from the synthetic dataset (vector).
#' @param col A string representing the column name being plotted.
#'
#' @return Two bar plots: One for the original dataset and one for the synthetic dataset.
#'         The function does not return a value but generates visual output.
#'
#' @examples
#' original_data <- sample(c("A", "B", "C"), 100, replace = TRUE)
#' synthetic_data <- sample(c("A", "B", "C"), 100, replace = TRUE)
#' bar_plot(original_data, synthetic_data, "Category")
#'
#' @keywords internal
bar_plot<-function(org,syn,col){
  # Categorical Data - Barplot
  freq_table <- table(org)
  colors <- rainbow(length(freq_table))
  legend_values <- names(freq_table)

  barplot( freq_table / NROW(org), main = paste0("Original ", col), col = colors, ylim = c(0, 1.1*max(freq_table / NROW(org))))
  legend("topright", legend = legend_values, fill = colors, bty = "n")

  freq_table_syn <- table(syn)
  legend_values_syn <- names(freq_table_syn)

  barplot(freq_table_syn / NROW(syn), main = paste0("Synthetic ", col), col = colors, ylim = c(0, 1.1*max(freq_table_syn / NROW(syn))))
  legend("topright", legend = legend_values_syn, fill = colors, bty = "n")

}

