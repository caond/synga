#' plot_categorical_comparison Function: Plots bar charts for categorical data
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
#' plot_categorical_comparison(original_data, synthetic_data, "Category")
#' @keywords internal
plot_categorical_comparison<-function(org,syn,col){
  # 1. Frequency tables
  freq_org <- table(org)
  freq_syn <- table(syn)

  # 2. All unique categories
  all_levels <- union(levels(factor(org)), levels(factor(syn)))

  # 3. Fill in missing categories
  prop_org <- freq_org[all_levels] / length(org)
  prop_syn <- freq_syn[all_levels] / length(syn)
  prop_org[is.na(prop_org)] <- 0
  prop_syn[is.na(prop_syn)] <- 0

  # 4. Combine into data frame for ggplot
  plot_data <- data.frame(
    Category = rep(all_levels, 2),
    Group = c(rep("Original", length(all_levels)),rep("Synthetic", length(all_levels))),
    Proportion = c(as.numeric(prop_org), as.numeric(prop_syn))
  )
  plot_data$Category <- factor(plot_data$Category, levels = all_levels)
  # 5. Calculate S_pMSE for title
  spmse <- round(calculate_S_pMSE(org, syn), 3)

  # 6. Plot with ggplot2
  p<-ggplot2::ggplot(plot_data, ggplot2::aes(x = Category, y = Proportion, fill = Group)) +
    ggplot2::geom_bar(stat = "identity", position = ggplot2::position_dodge()) +
    ggplot2::labs(
      title = paste0(col, ": S_pMSE = ", spmse),
      y = "Proportion", x = NULL
    ) +
    ggplot2::scale_fill_manual(values = c("steelblue", "tomato")) +
    ggplot2::theme_light(base_size = 13)+
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(angle = 45, hjust = 1)
    )

  return(p)
}

