detect_all_logical_rules <- function(df, n_core = 1) {
  CONFIDENCE <- 1
  MIN_SUPPORT <- 10

  df <- as.data.frame(df)
  columns <- names(df)

  rule_finder <- function(a) {
    local_rules <- list()
    is_numeric <- is.numeric(df[[a]])

    # For numeric columns, define cutoffs to use
    if (is_numeric) {
      # Use quantiles to define representative thresholds
      quantiles <- unique(quantile(df[[a]], probs = seq(0.1, 0.9, 0.1), na.rm = TRUE))
      a_values <- c(quantiles, NA)
    } else {
      a_values <- unique(df[[a]])
    }

    for (b in setdiff(columns, a)) {
      for (a_val in a_values) {
        # Build mask
        if (is.na(a_val)) {
          mask <- is.na(df[[a]])
        } else if (is_numeric) {
          mask <- df[[a]] <= a_val & !is.na(df[[a]])
        } else {
          mask <- df[[a]] == a_val
        }

        b_vals <- df[[b]][mask]
        b_tab <- table(as.character(b_vals), useNA = "ifany")
        total <- sum(b_tab)

        if (total >= MIN_SUPPORT) {
          most_common <- names(which.max(b_tab))
          conf <- max(b_tab) / total
          if (conf >= CONFIDENCE) {
            local_rules[[length(local_rules) + 1]] <- list(
              if_column = a,
              if_value = ifelse(is.na(a_val), NA, a_val),
              if_operator = if (is_numeric && !is.na(a_val)) "<=" else "==",
              then_column = b,
              then_value = ifelse(most_common == "NA", NA, most_common),
              confidence = round(conf, 3),
              support = total
            )
          }
        }
      }
    }

    return(local_rules)
  }

  if (n_core > 1) {
    #cl <- get_parallel_cluster()
    results <- foreach(a = columns, .combine = c, .packages = "base") %dopar% rule_finder(a)
  } else {
    results <- unlist(lapply(columns, rule_finder), recursive = FALSE)
  }

  return(results)
}
