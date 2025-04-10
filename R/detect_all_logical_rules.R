detect_all_logical_rules <- function(df, n_core = 1) {

  CONFIDENCE <- 1
  MIN_SUPPORT<- 5

  df <- as.data.frame(df)
  df[] <- lapply(df, function(x) if (is.character(x)) factor(x) else x)
  columns <- names(df)

  rule_finder <- function(a) {
    local_rules <- list()
    for (b in setdiff(columns, a)) {
      a_values <- unique(df[[a]])
      for (a_val in a_values) {
        mask <- if (is.na(a_val)) is.na(df[[a]]) else df[[a]] == a_val
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
    results <- foreach(a = columns, .combine = c, .packages = "base") %dopar% rule_finder(a)
  } else {
    results <- unlist(lapply(columns, rule_finder), recursive = FALSE)
  }

  return(results)
}
