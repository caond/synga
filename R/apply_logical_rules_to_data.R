apply_logical_rules_to_data <- function(df, rules) {
  for (rule in rules) {
    a <- rule$if_column
    a_val <- rule$if_value
    b <- rule$then_column
    b_val <- rule$then_value

    # Identify rows where condition is TRUE
    if (is.na(a_val)) {
      idx <- is.na(df[[a]])
    } else {
      idx <- df[[a]] == a_val
    }

    # Enforce the rule in those rows
    if (is.na(b_val)) {
      df[[b]][idx] <- NA
    } else {
      df[[b]][idx] <- b_val
    }
  }

  return(df)
}
