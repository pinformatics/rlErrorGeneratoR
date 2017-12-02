swap_fields <- function(df, n_errors, col_names){
  # give col names as pairs

  n <- seq_len(nrow(df))
  p <- length(col_names)/2
  errors_col <- floor(n_errors/p)

  for(i in seq(1,length(col_names), 2)){
    col_1 <- col_names[i]
    col_2 <- col_names[i + 1]
    rows <- sample(n, n_errors)
    c1 <- df[rows, ][[col_1]]
    c2 <- df[rows, ][[col_2]]
    df[rows, col_1] <- df[rows, col_2]
    df[rows, col_2] <- c1

    lookup <-
      tibble(c1 = c1, c2 = c2) %>%
      mutate(before = str_c(c1, c2, sep = ", "),
             after = str_c(c2, c1, sep = ", "))

    df <- update_error_record(df,
                              df$id[rows],
                              paste0(col_1, ", ", col_2),
                              "swap",
                              lookup$before,
                              lookup$after)
  }

  df
}
