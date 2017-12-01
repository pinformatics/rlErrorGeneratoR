first_letter_abbreviate <- function(df, n_errors, col_names){
  n <- seq_len(nrow(df))
  p <- length(col_names)
  errors_col <- floor(n_errors/p)

  if((n_errors < nrow(df)*p) && (errors_col > 0)){
    for(i in seq_len(p)) {
      rows <- sample(n, errors_col)
      col_name <- col_names[i]
      before <- df %>% filter(row_number() %in% rows) %>% pull(col_name)
      after <- before %>% str_sub(1,1)
      df[rows, col_name] <-  after
      df <- update_error_record(df, df$id[rows], col_name, "first_letter_abbreviate", before, after)
    }
  } else {
    for(i in seq_len(n_errors)){
      rows <- sample(n, 1)
      col_name <- sample(col_names, 1)
      before <- df%>% filter(row_number() %in% rows) %>% pull(col_name)
      after <- before %>% str_sub(1,1)()
      df[rows, col_name] <-  after
      df <- update_error_record(df, df$id[rows], col_name, "first_letter_abbreviate", before, after)
    }
  }

  df
}
