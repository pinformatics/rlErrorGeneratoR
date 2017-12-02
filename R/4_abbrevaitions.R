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

blanks_to_hyphens <- function(df, n_errors, col_names, all = T){
  ch1_to_ch2(df, n_errors, col_names, " ", "-", all)
}

hyphens_to_blanks <- function(df, n_errors, col_names, all = T){
  ch1_to_ch2(df, n_errors, col_names, "-", " ", all)
}




ch1_to_ch2 <- function(df, n_errors, col_names, ch1, ch2, all = T){
  n <- seq_len(nrow(df))
  p <- length(col_names)
  errors_col <- floor(n_errors/p)

  org_colnames <- names(df)

  for(i in seq_len(length(col_names))){
    error_record <- attr(df, "error_record")
    current_col <- rlang::sym(col_names[i])

    current_col_name <- quo_name(col_names[i])

    # warning(paste0("Not enough records with ", ch1,  " found in column"));return(
    feasible_recs <-
    df %>%
    filter(str_detect(!!current_col, ALNUM %R% literal(ch1) %R% ALNUM)) %>%
    sample_n(ifelse(errors_col > nrow(.),
                    nrow(.),
                    errors_col)) %>%
    mutate(blanks = TRUE) %>%
    select(id, blanks)


    if(all){
    df <-
      df %>%
      left_join(feasible_recs, by = "id") %>%
      mutate(old_names = (!!current_col),
             !!current_col_name := if_else(!is.na(blanks),
                                           str_replace_all((!!current_col),literal(ch1),ch2),
                                           (!!current_col)))
    } else {
      df <-
        df %>%
        left_join(feasible_recs, by = "id") %>%
        mutate(old_names = (!!current_col),
               !!current_col_name := if_else(!is.na(blanks),
                                             str_replace(old_names,literal(ch1),ch2),
                                             old_names))
      }

    error_table <-
      df %>%
      filter(blanks)

    attr(df, "error_record") <- error_record
    df <- update_error_record(df,
                              error_table$id,
                              col_names[i],
                              paste0(ch1,"to",ch2),
                              error_table$old_names,
                              error_table[[col_names[i]]])
  }

  df %>%
    select(one_of(org_colnames))
}
