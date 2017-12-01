real_to_nicknames <- function(df, n_errors, col_names){
  n <- seq_len(nrow(df))
  p <- length(col_names)
  errors_col <- floor(n_errors/p)

  org_colnames <- names(df)

  for(i in seq_len(length(col_names))){
    error_record <- attr(df, "error_record")
    current_col <- rlang::sym(col_names[i])

    current_col_name <- quo_name(col_names[i])

    (lookup <-
      names_lookup %>%
      filter(lookup_type == "to_nick") %>%
      inner_join(df, by = c("lookup_name" = current_col_name)) %>%
      group_by(lookup_name) %>%
      sample_n(1) %>%
      ungroup() %>%
      sample_n(ifelse(errors_col < nrow(.),
                      errors_col,
                      {
                        warning("Not enough matches found for nicknames");
                        nrow(.)
                      })) %>%
      select(starts_with("lookup"), id))

    df <-
      df %>%
      left_join(lookup, by = "id") %>%
      mutate(old_names = (!!current_col),
             !!current_col_name := if_else(is.na(lookup_alternate),
                                           (!!current_col),
                                           lookup_alternate))

    error_table <-
    df %>%
    filter(!is.na(lookup_alternate))

    attr(df, "error_record") <- error_record
    df <- update_error_record(df,
                              error_table$id,
                              col_names[i],
                              "to_nickname",
                              error_table$old_names,
                              error_table[[col_names[i]]])
  }

  df %>%
    select(one_of(org_colnames))
}


nick_to_realnames <- function(df, n_errors, col_names){
  n <- seq_len(nrow(df))
  p <- length(col_names)
  errors_col <- floor(n_errors/p)

  org_colnames <- names(df)

  for(i in seq_len(length(col_names))){
    error_record <- attr(df, "error_record")
    current_col <- rlang::sym(col_names[i])

    current_col_name <- quo_name(col_names[i])

    (lookup <-
        names_lookup %>%
        filter(lookup_type == "to_proper") %>%
        inner_join(df, by = c("lookup_name" = current_col_name)) %>%
        group_by(lookup_name) %>%
        sample_n(1) %>%
        ungroup() %>%
        sample_n(ifelse(errors_col < nrow(.),
                        errors_col,
                        {
                          warning("Not enough matches found for realnames");
                          nrow(.)
                        })) %>%
        select(starts_with("lookup"), id))

    df <-
      df %>%
      left_join(lookup, by = "id") %>%
      mutate(old_names = (!!current_col),
             !!current_col_name := if_else(is.na(lookup_alternate),
                                           (!!current_col),
                                           lookup_alternate))

    error_table <-
      df %>%
      filter(!is.na(lookup_alternate))

    attr(df, "error_record") <- error_record
    df <- update_error_record(df,
                              error_table$id,
                              col_names[i],
                              "to_realname",
                              error_table$old_names,
                              error_table[[col_names[i]]])
  }

  df %>%
    select(one_of(org_colnames))
}

invert_real_and_nicknames <- function(df, n_errors, col_names){
  n <- seq_len(nrow(df))
  p <- length(col_names)
  errors_col <- floor(n_errors/p)

  org_colnames <- names(df)

  for(i in seq_len(length(col_names))){
    error_record <- attr(df, "error_record")
    current_col <- rlang::sym(col_names[i])

    current_col_name <- quo_name(col_names[i])

    (lookup <-
        names_lookup %>%
        inner_join(df, by = c("lookup_name" = current_col_name)) %>%
        group_by(lookup_name) %>%
        sample_n(1) %>%
        ungroup() %>%
        sample_n(ifelse(errors_col < nrow(.),
                        errors_col,
                        {
                          warning("Not enough matches found for nick or realnames");
                          nrow(.)
                        })) %>%
        select(starts_with("lookup"), id))

    df <-
      df %>%
      left_join(lookup, by = "id") %>%
      mutate(old_names = (!!current_col),
             !!current_col_name := if_else(is.na(lookup_alternate),
                                           (!!current_col),
                                           lookup_alternate))

    error_table <-
      df %>%
      filter(!is.na(lookup_alternate))

    attr(df, "error_record") <- error_record
    df <- update_error_record(df,
                              error_table$id,
                              col_names[i],
                              "invert_nick_realnames",
                              error_table$old_names,
                              error_table[[col_names[i]]])
  }

  df %>%
    select(one_of(org_colnames))
}
