



married_name_change <- function(df, n_errors, lname, sex, dob = NULL, age = NULL){
  df_s <- df[df[[sex]] == "f",]

  if(!is.null(dob)){
    #fill in code
  } else if(!is.null(age)){
    #filter age
    df_s <- df_s[df_s[[age]] > 20,]
  }

  if (n_errors > nrow(df_s)){

    warning("Not enough samples found for simulating married last name change.")
    n_errors <- nrow(df_s)
  }

  candidate_ids <- sort(sample(df_s$id, n_errors))

  old_lnames <- df[[lname]][df$id %in% candidate_ids]
  new_names <- lnames_all %>%
    sample(length(candidate_ids))

  df[df$id %in% candidate_ids, lname] <- new_names


  error_record <- attr(df, "error_record")
  df <- update_error_record(df,
                              candidate_ids,
                              lname,
                              "married_name_change",
                              old_lnames,
                              new_names)



  df
}

#give an id_field you would like to use
duplicates <- function(df, n_errors, id_col){

  # stopifnot(length(col_names) == 1)

  if(n_errors > nrow(df)){
    warning("Nor enough samples found for generating duplicates")
    n_errors <- nrow(df)
  }

  candidate_ids <- sample(df$id, n_errors)

  dup_df <- df[df$id %in% candidate_ids,]
  old_vals <- dup_df[[id_col]]
  new_vals <- sample(df[[id_col]][!df$id %in% candidate_ids], n_errors) %>%
    indel() %>%
    repl() %>%
    transpose()
  if(all(str_length(str_extract(new_vals, one_or_more(DIGIT))) == str_length(new_vals))){
    new_vals <- as.integer(new_vals)
  }

  dup_df[, id_col] <- new_vals

  error_record <- attr(df, "error_record")

  df <- bind_rows(dup_df, df) %>% arrange(id)

  attr(df, "error_record") <- error_record

  # browser()
  df <- update_error_record(df,
                            candidate_ids,
                            id_col,
                            "duplicates",
                            old_vals,
                            new_vals)
  df
}


twins_generate <- function(df, n_errors, fname, id_col = NULL, sex = NULL){

  # fname <- col_names


  if(n_errors > nrow(df)){
    warning("Nor enough samples found for generating duplicates")
    n_errors <- nrow(df)
  }

  fnames_lookup <-
    tibble(fname = fnames_male, sex = "m") %>%
      bind_rows(tibble(fname = fnames_female, sex = "f") %>% sample_n(3000)) %>%
      sample_n(nrow(.)) %>%
      mutate(fname_len = str_length(fname))

  search_name <- function(name){
    fnames_lookup %>%
      filter(fname_len == str_length(name),
             str_sub(fname, 1, 1) == str_sub(name, 1, 1)) %>%
      sample_n(1) %>%
      select(-fname_len)
  }

  candidate_ids <- sort(sample(df$id, n_errors))

  twins_df <- df[df$id %in% candidate_ids,]
  twins_df_cp <- twins_df
  fnames_old <- twins_df[[fname]]

  twins_df_new <- map_df(fnames_old, search_name)

  twins_df[[fname]] <- twins_df_new$fname

  twins_df$id <- str_c("123", twins_df$id, "789") %>% as.integer()

  if(!is.null(sex)){
    twins_df[[sex]] <- twins_df_new$sex
  }

  if(!is.null(id_col)){
    twins_df[[id_col]] <- repl(twins_df[[id_col]])
  }

  error_record <- attr(df, "error_record")

  df <- df %>% bind_rows(twins_df) %>% arrange(id)

  attr(df, "error_record") <- error_record

  df <- update_error_record(df,
                            candidate_ids,
                            fname,
                            "twins",
                            twins_df_cp$fname,
                            twins_df[[fname]])
  df

}



