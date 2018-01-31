



married_name_change <- function(df, n_errors, col_names, lname, sex, dob = NULL, age = NULL){
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
duplicates <- function(df, n_errors, col_names){

  n <- seq_len(nrow(df))
  p <- length(col_names)

  if(n_errors > nrow(df)){
    warning("Nor enough samples found for generating duplicates")
    n_errors <- nrow(df)
  }

  candidate_ids <- sample(df$id, n_errors)

  dup_df <- df[df$id %in% candidate_ids,]
  old_vals <- dup_df[[col_names]]
  new_vals <- sample(df[[col_names]][!df$id %in% candidate_ids], n_errors) %>%
    indel() %>%
    repl() %>%
    transpose()
  dup_df[, col_names] <- new_vals

  error_record <- attr(df, "error_record")

  df <- bind_rows(dup_df, df) %>% arrange(id)

  df <- update_error_record(df,
                            candidate_ids,
                            col_names,
                            "duplicates",
                            old_vals,
                            new_vals)
  df
}




