



married_name_change <- function(df, n_errors, lname, sex = NULL, dob = NULL, age = NULL){
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
    sample_n(length(candidate_ids)) %>%
    pull(lastname)

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




