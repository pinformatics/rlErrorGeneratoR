married_name_change <- function(df, n_errors, lname, sex, dob = NULL, age = NULL){
  df_s <- df[df[[sex]] == "f",]

  if(!is.null(dob)){
    # interval(start = dob, end = givendate) /
    #   duration(num = 1, units = "years")
  } else if(!is.null(age)){
    #filter age
    df_s <- df_s[df_s[[age]] > 20,]
  }

  if (n_errors > nrow(df_s)){
    warning("Not enough samples found for simulating married last name change.")
    n_errors <- nrow(df_s)
  }
  # browser()
  candidate_ids <- sample(df_s$id, n_errors)

  old_lnames <- df[df$id %in% candidate_ids,][[lname]]
  new_names <- lnames_all %>%
    sample(length(candidate_ids))

  df[df$id %in% candidate_ids, lname] <- new_names


  # error_record <- attr(df, "error_record")
  df <- update_error_record(df,
                            df[df$id %in% candidate_ids, ][["id"]],
                            lname,
                            "married_name_change",
                            old_lnames,
                            new_names)



  df
}

add_duplicates <- function(df_pairs, n_errors){

  df_original <- df_pairs$df_original
  df_secondary <- df_pairs$df_secondary

  ids <-
    attributes(df_secondary) %>%
    pluck("error_record") %>%
    sample_n(nrow(.)) %>%
    pull(id) %>%
    unique()

  ids <- sample(ids, n_errors)

  df_secondary_new <-
    df_original %>%
    filter(id %in% ids) %>%
    bind_rows(df_secondary)
  # browser()
  attr(df_secondary_new, "error_record") <-
    attr(df_secondary, "error_record")

  df_secondary_new <-
    update_error_record(df_secondary_new,
                        ids,
                        "all_fields",
                        "duplicate",
                        "original",
                        "original")

  df_pairs$df_secondary <- df_secondary_new
  attr(df_pairs$df_secondary, "error_record") <-
    attr(df_secondary_new, "error_record")

  df_pairs
}

#give an id_field you would like to use
# duplicates <- function(df, n_errors, id_col){
#
#   # stopifnot(length(col_names) == 1)
#
#   if(n_errors > nrow(df)){
#     warning("Nor enough samples found for generating duplicates")
#     n_errors <- nrow(df)
#   }
#
#   if (nrow(df) == 1){
#     candidate_ids <- df$id
#     dup_df <- df
#     old_vals <- dup_df[[id_col]]
#     new_vals <- list(df[[id_col]] %>%
#         indel() %>%
#         repl()) %>%
#       transpose()
#   } else{
#     candidate_ids <- sample(df$id, n_errors)
#     dup_df <- df[df$id %in% candidate_ids,]
#     old_vals <- dup_df[[id_col]]
#     new_vals <- list(
#       sample(df[[id_col]][!df$id %in% candidate_ids], n_errors) %>%
#         indel() %>%
#         repl()) %>%
#       transpose()
#   }
#
#   new_vals <- unlist(new_vals)
#   if(all(str_length(str_extract(new_vals, one_or_more(DIGIT))) == str_length(new_vals))){
#     new_vals <- as.integer(new_vals)
#   }
#
#   dup_df[, id_col] <- new_vals
#
#   error_record <- attr(df, "error_record")
#
#   df <- bind_rows(dup_df, df) %>% arrange(id)
#
#   attr(df, "error_record") <- error_record
#
#   # browser()
#   df <- update_error_record(df,
#                             candidate_ids,
#                             id_col,
#                             "duplicates",
#                             old_vals,
#                             new_vals)
#   df
# }


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

  if (nrow(df) == 1){
    candidate_ids <- df$id
    twins_df <- df[df$id %in% candidate_ids,]
  } else{
    candidate_ids <- sort(sample(df$id, n_errors))
    twins_df <- df[df$id %in% candidate_ids,]
  }

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




twins_identify <- function(){

}



