prepare_data <- function(df) {
  dob <- read.csv("dob.csv")

  if(day(today()) == 31 && month(today()) == 12){
    as_of_year_end <- year(today())
  } else {
    as_of_year_end <- year(today()) - 1
  }

  age <- df$age
  bday <- dob$DOB %>% sample(length(age)) %>% as.Date()
  year(bday) <- as_of_year_end - age
  df$bday <- bday

  # add id
  df <- df %>%
    mutate(file = 0, id = row_number()) %>%
    select(file, id, everything())

  df

}

perturb_data <- function(df_original, n_perturb, no_errs) {
  df_original <-
    df_original %>%
    mutate_if(is.character, str_to_lower)

  df_secondary <- df_original
  for (i in 2:n_dup){
    dup <- df_original %>% mutate(file = i) %>% mutate(id = id + max(df_secondary$id))
    df_secondary <- rbind(df_secondary, dup)
  }
  #df_secondary <- df_secondary %>% mutate(err_type = sample(no_errs, nrow(df_secondary), replace = TRUE))
  df_secondary <- df_secondary %>% mutate(err_type = 0)
  for (j in 1:nrow(df_original)){
    err <- sample(no_errs, n_dup)
    df_secondary$err_type[j] <- err[1]
    for (k in 2:n_dup)
      df_secondary$err_type[j + (k - 1) * nrow(df_original)] <- err[k]
  }

  structure(list(df_original = df_original,
                 df_secondary = df_secondary),
            class = "df_pairs")
}

add_error <- function(df_data, error_table, error_record = error_record){
  if(is.null(error_record)){
    attr(df_data, "error_record") <- tribble(~id,
                                             ~field,
                                             ~error,
                                             ~before,
                                             ~after)
  } else{
    attr(df_data, "error_record") <- error_record
  }

  dup <- data.frame()
  for(i in seq_len(nrow(df_data))){
    error_function <- error_table[df_data$err_type[i],] %>% pull(1)
    message(error_function)

    error_record <- attr(df_data, "error_record")
    df = df_data[i,]
    attr(df, "error_record") <- error_record
    arguments <- list(df = df)

    arguments$n_errors <- 1

    #columns the errors apply to
    col_names <- str_split(error_table[df_data$err_type[i],] %>% pull(3), ",", simplify = T) %>% str_trim()
    if(all(!is.na(col_names))){
      arguments$col_names <- col_names
    }

    #adding additonal arguments if any
    if(!is.na(error_table[df_data$err_type[i],] %>% pull(4))){
      arguments <- append(arguments, as.list(parse(text=paste0("f(", error_table[df_data$err_type[i],] %>% pull(4) , ")"))[[1]])[-1])
    }

    df <- do.call(error_function, args = arguments)
    error_record <- attr(df, "error_record")

    if (error_function == "duplicates"){
      dup <- bind_rows(dup, df[1,])
      df_data[i,] <- df[2,]
    } else{
      df_data[i,] <- df
    }
    attr(df_data, "error_record") <- error_record
  }

  error_record <- attr(df_data, "error_record")
  df_data <- bind_rows(dup, df_data) %>% arrange(id)
  attr(df_data, "error_record") <- error_record
  df_data

}

make_twins <- function(df){
  attr(df, "error_record") <- tribble(~id,
                                      ~field,
                                      ~error,
                                      ~before,
                                      ~after)
  twins <- df[duplicated(df[c(5,8,9)]),]  # same lname, birth year, and street address
  n <- floor(nrow(twins) / 2) # make 50% of this data Twins
  twins <- sample_n(twins, n)

  for (i in 1:n){
    twin1 <- twins[i,]
    twin_index <- which(df$age == twins[i, "age"] & df$street_address == twins[i,"street_address"] & df$id != twins[i, "id"])
    twin2 <- df[twin_index,]
    candidate_ids <- twin2$id
    df[twin_index,"bday"] = twin1$bday
    df <- update_error_record(df,
                              candidate_ids,
                              "bday",
                              "twins",
                              twin2$bday,
                              twin1$bday)
    print(twins[i,])
    print(df[twin_index,])
    print("____________________________________________________________________________________________")
  }
  df
}
