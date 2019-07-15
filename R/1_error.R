
### create original(file = a) and secondry(copy of original but file = b) from data
prep_data <- function(df_original) {

  # mutate_all(as.character()) %>%
  df_original <-
    df_original %>%
    mutate(file = "A",
           id = row_number()) %>%
    select(file, id, everything())

  # df_original <-
  #   map_df(df_original, str_to_lower)

  df_original <-
    df_original %>%
    mutate_if(is.character, str_to_lower)

  df_secondary <-
    df_original %>%
    mutate(file = "b")

  structure(list(df_original = df_original,
                 df_secondary = df_secondary),
            class = "df_pairs")
}



update_error_record <- function(df, ids, field, error, before, after){
  error_record <- tibble(id = ids, field = field, error = error,
                         before = as.character(before), after = as.character(after))
  attr(df, "error_record") <- bind_rows(attr(df, "error_record"),
                                        error_record)
  df
}







mess_data <- function(x, ...){
  UseMethod("mess_data")
}

mess_data.data.frame <- function(df_data, error_lookup, add_counting_dups = F, verbose = T){

  if(add_counting_dups){
    n <- add_counting_dups
  } else {
    n <- nrow(df_data)
  }

  attr(df_data, "error_record") <- tribble(~id,
                                           ~field,
                                           ~error,
                                           ~before,
                                           ~after)

  for(i in seq_len(nrow(error_lookup))){
  ###for(i in 1:15){

    error_function <- error_lookup[i,] %>% pull(1)

    if(verbose){
      message()
      message(error_function)
    }

    #setting the arguments to be passed to do.call one by one

    #firstly, we pass the df as it causes the s3 dispatch
    arguments <- list(df = df_data)

    #number of errors
    e <- error_lookup[i,] %>% pull(2)
    if(e < 1) e <- ceiling(e*n)
    arguments$n_errors <- e

    #columns the errors apply to
    col_names <- str_split(error_lookup[i,] %>% pull(3), ",", simplify = T) %>% str_trim()
    if(all(!is.na(col_names))){
      arguments$col_names <- col_names
    }

    #adding additonal arguments if any
    if(!is.na(error_lookup[i,] %>% pull(4))){
      arguments <- append(arguments, as.list(parse(text=paste0("f(", error_lookup[i,] %>% pull(4) , ")"))[[1]])[-1])
    }

    # message(lsf.str())

    funcs_global <- lsf.str(envir = globalenv()) %>% as.vector()
    if(error_function %in% funcs_global){
      df_data <- do.call(error_function, args = arguments, envir = globalenv())
    } else {
      df_data <- do.call(error_function, args = arguments)
    }

    if(verbose){
      print(attr(df_data, "error_record") %>% tail())
    }

  }

  df_data
}

mess_data.df_pairs <- function(df_pairs, error_lookup, add_counting_dups = F, ...){
  # browser()
  n <- nrow(df_pairs$df_original)

  e <- error_lookup %>% filter(error == "add_duplicates") %>% pull(amount)
  if(e < 1) e <- ceiling(e*n)

  if(add_counting_dups) add_counting_dups <- n + e

  df_pairs$df_secondary <- mess_data(df_pairs$df_secondary,
                                     error_lookup %>% filter(error != "add_duplicates"),
                                     add_counting_dups = add_counting_dups, ...)

  df_pairs %>%
    add_duplicates(e)
  # df_pairs
}


convert_cols <- function(obj,types){
  out <- lapply(1:length(obj),FUN = function(i){FUN1 <- switch(types[i],character = as.character,numeric = as.numeric,factor = as.factor); FUN1(obj[,i])})
  names(out) <- colnames(obj)
  as.data.frame(out,stringsAsFactors = FALSE)
}
