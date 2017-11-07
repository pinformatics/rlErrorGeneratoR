

prep_data <- function(df_original) {

  df_original <-
    df_original %>%
    mutate_all(as.character()) %>%
    mutate(file = "A",
           id = row_number()) %>%
    select(file, id, everything())

  df_secondary <-
    df_original %>%
    mutate(file = "B")

  structure(list(df_original = df_original,
                 df_secondary = df_secondary),
            class = "df_pairs")
}

indel <- function(x, ...){
  UseMethod("indel")
}

update_error_record <- function(df, ids, field, error, before, after){
  error_record <- tibble(id = ids, field = field, error = error,
                         before = unclass(before), after = unclass(after))
  attr(df, "error_record") <- bind_rows(attr(df, "error_record"),
                                        error_record)
  df
}

indel.data.frame <- function(df, n_errors, col_names){
  n <- seq_len(nrow(df))
  p <- length(col_names)
  errors_col <- floor(n_errors/p)

  if(n_errors < nrow(df)*p){
    for(i in seq_len(p)) {
      rows <- sample(n, errors_col)
      col_name <- col_names[i]
      before <- df %>% filter(row_number() %in% rows) %>% pull(col_name)
      after <- before %>% indel()
      df[rows, col_name] <-  after
      df <- update_error_record(df, df$id[rows], col_name, "indel", before, after)
    }
  } else {
    for(i in seq_len(n_errors)){
      rows <- sample(n, 1)
      col_name <- sample(col_names, 1)
      before <- df%>% filter(row_number() %in% rows) %>% pull(col_name)
      after <- before %>% indel()
      df[rows, col_name] <-  after
      df <- update_error_record(df, df$id[rows], col_name, "indel", before, after)
    }
  }

  df
}

indel.character <- function(error_strings){
  map_chr(error_strings, indel.default, LETTERS)
}

indel.numeric <- function(error_strings){
  map_chr(error_strings, indel.default, 0:9)
}

indel.number <- function(error_strings){
  structure(map_chr(error_strings, indel.default, 0:9), class = "number")
}


indel.default <- function(edit_string,  error_chars = LETTERS, fix_len = FALSE){
  len <- str_length(edit_string)

  if(runif(1) > 0.5){
    #insert
    add_letter <- sample(error_chars, 1)
    cut <- sample(0:len, 1)
    if(cut == 0){
      str_c(add_letter,edit_string, collapse = "")
    } else if  (cut == len) {
      str_c(edit_string, add_letter, collapse = "")
    } else {
      a <- str_sub(edit_string, 1, cut)
      b <- str_sub(edit_string, cut + 1, len)
      str_c(a, add_letter, b,collapse = "")
    }

  } else{
    #delete
    rem <- sample(1:len, 1)
    if(rem == 1){
      str_sub(edit_string, 2)
    } else if  (rem == len) {
      str_sub(edit_string, 1, len - 1)
    } else {
      a <- str_sub(edit_string, 1, rem - 1)
      b <- str_sub(edit_string, rem + 1, len)
      str_c(a, b,collapse = "")
    }
  }
}

repl <- function(x, ...) {
  UseMethod("repl")
}

repl.data.frame <- function(df, n_errors, col_names){
  n <- seq_len(nrow(df))
  p <- length(col_names)
  errors_col <- floor(n_errors/p)

  if(n_errors < nrow(df)*p){
    for(i in seq_len(p)) {
      rows <- sample(n, floor(n_errors/p))
      col_name <- col_names[i]
      before <- df %>% filter(row_number() %in% rows) %>% pull(col_name)
      after <- before %>% repl()
      df[rows, col_name] <- after
      df <- update_error_record(df, df$id[rows], col_name, "repl", before, after)
    }
  } else {
    for(i in seq_len(n_errors)){
      rows <- sample(n, 1)
      col_name <- sample(col_names, 1)
      before <- df %>% filter(row_number() %in% rows) %>% pull(col_name)
      after <- before %>% repl()
      df[rows, col_name] <- after
      df <- update_error_record(df, df$id[rows], col_name, "repl", before, after)
    }
  }

  df
}

repl.character <- function(error_strings){
  # repl(error_strings, LETTERS)
  map_chr(error_strings, repl.default, LETTERS)
}

repl.numeric <- function(error_strings){
  # repl(error_strings, 0:9)
  map_chr(error_strings, repl.default, 0:9)
}

repl.number <- function(error_strings){
  # repl(error_strings, 0:9)
  structure(map_chr(error_strings, repl.default, 0:9), class="number")
}


repl.default <- function(edit_string, error_chars = LETTERS){
  repl_index <- sample(1:str_length(edit_string), 1)

  edit_string <- edit_string %>%
    str_split("") %>%
    .[[1]]
  subs <- sample(error_chars, 1)

  while(edit_string[repl_index] == subs){
    subs <- sample(error_chars, 1)
  }
  edit_string[repl_index] <- subs

  edit_string %>%
    str_c(collapse = "")
}





mess_data <- function(x, ...){
  UseMethod("mess_data")
}

mess_data.data.frame <- function(df_data, error_lookup){

  n <- nrow(df_data)
  attr(df_data, "error_record") <- tribble(~id,
                                           ~field,
                                           ~error,
                                           ~before,
                                           ~after)

  for(i in seq_len(nrow(error_lookup))){

    error_function <- error_lookup[i,] %>% pull(1)
    e <- error_lookup[i,] %>% pull(2)
    col_names <- str_split(error_lookup[i,] %>% pull(3), ",", simplify = T) %>% str_trim()

    if(e < 1) e <- ceiling(e*n)

    # col_classes <- map_chr(df_data,class)
    error_function <- match.fun(error_function)
    df_data <- error_function(df_data, e, col_names = col_names)
    # map(seq_len(ncol(df_data)), function(i){
    #   class(df_data[[i]]) <<- col_classes[i]
    # })
  }

  df_data
}

mess_data.df_pairs <- function(df_pairs, error_lookup){
  df_pairs$df_secondary <- mess_data(df_pairs$df_secondary, error_lookup)
  df_pairs
}


convert_cols <- function(obj,types){
  out <- lapply(1:length(obj),FUN = function(i){FUN1 <- switch(types[i],character = as.character,numeric = as.numeric,factor = as.factor); FUN1(obj[,i])})
  names(out) <- colnames(obj)
  as.data.frame(out,stringsAsFactors = FALSE)
}
