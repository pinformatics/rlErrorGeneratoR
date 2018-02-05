indel <- function(x, ...) {
  UseMethod("indel")
}

indel.data.frame <- function(df, n_errors, col_names){
  n <- seq_len(nrow(df))
  p <- length(col_names)
  errors_col <- floor(n_errors/p)

  if((n_errors < nrow(df)*p) && (errors_col > 0)) {
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
  map_chr(error_strings, indel.default, letters)
}

indel.numeric <- function(error_strings){
  map_chr(error_strings, indel.default, 0:9) %>% as.integer()
}

indel.number <- function(error_strings){
  structure(map_chr(error_strings, indel.default, 0:9), class = "number")
}


indel.default <- function(edit_string,  error_chars = letters, fix_len = FALSE){
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

  if((n_errors < nrow(df)*p) && (errors_col > 0)){
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
  # repl(error_strings, letters)
  map_chr(error_strings, repl.default, letters)
}

repl.numeric <- function(error_strings){
  # repl(error_strings, 0:9)
  map_chr(error_strings, repl.default, 0:9)%>% as.integer()
}

repl.number <- function(error_strings){
  # repl(error_strings, 0:9)
  structure(map_chr(error_strings, repl.default, 0:9), class="number")
}


repl.default <- function(edit_string, error_chars = letters){
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

transpose <- function(x, ...) {
  UseMethod("transpose")
}

transpose.data.frame <- function(df, n_errors, col_names){
  n <- seq_len(nrow(df))
  p <- length(col_names)
  errors_col <- floor(n_errors/p)

  if((n_errors < nrow(df)*p) && (errors_col > 0)){
    for(i in seq_len(p)) {
      rows <- sample(n, floor(n_errors/p))
      col_name <- col_names[i]
      before <- df %>% filter(row_number() %in% rows) %>% pull(col_name)
      after <- before %>% transpose()
      df[rows, col_name] <- after
      df <- update_error_record(df, df$id[rows], col_name, "transpose", before, after)
    }
  } else {
    for(i in seq_len(n_errors)){
      rows <- sample(n, 1)
      col_name <- sample(col_names, 1)
      before <- df %>% filter(row_number() %in% rows) %>% pull(col_name)
      after <- before %>% transpose()
      df[rows, col_name] <- after
      df <- update_error_record(df, df$id[rows], col_name, "transpose", before, after)
    }
  }

  df
}

transpose.default <- function(error_strings){
  # transpose(error_strings, letters)
  map_chr(error_strings, transpose.base)
}

transpose.base <- function(edit_string){
  stopifnot(edit_string %>% str_length() > 1)

  transpose_index <- sample(2:str_length(edit_string), 1)
  transpose_index_l <- transpose_index - 1

  edit_string <- edit_string %>%
    str_split("") %>%
    .[[1]]

  if(edit_string %>% unique() %>% length() > 1){
    while(edit_string[transpose_index] == edit_string[transpose_index_l]){
      transpose_index <- sample(2:length(edit_string), 1)
      transpose_index_l <- transpose_index - 1
    }
  } else{
    warning("All charcters are the same. Transpose is not valid!")
  }

  transpose_char <- edit_string[transpose_index]
  transpose_char_l <- edit_string[transpose_index_l]

  edit_string[transpose_index] <- transpose_char_l
  edit_string[transpose_index_l] <- transpose_char

  edit_string %>%
    str_c(collapse = "")
}


