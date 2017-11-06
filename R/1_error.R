

id_data <- function(df_original) {

  df_original <-
    df_original %>%
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


indel.df <- function(df_data, col_names){

}

indel.default <- function(edit_string, fix_len = FALSE){
  len <- str_length(edit_string)

  if(runif(1) > 0.5){
    #insert
    add_letter <- sample(LETTERS, 1)
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


mess_data <- function(df_data, error_table){

  n <- nrow(df_data)

  for(i in seq_len(nrow(error_table))){
    e <- error_table[i,2]
    if(e < 1) {
      e <- e*n
    }
    sample()



  }

}
