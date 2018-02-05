gen_birthday_from_age <- function(df, age, as_of_year_end = "most_recent_year_end"){

  age <- df[[age]]

  if(as_of_year_end == "most_recent_year_end"){
    if(day(today()) == 31 && month(today()) == 12){
      as_of_year_end <- year(today())
    } else {
      as_of_year_end <- year(today()) - 1
    }
  } else{
    as_of_year_end <- as_of_year_end %>% as.integer()
  }

  start <- (as_of_year_end - 3) %>% str_c("0101") %>% ymd
  end <- as_of_year_end %>% str_c("1231") %>% ymd

  bday <-seq(start, end, by="day") %>% sample(length(age))

  year(bday) <- as_of_year_end - age

  df$bday <- bday

  df
}


date_month_swap <- function(df, n_errors, date){

  potential_candidates <- df[day(df[[date]]) < 13,]

  potential_candidates <-
    potential_candidates %>%
    anti_join(attr(df, "error_record") %>% filter(error == "duplicates"), by = "id")

  if(n_errors > nrow(potential_candidates)){
    warning("Not enough candidate dates found")
    n_errors <- nrow(potential_candidates)
  }

  candidate_ids <- sample(potential_candidates$id, n_errors)

  old_values <- df[df$id %in% candidate_ids,][[date]]
  new_values <- old_values
  day(new_values) <- month(old_values)
  month(new_values) <- day(old_values)
  df[df$id %in% candidate_ids,date] <- new_values

  error_record <- attr(df, "error_record")


  df <- update_error_record(df,
                            candidate_ids,
                            date,
                            "date_month_swap",
                            old_values,
                            new_values)
  df

}
