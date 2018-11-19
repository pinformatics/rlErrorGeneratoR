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


date_swap <- function(df, n_errors, date){
  # browser()
  if (nrow(df) == 1){
    days <- day(df[[date]])
    if(!is.na(days) && days > 12){
      warning("Not enough candidate dates found")
    } else{
      candidate_ids <- df$id
      old_values <- df[[date]]
      new_values <- old_values
      day(new_values) <- month(old_values)
      month(new_values) <- day(old_values)
      df[[date]] <- new_values

      df <- update_error_record(df,
                                candidate_ids,
                                date,
                                "date_month_swap",
                                old_values,
                                new_values)
    }
  } else {
    days <- day(df[[date]])

    potential_candidates <- df[days < 13,]

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

    # error_record <- attr(df, "error_record")

    df <- update_error_record(df,
                              candidate_ids,
                              date,
                              "date_month_swap",
                              old_values,
                              new_values)
  }
  df
}


date_transpose <- function(df, n_errors, date, token = "year") {
  if(token == "year"){
    if (nrow(df) == 1){
      old_values <- df[[date]]
      new_values <- old_values
      old_values_yr <- year(old_values)
      year(new_values) <- str_sub(old_values_yr, 1, 2) %>% str_c(old_values_yr %>% str_sub(4)) %>% str_c(old_values_yr %>% str_sub(3,3)) %>% as.integer()
      df[[date]] <- new_values
      candidate_ids <- df$id
    } else {
      all_dates <- df[[date]]
      last_two_digits_different <- df$id[str_sub(all_dates, 3,3) != str_sub(all_dates, 4,4)]
      duplicate_ids <- attr(df, "error_record") %>% filter(error == "duplicates") %>% pull(id)

      candidate_ids <- sort(sample(setdiff(last_two_digits_different,duplicate_ids), n_errors))
      old_values <- df[df$id %in% candidate_ids,][[date]]
      new_values <- old_values
      old_values_yr <- year(old_values)
      ###year(new_values) <- str_sub(old_values_yr, 1, 2) %>% str_c(old_values_yr %>% str_sub(3, 4) %>% transpose()) %>% as.integer()
      year(new_values) <- str_sub(old_values_yr, 1, 2) %>% str_c(old_values_yr %>% str_sub(4)) %>% str_c(old_values_yr %>% str_sub(3,3)) %>% as.integer()
      df[df$id %in% candidate_ids,date] <- new_values
    }
    df <- update_error_record(df,
                              candidate_ids,
                              date,
                              "date_transpose",
                              old_values,
                              new_values)
    df
  } else if(token == "month"){
    ids <- df$id
    old_dates <- df[[date]]
    new_dates <- ymd(str_c(year(old_dates), transpose(month(old_dates)), day(old_dates)))
    candidate_ids <- ids[!is.na(new_dates)] %>% sample(n_errors)

    df[id %in% candidate_ids, date] <- ymd(str_c(year(df[id %in% candidate_ids, date]),
                                                 transpose(month(df[id %in% candidate_ids, date])),
                                                 day(df[id %in% candidate_ids, date])))

    df

  } else {
    ids <- df$id
    old_dates <- df[[date]]
    new_dates <- ymd(str_c(year(old_dates), month(old_dates), transpose(day(old_dates))))
    candidate_ids <- ids[!is.na(new_dates)] %>% sample(n_errors)

    df[id %in% candidate_ids, date] <- ymd(str_c(year(df[id %in% candidate_ids, date]),
                                                 month(df[id %in% candidate_ids, date]),
                                                 transpose(day(df[id %in% candidate_ids, date]))))

    df
  }
}
