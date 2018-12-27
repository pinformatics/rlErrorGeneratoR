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
    dates <- df[[date]]
    days <- day(dates)

    potential_candidates <- df[days < 13 & month(dates) != day(dates),]

    # potential_candidates <-
    #   potential_candidates %>%
    #   anti_join(attr(df, "error_record") %>% filter(error == "add_duplicates"), by = "id")

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
                              df[df$id %in% candidate_ids,][["id"]],
                              date,
                              "date_month_swap",
                              old_values,
                              new_values)
  }
  df
}


date_transpose <- function(df, n_errors, date, token = "year"){
  if(token == "year"){
    if (nrow(df) == 1){
      old_values <- df[[date]]
      new_values <- old_values
      old_values_yr <- year(old_values)
      year(new_values) <- str_sub(old_values_yr, 1, 2) %>% str_c(old_values_yr %>% str_sub(4)) %>% str_c(old_values_yr %>% str_sub(3,3)) %>% as.integer()
      df[[date]] <- new_values
      candidate_ids <- df$id
    } else {
      ids <- df$id
      old_dates <- df[[date]]
      old_dates_year3 <- str_sub(year(old_dates),3,3)
      old_dates_year4 <- str_sub(year(old_dates),4,4)
      candidate_ids <- sample(ids[old_dates_year3 != old_dates_year4], n_errors)
      candidate_dates <- old_dates[ids %in% candidate_ids]

      new_dates <- ymd(str_c(str_sub(year(candidate_dates),1,2), tpose(str_sub(year(candidate_dates),3,4)) , "-",
                             month(candidate_dates), "-",
                             day(candidate_dates)))


      df[ids %in% candidate_ids, date] <- new_dates

      df <- update_error_record(df,
                                df[df$id %in% candidate_ids,][["id"]],
                                date,
                                "date_transpose_year",
                                candidate_dates,
                                new_dates)

      df

    #   all_dates <- df[[date]]
    #   last_two_digits_different <- df$id[str_sub(all_dates, 3,3) != str_sub(all_dates, 4,4)]
    #   # duplicate_ids <- attr(df, "error_record") %>% filter(error == "duplicates") %>% pull(id)
    #
    #   # candidate_ids <- sort(sample(setdiff(last_two_digits_different,duplicate_ids), n_errors))
    #   candidate_ids <- last_two_digits_different
    #   old_values <- df[df$id %in% candidate_ids,][[date]]
    #   new_values <- old_values
    #   old_values_yr <- year(old_values)
    #   ###year(new_values) <- str_sub(old_values_yr, 1, 2) %>% str_c(old_values_yr %>% str_sub(3, 4) %>% transpose()) %>% as.integer()
    #   year(new_values) <- str_sub(old_values_yr, 1, 2) %>% str_c(old_values_yr %>% str_sub(4)) %>% str_c(old_values_yr %>% str_sub(3,3)) %>% as.integer()
    #   df[df$id %in% candidate_ids,date] <- new_values
    # }
    # df <- update_error_record(df,
    #                           candidate_ids,
    #                           date,
    #                           "date_transpose_year",
    #                           old_values,
    #                           new_values)
    # df
    }
  } else if(token == "month"){
    # browser()
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
    transposable_days_all <- c(1,2,10,12,20,21,30,31)
    transposable_dates <-
      (day(old_dates) %in% transposable_days_all) |
      ((month(old_dates) != 2) &
         (day(old_dates) %in% c(3, transposable_days_all))) |
      ((month(old_dates) %in% c(1,3,5,7,8,10,12)) &
         (day(old_dates) %in% c(13, transposable_days_all)))
    potential_ids <- ids[transposable_dates]

    if(length(potential_ids) < n_errors){
      n_errors <- length(potential_ids)
      warning("Not enough transposable dates found. Transposing all available.")
    }

    candidate_ids <- sample(potential_ids, n_errors)
    candidate_dates <- old_dates[ids %in% candidate_ids]
    # browser()
    new_dates <- ymd(str_c(year(candidate_dates), "-",
                           month(candidate_dates), "-",
                           tpose(str_pad(day(candidate_dates),
                                             width = 2,
                                             side = "left",
                                             pad = "0"))
                           ))

    # new_dates <- ymd(str_c(year(old_dates), month(old_dates), transpose(day(old_dates))))
    # candidate_ids <- ids[!is.na(new_dates)] %>% sample(n_errors)

    df[ids %in% candidate_ids, date] <- new_dates

    df <- update_error_record(df,
                              df[df$id %in% candidate_ids,][["id"]],
                              date,
                              "date_transpose_day",
                              candidate_dates,
                              new_dates)

    df
  }
}


date_replace <- function(df, n_errors, date, token = "year"){
  ids <- df$id
  old_dates <- df[[date]]

  if(length(ids) < n_errors){
    n_errors <- length(male_ids)
    warning("Not enough candidates for date replaces found.")
  }

  candidate_ids <- sample(ids, n_errors)
  candidate_dates <- old_dates[ids %in% candidate_ids]

  if(token == "year"){
    replacements <-
      year(candidate_dates) %>%
      str_sub(3,4) %>%
      as.integer() %>%
      repl() %>%
      str_pad(width = 2, side = "left", pad = "0")
    new_years <-
      year(candidate_dates) %>%
      str_sub(1,2) %>%
      str_c(replacements) %>%
      as.integer()
    new_dates <- candidate_dates
    year(new_dates) <- new_years
  } else if(token == "month"){
    new_months <-
      map_dbl(candidate_dates, function(date_x){
        sample(valid_months(day(date_x), year(date_x)), 1)
      })
    new_dates <- candidate_dates
    month(new_dates) <- new_months
  } else {
    new_days <-
      map_dbl(candidate_dates, function(date_x){
        # browser()
        valid_day <- valid_days(day(date_x), year(date_x))
        replacement <-
          day(date_x) %>%
          str_pad(width = 2, side = "left", pad = "0") %>%
          repl.default(error_chars = 0:9) %>%
          as.integer()
        min(max(valid_day), replacement)
      })
    new_dates <- candidate_dates
    day(new_dates) <- new_days
  }

  df[ids %in% candidate_ids, date] <- new_dates

  df <- update_error_record(df,
                            df[df$id %in% candidate_ids,][["id"]],
                            date,
                            str_c("date_replace_", token),
                            candidate_dates,
                            new_dates)

  df
}

######################
valid_days <- function(month, year){
  if(month %in% c(1,3,5,7,8,10,12)){
    1:31
  } else if(month == 2 & lubridate::leap_year(year)){
    1:29
  } else if(month == 2){
    1:28
  } else {
    30
  }
}

valid_months <- function(day, year){
  if(day <= 29 & lubridate::leap_year(year)){
    1:12
  } else if(day <= 28){
    1:12
  } else if(day <= 30){
    c(1,3:12)
  } else {
    c(1,3,5,7,8,10,12)
  }
}
#
# valid_year <- function(day, month, start = 1900, end = 2016){
#   if(day == 29 & month == 2){
#     (start:end)[lubridate::leap_year(start:end)]
#   } else {
#     start:end
#   }
# }
#
# err_date <- function(x, ...) {
#   UseMethod("err_date")
# }
#
#
# err_date.data.frame <- function(df, n_errors, col_names = "dob"){
#   n <- seq_len(nrow(df))
#   p <- length(col_names)
#   errors_col <- floor(n_errors/p)
#
#   if((n_errors < nrow(df)*p) && (errors_col > 0)) {
#     for(i in seq_len(p)) {
#       rows <- sample(n, errors_col)
#       col_name <- col_names[i]
#       before <- df %>% filter(row_number() %in% rows) %>% pull(col_name)
#       after <- before %>% err_date.default()
#       df[rows, col_name] <-  after
#       df <- update_error_record(df, df$id[rows], col_name, "indel", before, after)
#     }
#   } else {
#     for(i in seq_len(n_errors)){
#       rows <- sample(n, 1)
#       col_name <- sample(col_names, 1)
#       before <- df%>% filter(row_number() %in% rows) %>% pull(col_name)
#       after <- before %>% err_date.default()
#       df[rows, col_name] <-  after
#       df <- update_error_record(df, df$id[rows], col_name, "indel", before, after)
#     }
#   }
#
#   df
# }
#
# err_date.default <- function(dates){
#   # browser()
#   map_dbl(dates, function(date_x){
#     day_x <- lubridate::day(date_x)
#     month_x <- lubridate::month(date_x)
#     year_x <- lubridate::year(date_x)
#     message(date_x)
#     error_part <- sample(c("day", "month", "year"), 1)
#     message(error_part)
#     if(error_part == "day"){
#       new_day_x <- repl.default(day_x, 0:9) %>% as.double()
#       while(!new_day_x %in% valid_days(month_x, year_x)){
#         print(new_day_x)
#         new_day_x <- repl.default(day_x, 0:9) %>% as.double()
#       }
#       lubridate::day(date_x) <- new_day_x %>% as.double()
#     } else if (error_part == "month"){
#       new_month_x <- repl.default(month_x, 0:9) %>% as.double()
#       while(!new_month_x %in% valid_months(day_x, year_x)){
#         print(new_month_x)
#         new_month_x <- repl.default(month_x, 0:9) %>% as.double()
#       }
#       lubridate::month(date_x) <- new_month_x %>% as.double()
#     } else {
#       new_year_x <- repl.default(year_x, 0:9) %>% as.double()
#       while(!new_year_x %in% valid_year(day_x, month_x)){
#         print(new_year_x)
#         new_year_x <- repl.default(year_x, 0:9) %>% as.double()
#       }
#       lubridate::year(date_x) <- new_year_x %>% as.double()
#     }
#     date_x
#   }) %>%
#     lubridate::as_date()
# }
