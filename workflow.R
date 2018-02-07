pacman::p_load(rebus, janitor, stringr, tidyverse, devtools, charlatan, generator)
load_all(".")

set.seed(1)


dt_a <-
  ncv %>%
  sample_n(250)

#duplicates should be last
(error_table <- read_csv("error_table.csv") %>%
    mutate(arguments = if_else(is.na(arguments), "", arguments)))

(error_result <-
  dt_a %>%
  gen_birthday_from_age(age = "age") %>%
  prep_data() %>%
  mess_data(error_table))

(error_record <- attr(error_result$df_secondary, "error_record"))
View(error_record)
error_record %>%
  count(error, sort = T)

