library(charlatan)
library(stringr)
library(tidyverse)
devtools::load_all(".")

set.seed(1)

dt_a <- ch_generate("name", "job", "phone_number")
class(dt_a$phone_number) <- c("number",class(dt_a$phone_number))
error_table <- tribble(~error, ~amount, ~col_names, ~arguments,
                       "swap_fields",5,"name, job","",
                       "blanks_to_hyphens",5,"name","all = T",
                       "hyphens_to_blanks",5,"name","all = F",
                       "ch1_to_ch2",5,"name","all = T, ch1 = ' ', ch2 = '#'",
                       "first_letter_abbreviate",5,"job","",
                       "invert_real_and_nicknames",5,"name","",
                       "nick_to_realnames",5,"name","",
                      "real_to_nicknames",5,"name","",
                      "indel", 2,"name, job, phone_number","",
                      "repl",2,"name, phone_number","",
                      "transpose",3,"name, phone_number",""
                      )

dt_a <-
  dt_a %>%
  mutate(name = c("Jonathan", "Chris", "Richard", name)[1:nrow(dt_a)])

dt_a <- dt_a %>%
  prep_data() %>%
  pluck("df_original")



dt_a$sex = sample(c("f","m"), nrow(dt_a), replace = T)

(error_result <-
  dt_a %>%
  prep_data() %>%
  mess_data(error_table))

error_record <- attr(error_result$df_secondary, "error_record")
View(error_record)
error_record %>%
  count(error, sort = T)

error_record %>%
  filter(id == 4, field == "phone_number") %>%
  mutate(dist = stringdist::stringdist(lag(after),after))
stringdist::stringdist("258.547.3512x17250", "258.5403713556317255")
