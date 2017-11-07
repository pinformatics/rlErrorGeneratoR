library(charlatan)
library(stringr)
library(tidyverse)

dt_a <- ch_generate("name", "job", "phone_number")
class(dt_a$phone_number) <- c("number",class(dt_a$phone_number))
error_table <- tribble(~error, ~amount,~col_names,
                      "indel", 45,"name, job, phone_number",
                      "repl",50,"name, phone_number"
                      )
(error_result <-
  dt_a %>%
  prep_data() %>%
  mess_data(error_table))

error_record <- attr(error_result$df_secondary, "error_record")
error_record %>%
  count(id, field, sort = T)

error_record %>%
  filter(id == 3, field == "phone_number") %>%
  mutate(dist = stringdist::stringdist(lag(after),after))
stringdist::stringdist("258.547.3512x17250", "258.5403713556317255")
