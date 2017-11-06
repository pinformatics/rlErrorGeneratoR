library(charlatan)
library(tidyverse)

dt_a <- ch_generate("name", "job", "phone_number")

dt_a %>%
  id_data()

error_table <- tribble(~error, ~amount,
                      "indel", 0.3,
                      "replace", 0.1)
