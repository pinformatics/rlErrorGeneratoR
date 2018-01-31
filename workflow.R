library(rebus)
library(generator)
library(stringr)
library(tidyverse)
library(devtools)
library(magrittr)
library(charlatan)
devtools::load_all(".")


#https://www.randomlists.com/random-addresses
# split_exp <- "," %R% SPACE %R%
#   UPPER %R% UPPER %R%
#   SPACE %R% DIGIT %R% DIGIT %R% DIGIT %R% DIGIT %R% DIGIT
#
# pin_st <- addresses %>% str_extract_all(split_exp) %>%.[[1]]
#
# addresses_1 <-
#   addresses %>%
#   str_split(split_exp) %>%
#   .[[1]] %>%
#   str_trim() %>%
#   str_replace_all(NEWLINE, "")%>%
#   str_replace_all("  ", "")
#
#
# (addr <-
#   tibble(full_address = addresses_1) %>%
#   filter(str_length(full_address) > 1) %>%
#   mutate(
#     full_address =str_c(full_address, pin_st),
#     zip = str_extract(full_address, one_or_more(DIGIT) %R% END),
#     state = str_match_all(full_address,
#                             "," %R% SPACE %R%
#                               capture(UPPER %R% UPPER)
#                             %R% SPACE %R% one_or_more(DIGIT) %R% END) %>%
#             map_chr(~.x[1,2]),
#     street_address = str_replace(full_address, pin_st,"")
#   ))
#
# address_tbl <- addr
#
set.seed(1)

n <- 100

dt_a <-
  tibble(sex = sample(c("f", "m"), 100, replace = T),
       lname = sample(lnames_all,25) %>% sample(100, T)) %>%
  mutate(fname =
           map_chr(sex, function(x){
             fname <-
               ifelse(x =="f", sample(fnames_female, 1), sample(fnames_male,1))
           }),
          phone = r_phone_numbers(n),
         ssn = r_national_identification_numbers(n),
         job = ch_generate("job", n = n) %>% pull(job),
         credit_card = r_credit_card_numbers(n),
         dob = r_date_of_births(n)) %>%
  bind_cols(sample_n(address_tbl, n))

error_table <- tribble(~error, ~amount, ~col_names, ~arguments,
                       "swap_fields",20,"lname, fname","",
                       "blanks_to_hyphens",20,"street_address","all = T",
                       "hyphens_to_blanks",20,"credit_card","all = F",
                       "ch1_to_ch2",20,"street_address","all = T, ch1 = ',', ch2 = '|'",
                       "first_letter_abbreviate",20,"job","",
                       "invert_real_and_nicknames",20,"fname","",
                       "nick_to_realnames",10,"fname","",
                      "real_to_nicknames",10,"fname","",
                      "indel", 20,"lname","",
                      "repl",20,"fname","",
                      "transpose",20,"lname, fname","",
                      "married_name_change",20,"lname","lname = 'lname', sex = 'sex'",
                      "duplicates",20,"credit_card",""
                      )

dt_a <- dt_a %>%
  prep_data() %>%
  pluck("df_original")




# dt_a$sex = sample(c("f","m"), nrow(dt_a), replace = T)

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
