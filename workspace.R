nicknames <- read_csv("C:/Users/gurud/Downloads/nicknames.csv")

nicknames <- nicknames %>%
  select(-id) %>%
  mutate(type = "to_nick")

nicknames2 <- tibble(name = nicknames$nickname, nickname = nicknames$name) %>%
  mutate(type = "to_proper")

names_lookup <-
  nicknames %>%
  bind_rows(nicknames2) %>%
  rename(lookup_alternate = nickname,
         lookup_name = name,
         lookup_type = type) %>%
  arrange(lookup_name) %>%
  mutate(lookup_id = row_number()) %>%
  select(lookup_id, everything())

devtools::use_data(names_lookup, overwrite = T)



x <- quo(lookup_name)

names_lookup %>%
  select(!!x)













ncv <-
ncv %>%
  mutate(zip = city_state %>% str_extract(one_or_more(DIGIT) %R% END),
         state = city_state %>% str_replace(zip, "") %>% str_split(", ", simplify = T) %>% .[,2] %>% str_trim(),
         city = str_replace(city_state, zip, "") %>% str_replace(state, "") %>% str_replace(",", "") %>% str_trim(),
         full_address = str_c(street_address, city_state, sep = ", ")) %>%
  rename(city_state_zip = city_state)


# dt_a <-
#   tibble(sex = sample(c("f", "m"), 100, replace = T),
#        lname = sample(lnames_all,25) %>% sample(100, T)) %>%
#   mutate(fname =
#            map_chr(sex, function(x){
#              fname <-
#                ifelse(x =="f", sample(fnames_female, 1), sample(fnames_male,1))
#            }),
#           phone = r_phone_numbers(n),
#          ssn = r_national_identification_numbers(n),
#          job = ch_generate("job", n = n) %>% pull(job),
#          credit_card = r_credit_card_numbers(n),
#          dob = r_date_of_births(n)) %>%
#   bind_cols(sample_n(address_tbl, n))
#
# error_table <- tribble(~error, ~amount, ~col_names, ~arguments,
#                        "swap_fields",20,"lname, fname","",
#                        "blanks_to_hyphens",20,"street_address","all = T",
#                        "hyphens_to_blanks",20,"credit_card","all = F",
#                        "ch1_to_ch2",20,"street_address","all = T, ch1 = ',', ch2 = '|'",
#                        "first_letter_abbreviate",20,"job","",
#                        "invert_real_and_nicknames",20,"fname","",
#                        "nick_to_realnames",10,"fname","",
#                       "real_to_nicknames",10,"fname","",
#                       "indel", 20,"lname","",
#                       "repl",20,"fname","",
#                       "transpose",20,"lname, fname","",
#                       "married_name_change",20,"lname","lname = 'lname', sex = 'sex'",
#                       "duplicates",20,"credit_card",""
#                       )

ncv <-
ncv %>%
  mutate(tmp = lname,
         lname = fname,
         fname = tmp) %>%
  select(-tmp)
