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
