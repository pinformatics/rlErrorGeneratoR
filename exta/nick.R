pacman::p_load(tidyverse, rebus)

# df_a <-
  # read_file("~/Downloads/nick/name_to_nick.csv")

df_already <-
  read_csv("~/Downloads/nick/nicknames.csv") %>%
  mutate_all(str_to_lower)

str_names <-
  str_c(
    read_file("~/Downloads/nick/names.csv"),
    read_file("~/Downloads/nick/name_to_nick.csv"),
    read_file("~/Downloads/nick/nick_to_name.csv")
    )
str_names <- str_split(str_names, NEWLINE) %>% .[[1]]

nick_table <-
  str_names %>%
  map_df(function(name_list){
    names <- name_list %>%
      str_split(",") %>%
      .[[1]]
    crossing(key = names, lookup = names) %>%
      filter(key != lookup)
  }) %>%
  bind_rows(df_already %>%
              select(key = name, lookup = nickname)) %>%
  distinct()


nick_table %>%
  write_csv("exta/name_lookup.csv")

df_a %>%
  mutate_if(is.character, str_to_lower) %>%
  inner_join(nick_table,
             c("fname" = "key")) %>%
  select(id_a, fname, lookup) %>%
  group_by(id_a) %>%
  sample_n(1) %>%
  write_csv("exta/mat13_lookedup.csv")

# nick_lookup <- function(name){
#   name <- "theo"
#   str_names %>%
#     str_subset(name) %>%
#     str_split(",", simplify = T)
# }



names_lookup %>%
  filter(lookup_type == "to_nick")
