#pacman::p_load(rebus, janitor, stringr, tidyverse, devtools, charlatan, generator)
packages <- c("rebus", "janitor", "stringr", "tidyverse", "devtools", "charlatan", "generator", "stringdist", "lubridate")
purrr::walk(packages, ~library(.x, character.only = T))
load_all(".")

set.seed(1)

#########################################################################
# YANCEY county
apr13 <- read.csv("apr13.csv")

## bigest city in ncv
#df <- ncv[which(ncv$city == tail(names(sort(table(ncv$city))), 1)),]

df <- apr13[c(3, 11, 10, 12, 29, 30, 14, 17, 16, 15)]
names(df) <- c("voter_registration_number", "fname", "lname", "mname", "gender", "age", "street_address", "zip", "state", "city")
df <- df %>%
  mutate_if(is.factor, as.character) %>%
  mutate_if(is.character, trimws)

natural_dup <- df[duplicated(df[2:6]),]
df <- df[!duplicated(df[2:6]),]

####### add twins #######

df_t <- df %>%
  prepare_data() %>%
  make_twins()
error_record <- attr(df_t, "error_record")

error_table <- read_csv("error_table.csv")
n_record <- 200
n_dup <- 2

df_a <- sample_n(df_t, n_record)

dup_result <- df_a %>%
  dup_data(n_dup = n_dup, no_errs = nrow(error_table))

error_result <- dup_result$df_secondary %>%
  add_error(error_table = error_table, error_record = error_record)

error_record <- attr(error_result, "error_record")

dup_result$df_secondary <- error_result
#########################################################################




###randomly pick 250 record
dt_a <-
  ncv %>%
  sample_n(250)

#duplicates should be last
###replace NA with "" in error_table
(error_table <-
    read_csv("error_table.csv") %>%
    mutate(arguments = if_else(is.na(arguments), "", arguments)))

### add date of birth, duplicate data and add error on duplicate
(error_result <-
    dt_a %>%
    gen_birthday_from_age(age = "age") %>%
    prep_data() %>%
    mess_data(error_table))
(error_record <- attr(error_result$df_secondary, "error_record"))

######################################


View(error_record)
error_record %>%
  count(error, sort = T)

error_result %>%
  pluck(1) %>%
  semi_join(
    error_record %>%
      filter(error == "twins_generate")
  )


error_result %>%
  pluck(2) %>%
  semi_join(
    error_record %>%
              filter(error == "twins_generate")
  )

