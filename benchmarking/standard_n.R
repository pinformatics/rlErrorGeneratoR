source("R/paper2/utils.R")

df_error_dist <- read_csv("R/paper2/error_table.csv")

counties <- 
  fs::dir_info("data/ncvote/apr13/") %>% 
  arrange(size) %>% 
  mutate(county = str_extract(path, "ncvoter\\d+") %>% str_extract("\\d+") %>% as.integer()) %>% 
  pull(county)


get_error_pcts_req_n <- function(){
  dir_ls("data/paper2/counties/") %>% 
    str_subset("rds") %>% 
    map_df(read_rds) %>% 
    mutate(n = df_pairs %>% map_int(nrow)) %>% 
    group_by(error_percent) %>% 
    summarise(n = sum(n)) %>% 
    filter(n < 27000) %>% 
    print() %>% 
    pull(error_percent)
}

present_counties <- function(){
  dir_ls("data/paper2/counties/") %>% 
    str_subset("rds") %>% 
    fs::path_file() %>% 
    str_extract("\\d+") %>% 
    as.integer()
}

try_counties <- counties[!counties %in% present_counties()]
for(i in try_counties){
  errors <- get_error_pcts_req_n()
  if(nrow(errors) == 0){
    break
  }
  generate_pairs_for_county(i, errors) %>% 
    write_rds(glue("data/paper2/counties/df_messed_collection_{i}.rds"))
}


fs::dir_info("data/ncvote/apr13/") %>% 
  arrange(size) %>% 
  mutate(county = str_extract(path, "ncvoter\\d+") %>% str_extract("\\d+") %>% as.integer()) %>% 
  filter(county %in% c(21,3,38,48,6,69,87,93,99)) %>% 
  mutate(county_name = map_chr(path, function(x){
    read_delim(x, delim = "\t") %>% 
      pull(county_desc) %>% 
      .[1]
  })) %>% 
  pull(county_name) %>% 
  str_to_title() %>% 
  str_c(collapse = ", ")



df_errors <- 
  dir_ls("data/paper2/counties/") %>% 
  str_subset("rds") %>% 
  map_df(read_rds) 

set.seed(133)
n <- 27000
indices <- (1:n)
test_indices <- sample(indices, 2000)
train_indices <- indices[!indices %in% test_indices]
train_indices <- sample(train_indices, length(train_indices))

df_messed_collection <- 
  df_errors %>% 
  group_by(error_percent) %>% 
  select(error_percent, df_pairs, df_feature) %>% 
  nest() %>% 
  mutate(
  df_feature = map(data, function(df_list){
    bind_rows(df_list$df_feature) 
  }),
  df_pairs = map(data, function(df_list){
    bind_rows(df_list$df_pairs) 
  }),
  indices_all = map(df_feature, function(x){
    sample(1:(nrow(x)), size = n)
  }),
  df_feature = map2(df_feature, indices_all, function(x,y){
    x[y, ] %>% sample_n(n)
  }),
  df_test = map(df_feature, ~.x[test_indices, ]),
  df_train = map(df_feature, ~.x[train_indices, ])
  ) %>% 
  select(-data) %>% 
  crossing(train_n = seq(1000, 25000, 3000)) %>% 
  mutate(
    df_train = map2(df_train, train_n, function(x, y){
      x[1:y, ]
    })
  ) %>% 
  select(error_percent, train_n, df_train, df_test, everything())

df_messed_collection %>% write_rds("data/paper2/df_messed_collection.rds")

df_messed_collection <- read_rds("data/paper2/df_messed_collection.rds")

summary_func <- function(data, lev = NULL, model = NULL) {
  pred <- data$match
  match <- ifelse(as.character(data$obs) == "match", 1, 0) 
  # browser()
  res <- calculate_metrics_prob(match, pred)
  vals <- res$value
  names(vals) <- res$metric
  vals
}

train_control <- 
  trainControl(method = "cv", 
               number = 10,
               verboseIter = T,
               savePredictions = TRUE,
               summaryFunction = summary_func,
               classProbs = TRUE)


path_model_data <- "data/paper2/models"
set.seed(13)
df_messed_collection %>% 
  filter((error_percent == 0.05 & train_n == 10000)) %>% 
  arrange(train_n, error_percent) %>%
  mutate(
    models = 
      pmap(list(df_train = df_train, 
                 df_test = df_test, 
                 error_percent = error_percent,
                 train_n = train_n), 
            function(df_train, df_test, error_percent, train_n){
              
              message(glue("Error Percent: {error_percent}
                            Train N: {train_n}"))
              
              run <- glue("{path_model_data}/e-{error_percent}_n-{train_n}")
              
              message("RF")
              model_rf <-
                train(
                  match ~ .,
                  data = df_train,
                  trControl = train_control,
                  tuneGrid = expand.grid(.mtry = seq(3, 15, 2)),
                  importance = TRUE,
                  keep.forest= TRUE,
                  metric = "accuracy",
                  preProcess = c("medianImpute"),
                  ntree = 350,
                  method = "rf",
                  na.action = na.pass)
              print(model_rf)
              print(caret::confusionMatrix(df_test$match,
                                           predict(model_rf, df_test,
                                                   na.action = na.pass),
                                           positive = "match"))
              model_rf %>% write_rds(glue("{run}_rf.rds"))


              message("SVM")
              model_svm <-
                train(
                  match ~ .,
                  data = df_train,
                  tuneGrid = expand.grid(sigma = seq(0.03546, 0.9, length.out = 3),
                                         C = c(0.1, 0.5, 1, 10)),
                  trControl = train_control,
                  metric = "accuracy",
                  preProcess = c("medianImpute"),
                  method = "svmRadial",
                  na.action = na.pass)
              print(model_svm)
              print(caret::confusionMatrix(df_test$match,
                                           predict(model_svm,
                                                   newdata = df_test,
                                                   na.action = na.pass),
                                           positive = "match"))
              model_svm %>% write_rds(glue("{run}_svm.rds"))
              
              message("NN")
              build_nn(df_train, glue("{run}_nn.h5"))
              
              NULL
            }) 
  )







