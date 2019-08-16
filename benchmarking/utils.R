if(!"pacman" %in% installed.packages()){
  install.packages("pacman")
}


pacman::p_load(tidyverse, stringdist, phonics, glue, caret, lubridate,
               rebus, fs, ModelMetrics, MLmetrics, caretEnsemble,
               PGRdup, doParallel, pushoverr, rlErrorGeneratoR, pROC,
               scico, RColorBrewer, ggthemes, LaCroixColoR, plotly, keras,
               janitor)

# source("data/ credentials.R")


# preprocessing ------------------------------------------------------

extract_major_token <- function(x){
  str_split(x, " ") %>% 
    map_chr(function(s){
      # print(s)
      if(any(is.na(s))){
        NA
      } else if(str_length(s[1]) >= 3){
        s[1]
      } else{
        longest_token <- s[str_length(s) == max(str_length(s))]
        sample(longest_token, 1)
      }
    })
}

# preprocess_data <- function(df, year_a = T){
#   
#   df  %>% 
#     select(starts_with("id"), 
#            fname = first_name, lname = last_name, 
#            birth_age, gender_code, race_code, 
#            voter_reg_num, name_suffix) %>% 
#     mutate(birth_age = birth_age %>% as.integer(),
#            name_suffix = if_else(is.na(name_suffix), "", name_suffix),
#            lname = glue("{lname} {name_suffix}") %>% str_trim(),
#            # fname_longest = extract_major_token(fname),
#            # lname_longest = extract_major_token(lname),
#            year_a = year_a,
#            birth_year = ifelse(year_a, 2013 - birth_age, 2017 - birth_age)) %>% 
#     select(-name_suffix, -year_a) %>% 
#     add_count(fname) %>%
#     rename(ffreq = n) %>% 
#     add_count(lname) %>% 
#     rename(lfreq = n) %>% 
#     mutate(ffreq = scale(ffreq),
#            lfreq = scale(lfreq))
# }

preprocess_data <- function(df,df_dob){
  # browser()
  df  %>% 
    select(starts_with("id"), 
           fname = first_name, lname = last_name, 
           birth_age, gender_code, race_code, 
           voter_reg_num, name_suffix) %>% #, ncid
    left_join(df_dob, by = "voter_reg_num") %>% 
    mutate(birth_age = birth_age %>% as.integer(),
           name_suffix = if_else(is.na(name_suffix), "", name_suffix),
           birth_year = year(dob)
    ) %>% 
    mutate_if(is.character, str_to_lower) %>% 
    add_count_name(fname, "ffreq", T) %>%
    add_count_name(lname, "lfreq", T)  %>% 
    mutate(lname = glue("{lname} {name_suffix}") %>% str_trim()) %>% 
    select(-name_suffix)
  
  # mname
  # mname = if_else(is.na(mname), "", mname),
  # fmname =  glue("{fname} {mname}") %>% str_trim(),
  
}

# views -----------------------------------------------------------------

vectors_to_pairs <- function(df, View = F){
  # id_var <- enquo(id_var)
  
  if(!"pair_id" %in% colnames(df)){
    df <-
      df %>% 
      mutate(pair_id = row_number())
  }
  
  df_a <-
    df %>%
    select(-contains("_b"))
  
  colnames(df_a) <- str_replace(colnames(df_a), "_a", "")
  
  df_b <-
    df %>%
    select(-contains("_a"))
  
  colnames(df_b) <- str_replace(colnames(df_b), "_b", "")
  
  df <- 
    bind_rows(df_a, df_b) %>%
    arrange(pair_id) %>% 
    select(pair_id, 
           fname, lname, gender_code, race_code, birth_year, 
           match, everything())
  
  if(View){
    View(df)
  } else {
    df
  }
  
}


pairs_to_vectors <- function(df){
  
  df_a <- 
    df %>% 
    group_by(pair_id) %>% 
    slice(1)%>% 
    ungroup()
  
  names(df_a) <- 
    str_c(names(df_a), "_a") %>% 
    str_replace("pair_id_a", "pair_id") %>% 
    str_replace("match_a", "match")
  
  df_b <- 
    df %>% 
    group_by(pair_id) %>% 
    slice(2) %>% 
    ungroup()
  
  names(df_b) <- 
    str_c(names(df_b), "_b") %>% 
    str_replace("pair_id_b", "pair_id") %>% 
    str_replace("match_b", "match")
  
  df_a %>% 
    left_join(df_b) %>% 
    select(pair_id, 
           id_a, fname_a, lname_a, birth_year_a, contains("bplstr_a"), 
           gender_code_a, race_code_a,
           id_b, fname_b, lname_b, birth_year_b, contains("bplstr_b"), 
           gender_code_b, race_code_b, 
           everything())
}

attach_dbs_to_ids <- function(df_id){
  df_id %>% 
    left_join(df_a_mod, by = "id_a") %>% 
    left_join(df_b_mod, by = "id_b", suffix = c("_a", "_b"))
}

attach_dbs_to_ids_dfs <- function(df_id, df_a_mod, df_b_mod){
  df_id %>% 
    left_join(df_a_mod, by = "id_a") %>% 
    left_join(df_b_mod, by = "id_b", suffix = c("_a", "_b"))
}




# feature_extraction ------------------------------------------------------
string_dist_norm <- function(x, y, ...){
  stringdist(x, y, ...)/map2_int(str_length(x),str_length(y), ~max(c(.x,.y)))
}

standardize_stringdist <- function(x, y, dist = "osa"){
  stringdist(x, y, dist)/map2_int(str_length(x), str_length(y), ~max(c(.x,.y)))
}

extract_initals <- function(x){
  x %>% str_split(" ") %>% map_chr(~str_c(str_sub(.x,1,1), collapse = ""))
}

summarise_all_string_metrics <- function(x, y, col_name){
  # "osa", "lv", "lcs", "qgram", "jaccard"
  methods <- c("dl", "jw", "soundex")
  
  dists <- map_dbl(methods, ~stringdist(x, y, method = .x))
  tibble(metric = str_c("metric_", col_name, "_", methods), dists = dists) %>% 
    spread(metric, dists)
}

add_feature_vector <- function(df, df_a_mod, df_b_mod){
  # browser()
  
  df %>%
    select(-contains("freq")) %>% 
    left_join(df_a_mod %>% 
                count(fname) %>% 
                rename(fname_a = fname,
                       ffreq_a = n) %>% 
                mutate(ffreq_a = scale(ffreq_a)[,1])) %>% 
    left_join(df_a_mod %>% 
                count(lname) %>% 
                rename(lname_a = lname,
                       lfreq_a = n) %>% 
                mutate(lfreq_a = scale(lfreq_a)[,1])) %>% 
    left_join(df_b_mod %>% 
                count(fname) %>% 
                rename(fname_b = fname,
                       ffreq_b = n) %>% 
                mutate(ffreq_b = scale(ffreq_b)[,1])) %>% 
    left_join(df_b_mod %>% 
                count(lname) %>% 
                rename(lname_b = lname,
                       lfreq_b = n) %>% 
                mutate(lfreq_b = scale(lfreq_b)[,1])) %>%
    mutate(pair_id = row_number(),
           # dob
           birth_year_a = as.double(year(dob_a)),
           birth_year_b = as.double(year(dob_b)),
           birth_day_a = as.double(day(dob_a)),
           birth_day_b = as.double(day(dob_b)),
           birth_month_a = as.double(month(dob_a)),
           birth_month_b = as.double(month(dob_b)),
           year_diff = as.integer(birth_year_a) - as.integer(birth_year_b),
           metric_dob_dist = stringdist(dob_a, dob_b, "dl"),
           metric_year_dist = stringdist(birth_year_a, birth_year_b, "dl"),
           metric_month_dist = stringdist(birth_month_a, birth_month_b, "dl"),
           metric_day_dist = stringdist(birth_day_a, birth_day_b, "dl"),
           metric_dm_swaps = 
             ((!is.na(dob_a)) & (!is.na(dob_b))) &
             (birth_month_a == birth_day_b) & 
             (birth_month_b == birth_day_a),
           metric_age_a = as.double(ymd("2013-04-30") - dob_a)/365,
           metric_age_b = as.double(ymd("2017-03-31") - dob_b)/365,
           
           # fname
           fname_metrics = map2(fname_a, fname_b, 
                                summarise_all_string_metrics, "fname"),
           metric_ffreq_a = ffreq_a,
           metric_ffreq_b = ffreq_b,
           # metric_ffreq_mean = map2_dbl(ffreq_a, ffreq_b, ~mean(c(.x,.y))),
           # metric_ffreq_diff = abs(ffreq_a - ffreq_b),
           
           # lname
           lname_metrics = map2(lname_a, lname_b, 
                                summarise_all_string_metrics, "lname"),
           metric_lfreq_a = lfreq_a,
           metric_lfreq_b = lfreq_b,
           # metric_lfreq_mean = map2_dbl(lfreq_a, lfreq_b, ~mean(c(.x,.y))),
           
           # name swaps
           metric_name_swaps = 
             ((!is.na(fname_a)) & (!is.na(fname_b)) & 
                (!is.na(lname_a)) & (!is.na(lname_b))) &
             (lname_a == fname_b) & 
             (lname_b == fname_a),
           
           # gender
           gender_code = map2_chr(gender_code_a, gender_code_b, function(x, y){
             str_c(sort(c(x, y)), collapse = "")
           }),
           metric_gender_code_ff = gender_code  %>% str_count("f"),
           metric_gender_code_mm = gender_code  %>% str_count("m"),
           # metric_gender_code_same = gender_code_a == gender_code_b,
           
           # race
           race_code = map2_chr(race_code_a, race_code_b, function(x, y){
             str_c(sort(c(x, y)), collapse = "")
           }),
           metric_race_code_same = race_code_a == race_code_b,
           metric_race_code_ww_bb = race_code == "ww" | race_code == "bb",
           
           # marriage
           metric_potential_marriage = 
             (!is.na(metric_age_a) | !is.na(metric_age_b)) &
             ((metric_age_a >= 20) | (metric_age_b >= 20)) &
             (!is.na(lname_a) & !is.na(lname_b)) &
             (lname_a != lname_b) &
             (gender_code_a == "f" | gender_code_b == "f")
           
           # metric_year_align = 
           #   stringdist(birth_year_a, birth_year_b, "dl")/3,
           # fname_letters_a = 
           #   fname_a %>% 
           #   str_split(" ") %>% 
           #   map_chr(~str_c(str_sub(.x,1,1), collapse = "")),
           # fname_letters_b = 
           #   fname_b %>% 
           #   str_split(" ") %>% 
           #   map_chr(~str_c(str_sub(.x,1,1), collapse = "")),
           # metric_fname_letters_align = 
           #   stringdist(fname_letters_a, fname_letters_b, "osa")/
           #   map2_int(str_length(fname_letters_b), str_length(fname_letters_a), ~max(c(.x,.y))),
           # metric_ffreq_min = map2_dbl(metric_ffreq_a, metric_ffreq_b, ~min(c(.x,.y))),
           # metric_ffreq_max = map2_dbl(metric_ffreq_a, metric_ffreq_b, ~max(c(.x,.y))),
           # metric_ffreq_diff = map2_dbl(metric_ffreq_a, metric_ffreq_b, ~(.x-.y)),
           # metric_lfreq_min = map2_dbl(metric_lfreq_a, metric_lfreq_b, ~min(c(.x,.y))),
           # metric_lfreq_max = map2_dbl(metric_lfreq_a, metric_lfreq_b, ~max(c(.x,.y))),
           # metric_lfreq_diff = map2_dbl(metric_lfreq_a, metric_lfreq_b, ~(.x-.y))
    ) %>%
    unnest() %>%
    select(pair_id, 
           fname_a, fname_b,
           # mname_a, mname_b,
           lname_a, lname_b,
           starts_with("dob"), 
           starts_with("gender_code"), 
           starts_with("race_code"),
           starts_with("metric"),
           everything()) %>%
    mutate(match = match %>% factor(levels = c("unmatch", "match"))) %>%
    as.data.frame()
}



# helpers -----------------------------------------------------------------

sample_ns <- function(..., seed = 1){
  set.seed(seed)
  sample_n(...)
}

extract_one_to_one <- function(df, field){
  field <- enquo(field)
  df %>% 
    add_count(!!field) %>% 
    filter(n == 1) %>% 
    select(-n)
}


read_data <- function(path){
  df_var <- 
    path %>% 
    path_file() %>%  
    str_replace(or(literal(".csv"), literal(".rds")), "")
  
  if(path %>% str_detect("csv")){
    assign(df_var, read_csv(path), parent.frame())
  } else {
    assign(df_var, read_rds(path), parent.frame())
  }
  
}

print.data.frame <- function(df, ...){
  df %>% as_tibble() %>% print(...)
}


# predict -----------------------------------------------------------------

predict_links_raw <- function(model, df_pair_vector){
  
  df_preds <- 
    predict(model, df_pair_vector , type = "prob", na.action = na.pass) %>% 
    as.tibble()
  
  names(df_preds) <- c("unmatch_prob", "match_prob")
  
  # df_preds_aug <- 
  df_preds %>% 
    mutate(pair_id = df_pair_vector$pair_id,
           conf = abs(match_prob - 0.5)*2,
           match_pred = as.integer(match_prob >= 0.5)) %>% 
    left_join(df_pair_vector, by = "pair_id")
}

links_1to1 <- function(df_preds_aug){
  
  df_pred_1to1 <- 
    df_preds_aug %>% 
    filter(match_pred == 1) %>% 
    select(id_a, id_b) %>% 
    add_count(id_a) %>% 
    add_count(id_b) %>% 
    filter(n == 1, nn == 1) %>% 
    mutate(one_one = TRUE) %>% 
    select(id_a, id_b,one_one)
  
  # df_preds_aug <- 
  df_preds_aug %>% 
    left_join(df_pred_1to1) %>% 
    mutate(match_pred_one_to_one = ifelse(!is.na(one_one), 1, 0)) %>% 
    view_as_pairs() %>% 
    arrange(conf, pair_id) %>% 
    select(pair_id, fname, lname, gender_code, race_code, birth_year, 
           match_prob, conf, match_pred, everything())
  
}

# postprocessing ----------------------------------------------------------

resolve_linkage <- function(df_aug){
  df_match <- 
    df_aug %>% 
    filter(match_pred == 1)
  
  df_aug %>% 
    left_join(df_match %>% 
                find_best_links_aggregated() %>% 
                mutate(match_pred_resolved = 1)) %>% 
    mutate(match_pred_resolved = ifelse(is.na(match_pred_resolved), 0, match_pred_resolved))
}



add_count_name <- function(df, var, var_name, scale = F){
  var_name <- quo_name(var_name)
  var <- enquo(var)
  
  df <- 
    df %>% 
    add_count(!!var)
  
  if(scale){
    df <- 
      df %>% 
      mutate(n = scale(n))
  }
  
  df %>% 
    rename((!!var_name) := n)
  
}

find_best_links_aggregated <- function(df_match){
  df_match_counts <-
    df_match %>%
    select(id_a, id_b, match_prob) %>%
    add_count_name(id_a, "n_a") %>%
    add_count_name(id_b, "n_b")
  
  df_1to1 <-
    df_match_counts %>%
    filter(n_a == 1, n_b == 1) %>% 
    select(id_a, id_b)
  
  # df_prob_1 <- 
  #   df_match_counts %>% 
  #   filter(match_prob == 1) %>% 
  
  
  df_match_counts %>% 
    filter(n_a > 1 | n_b > 1) %>%
    find_best_links(id_a) %>% 
    bind_rows(df_match_counts %>% 
                find_best_links(id_b)) %>% 
    distinct() %>% 
    add_count(id_a) %>% 
    add_count(id_b) %>% 
    filter(n  == 1, nn == 1) %>% 
    select(id_a, id_b) %>% 
    bind_rows(df_1to1)
  
}

find_best_links <- function(df_match, id){
  id = enquo(id)
  
  df_match %>% 
    filter(n_a > 1 | n_b > 1) %>% 
    group_by(!!id) %>% 
    arrange(desc(match_prob), .by_group = T) %>% 
    mutate(odds = match_prob - match_prob[2],
           match_pred_mod = ifelse(odds > 0.4, 1, 0)) %>% 
    select(odds, id_a, id_b, match_pred_mod, everything()) %>% 
    filter(match_pred_mod == 1) %>% 
    ungroup() %>% 
    select(id_a, id_b)
}




# pipeline ----------------------------------------------------------------

link_datasets <- function(df_a, df_b, model){
  
  df_blocked <- 
    multipass_blocking(df_a, df_b) %>% 
    distinct()
  
  df_pair_vector <- 
    df_blocked %>% 
    add_feature_vector()
  
  df_blocked <- 
    df_pair_vector %>% 
    select(id_a, id_b) %>% 
    left_join(df_blocked)
  
  df_preds_aug <- 
    predict_links_raw(model, df_pair_vector)
  
  # predict_links_1to1 <- 
  pf_preds_aug %>% 
    resolve_linkage()
  
}


generate_pairs <- function(df_a_mod, df_b_mod, data_pref){
  data_path <- "data/paper2/"
  data_path_file <- function(file_name) glue("{data_path}{data_pref}_{file_name}.rds")
  write_rds_mod <-  function(file, file_name) write_rds(file, data_path_file(file_name))
  
  message("Exact matches")
  (df_exact_matches <- 
      df_a_mod %>% 
      select(id_a, fname, lname, 
             dob, gender_code, race_code) %>% 
      inner_join(df_b_mod %>% 
                   select(id_b, fname, lname, 
                          dob, gender_code, race_code)) %>% 
      select(starts_with("id")))
  
  df_exact_matches %>% 
    write_rds_mod("df_exact_matches")
  
  
  message("Id matches")
  (df_vrn_matches <- 
      df_a_mod %>% 
      select(id_a, voter_reg_num) %>% 
      inner_join(df_b_mod %>% 
                   select(id_b, voter_reg_num), 
                 by = "voter_reg_num") %>% 
      select(starts_with("id")) %>% 
      mutate(match = "match") %>% 
      distinct())
  
  df_vrn_matches %>% 
    write_rds_mod("df_vrn_matches")
  
  message("Unexact Matches (what will finally be used)")
  (df_matches_unexact <- 
      df_vrn_matches  %>% 
      anti_join(df_exact_matches) %>% 
      left_join(df_a_mod, by = "id_a") %>% 
      left_join(df_b_mod, by = "id_b", suffix = c("_a", "_b")) %>% 
      mutate(pair_id = row_number()))
  
  
  df_matches_unexact %>% 
    write_rds_mod("df_matches_unexact")
  
  message("Generating close non matches")
  df_blocks <- 
    bind_rows(
      
      df_matches_unexact %>% 
        select(id_a, fname_a) %>%
        inner_join(df_b_mod %>% select(id_b, fname),
                   by = c("fname_a" = "fname")) %>% 
        select(id_a, id_b),
      
      df_matches_unexact %>% 
        select(id_b, fname_b) %>%
        inner_join(df_a_mod %>% select(id_a, fname),
                   by = c("fname_b" = "fname")) %>% 
        select(id_a, id_b),
      
      df_matches_unexact %>% 
        select(id_a, lname_a) %>%
        inner_join(df_b_mod %>% select(id_b, lname),
                   by = c("lname_a" = "lname")) %>% 
        select(id_a, id_b),
      
      df_matches_unexact %>% 
        select(id_b, lname_b) %>%
        inner_join(df_a_mod %>% select(id_a, lname),
                   by = c("lname_b" = "lname")) %>% 
        select(id_a, id_b),
      
      df_matches_unexact %>% 
        select(id_a, dob_a) %>%
        inner_join(df_b_mod %>% select(id_b, dob),
                   by = c("dob_a" = "dob")) %>% 
        select(id_a, id_b),
      
      df_matches_unexact %>% 
        select(id_b, dob_b) %>%
        inner_join(df_a_mod %>% select(id_a, dob),
                   by = c("dob_b" = "dob")) %>% 
        select(id_a, id_b)
      
    ) %>% 
    distinct() %>% 
    anti_join(df_exact_matches, by = c("id_a", "id_b")) %>% 
    anti_join(df_vrn_matches, by = c("id_a", "id_b")) %>% 
    attach_dbs_to_ids_dfs(df_a_mod, df_b_mod) %>% 
    distinct()
  
  df_blocks <- 
    df_blocks %>% 
    mutate(fname_jw = stringdist(fname_a, fname_b, "jw", p = 0.1), 
           lname_jw = stringdist(lname_a, lname_b, "lv", p = 0.1), 
           day_match = day(dob_a) == day(dob_b),
           month_match = month(dob_a) == month(dob_b),
           year_match = year(dob_a) == year(dob_b),
           swap_match = (day(dob_a) == month(dob_b)) & (day(dob_b) == month(dob_a))) %>% 
    select(id_a, id_b, contains("jw"), contains("match"))
  
  
  
  df_thresholds <- 
    df_blocks %>% 
    mutate(thresh_fname = fname_jw < 0.15,
           thresh_lname = lname_jw < 0.15,
           thresh_dob = 
             (day_match + month_match + swap_match + year_match) >= 2,
           thresh_criteria = thresh_fname + thresh_lname + thresh_dob,
           
           thresh_weight = 
             (1 - fname_jw) + 
             (1 - lname_jw) + 
             (day_match + month_match + swap_match + year_match)/3) %>% 
    select(contains("id"), contains("thresh"))
  
  (df_unmatches_unexact <- 
      df_thresholds %>% 
      sample_n(nrow(.)) %>% 
      arrange(desc(thresh_criteria)) %>% 
      # top_n(4*nrow(df_matches_unexact), thresh_weight) %>% 
      slice(1:(4*nrow(df_matches_unexact))) %>% 
      select(contains(("id"))) %>% 
      mutate(match = "unmatch") %>% 
      attach_dbs_to_ids_dfs(df_a_mod, df_b_mod))
  
  df_unmatches_unexact %>% 
    write_rds_mod("df_unmatches_unexact")
  
  message("Pairs")
  # set.seed(1)
  df_pairs <- 
    df_unmatches_unexact %>%
    bind_rows(df_matches_unexact) %>% 
    sample_n((nrow(.)))
  
  df_pairs %>% 
    write_rds_mod("df_pairs")
  
  df_pairs %>% 
    as.data.frame()
}


calc_perc_id_err <- function(df_a_err, n_remaining = F, n_inc = F, e = 0.4){
  df_error_record <- attr(df_a_err, "error_record")
  n_err <- length(unique(df_error_record$id))
  n <- nrow(df_a_err)
  if(!n_inc){
    if(n_remaining){
      ceiling(n*e - n_err)
    } else {
      n_err/nrow(df_a_err)
    }
  } else {
    (e*n - n_err)/(1-e)  
  }
}

make_twins <- function(df, n_errors){
  # df <- df_a_mod$df_secondary 
  
  twin_ids <-
    df %>%
    filter(!is.na(twin_id)) %>%
    add_count(twin_id) %>% 
    filter(n > 1, dob != bday_twin) %>% 
    pull(twin_id) %>%
    unique()
  
  dobs <- df[["dob"]]
  ids <- df[["id"]]
  if(n_errors > length(twin_ids)){
    message("Not enough twins found.")
    n_errors  <- length(twin_ids)
  }
  candidate_ids <- sample(twin_ids, n_errors)
  twin_groups <- df$twin_id %in% candidate_ids
  df[twin_groups, "dob"] <- df[twin_groups, "bday_twin"]
  df[twin_groups, "twin_id"] <- NA
  ids_changed <- ids[dobs != df[["dob"]]]
  before <- dobs[dobs != df[["dob"]]]
  after <- df[["dob"]][dobs != df[["dob"]]]
  df <- update_error_record(
    df,
    ids_changed,
    "dob",
    "twin",
    before,
    after
  ) 
  
  df
}

generate_error_mult <- function(df, df_error_dist, e_target, err_mult){
  df <- df %>% 
    mess_data(df_error_dist %>% 
                mutate(amount = amount*e_target*err_mult),
              add_counting_dups = F,
              verbose = F) %>% 
    pluck("df_secondary") %>% 
    select(-file)
  message(glue("\nmultiplier: {err_mult} - id_error_percent: {calc_perc_id_err(df)}"))
  df
}


generate_error <- function(df_a, 
                           df_b, 
                           df_dob, 
                           df_error_dist, 
                           e_target,
                           err_mult = 1, 
                           err_mult_inc = 0.01){
  (df_a_mod <- 
     df_a %>% 
     preprocess_data(df_dob) %>% 
     prep_data()) 
  
  df_err_a_mod <- generate_error_mult(df_a_mod, df_error_dist, e_target, err_mult)
  while(calc_perc_id_err(df_err_a_mod) < e_target){
    err_mult <- err_mult + err_mult_inc
    df_err_a_mod <- generate_error_mult(df_a_mod, df_error_dist, e_target, err_mult)
  }
  
  df_error_record <- 
    attr(df_err_a_mod, "error_record") %>% 
    mutate(ts = row_number())
  
  twin_ids_realized <- 
    df_a_mod$df_original %>% 
    semi_join(df_error_record %>% 
                filter(error == "twin"),
              by = "id") %>% 
    pull(twin_id)
  
  
  df_b_mod <- 
    df_b %>% 
    preprocess_data(df_dob %>% 
                      mutate(dob = 
                               ifelse(twin_id %in% twin_ids_realized, 
                                      bday_twin, 
                                      dob) %>% 
                               as_date())) %>% 
    mutate(id_b = row_number()) %>% 
    select(-contains("twin"))
  
  df_a_mod <- 
    df_err_a_mod %>% 
    mutate(id_a = row_number()) %>% 
    select(-contains("twin"))
  
  attr(df_a_mod, "error_record") <- df_error_record
  
  list(
    df_a_mod = df_a_mod,
    df_b_mod = df_b_mod,
    df_error_record = df_error_record,
    err_mult = err_mult,
    percent_error_ids = calc_perc_id_err(df_a_mod),
    percent_errors = nrow(df_error_record)/(nrow(df_a_mod)*3)
  )
}







# summarise_results -------------------------------------------------------

summarise_results <- function(){
  
}




n_unique <- function(x) x %>% unique() %>% length()
rename_weight <- function(x) str_c(x, "_weight")
df_to_vector <- function(df) df %>% .[1, ] %>% unclass() %>% as.double()

calculate_hamming_fields <- function(df, weight_vector){
  
  df_equality <- 
    df %>% 
    mutate(fname_equal = fname_a == fname_b,
           mname_equal = mname_a == mname_b,
           lname_equal = lname_a == lname_b,
           gender_code_equal = gender_code_a == gender_code_b,
           race_code_equal = race_code_a == race_code_b,
           dob_equal = dob_a == dob_b,
           year_equal = year(dob_a) == year(dob_b),
           md_equal = 
             glue("{month(dob_a)}-{day(dob_a)}") == 
             glue("{month(dob_b)}-{day(dob_b)}")
    ) %>% 
    select(contains("equal")) %>% 
    gather(col, equal) %>% 
    arrange(col) %>% 
    mutate(equal = equal %>% as.integer())
  
  sum(df_equality$equal * weight_vector)
}



# ml diagnostics ----------------------------------------------------------

evaluate_model <- function(model, df_test, ...){
  calculate_metrics(predict_model(model, df_test), df_test, ...)
}

predict_model <- function(model, df_test){
  df_preds <- 
    predict(model, df_test, type = "prob", na.action = na.pass) %>% 
    as.tibble()
  
  names_preds <- names(df_preds)
  
  if(length(names_preds) == 1){
    tibble(match_prob = df_preds %>% pull(1))
  } else {
    names(df_preds) <- names_preds %>% str_c("_prob")
    df_preds
  }
  
}

predict_nn <- function(model_name, df_ts, df_tr){
  
  model <- load_model_hdf5(model_name)

  df_tr <-
    df_tr %>% 
    select(contains("metric"), match) %>% 
    mutate_if(is.logical, as.integer) %>% 
    mutate_if(is.factor, as.integer) %>% 
    mutate_if(is.integer, as.double)  
  
  tr_means <- map_dbl(df_tr, ~mean(.x, na.rm = T))
  tr_sds <- map_dbl(df_tr, ~sd(.x, na.rm = T))
  
  df_ts <-
    df_ts %>% 
    select(contains("metric"), match) %>% 
    mutate_if(is.logical, as.integer) %>% 
    mutate_if(is.factor, as.integer) %>% 
    mutate_if(is.integer, as.double)
  
  df_ts <- 
    pmap_df(list(a = df_ts, b = tr_means, c = tr_sds), function(a, b, c){
      (a -b)/c
    }) %>% 
    mutate_all(fill_na_0) %>% 
    mutate(match = ifelse(match == min(match), 0, 1))
  
  ts_x <- 
    df_ts %>% 
    select(-match) %>% 
    as.matrix()
  
  ts_y <- 
    df_ts$match
  
  model$predict(ts_x)[,1]
}


predict_nn_2 <- function(model, df_ts, df_tr){
  
  df_tr <-
    df_tr %>% 
    select(contains("metric"), match) %>% 
    mutate_if(is.logical, as.integer) %>% 
    mutate_if(is.factor, as.integer) %>% 
    mutate_if(is.integer, as.double)  
  
  tr_means <- map_dbl(df_tr, ~mean(.x, na.rm = T))
  tr_sds <- map_dbl(df_tr, ~sd(.x, na.rm = T))
  
  df_ts <-
    df_ts %>% 
    select(contains("metric"), match) %>% 
    mutate_if(is.logical, as.integer) %>% 
    mutate_if(is.factor, as.integer) %>% 
    mutate_if(is.integer, as.double)
  
  df_ts <- 
    pmap_df(list(a = df_ts, b = tr_means, c = tr_sds), function(a, b, c){
      (a -b)/c
    }) %>% 
    mutate_all(fill_na_0) %>% 
    mutate(match = ifelse(match == min(match), 0, 1))
  
  ts_x <- 
    df_ts %>% 
    select(-match) %>% 
    as.matrix()
  
  ts_y <- 
    df_ts$match
  
  model$predict(ts_x)[,1]
}




build_model <- function(inp_len) {
  
  model <- 
    keras_model_sequential() %>%
    layer_dense(units = 64, activation = "relu",
                input_shape = inp_len) %>%
    # kernel_regularizer = regularizer_l2(0.0001)
    layer_batch_normalization() %>% 
    layer_dropout(rate = 0.001) %>%
    layer_dense(units = 64, 
                activation = "relu") %>%
    layer_batch_normalization() %>% 
    layer_dense(units = 1, activation = "sigmoid")
  
  model %>% compile(
    loss = "binary_crossentropy",
    optimizer = optimizer_rmsprop(),
    metrics = list("accuracy")
  )
  
  model
}

fill_na_0 <- function(x, repl = 0){
  x[is.na(x)] <- repl
  x
}

build_nn <- function(df_tr, err, tr_sz, epochs=50){
  message(glue("{err}-{tr_sz}"))
  
  df_tr <-
    df_tr %>% 
    select(contains("metric"), match) %>% 
    mutate_if(is.logical, as.integer) %>% 
    mutate_if(is.factor, as.integer) %>% 
    mutate_if(is.integer, as.double)  
  
  tr_means <- map_dbl(df_tr, ~mean(.x, na.rm = T))
  tr_sds <- map_dbl(df_tr, ~sd(.x, na.rm = T))
  
  df_tr <- 
    pmap_df(list(a = df_tr, b = tr_means, c = tr_sds), function(a, b, c){
      (a -b)/c
    }) %>% 
    mutate_all(fill_na_0) %>% 
    mutate(match = ifelse(match == min(match), 0, 1))
  
  tr_x <- 
    df_tr %>% 
    select(-match) %>% 
    as.matrix()
  
  tr_y <- 
    df_tr$match
  
  model <- build_model(inp_len = 23)
  model_name <- glue("./data/paper2/best_nn-{err}-{tr_sz}.h5")
  
  history <- 
    model %>% 
    fit(
      tr_x,
      tr_y,
      epochs = epochs,
      validation_split = 0.2,
      verbose = 1,
      callbacks = list(
        callback_model_checkpoint(model_name,
                                  verbose = T,
                                  save_best_only = T,
                                  monitor = "val_acc"),
        callback_reduce_lr_on_plateau(patience=10, 
                                      verbose = T,
                                      factor = 0.8)
      )
    )
  
  model_name
}

calculate_metrics <- function(df_test, pred_prob){
  message(nrow(df_test))
  match <- ifelse(as.character(df_test$match) == "match", 1, 0) 
  calculate_metrics_prob(match, pred_prob)
}



calculate_metrics_prob <- function(match, pred_prob){
  match_pred <- ifelse(pred_prob >= 0.5, 1, 0)
  metrics <- list()
  
  metrics$review_pct_100 <- calc_review_pct(match, pred_prob, 1.00, 1.00)
  metrics$review_pct_99 <- calc_review_pct(match, pred_prob, 0.99, 0.99)
  metrics$review_pct_98 <- calc_review_pct(match, pred_prob, 0.98, 0.98)
  metrics$review_pct_97 <- calc_review_pct(match, pred_prob, 0.97, 0.97)
  metrics$review_pct_96 <- calc_review_pct(match, pred_prob, 0.96, 0.96)
  metrics$review_pct_95 <- calc_review_pct(match, pred_prob, 0.95, 0.95)
  metrics$review_pct_94 <- calc_review_pct(match, pred_prob, 0.94, 0.94)
  metrics$review_pct_93 <- calc_review_pct(match, pred_prob, 0.93, 0.93)
  metrics$review_pct_92 <- calc_review_pct(match, pred_prob, 0.92, 0.92)
  metrics$review_pct_91 <- calc_review_pct(match, pred_prob, 0.91, 0.91)
  metrics$review_pct_90 <- calc_review_pct(match, pred_prob, 0.90, 0.90)
  
  metrics$accuracy <- Accuracy(match, match_pred)
  metrics$auc <- MLmetrics::AUC(match_pred, match)
  metrics$precision <- precision(match, match_pred)
  metrics$sensitivity <- recall(match, match_pred)
  metrics$specificity <- specificity(match, match_pred)
  metrics$npv <- npv(match, match_pred)
  metrics$f1 <- f1Score(match, match_pred)
  metrics$error <- ce(match, match_pred)
  metrics$brier <- brier(match, pred_prob)
  metrics$brier_sqrt <- sqrt(metrics$brier)
  metrics$recall <- recall(match, match_pred)
  metrics$auc <- auc(match, pred_prob)
  metrics$gini <- Gini(match, pred_prob)
  
  # metrics$df_metric_table <-
  metrics %>% 
    enframe() %>% 
    rename(metric = name) %>% 
    mutate(value = map_dbl(value, 1)) 
  
  
  # metrics$confusion_matrix <- ConfusionMatrix(match, match_pred)
  # metrics$f_scores <- structure(FBeta_Score(match, match_pred, positive = 1, beta = 1:10), names = 1:10) 
  
  # metrics
  
}


calc_review_pct <- function(match, match_prob, ppv = 1, npv = 1){
  # browser()
  match <- as.integer(match)
  match_prob_order <- order(match_prob)
  match_prob <- match_prob[match_prob_order]
  match <- match[match_prob_order]
  pos_probs <- match_prob[match_prob > 0.5]
  neg_probs <- rev(match_prob[match_prob <= 0.5])
  
  ## neg reviews
  for(t1 in neg_probs){
    negs <- (match_prob <= t1)
    # probs <- match_prob[negs]
    actuals <- match[negs]
    # probs[probs<=t1] = 0
    if(mean(actuals == 0) >= npv){
      break
    }
  }
  
  for(t2 in pos_probs){
    pos <- (match_prob >=t2)
    # probs <- match_prob[negs]
    actuals <- match[pos]
    # probs[probs<=t1] = 0
    if(mean(actuals == 0) >= ppv){
      break
    }
  }
  
  mean((match_prob > t1) & (match_prob < t2))
}


explain_metrics <- function() rstudioapi::viewer("https://en.wikipedia.org/wiki/Precision_and_recall#Definition_(classification_context)") 


print.rl_results <- function(results){
  walk2(results$confidence, names(results$confidence), 
        function(data, desc){
          cat(paste0("\n", desc, "\n"))
          print(data %>% select(1:10), n = 6)
        })
  
  cat("\n")
  print(results$metrics$confusion_matrix)
  
  cat(paste0("\nmetrics\n"))
  print(results$metrics$df_metric_table, n = 6)
}


extract_model_component <- function(df_model_results, model_select, component){
  if(is.character(model_select)){
    component <- 
      df_model_results %>% 
      filter(model_name == model_select) %>% 
      pull(component)
  } else{
    component <- 
      df_model_results %>% 
      slice(model_select) %>% 
      pull(component)
  }
  
  if(is.list(component)){
    component[[1]]
  }
}


calc_threshold_for_metric_value <- 
  function(y_true, 
           y_prob, 
           metric = precision, 
           value = 0.99, 
           k_range = seq(0.5, 1, 0.001), 
           lowest = TRUE){
    # browser()
    
    y_true <- y_true == "match"  
    
    df_values_thresholds <- 
      tibble(k_range = k_range,
             values =
               map_dbl(k_range, function(x){
                 metric(y_true, y_prob >= x)
               })) 
    
    if(lowest){
      df_filtered <- 
        df_values_thresholds %>% 
        filter(values >= value) %>% 
        arrange(k_range) %>% 
        slice(1) 
      
      
      
    } else {
      df_filtered <- 
        df_values_thresholds %>% 
        filter(values <= value) %>% 
        arrange(desc(k_range)) %>% 
        slice(1) 
    }
    
    message(glue("Metric has achieved {df_filtered$values} \\
                 at threshold {df_filtered$k_range}"))
    
    df_filtered$k_range
  }


plot_roc_all <- function(df_evaluated, model_col = model, results_col = results){
  model_col <- enquo(model_col)
  results_col <- enquo(results_col)
  p <- df_evaluated %>% 
    mutate(df_roc = !!results_col %>% map("metrics") %>% map("df_roc")) %>% 
    select(!!model_col, df_roc) %>% 
    unnest() %>% 
    ggplot(aes(fpr, 
               sensitivity, 
               color = !!model_col, 
               group = !!model_col,
               text = glue("threshold: {thresholds}
                            fpr: {round(fpr, 3)}
                            sensitivity: {round(sensitivity, 3)} 
                            specificity: {round(specificity, 3)}")))+
    geom_line(size = 1, alpha = 0.7) +
    geom_point(size = 0.5, 
               alpha = 0.5) +
    scale_x_continuous(limits = c(0, 1), breaks = seq(0, 1, .1)) +
    scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, .1)) +
    labs(title= "ROC curve", 
         x = "False Positive Rate (1-Specificity)", 
         y = "True Positive Rate (Sensitivity)") +
    theme_light() +
    scale_color_continuous(low = "gray13", high = "red")
  
  plotly::ggplotly(p, tooltip="text")
}




filter_na_rows <- function(df){
  df %>% 
    filter_all(any_vars(is.na(.)))
}



# county funcs ----------------------------------------------------------


gen_birthday_from_age_scr <- function(as_of, age = 25){
  # print(as_of)
  date_from <- as_of + 1 - years(age + 1)
  date_to <- as_of - years(age)
  date_range <- seq(date_from, date_to, by = 1) 
  # print(glue("{date_range[1]} - {date_range[length(date_range)]}"))
  date_range %>% sample(1)
}


get_bdays <- function(df_a, df_b){
  df_a_t <- 
    df_a %>% 
    select(voter_reg_num, first_name, mname, last_name, birth_age, gender_code, race_code, res_street_address)
  
  df_b_t <- 
    df_b %>% 
    select(voter_reg_num, first_name, mname, last_name, birth_age, gender_code, race_code, res_street_address)
  
  df_a_minus_b_t <- 
    df_a_t %>% 
    anti_join(df_b_t, by = "voter_reg_num") %>% 
    mutate(as_of_month = 4,
           as_of_year = 2013,
           as_of_day = 30)
  
  df_a_int_b_t <- 
    df_a_t %>% 
    semi_join(df_b_t, by = "voter_reg_num") %>% 
    mutate(as_of_month = 4,
           as_of_year = 2013,
           as_of_day = 30)
  
  df_b_minus_a_t <- 
    df_b_t %>% 
    anti_join(df_a_t, by = "voter_reg_num") %>% 
    mutate(as_of_month = 3,
           as_of_year = 2017,
           as_of_day = 31)
  
  
  
  (df_t <- 
      bind_rows(
        df_a_minus_b_t, 
        df_a_int_b_t, 
        df_b_minus_a_t
      ) %>% 
      # slice(1:5) %>% 
      mutate(as_of = ymd(glue("{as_of_year}-{as_of_month}-{as_of_day}")),
             bday = 
               map2_dbl(as_of, birth_age, gen_birthday_from_age_scr) %>% as_date()))
  
  df_t %>% 
    select(voter_reg_num, bday) 
  
  df_t %>% 
    group_by(last_name, res_street_address, birth_age) %>% 
    left_join(
      df_t %>% 
        group_by(last_name, res_street_address, birth_age) %>% 
        filter(n() > 1, res_street_address != "REMOVED") %>% 
        select(last_name, res_street_address, birth_age, bday) %>% 
        sample_n(1) %>% 
        rename(bday_twin = bday) %>% 
        ungroup() %>% 
        mutate(twin_id = row_number())
    ) %>% 
    ungroup() %>% 
    select(voter_reg_num, contains("bday"), twin_id) %>% 
    rename(dob = bday)
  
}




generate_pairs_for_county <- function(county, 
                                      error_pcts = seq(0, 0.6, 0.05)){
  df_a <- 
    read_delim(glue("data/ncvote/apr13/ncvoter{county}.txt"), delim = "\t") %>% 
    rename(name_suffix = name_sufx_cd,
           mname = midl_name) %>% 
    mutate_if(is.character, str_squish)
  
  
  df_b <- 
    read_delim(glue("data/ncvote/mar17/ncvoter{county}.txt"), delim = "\t") %>% 
    rename(name_suffix = name_suffix_lbl,
           mname = middle_name) %>% 
    mutate_if(is.character, str_squish)
  
  df_dob <- get_bdays(df_a, df_b)
  
  # browser()
  set.seed(37)
  df_messed_collection <- 
    tibble(error_percent = error_pcts) %>% 
    print() %>% 
    mutate(
      ls_egen = map(error_percent, function(e){
        message(glue("\n\nerror percent target: {e}"))
        generate_error(df_a, 
                       df_b, 
                       df_dob, 
                       df_error_dist, 
                       e, 
                       err_mult = 1,
                       err_mult_inc = 0.01)
      })
    ) %>% 
    print() %>% 
    mutate(df_a_mod = map(ls_egen, "df_a_mod"),
           df_b_mod = map(ls_egen, "df_b_mod"),
           df_error_record = map(ls_egen, "df_error_record"),
           err_mult = map_dbl(ls_egen, "err_mult"),
           percent_error_ids = map_dbl(ls_egen, "percent_error_ids"),
           percent_errors = map_dbl(ls_egen, "percent_errors")) %>% 
    print() %>% 
    mutate(
      df_pairs = pmap(list(a = df_a_mod, 
                           b = df_b_mod, 
                           e = error_percent), function(a, b, e){
                             generate_pairs(a, b, data_pref = glue("percent_{e*100}"))
                           }),
      size = map_int(df_pairs, nrow)) %>% 
    print()
  
  df_messed_collection <-
    df_messed_collection %>% 
    # slice(13) %>% 
    mutate(df_feature = pmap(list(df_pair = df_pairs, 
                                  df_a = df_a_mod,
                                  df_b = df_b_mod), 
                             function(df_pair, df_a, df_b){
                               # browser()
                               message(glue("calculating feature vector for {nrow(df_pair)} records"))
                               st <- Sys.time()
                               df_res <- 
                                 df_pair %>%
                                 # sample_n(500) %>%
                                 add_feature_vector(df_a, df_b) %>%
                                 select(starts_with("metric"), match) %>%
                                 mutate(match = match %>% factor(levels = c("unmatch", "match"))) %>%
                                 as.data.frame()
                               message(glue("{difftime(Sys.time(), st, units = 'mins')} minutes elapsed"))
                               df_res
                             }))
  
  df_messed_collection <- 
    df_messed_collection %>% 
    mutate(size = df_feature %>% map_int(~(nrow(.))))
  
  df_messed_collection  
  
}



#### Neural Net -----------------

build_model <- function(inp_len) {
  
  model <- 
    keras_model_sequential() %>%
    layer_dense(units = 64, activation = "relu",
                input_shape = inp_len) %>%
    # kernel_regularizer = regularizer_l2(0.0001)
    layer_batch_normalization() %>% 
    layer_dropout(rate = 0.001) %>%
    layer_dense(units = 64, 
                activation = "relu") %>%
    layer_batch_normalization() %>% 
    layer_dense(units = 1, activation = "sigmoid")
  
  model %>% compile(
    loss = "binary_crossentropy",
    optimizer = optimizer_rmsprop(),
    metrics = list("accuracy")
  )
  
  model
}

fill_na_0 <- function(x, repl = 0){
  x[is.na(x)] <- repl
  x
}

build_nn <- function(df_tr, path_nn, epochs=50){
  
  df_tr <-
    df_tr %>% 
    select(contains("metric"), match) %>% 
    mutate_if(is.logical, as.integer) %>% 
    mutate_if(is.factor, as.integer) %>% 
    mutate_if(is.integer, as.double)  
  
  tr_means <- map_dbl(df_tr, ~mean(.x, na.rm = T))
  tr_sds <- map_dbl(df_tr, ~sd(.x, na.rm = T))
  
  df_tr <- 
    pmap_df(list(a = df_tr, b = tr_means, c = tr_sds), function(a, b, c){
      (a -b)/c
    }) %>% 
    mutate_all(fill_na_0) %>% 
    mutate(match = ifelse(match == min(match), 0, 1))
  
  tr_x <- 
    df_tr %>% 
    select(-match) %>% 
    as.matrix()
  
  tr_y <- 
    df_tr$match
  
  model <- build_model(inp_len = 23)
  
  history <- 
    model %>% 
    fit(
      tr_x,
      tr_y,
      epochs = epochs,
      validation_split = 0.2,
      verbose = 1,
      callbacks = list(
        callback_model_checkpoint(path_nn,
                                  verbose = T,
                                  save_best_only = T,
                                  monitor = "val_acc"),
        callback_reduce_lr_on_plateau(patience=10, 
                                      verbose = T,
                                      factor = 0.8)
      )
    )
}





