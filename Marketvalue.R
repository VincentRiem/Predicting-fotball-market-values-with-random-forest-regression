
# Merge the transfermarkt datasets so the player names are in the valuation df
## Not really clean code, tried many things, some didn't work out but kept them in
players_unique <- players %>%
  distinct(player_id, .keep_all = TRUE)

player_valuations <- player_valuations %>%
  left_join(
    players_unique %>% select(player_id, name),
    by = "player_id"
  )

#Perfect work

library(dplyr)
library(lubridate)

library(stringr)
library(stringi)


# 1) Helpers: normalize player names + normalize season strings

normalize_name <- function(x) {
  x %>%
    str_squish() %>%
    str_to_lower() %>%
    stri_trans_general("Latin-ASCII") %>%
    str_replace_all("[^a-z\\s\\-']", "") %>%
    str_squish()
}

normalize_season <- function(s) {
  s %>%
    as.character() %>%
    str_squish() %>%
    str_replace_all("–", "-") %>%  # en dash -> hyphen
    str_replace_all("—", "-") %>%  # em dash -> hyphen
    str_replace_all("/", "-") %>%  # slash -> hyphen
    str_replace_all("\\s", "")     # remove spaces
}


# 2) Transfermarkt: create season_key from valuation date

player_valuations_seasoned <- player_valuations %>%
  mutate(
    date = as.Date(date),
    y = year(date),
    m = month(date),
    season_start = if_else(m >= 7, y, y - 1),
    season_end   = season_start + 1,
    season_key   = paste0(season_start, "-", season_end),
    season_key   = normalize_season(season_key),
    name_key     = normalize_name(name)
  )

# 3) Build end-of-season valuation table for seasons <= 2023-2024

anchor_month <- 6  # June (end of season in top 5 leagues)

player_val_eos <- player_valuations_seasoned %>%
  filter(season_key %in% c("2021-2022", "2022-2023", "2023-2024")) %>%
  mutate(
    season_end = as.integer(substr(season_key, 6, 9)),
    anchor_date = as.Date(sprintf("%d-%02d-01", season_end, anchor_month)),
    dist_days = abs(as.integer(date - anchor_date))
  ) %>%
  group_by(player_id, season_key) %>%
  slice_min(order_by = dist_days, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  select(player_id, name_key, season_key, market_value_in_eur, date)

# 4) Build "latest available valuation" for season 2024-2025 only

player_val_latest_2425 <- player_valuations %>%
  mutate(date = as.Date(date)) %>%
  arrange(player_id, date) %>%
  group_by(player_id) %>%
  slice_tail(n = 1) %>%   # latest valuation per player
  ungroup() %>%
  mutate(
    season_key = "2024-2025",
    season_key = normalize_season(season_key),
    name_key = normalize_name(name)
  ) %>%
  select(player_id, name_key, season_key, market_value_in_eur, date)

# 5) Combine valuation tables into one final valuation lookup
player_val_final <- bind_rows(
  player_val_eos,
  player_val_latest_2425
) %>%
  # Deduplicate safely: player_id+season_key should be unique
  arrange(player_id, season_key, desc(date)) %>%
  distinct(player_id, season_key, .keep_all = TRUE) %>%
  select(name_key, season_key, market_value_in_eur)

# 6) Stats cleaning (Nation, Comp, Pos) + keys

Stats_clean <- Stats %>%
  mutate(
    Nation = str_extract(Nation, "[A-Z]{3}$"),
    Comp   = str_remove(Comp, "^[a-z]+\\s+"),
    Pos = case_when(
      str_detect(Pos, "MF") & str_detect(Pos, "FW") ~ "OMF",
      str_detect(Pos, "MF") & str_detect(Pos, "DF") ~ "DMF",
      TRUE ~ Pos
    ),
    name_key   = normalize_name(Player),
    season_key = normalize_season(as.character(Season))
  )

# 7) Ensure ONE row per player-season in Stats (transfer rows etc.)
Stats_clean <- Stats_clean %>%
  mutate(TklWinPct = TklW / Tkl...79)



Stats_unique <- Stats_clean %>%
  group_by(name_key, season_key) %>%
  summarise(
    # --- identifiers ---
    Player = first(Player),
    Season = first(Season),
    Pos    = first(Pos),
    Nation = first(Nation),
    Comp   = first(Comp),
    Age    = first(Age),
    
    # keep row-level minutes as weights
    Min_total = sum(Min, na.rm = TRUE),
    
    # --- SUM (counting totals) ---
    Gls        = sum(Gls...24, na.rm = TRUE),
    Ast        = sum(Ast, na.rm = TRUE),
    SCA_50     = sum(SCA...50, na.rm = TRUE),
    GCA_57     = sum(GCA...57, na.rm = TRUE),
    PassDist   = sum(TotDist...39, na.rm = TRUE),
    CarryDist  = sum(TotDist...65, na.rm = TRUE),
    Blocks     = sum(Blocks, na.rm = TRUE),
    Recov      = sum(Recov...107, na.rm = TRUE),
    Att        = sum(Att...32, na.rm = TRUE),
    Carries    = sum(Carries, na.rm = TRUE),
    PKwon      = sum(PKwon, na.rm = TRUE),
    Off        = sum(Off, na.rm = TRUE),
    
    # --- MINUTES-WEIGHTED MEANS (use row-level Min as weights!) ---
    PassCmpPct = weighted.mean(`Cmp%...33`, w = Min, na.rm = TRUE),
    TakeOnSucc = weighted.mean(`Succ%...76`, w = Min, na.rm = TRUE),
    TklWinPct  = weighted.mean(TklWinPct, w = Min, na.rm = TRUE),
    AerialWinPct = weighted.mean(`Won%...97`, w = Min, na.rm = TRUE),
    Min_per_MP = weighted.mean(`Mn/MP...98`, w = Min, na.rm = TRUE),
    
    .groups = "drop"
  ) %>%
  # optional: rename Min_total back to Min to keep your downstream code unchanged
  rename(Min = Min_total)



# 8) Final merge: Stats structure + add market value

final_df <- Stats_unique %>%
  left_join(
    player_val_final,
    by = c("name_key", "season_key")
  )


### Wikipedia add

install.packages("remotes")
remotes::install_github("ironholds/pageviews")
2
library(pageviews)

pv <- article_pageviews(
  project = "en.wikipedia",
  article = "Robert_Andrich",
  start = "2023080100",
  end   = "2024063023"
)

sum(pv$views)


library(dplyr)
library(stringr)
library(lubridate)
library(pageviews)
library(purrr)


# 1) Helper: build wikipedia title

make_wiki_title <- function(player_name) {
  player_name %>%
    str_squish() %>%
    str_replace_all(" ", "_")
}


# 2) Helper: season_key -> start/end dates

season_window <- function(season_key) {
  start_year <- as.integer(substr(season_key, 1, 4))
  end_year   <- as.integer(substr(season_key, 6, 9))
  
  start_date <- as.Date(sprintf("%d-08-01", start_year))
  end_date   <- as.Date(sprintf("%d-07-31", end_year))
  
  list(start_date = start_date, end_date = end_date)
}

# 3) Helper: fetch pageviews total for one article + date window

get_wiki_views_window <- function(article, start_date, end_date) {
  start_ts <- format(as.POSIXct(start_date), "%Y%m%d00")
  end_ts   <- format(as.POSIXct(end_date), "%Y%m%d23")
  
  pv <- article_pageviews(
    project = "en.wikipedia",
    article = article,
    start = start_ts,
    end   = end_ts
  )
  
  sum(pv$views, na.rm = TRUE)
}

# 4) Cache setup (VERY important)

cache_path <- "wiki_views_cache.rds"

if (file.exists(cache_path)) {
  wiki_cache <- readRDS(cache_path)
} else {
  wiki_cache <- tibble(
    wiki_title = character(),
    season_key = character(),
    wiki_views = numeric()
  )
}

save_cache <- function(cache_tbl) {
  saveRDS(cache_tbl, cache_path)
}


# 5) Function: get views with cache + rate limit
get_views_cached <- function(wiki_title, season_key, sleep_sec = 0.1) {
  
  # already cached?
  hit <- wiki_cache %>%
    filter(wiki_title == !!wiki_title, season_key == !!season_key)
  
  if (nrow(hit) == 1) return(hit$wiki_views[[1]])
  
  # compute window
  win <- season_window(season_key)
  
  # try fetch (catch errors: missing pages, disambiguation, etc.)
  views <- tryCatch(
    get_wiki_views_window(wiki_title, win$start_date, win$end_date),
    error = function(e) NA_real_
  )
  
  # update cache in parent environment
  wiki_cache <<- bind_rows(
    wiki_cache,
    tibble(wiki_title = wiki_title, season_key = season_key, wiki_views = views)
  ) %>%
    distinct(wiki_title, season_key, .keep_all = TRUE)
  
  save_cache(wiki_cache)
  
  # be nice to the API
  Sys.sleep(sleep_sec)
  
  views
}


# 6) Build the (player, season) keys to query

wiki_keys <- final_df %>%
  transmute(
    Player,
    season_key,
    wiki_title = make_wiki_title(Player)
  ) %>%
  distinct(wiki_title, season_key)


# 7) Fetch views for all keys (this can take time for many players)

wiki_results <- wiki_keys %>%
  mutate(
    wiki_views = map2_dbl(wiki_title, season_key, ~ get_views_cached(.x, .y))
  )


# 8) Join back into final_df

final_df <- final_df %>%
  mutate(wiki_title = make_wiki_title(Player)) %>%
  left_join(wiki_results, by = c("wiki_title", "season_key")) %>%
  mutate(
    log_wiki_views = log1p(wiki_views)
  )

final_df <- final_df %>%
  filter(name_key != "carlos alcaraz")

# 9) Diagnostics: match rates overall and by season

overall_match_rate <- mean(!is.na(final_df$market_value_in_eur))
overall_match_rate

match_by_season <- final_df %>%
  group_by(season_key) %>%
  summarise(
    n = n(),
    match_rate = mean(!is.na(market_value_in_eur)),
    .groups = "drop"
  ) %>%
  arrange(season_key)

match_by_season


#Split Dataset
# Main modeling dataset: drop 2025-2026

final_df_model <- final_df %>%
  filter(season_key %in% c("2021-2022", "2022-2023", "2023-2024", "2024-2025"))

# Train:
train_df <- final_df_model %>%
  filter(season_key %in% c("2021-2022", "2022-2023", "2023-2024"),
         !is.na(market_value_in_eur))

# Test/Eval:
eval_df <- final_df_model %>%
  filter(season_key == "2024-2025",
         !is.na(market_value_in_eur))

# Optional:
pred_2425_df <- final_df_model %>%
  filter(season_key == "2024-2025",
         is.na(market_value_in_eur))


# Separate prediction dataset

pred_2526_df <- final_df %>%
  filter(season_key == "2025-2026") %>%
  select(-market_value_in_eur)  # no target needed for prediction


# Consistent filters for modeling stability

min_cut <- 360

train_df <- train_df %>%
  filter(Min >= min_cut) %>%
  mutate(target = log1p(market_value_in_eur))

eval_df <- eval_df %>%
  filter(Min >= min_cut) %>%
  mutate(target = log1p(market_value_in_eur))

pred_2425_df <- pred_2425_df %>% filter(Min >= min_cut)
pred_2526_df <- pred_2526_df %>% filter(Min >= min_cut)


market_cut <- 1000000000
market_cut_1 <- 1000000000
train_df_c <- train_df %>%
  filter(market_value_in_eur <= market_cut)

eval_df_c <- eval_df %>%
  filter(market_value_in_eur <= market_cut_1)

# quick sanity checks
nrow(train_df)
nrow(eval_df)
nrow(pred_2526_df)

num_vars <- c(
  "Age",
  "Gls",
  "SCA_50",
  "Min",
  "PassCmpPct",
  "Recov",
  "Blocks",
  "log_wiki_views"
)

cat_vars <- c("Nation")   # keep league context only

library(dplyr)
library(tidyverse)


train_df_c <- train_df_c %>%
  mutate(target = log1p(market_value_in_eur))

eval_df_c <- eval_df_c %>%
  mutate(target = log1p(market_value_in_eur))

train_rf <- train_df_c %>%
  select(target, all_of(num_vars), all_of(cat_vars)) %>%
  drop_na()

eval_rf <- eval_df_c %>%
  select(target, all_of(num_vars), all_of(cat_vars)) %>%
  drop_na()

train_rf <- train_rf %>%
  mutate(across(all_of(cat_vars), as.factor))

eval_rf <- eval_rf %>%
  mutate(across(all_of(cat_vars), as.factor))

library(ranger)

set.seed(42)

rf_model <- ranger(
  formula = target ~ .,
  data = train_rf,
  num.trees = 1000,
  mtry = 3,                 # small mtry works better with few strong vars
  min.node.size = 20,       # regularization
  importance = "permutation",
  respect.unordered.factors = "order"
)

eval_pred_log <- predict(rf_model, data = eval_rf)$predictions
eval_pred_eur <- expm1(eval_pred_log)
actual_eur <- expm1(eval_rf$target)

rmse_log <- sqrt(mean((eval_pred_log - eval_rf$target)^2))
rmse_log

rmse_eur <- sqrt(mean((eval_pred_eur - actual_eur)^2))
rmse_eur




######
plot_rf_importance <- function(rf, top_n = 15, title = "Variable importance (permutation)") {
  imp <- rf$variable.importance
  
  tibble(
    variable = names(imp),
    importance = as.numeric(imp)
  ) %>%
    arrange(desc(importance)) %>%
    slice_head(n = top_n) %>%
    mutate(variable = reorder(variable, importance)) %>%
    ggplot(aes(x = variable, y = importance)) +
    geom_col() +
    coord_flip() +
    labs(title = title, x = NULL, y = "Importance") +
    theme_minimal()
}


plot_rf_importance(rf_model, top_n = 20)

plot_rf_importance(res_DF$model, top_n = 20, title = "DF importance")
plot_rf_importance(res_MF$model, top_n = 20, title = "MF importance")
plot_rf_importance(res_FW$model, top_n = 20, title = "FW importance")


#### per 90 model and smoothing

library(dplyr)
library(stringr)


totals_vars <- c(
  "Gls", "Ast", "SCA_50", "GCA_57",
  "PassDist", "CarryDist", "Blocks", "Recov"
)


safe_per90 <- function(x, min) {
  ifelse(is.na(min) | min <= 0, NA_real_, 90 * x / min)
}


w_current <- 0.70
w_lag1    <- 0.30


id_col <- if ("name_key" %in% names(final_df)) "name_key" else "Player"

final_df_fe <- final_df %>%
  mutate(
    # numeric season order key (e.g., "2022-2023" -> 2022)
    season_start = as.integer(substr(season_key, 1, 4))
  ) %>%
  arrange(.data[[id_col]], season_start) %>%
  group_by(.data[[id_col]]) %>%
  # --- create per90 for totals ---
  mutate(across(all_of(totals_vars),
                ~ safe_per90(.x, Min),
                .names = "{.col}_per90")) %>%
  mutate(across(c(all_of(totals_vars), paste0(totals_vars, "_per90")),
                ~ dplyr::lag(.x, 1),
                .names = "{.col}_lag1")) %>%
  mutate(
    Min_lag1        = dplyr::lag(Min, 1),
    Min_per_MP_lag1 = dplyr::lag(Min_per_MP, 1),
    Age_lag1        = dplyr::lag(Age, 1),
    log_wiki_views_lag1 = if ("log_wiki_views" %in% names(.)) dplyr::lag(log_wiki_views, 1) else NA_real_
  ) %>%
  mutate(
    Min_smoothed        = ifelse(is.na(Min_lag1), Min, w_current * Min + w_lag1 * Min_lag1),
    Min_per_MP_smoothed = ifelse(is.na(Min_per_MP_lag1), Min_per_MP, w_current * Min_per_MP + w_lag1 * Min_per_MP_lag1),
    log_wiki_views_smoothed = if ("log_wiki_views" %in% names(.)) {
      ifelse(is.na(log_wiki_views_lag1), log_wiki_views, w_current * log_wiki_views + w_lag1 * log_wiki_views_lag1)
    } else NA_real_
  ) %>%
  mutate(
    across(all_of(totals_vars),
           ~ ifelse(is.na(dplyr::lag(.x, 1)), .x, w_current * .x + w_lag1 * dplyr::lag(.x, 1)),
           .names = "{.col}_smoothed"),
    across(paste0(totals_vars, "_per90"),
           ~ ifelse(is.na(dplyr::lag(.x, 1)), .x, w_current * .x + w_lag1 * dplyr::lag(.x, 1)),
           .names = "{.col}_smoothed")
  ) %>%
  ungroup()

num_vars_model <- c(
  "Age",
  "Min_smoothed",
  "log_wiki_views_smoothed",
  "Blocks_smoothed",
  "Recov_smoothed",
  "PassDist_smoothed",
  "CarryDist_smoothed"
)

cat_vars_model <- c("Comp")

num_vars_model <- c(num_vars_model,
                    "Gls_smoothed", "SCA_50_smoothed", "GCA_57_smoothed"
)

train_df <- final_df_fe %>%
  filter(season_key %in% c("2021-2022","2022-2023","2023-2024"),
         !is.na(market_value_in_eur)) %>%
  mutate(target = log1p(market_value_in_eur))

eval_df <- final_df_fe %>%
  filter(season_key == "2024-2025",
         !is.na(market_value_in_eur)) %>%
  mutate(target = log1p(market_value_in_eur))

pred_2526_df <- final_df_fe %>%
  filter(season_key == "2025-2026") 

library(ranger)
library(tidyr)

train_rf <- train_df %>%
  select(target, all_of(num_vars_model), all_of(cat_vars_model)) %>%
  drop_na() %>%
  mutate(across(all_of(cat_vars_model), as.factor))

eval_rf <- eval_df %>%
  select(target, all_of(num_vars_model), all_of(cat_vars_model)) %>%
  drop_na() %>%
  mutate(across(all_of(cat_vars_model), as.factor))

rf_model <- ranger(
  target ~ .,
  data = train_rf,
  num.trees = 1200,
  mtry = max(2, floor(sqrt(ncol(train_rf) - 1))),
  min.node.size = 20,
  importance = "permutation",
  respect.unordered.factors = "order",
  seed = 42
)

pred_log <- predict(rf_model, data = eval_rf)$predictions
rmse_log <- sqrt(mean((pred_log - eval_rf$target)^2))
rmse_log

# €-RMSE
rmse_eur <- sqrt(mean((expm1(pred_log) - expm1(eval_rf$target))^2))
rmse_eur

rf_model <- ranger(
  target ~ .,
  data = train_rf,
  num.trees = 1200,
  mtry = max(2, floor(sqrt(ncol(train_rf) - 1))),
  min.node.size = 20,
  importance = "permutation",
  respect.unordered.factors = "order",
  seed = 42
)

imp <- rf_model$variable.importance

library(dplyr)
library(ggplot2)
library(tibble)

imp_df <- tibble(
  variable = names(imp),
  importance = as.numeric(imp)
)

imp_df %>%
  arrange(desc(importance)) %>%
  slice_head(n = 20) %>%
  mutate(variable = reorder(variable, importance)) %>%
  ggplot(aes(x = variable, y = importance)) +
  geom_col() +
  coord_flip() +
  theme_minimal() +
  labs(
    title = "Permutation importance (RMSE impact)",
    x = NULL,
    y = "Increase in prediction error"
  )

label_map <- c(
  Age = "Age",
  Min_smoothed = "Minutes",
  CarryDist_smoothed = "Total Carrying Distance",
  PassDist_smoothed = "Total Passing Distance",
  Ast_per90_smoothed = "Assists per 90",
  SCA_50_smoothed = "Shot-creating actions",
  GCA_57_smoothed = "Goal-creating actions",
  Gls_smoothed = "Goals scored",
  Recov_smoothed = "Ball Recoveries",
  Blocks_smoothed = "Pass and Shot Blocks",
  log_wiki_views_smoothed = "Wikipedia views (log, smoothed)",
  Comp = "Competition (League)"
)

imp_df %>%
  filter(variable %in% names(label_map)) %>%
  mutate(variable = label_map[variable]) %>%
  arrange(importance) %>%
  ggplot(aes(x = reorder(variable, importance), y = importance)) +
  geom_col() +
  coord_flip() +
  theme_minimal() +
  labs(title = "Feature importance (RF)", x = NULL, y = "Permutation importance")

## Plot 1: Actual vs Predicted
num_vars_model <- c(
  "Age","Min_smoothed","log_wiki_views",
  "Blocks_smoothed","Recov_smoothed","PassDist_smoothed","CarryDist_smoothed",
  "Gls_smoothed","SCA_50_smoothed","GCA_57_smoothed"
)
cat_vars_model <- c("Comp")

library(dplyr)
library(ggplot2)
library(tidyr)

# Build eval set (example: 2024-2025)
eval_df_plot <- final_df_fe %>%
  filter(season_key == "2024-2025", !is.na(market_value_in_eur)) %>%
  mutate(target = log1p(market_value_in_eur)) %>%
  select(Player, market_value_in_eur, target, all_of(num_vars_model), all_of(cat_vars_model)) %>%
  drop_na() %>%
  mutate(across(all_of(cat_vars_model), as.factor))

# Predict
pred_log <- predict(rf_model, data = eval_df_plot)$predictions

plot_df <- eval_df_plot %>%
  mutate(
    pred_log = pred_log,
    pred_eur = expm1(pred_log),
    actual_eur = market_value_in_eur
  )

rmse_eur <- sqrt(mean((plot_df$pred_eur - plot_df$actual_eur)^2))
rmse_log <- sqrt(mean((plot_df$pred_log - plot_df$target)^2))

ggplot(plot_df, aes(x = actual_eur, y = pred_eur)) +
  geom_point(alpha = 0.35) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
  scale_x_log10(
    labels = scales::label_number(scale_cut = scales::cut_si(" "))
  ) +
  scale_y_log10(
    labels = scales::label_number(scale_cut = scales::cut_si(" "))
  ) +
  theme_minimal() +
  labs(
    title = "Predicted vs Actual market value (Eval season 2024–25)",
    subtitle = paste0(
      "RMSE €: ",
      scales::label_number(scale_cut = scales::cut_si(" "))(rmse_eur),
      " | RMSE log: ",
      round(rmse_log, 3)
    ),
    x = "Actual market value (€) [log scale]",
    y = "Predicted market value (€) [log scale]"
  )

## Plot 2: Where are errors
bucket_df <- plot_df %>%
  mutate(
    abs_error = abs(pred_eur - actual_eur),
    rel_error = abs_error / actual_eur,
    value_bucket = cut(
      actual_eur,
      breaks = c(0, 5e6, 10e6, 25e6, 50e6, 1e9),
      labels = c("<5M", "5–10M", "10–25M", "25–50M", "50M+")
    )
  )

ggplot(bucket_df, aes(x = value_bucket, y = abs_error)) +
  geom_boxplot(outlier.alpha = 0.15) +
  scale_y_continuous(labels = scales::label_number(scale_cut = scales::cut_si(" "))) +
  theme_minimal() +
  labs(
    title = "Prediction error by market value bucket (2024–25)",
    x = "Actual market value bucket",
    y = "Absolute error (€)"
  )

## Plot 3
library(dplyr)
library(tidyr)


eval_2425 <- final_df_fe %>%
  filter(season_key == "2024-2025", !is.na(market_value_in_eur)) %>%
  mutate(
    target = log1p(market_value_in_eur)
  ) %>%
  select(
    Player, Comp, market_value_in_eur, target,
    Age, Min_smoothed, log_wiki_views_smoothed, Blocks_smoothed, Recov_smoothed,
    PassDist_smoothed, CarryDist_smoothed, Gls_smoothed, SCA_50_smoothed, GCA_57_smoothed
  ) %>%
  drop_na() %>%
  mutate(Comp = as.factor(Comp))


pred_log <- predict(rf_model, data = eval_2425)$predictions

eval_2425 <- eval_2425 %>%
  mutate(
    pred_log   = pred_log,
    pred_eur   = expm1(pred_log),
    actual_eur = market_value_in_eur,
    delta_eur  = pred_eur - actual_eur,
    delta_pct  = delta_eur / actual_eur
  )

library(ggplot2)
library(scales)

eval_2425 %>%
  mutate(
    status = case_when(
      delta_pct >= 0.25  ~ "Undervalued",
      delta_pct <= -0.25 ~ "Overvalued",
      TRUE               ~ "Fair"
    )
  ) %>%
  ggplot(aes(x = actual_eur, y = pred_eur)) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", alpha = 0.6) +
  geom_point(
    aes(size = abs(delta_eur), color = status),
    alpha = 0.55
  ) +
  scale_x_log10(labels = label_number(scale_cut = cut_si(" "))) +
  scale_y_log10(labels = label_number(scale_cut = cut_si(" "))) +
  scale_size_continuous(labels = label_number(scale_cut = cut_si(" "))) +
  theme_minimal(base_size = 13) +
  labs(
    title = "Under- & overvalued players (2024–25)",
    subtitle = "Model-predicted vs Transfermarkt market values",
    x = "Actual market value (€)",
    y = "Predicted market value (€)",
    size = "|Δ| (€)",
    color = "Valuation status"
  )

### Plot 4: Undervalued players
library(dplyr)
library(ggplot2)
library(scales)

top_n <- 10

eval_2425 %>%
  filter(Player != "Diego López") %>%   # <- remove manually
  slice_max(delta_eur, n = top_n) %>%
  mutate(
    Player = reorder(Player, delta_eur),
    label = paste0(
      Comp, " | +",
      scales::label_number(scale_cut = scales::cut_si(" "))(delta_eur)
    )
  ) %>%
  ggplot(aes(x = Player, y = delta_eur, fill = Comp)) +
  geom_col(alpha = 0.9) +
  geom_text(aes(label = label), hjust = -0.05, size = 3.2) +
  coord_flip(clip = "off") +
  scale_y_continuous(
    labels = scales::label_number(scale_cut = scales::cut_si(" "))
  ) +
  theme_minimal(base_size = 13) +
  theme(
    legend.position = "none",
    plot.margin = margin(5.5, 120, 5.5, 5.5)
  ) +
  labs(
    title = paste0("Top ", top_n, " undervalued players (2024–25)"),
    subtitle = "Δ = Predicted - Transfermarkt market value",
    x = NULL,
    y = "Δ market value (€)"
  )

