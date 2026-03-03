# Hyperparameter Tuning
# Grid search to find optimal XGBoost parameters

library(tidyverse)
library(xgboost)

message("Hyperparameter tuning for matchup model\n")

# Load data (use the good approach - 20+ PA only)
statcast <- readRDS("data/statcast_data.rds")
pitcher_features <- readRDS("models/pitcher_features.rds")
batter_features <- readRDS("models/batter_features.rds")
pitcher_quality <- readRDS("models/pitcher_quality.rds")
batter_quality <- readRDS("models/batter_quality.rds")

message("Building matchup dataset (20+ PA only)...")

matchups_all <- statcast %>%
  filter(!is.na(delta_run_exp)) %>%
  group_by(pitcher, batter, season, p_throws, stand, level) %>%
  summarize(
    n_pitches = n(),
    actual_delta_re = mean(delta_run_exp, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  filter(n_pitches >= 20)

# Join features and add derived features (same as before)
matchup_data <- matchups_all %>%
  left_join(
    pitcher_features %>% rename_with(~paste0(., "_p"), -c(pitcher, season, p_throws, stand, level)),
    by = c("pitcher", "season", "p_throws", "stand", "level")
  ) %>%
  left_join(
    batter_features %>% rename_with(~paste0(., "_b"), -c(batter, season, stand, p_throws, level)),
    by = c("batter", "season", "stand", "p_throws", "level")
  ) %>%
  filter(!is.na(pa_vs_hand_p), !is.na(pa_vs_hand_b))

log5 <- function(p, b) {
  num <- p * (1 - b)
  denom <- num + b * (1 - p)
  return(num / denom)
}

matchup_data <- matchup_data %>%
  left_join(pitcher_quality %>% select(pitcher, season, level, pitcher_quality),
            by = c("pitcher", "season", "level")) %>%
  left_join(batter_quality %>% select(batter, season, level, batter_quality),
            by = c("batter", "season", "level")) %>%
  filter(!is.na(pitcher_quality), !is.na(batter_quality)) %>%
  mutate(
    log5_prob = log5(pitcher_quality, batter_quality),
    log5_base = (0.5 - log5_prob) * 0.2,
    pitcher_platoon_effect = delta_re_vs_hand_p - (pitcher_quality * (-0.2)),
    batter_platoon_effect = delta_re_vs_hand_b - (batter_quality * 0.2),
    ff_matchup_whiff = ff_pct_p * ff_whiff_rate_p * ff_whiff_rate_b,
    si_matchup_whiff = si_pct_p * si_whiff_rate_p * si_whiff_rate_b,
    fc_matchup_whiff = fc_pct_p * fc_whiff_rate_p * fc_whiff_rate_b,
    sl_matchup_whiff = sl_pct_p * sl_whiff_rate_p * sl_whiff_rate_b,
    st_matchup_whiff = st_pct_p * st_whiff_rate_p * st_whiff_rate_b,
    cu_matchup_whiff = cu_pct_p * cu_whiff_rate_p * cu_whiff_rate_b,
    kc_matchup_whiff = kc_pct_p * kc_whiff_rate_p * kc_whiff_rate_b,
    ch_matchup_whiff = ch_pct_p * ch_whiff_rate_p * ch_whiff_rate_b,
    fs_matchup_whiff = fs_pct_p * fs_whiff_rate_p * fs_whiff_rate_b,
    ff_matchup_runs = ff_pct_p * (-ff_delta_re_p) * ff_delta_re_b,
    si_matchup_runs = si_pct_p * (-si_delta_re_p) * si_delta_re_b,
    fc_matchup_runs = fc_pct_p * (-fc_delta_re_p) * fc_delta_re_b,
    sl_matchup_runs = sl_pct_p * (-sl_delta_re_p) * sl_delta_re_b,
    st_matchup_runs = st_pct_p * (-st_delta_re_p) * st_delta_re_b,
    cu_matchup_runs = cu_pct_p * (-cu_delta_re_p) * cu_delta_re_b,
    kc_matchup_runs = kc_pct_p * (-kc_delta_re_p) * kc_delta_re_b,
    ch_matchup_runs = ch_pct_p * (-ch_delta_re_p) * ch_delta_re_b,
    fs_matchup_runs = fs_pct_p * (-fs_delta_re_p) * fs_delta_re_b,
    ff_velo_advantage = ff_velo_p - 93,
    si_velo_advantage = si_velo_p - 93,
    sl_velo_advantage = sl_velo_p - 84,
    cu_velo_advantage = cu_velo_p - 78,
    ff_spin_advantage = ff_spin_p - 2200,
    sl_spin_advantage = sl_spin_p - 2400,
    cu_spin_advantage = cu_spin_p - 2500,
    k_rate_diff = k_rate_vs_hand_p - k_rate_vs_hand_b,
    bb_rate_diff = bb_rate_vs_hand_b,
    same_hand = as.numeric(p_throws == stand),
    exit_velo_vs_chase = avg_exit_velo_b * (1 - chase_rate_b),
    hard_hit_vs_k_diff = hard_hit_rate_b * exp(-k_rate_diff)
  )

message(sprintf("Dataset: %s matchups\n", format(nrow(matchup_data), big.mark = ",")))

# Features
feature_cols <- c(
  "log5_base", "pitcher_platoon_effect", "batter_platoon_effect",
  "ff_matchup_whiff", "si_matchup_whiff", "fc_matchup_whiff",
  "sl_matchup_whiff", "st_matchup_whiff", "cu_matchup_whiff",
  "kc_matchup_whiff", "ch_matchup_whiff", "fs_matchup_whiff",
  "ff_matchup_runs", "si_matchup_runs", "fc_matchup_runs",
  "sl_matchup_runs", "st_matchup_runs", "cu_matchup_runs",
  "kc_matchup_runs", "ch_matchup_runs", "fs_matchup_runs",
  "ff_velo_advantage", "si_velo_advantage", "sl_velo_advantage", "cu_velo_advantage",
  "ff_spin_advantage", "sl_spin_advantage", "cu_spin_advantage",
  "exit_velo_vs_chase", "hard_hit_vs_k_diff",
  "k_rate_diff", "bb_rate_diff",
  "ff_pct_p", "si_pct_p", "fc_pct_p", "sl_pct_p", "st_pct_p",
  "cu_pct_p", "kc_pct_p", "ch_pct_p", "fs_pct_p",
  "chase_rate_b", "zone_contact_rate_b", "hard_hit_rate_b", "barrel_rate_b",
  "same_hand"
)

# Create folds
set.seed(42)
n_folds <- 5
matchup_data$fold <- sample(rep(1:n_folds, length.out = nrow(matchup_data)))

# Hyperparameter grid
param_grid <- expand.grid(
  max_depth = c(3, 4, 5, 6),
  eta = c(0.01, 0.03, 0.05, 0.1),
  min_child_weight = c(1, 3, 5),
  subsample = c(0.7, 0.8, 0.9),
  colsample_bytree = c(0.7, 0.8, 0.9)
)

message(sprintf("Testing %d parameter combinations\n", nrow(param_grid)))
message("This will take a while...\n")

# Store results
results <- data.frame()

# Test each combination
for (i in 1:nrow(param_grid)) {
  if (i %% 10 == 1) {
    cat(sprintf("[%d/%d] ", i, nrow(param_grid)))
  }
  
  params <- list(
    objective = "reg:squarederror",
    max_depth = param_grid$max_depth[i],
    eta = param_grid$eta[i],
    min_child_weight = param_grid$min_child_weight[i],
    subsample = param_grid$subsample[i],
    colsample_bytree = param_grid$colsample_bytree[i]
  )
  
  # 5-fold CV
  fold_cors <- numeric(n_folds)
  
  for (fold_i in 1:n_folds) {
    train_data <- matchup_data[matchup_data$fold != fold_i, ]
    val_data <- matchup_data[matchup_data$fold == fold_i, ]
    
    train_matrix <- xgb.DMatrix(
      data = as.matrix(train_data[, feature_cols]),
      label = train_data$actual_delta_re
    )
    
    val_matrix <- xgb.DMatrix(
      data = as.matrix(val_data[, feature_cols]),
      label = val_data$actual_delta_re
    )
    
    model_fold <- xgb.train(
      params = params,
      data = train_matrix,
      nrounds = 200,
      watchlist = list(val = val_matrix),
      early_stopping_rounds = 20,
      verbose = 0
    )
    
    preds <- predict(model_fold, val_matrix)
    fold_cors[fold_i] <- cor(val_data$actual_delta_re, preds)
  }
  
  avg_cor <- mean(fold_cors)
  
  results <- rbind(results, data.frame(
    max_depth = param_grid$max_depth[i],
    eta = param_grid$eta[i],
    min_child_weight = param_grid$min_child_weight[i],
    subsample = param_grid$subsample[i],
    colsample_bytree = param_grid$colsample_bytree[i],
    correlation = avg_cor
  ))
  
  if (i %% 10 == 0) {
    cat(sprintf("Best so far: %.3f\n", max(results$correlation)))
  }
}

# Find best parameters
results <- results %>% arrange(desc(correlation))

message("\n=== TOP 10 PARAMETER COMBINATIONS ===\n")
print(head(results, 10))

message("\n=== BEST PARAMETERS ===")
best_params <- results[1, ]
message(sprintf("Correlation: %.3f", best_params$correlation))
message(sprintf("max_depth: %d", best_params$max_depth))
message(sprintf("eta: %.3f", best_params$eta))
message(sprintf("min_child_weight: %d", best_params$min_child_weight))
message(sprintf("subsample: %.2f", best_params$subsample))
message(sprintf("colsample_bytree: %.2f", best_params$colsample_bytree))

# Save results
write.csv(results, "models/hyperparameter_tuning_results.csv", row.names = FALSE)
saveRDS(best_params, "models/best_params.rds")

message("\nSaved: models/hyperparameter_tuning_results.csv")
message("Saved: models/best_params.rds")
message("\nUse these parameters in 05_train_model.R for best performance!")
