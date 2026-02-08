# Model Training
# Trains XGBoost model on pitcher-batter matchups

library(tidyverse)
library(xgboost)

message("Training matchup model\n")

# Load
statcast <- readRDS("data/statcast_data.rds")
pitcher_features <- readRDS("models/pitcher_features.rds")
batter_features <- readRDS("models/batter_features.rds")
pitcher_quality <- readRDS("models/pitcher_quality.rds")
batter_quality <- readRDS("models/batter_quality.rds")

# Build matchups
message("Building matchup dataset...")

matchups_all <- statcast %>%
  filter(!is.na(delta_run_exp)) %>%
  group_by(pitcher, batter, season, p_throws, stand, level) %>%
  summarize(
    n_pitches = n(),
    actual_delta_re = mean(delta_run_exp, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  filter(n_pitches >= 20)

message(sprintf("Found %s matchups (2020-2025)", format(nrow(matchups_all), big.mark = ",")))

# Join features
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

message(sprintf("Retained %s with complete features\n", format(nrow(matchup_data), big.mark = ",")))

# Add derived features
message("Computing matchup interactions...")

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
    
    # Whiff matchups
    ff_matchup_whiff = ff_pct_p * ff_whiff_rate_p * ff_whiff_rate_b,
    si_matchup_whiff = si_pct_p * si_whiff_rate_p * si_whiff_rate_b,
    fc_matchup_whiff = fc_pct_p * fc_whiff_rate_p * fc_whiff_rate_b,
    sl_matchup_whiff = sl_pct_p * sl_whiff_rate_p * sl_whiff_rate_b,
    st_matchup_whiff = st_pct_p * st_whiff_rate_p * st_whiff_rate_b,
    cu_matchup_whiff = cu_pct_p * cu_whiff_rate_p * cu_whiff_rate_b,
    kc_matchup_whiff = kc_pct_p * kc_whiff_rate_p * kc_whiff_rate_b,
    ch_matchup_whiff = ch_pct_p * ch_whiff_rate_p * ch_whiff_rate_b,
    fs_matchup_whiff = fs_pct_p * fs_whiff_rate_p * fs_whiff_rate_b,
    
    # Run matchups
    ff_matchup_runs = ff_pct_p * (-ff_delta_re_p) * ff_delta_re_b,
    si_matchup_runs = si_pct_p * (-si_delta_re_p) * si_delta_re_b,
    fc_matchup_runs = fc_pct_p * (-fc_delta_re_p) * fc_delta_re_b,
    sl_matchup_runs = sl_pct_p * (-sl_delta_re_p) * sl_delta_re_b,
    st_matchup_runs = st_pct_p * (-st_delta_re_p) * st_delta_re_b,
    cu_matchup_runs = cu_pct_p * (-cu_delta_re_p) * cu_delta_re_b,
    kc_matchup_runs = kc_pct_p * (-kc_delta_re_p) * kc_delta_re_b,
    ch_matchup_runs = ch_pct_p * (-ch_delta_re_p) * ch_delta_re_b,
    fs_matchup_runs = fs_pct_p * (-fs_delta_re_p) * fs_delta_re_b,
    
    # Stuff advantages
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

message(sprintf("Final dataset: %s matchups\n", format(nrow(matchup_data), big.mark = ",")))

# Current players (2024-2025)
message("Identifying current players...")

current_pitchers <- unique(pitcher_features$pitcher[pitcher_features$season >= 2024])
current_batters <- unique(batter_features$batter[batter_features$season >= 2024])

message(sprintf("Current pitchers: %s", format(length(current_pitchers), big.mark = ",")))
message(sprintf("Current batters: %s\n", format(length(current_batters), big.mark = ",")))

matchup_data <- matchup_data %>%
  mutate(
    pitcher_is_current = pitcher %in% current_pitchers,
    batter_is_current = batter %in% current_batters,
    both_current = pitcher_is_current & batter_is_current
  )

message("Matchup breakdown:")
message(sprintf("  Both current: %s", format(sum(matchup_data$both_current), big.mark = ",")))
message(sprintf("  At least one: %s", format(sum(matchup_data$pitcher_is_current | matchup_data$batter_is_current), big.mark = ",")))
message(sprintf("  Neither: %s\n", format(sum(!matchup_data$pitcher_is_current & !matchup_data$batter_is_current), big.mark = ",")))

# Features
feature_cols <- c(
  "log5_base",
  "pitcher_platoon_effect", "batter_platoon_effect",
  
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

message(sprintf("Training with %d features\n", length(feature_cols)))

# Load best parameters if available, otherwise use defaults
if (file.exists("models/best_params.rds")) {
  message("Loading tuned hyperparameters...")
  best_params <- readRDS("models/best_params.rds")
  params <- list(
    objective = "reg:squarederror",
    max_depth = best_params$max_depth,
    eta = best_params$eta,
    min_child_weight = best_params$min_child_weight,
    subsample = best_params$subsample,
    colsample_bytree = best_params$colsample_bytree
  )
  message(sprintf("Using tuned params: depth=%d, eta=%.3f, min_child_weight=%d, subsample=%.2f, colsample=%.2f\n",
                  params$max_depth, params$eta, params$min_child_weight, 
                  params$subsample, params$colsample_bytree))
} else {
  message("Using default hyperparameters (run tune_hyperparameters.R to optimize)")
  params <- list(
    objective = "reg:squarederror",
    max_depth = 4,
    eta = 0.05,
    subsample = 0.8,
    colsample_bytree = 0.8
  )
  message("Default params: depth=4, eta=0.05, subsample=0.8, colsample=0.8\n")
}

# Train with 5-fold CV
message("Training with 5-fold cross-validation...")

set.seed(42)
n_folds <- 5
matchup_data$fold <- sample(rep(1:n_folds, length.out = nrow(matchup_data)))

all_predictions <- data.frame(
  actual = numeric(nrow(matchup_data)),
  log5_pred = numeric(nrow(matchup_data)),
  hybrid_pred = numeric(nrow(matchup_data)),
  fold = integer(nrow(matchup_data)),
  both_current = logical(nrow(matchup_data))
)

for (fold_i in 1:n_folds) {
  cat(sprintf("Fold %d/%d... ", fold_i, n_folds))
  
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
    watchlist = list(train = train_matrix, val = val_matrix),
    early_stopping_rounds = 20,
    verbose = 0
  )
  
  val_idx <- which(matchup_data$fold == fold_i)
  all_predictions$actual[val_idx] <- val_data$actual_delta_re
  all_predictions$log5_pred[val_idx] <- val_data$log5_base
  all_predictions$hybrid_pred[val_idx] <- predict(model_fold, val_matrix)
  all_predictions$fold[val_idx] <- fold_i
  all_predictions$both_current[val_idx] <- val_data$both_current
  
  fold_log5 <- cor(val_data$actual_delta_re, val_data$log5_base)
  fold_hybrid <- cor(val_data$actual_delta_re, all_predictions$hybrid_pred[val_idx])
  
  cat(sprintf("Log5: %.3f | Hybrid: %.3f | Δ: %+.3f\n",
              fold_log5, fold_hybrid, fold_hybrid - fold_log5))
}

# Final model
message("\nTraining final model...")
full_matrix <- xgb.DMatrix(
  data = as.matrix(matchup_data[, feature_cols]),
  label = matchup_data$actual_delta_re
)

final_model <- xgb.train(
  params = params,
  data = full_matrix,
  nrounds = 150,
  verbose = 0
)

# Evaluate
message("\n=== Results (All Matchups) ===\n")

log5_cor <- cor(all_predictions$actual, all_predictions$log5_pred)
hybrid_cor <- cor(all_predictions$actual, all_predictions$hybrid_pred)

message(sprintf("Log5:   %.3f", log5_cor))
message(sprintf("Hybrid: %.3f", hybrid_cor))
message(sprintf("Improvement: %+.3f (%.1f%% better)\n",
                hybrid_cor - log5_cor, 100 * (hybrid_cor - log5_cor) / log5_cor))

message("=== Results (Current Players Only) ===\n")

current_preds <- all_predictions %>% filter(both_current)

if (nrow(current_preds) > 0) {
  log5_cor_current <- cor(current_preds$actual, current_preds$log5_pred)
  hybrid_cor_current <- cor(current_preds$actual, current_preds$hybrid_pred)
  
  message(sprintf("Matchups: %s", format(nrow(current_preds), big.mark = ",")))
  message(sprintf("Log5:   %.3f", log5_cor_current))
  message(sprintf("Hybrid: %.3f", hybrid_cor_current))
  message(sprintf("Improvement: %+.3f\n", hybrid_cor_current - log5_cor_current))
}

# Tier analysis
all_predictions <- all_predictions %>%
  mutate(hybrid_quintile = ntile(hybrid_pred, 5))

tiers <- all_predictions %>%
  group_by(hybrid_quintile) %>%
  summarize(
    n = n(),
    avg_actual = mean(actual),
    avg_pred = mean(hybrid_pred),
    .groups = "drop"
  )

tier_cor <- cor(tiers$avg_pred, tiers$avg_actual)

message("Tier Performance:")
print(tiers)
message(sprintf("\nTier correlation: %.3f\n", tier_cor))

# Feature importance
importance <- xgb.importance(model = final_model)
message("Top 20 Features:")
print(head(importance, 20))

# Save
message("\n=== Saving ===")
xgb.save(final_model, "models/matchup_model.json")
saveRDS(feature_cols, "models/model_features.rds")
write.csv(all_predictions, "models/predictions.csv", row.names = FALSE)
saveRDS(matchup_data, "models/matchup_data.rds")

saveRDS(list(
  pitchers = current_pitchers,
  batters = current_batters
), "models/current_players.rds")

message("\nModel training complete!")
message(sprintf("  %s total matchups", format(nrow(matchup_data), big.mark = ",")))
message(sprintf("  %s current player matchups", format(sum(matchup_data$both_current), big.mark = ",")))
message(sprintf("  %d features", length(feature_cols)))
message(sprintf("  %.3f correlation", hybrid_cor))
message(sprintf("  %.3f tier correlation", tier_cor))