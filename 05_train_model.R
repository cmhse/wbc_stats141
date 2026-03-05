# Model Training with Proper Holdout Validation
# Step 1: Train on 2020-2024, validate on 2025
# Step 2: Retrain on all data (2020-2025) for final deployment

library(tidyverse)
library(xgboost)

TRAIN_VALIDATION_MODEL <- TRUE  # Set to FALSE to skip validation model
TRAIN_FINAL_MODEL <- TRUE       # Set to TRUE to train final model on all data

message("=== Matchup Model Training (MLB DATA ONLY) ===\n")

# Load
message("Loading data...")
statcast_all <- readRDS("data/statcast_data.rds")

# FILTER TO MLB ONLY FOR TRAINING
# This gives cleaner signal by excluding small-sample AAA matchups
# We still build features/quality for ALL players (MLB + AAA)
message("Filtering to MLB data only for training...")
statcast <- statcast_all %>% filter(level == "MLB")
message(sprintf("  Total pitches: %s", format(nrow(statcast_all), big.mark = ",")))
message(sprintf("  MLB pitches: %s (%.1f%%)", 
                format(nrow(statcast), big.mark = ","),
                100 * nrow(statcast) / nrow(statcast_all)))
message(sprintf("  AAA excluded: %s (%.1f%%)\n", 
                format(nrow(statcast_all) - nrow(statcast), big.mark = ","),
                100 * (nrow(statcast_all) - nrow(statcast)) / nrow(statcast_all)))

pitcher_features <- readRDS("models/pitcher_features.rds")
batter_features <- readRDS("models/batter_features.rds")
pitcher_quality <- readRDS("models/pitcher_quality.rds")
batter_quality <- readRDS("models/batter_quality.rds")

# Build matchups function
build_matchups <- function(data, seasons_filter) {
  data %>%
    filter(!is.na(delta_run_exp), season %in% seasons_filter) %>%
    group_by(pitcher, batter, season, p_throws, stand, level) %>%
    summarize(
      n_pitches = n(),
      actual_delta_re = mean(delta_run_exp, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    filter(n_pitches >= 20)
}

# Add features function
add_features <- function(matchups, pitcher_feat, batter_feat, pitcher_qual, batter_qual) {
  matchup_data <- matchups %>%
    left_join(
      pitcher_feat %>% rename_with(~paste0(., "_p"), -c(pitcher, season, p_throws, stand, level)),
      by = c("pitcher", "season", "p_throws", "stand", "level")
    ) %>%
    left_join(
      batter_feat %>% rename_with(~paste0(., "_b"), -c(batter, season, stand, p_throws, level)),
      by = c("batter", "season", "stand", "p_throws", "level")
    ) %>%
    filter(!is.na(pa_vs_hand_p), !is.na(pa_vs_hand_b))
  
  # Add log5
  matchup_data <- matchup_data %>%
    left_join(
      pitcher_qual %>% select(pitcher, season, level, pitcher_quality),
      by = c("pitcher", "season", "level")
    ) %>%
    left_join(
      batter_qual %>% select(batter, season, level, batter_quality),
      by = c("batter", "season", "level")
    ) %>%
    filter(!is.na(pitcher_quality), !is.na(batter_quality))
  
  log5 <- function(p, b) {
    num <- p * (1 - b)
    denom <- num + b * (1 - p)
    ifelse(denom == 0, 0.5, num / denom)
  }
  
  matchup_data <- matchup_data %>%
    mutate(
      log5_prob = log5(pitcher_quality, batter_quality),
      log5_base = (0.5 - log5_prob) * 0.2
    )
  
  return(matchup_data)
}

# Build feature columns (same as original)
build_feature_matrix <- function(data) {
  safe_val <- function(x) ifelse(is.na(x) | !is.finite(x), 0, x)
  
  # All matchup features
  features <- data %>%
    transmute(
      log5_base = safe_val(log5_base),
      pitcher_platoon_effect = safe_val(delta_re_vs_hand_p),
      batter_platoon_effect = safe_val(delta_re_vs_hand_b),
      
      # Whiff interactions
      ff_matchup_whiff = safe_val(ff_pct_p * ff_whiff_rate_p * ff_whiff_rate_b),
      si_matchup_whiff = safe_val(si_pct_p * si_whiff_rate_p * si_whiff_rate_b),
      fc_matchup_whiff = safe_val(fc_pct_p * fc_whiff_rate_p * fc_whiff_rate_b),
      sl_matchup_whiff = safe_val(sl_pct_p * sl_whiff_rate_p * sl_whiff_rate_b),
      st_matchup_whiff = safe_val(st_pct_p * st_whiff_rate_p * st_whiff_rate_b),
      cu_matchup_whiff = safe_val(cu_pct_p * cu_whiff_rate_p * cu_whiff_rate_b),
      kc_matchup_whiff = safe_val(kc_pct_p * kc_whiff_rate_p * kc_whiff_rate_b),
      ch_matchup_whiff = safe_val(ch_pct_p * ch_whiff_rate_p * ch_whiff_rate_b),
      fs_matchup_whiff = safe_val(fs_pct_p * fs_whiff_rate_p * fs_whiff_rate_b),
      
      # Run value interactions
      ff_matchup_runs = safe_val(ff_pct_p * ff_delta_re_p * ff_delta_re_b),
      si_matchup_runs = safe_val(si_pct_p * si_delta_re_p * si_delta_re_b),
      fc_matchup_runs = safe_val(fc_pct_p * fc_delta_re_p * fc_delta_re_b),
      sl_matchup_runs = safe_val(sl_pct_p * sl_delta_re_p * sl_delta_re_b),
      st_matchup_runs = safe_val(st_pct_p * st_delta_re_p * st_delta_re_b),
      cu_matchup_runs = safe_val(cu_pct_p * cu_delta_re_p * cu_delta_re_b),
      kc_matchup_runs = safe_val(kc_pct_p * kc_delta_re_p * kc_delta_re_b),
      ch_matchup_runs = safe_val(ch_pct_p * ch_delta_re_p * ch_delta_re_b),
      fs_matchup_runs = safe_val(fs_pct_p * fs_delta_re_p * fs_delta_re_b),
      
      # Hard hit interactions
      ff_matchup_hard_hit = safe_val(ff_pct_p * ff_hard_hit_rate_b),
      si_matchup_hard_hit = safe_val(si_pct_p * si_hard_hit_rate_b),
      fc_matchup_hard_hit = safe_val(fc_pct_p * fc_hard_hit_rate_b),
      sl_matchup_hard_hit = safe_val(sl_pct_p * sl_hard_hit_rate_b),
      st_matchup_hard_hit = safe_val(st_pct_p * st_hard_hit_rate_b),
      cu_matchup_hard_hit = safe_val(cu_pct_p * cu_hard_hit_rate_b),
      kc_matchup_hard_hit = safe_val(kc_pct_p * kc_hard_hit_rate_b),
      ch_matchup_hard_hit = safe_val(ch_pct_p * ch_hard_hit_rate_b),
      fs_matchup_hard_hit = safe_val(fs_pct_p * fs_hard_hit_rate_b),
      
      # Barrel interactions  
      ff_matchup_barrel = safe_val(ff_pct_p * ff_barrel_rate_b),
      si_matchup_barrel = safe_val(si_pct_p * si_barrel_rate_b),
      fc_matchup_barrel = safe_val(fc_pct_p * fc_barrel_rate_b),
      sl_matchup_barrel = safe_val(sl_pct_p * sl_barrel_rate_b),
      st_matchup_barrel = safe_val(st_pct_p * st_barrel_rate_b),
      cu_matchup_barrel = safe_val(cu_pct_p * cu_barrel_rate_b),
      kc_matchup_barrel = safe_val(kc_pct_p * kc_barrel_rate_b),
      ch_matchup_barrel = safe_val(ch_pct_p * ch_barrel_rate_b),
      fs_matchup_barrel = safe_val(fs_pct_p * fs_barrel_rate_b),
      
      # Velo/spin advantages
      ff_velo_advantage = safe_val(ff_velo_p - 93),
      si_velo_advantage = safe_val(si_velo_p - 93),
      sl_velo_advantage = safe_val(sl_velo_p - 84),
      cu_velo_advantage = safe_val(cu_velo_p - 78),
      
      ff_spin_advantage = safe_val(ff_spin_p - 2200),
      sl_spin_advantage = safe_val(sl_spin_p - 2400),
      cu_spin_advantage = safe_val(cu_spin_p - 2500),
      
      # Derived interactions
      exit_velo_vs_chase = safe_val(avg_exit_velo_b * (1 - chase_rate_b)),
      hard_hit_vs_k_diff = safe_val(hard_hit_rate_b * exp(-(k_rate_vs_hand_p - k_rate_vs_hand_b))),
      
      k_rate_diff = safe_val(k_rate_vs_hand_p - k_rate_vs_hand_b),
      bb_rate_diff = safe_val(bb_rate_vs_hand_b),
      
      # Pitcher pitch mix
      ff_pct_p = safe_val(ff_pct_p),
      si_pct_p = safe_val(si_pct_p),
      fc_pct_p = safe_val(fc_pct_p),
      sl_pct_p = safe_val(sl_pct_p),
      st_pct_p = safe_val(st_pct_p),
      cu_pct_p = safe_val(cu_pct_p),
      kc_pct_p = safe_val(kc_pct_p),
      ch_pct_p = safe_val(ch_pct_p),
      fs_pct_p = safe_val(fs_pct_p),
      
      # Batter characteristics
      chase_rate_b = safe_val(chase_rate_b),
      zone_contact_rate_b = safe_val(zone_contact_rate_b),
      hard_hit_rate_b = safe_val(hard_hit_rate_b),
      barrel_rate_b = safe_val(barrel_rate_b),
      
      same_hand = as.numeric(p_throws == stand)
    )
  
  return(features)
}

# ==================== STEP 1: TRAIN ON 2020-2024, VALIDATE ON 2025 ====================

if (TRAIN_VALIDATION_MODEL) {
  message("\n=== STEP 1: Training Validation Model (2020-2024) ===\n")
  
  # Build training data (2020-2024)
  message("Building TRAINING matchups (2020-2024)...")
  matchups_train <- build_matchups(statcast, 2020:2024)
  message(sprintf("Found %s training matchups", format(nrow(matchups_train), big.mark = ",")))
  
  matchup_train_data <- add_features(matchups_train, pitcher_features, batter_features, 
                                     pitcher_quality, batter_quality)
  message(sprintf("Retained %s with complete features\n", format(nrow(matchup_train_data), big.mark = ",")))
  
  # Build feature matrix
  message("Building feature matrix...")
  X_train <- build_feature_matrix(matchup_train_data)
  y_train <- matchup_train_data$actual_delta_re
  
  feature_cols <- colnames(X_train)
  
  # Train model
  message("Training XGBoost model...")
  dtrain <- xgb.DMatrix(data = as.matrix(X_train), label = y_train)
  
  params <- list(
    objective = "reg:squarederror",
    eta = 0.05,
    max_depth = 6,
    subsample = 0.8,
    colsample_bytree = 0.8,
    min_child_weight = 5
  )
  
  model_2024 <- xgb.train(
    params = params,
    data = dtrain,
    nrounds = 500,
    verbose = 1,
    print_every_n = 50
  )
  
  # Save validation model
  xgb.save(model_2024, "models/matchup_model_2024.json")
  saveRDS(feature_cols, "models/model_features.rds")
  message("\nSaved validation model to models/matchup_model_2024.json")
  
  # Feature importance
  importance <- xgb.importance(model = model_2024, feature_names = feature_cols)
  write.csv(importance, "models/feature_importance_2024.csv", row.names = FALSE)
  message("Saved feature importance to models/feature_importance_2024.csv\n")
}

# ==================== STEP 2: RETRAIN ON ALL DATA (2020-2025) ====================

if (TRAIN_FINAL_MODEL) {
  message("\n=== STEP 2: Training Final Model (2020-2025) ===\n")
  
  # Build ALL data
  message("Building FULL matchups (2020-2025)...")
  matchups_all <- build_matchups(statcast, 2020:2025)
  message(sprintf("Found %s total matchups", format(nrow(matchups_all), big.mark = ",")))
  
  matchup_all_data <- add_features(matchups_all, pitcher_features, batter_features,
                                   pitcher_quality, batter_quality)
  message(sprintf("Retained %s with complete features\n", format(nrow(matchup_all_data), big.mark = ",")))
  
  # Build feature matrix
  message("Building feature matrix...")
  X_all <- build_feature_matrix(matchup_all_data)
  y_all <- matchup_all_data$actual_delta_re
  
  feature_cols <- colnames(X_all)
  
  # Train model
  message("Training XGBoost model on ALL data...")
  dall <- xgb.DMatrix(data = as.matrix(X_all), label = y_all)
  
  params <- list(
    objective = "reg:squarederror",
    eta = 0.05,
    max_depth = 6,
    subsample = 0.8,
    colsample_bytree = 0.8,
    min_child_weight = 5
  )
  
  model_final <- xgb.train(
    params = params,
    data = dall,
    nrounds = 500,
    verbose = 1,
    print_every_n = 50
  )
  
  # Save final model (this is what the app uses)
  xgb.save(model_final, "models/matchup_model.json")
  saveRDS(feature_cols, "models/model_features.rds")
  message("\nSaved FINAL model to models/matchup_model.json")
  
  # Feature importance
  importance <- xgb.importance(model = model_final, feature_names = feature_cols)
  write.csv(importance, "models/feature_importance.csv", row.names = FALSE)
  message("Saved feature importance to models/feature_importance.csv\n")
  
  # Get current players for app
  current_pitchers <- unique(pitcher_features$pitcher[pitcher_features$season >= 2024])
  current_batters <- unique(batter_features$batter[batter_features$season >= 2024])
  
  saveRDS(current_pitchers, "models/current_pitchers.rds")
  saveRDS(current_batters, "models/current_batters.rds")
  message("Saved current player lists\n")
}

message("\n=== TRAINING COMPLETE ===")
message("Next steps:")
message("1. Run validate_model_2025.R to validate the 2024 model on 2025 data")
message("2. Use matchup_model.json (final model) in the Shiny app")