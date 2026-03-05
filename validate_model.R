# Complete 2025 Holdout Validation
# Matches original validation: bucket analysis + overall correlation

library(tidyverse)
library(xgboost)

message("=== 2025 Holdout Validation ===\n")
message("Model trained on 2020-2024, tested on 2025 (TRUE holdout)\n")

# Load data
statcast <- readRDS("data/statcast_data.rds")
pitcher_features <- readRDS("models/pitcher_features.rds")
batter_features <- readRDS("models/batter_features.rds")
pitcher_quality <- readRDS("models/pitcher_quality.rds")
batter_quality <- readRDS("models/batter_quality.rds")

# Use 2024 model for holdout validation
if (file.exists("models/matchup_model_2024.json")) {
  message("✓ Using 2024 model (trained on 2020-2024 only)\n")
  model <- xgb.load("models/matchup_model_2024.json")
} else {
  stop("ERROR: Need matchup_model_2024.json for proper holdout validation!")
}

feature_cols <- readRDS("models/model_features.rds")

# Build matchup features
build_matchup_features <- function(pitcher_id, batter_id) {
  # Use 2025 data for validation
  p_feat <- pitcher_features %>% filter(pitcher == pitcher_id, season == 2025)
  b_feat <- batter_features %>% filter(batter == batter_id, season == 2025)
  p_qual <- pitcher_quality %>% filter(pitcher == pitcher_id, season == 2025, level == "MLB")
  b_qual <- batter_quality %>% filter(batter == batter_id, season == 2025, level == "MLB")
  
  if (nrow(p_feat) == 0 || nrow(b_feat) == 0 || nrow(p_qual) == 0 || nrow(b_qual) == 0) {
    return(NULL)
  }
  
  batter_hand <- unique(b_feat$stand)[1]
  pitcher_hand <- unique(p_feat$p_throws)[1]
  
  p_feat <- p_feat %>% filter(stand == batter_hand)
  b_feat <- b_feat %>% filter(p_throws == pitcher_hand)
  
  if (nrow(p_feat) == 0 || nrow(b_feat) == 0) {
    return(NULL)
  }
  
  p_feat <- p_feat[1,]
  b_feat <- b_feat[1,]
  
  # Log5
  log5_prob <- (p_qual$pitcher_quality[1] * (1 - b_qual$batter_quality[1])) /
    ((p_qual$pitcher_quality[1] * (1 - b_qual$batter_quality[1])) + 
       (b_qual$batter_quality[1] * (1 - p_qual$pitcher_quality[1])))
  log5_base <- (0.5 - log5_prob) * 0.2
  
  safe_val <- function(x) ifelse(is.na(x) | !is.finite(x), 0, x)
  
  features <- data.frame(
    log5_base = safe_val(log5_base),
    pitcher_platoon_effect = safe_val(p_feat$delta_re_vs_hand),
    batter_platoon_effect = safe_val(b_feat$delta_re_vs_hand),
    
    ff_matchup_whiff = safe_val(p_feat$ff_pct * p_feat$ff_whiff_rate * b_feat$ff_whiff_rate),
    si_matchup_whiff = safe_val(p_feat$si_pct * p_feat$si_whiff_rate * b_feat$si_whiff_rate),
    fc_matchup_whiff = safe_val(p_feat$fc_pct * p_feat$fc_whiff_rate * b_feat$fc_whiff_rate),
    sl_matchup_whiff = safe_val(p_feat$sl_pct * p_feat$sl_whiff_rate * b_feat$sl_whiff_rate),
    st_matchup_whiff = safe_val(p_feat$st_pct * p_feat$st_whiff_rate * b_feat$st_whiff_rate),
    cu_matchup_whiff = safe_val(p_feat$cu_pct * p_feat$cu_whiff_rate * b_feat$cu_whiff_rate),
    kc_matchup_whiff = safe_val(p_feat$kc_pct * p_feat$kc_whiff_rate * b_feat$kc_whiff_rate),
    ch_matchup_whiff = safe_val(p_feat$ch_pct * p_feat$ch_whiff_rate * b_feat$ch_whiff_rate),
    fs_matchup_whiff = safe_val(p_feat$fs_pct * p_feat$fs_whiff_rate * b_feat$fs_whiff_rate),
    
    ff_matchup_runs = safe_val(p_feat$ff_pct * p_feat$ff_delta_re * b_feat$ff_delta_re),
    si_matchup_runs = safe_val(p_feat$si_pct * p_feat$si_delta_re * b_feat$si_delta_re),
    fc_matchup_runs = safe_val(p_feat$fc_pct * p_feat$fc_delta_re * b_feat$fc_delta_re),
    sl_matchup_runs = safe_val(p_feat$sl_pct * p_feat$sl_delta_re * b_feat$sl_delta_re),
    st_matchup_runs = safe_val(p_feat$st_pct * p_feat$st_delta_re * b_feat$st_delta_re),
    cu_matchup_runs = safe_val(p_feat$cu_pct * p_feat$cu_delta_re * b_feat$cu_delta_re),
    kc_matchup_runs = safe_val(p_feat$kc_pct * p_feat$kc_delta_re * b_feat$kc_delta_re),
    ch_matchup_runs = safe_val(p_feat$ch_pct * p_feat$ch_delta_re * b_feat$ch_delta_re),
    fs_matchup_runs = safe_val(p_feat$fs_pct * p_feat$fs_delta_re * b_feat$fs_delta_re),
    
    ff_matchup_hard_hit = safe_val(p_feat$ff_pct * b_feat$ff_hard_hit_rate),
    si_matchup_hard_hit = safe_val(p_feat$si_pct * b_feat$si_hard_hit_rate),
    fc_matchup_hard_hit = safe_val(p_feat$fc_pct * b_feat$fc_hard_hit_rate),
    sl_matchup_hard_hit = safe_val(p_feat$sl_pct * b_feat$sl_hard_hit_rate),
    st_matchup_hard_hit = safe_val(p_feat$st_pct * b_feat$st_hard_hit_rate),
    cu_matchup_hard_hit = safe_val(p_feat$cu_pct * b_feat$cu_hard_hit_rate),
    kc_matchup_hard_hit = safe_val(p_feat$kc_pct * b_feat$kc_hard_hit_rate),
    ch_matchup_hard_hit = safe_val(p_feat$ch_pct * b_feat$ch_hard_hit_rate),
    fs_matchup_hard_hit = safe_val(p_feat$fs_pct * b_feat$fs_hard_hit_rate),
    
    ff_matchup_barrel = safe_val(p_feat$ff_pct * b_feat$ff_barrel_rate),
    si_matchup_barrel = safe_val(p_feat$si_pct * b_feat$si_barrel_rate),
    fc_matchup_barrel = safe_val(p_feat$fc_pct * b_feat$fc_barrel_rate),
    sl_matchup_barrel = safe_val(p_feat$sl_pct * b_feat$sl_barrel_rate),
    st_matchup_barrel = safe_val(p_feat$st_pct * b_feat$st_barrel_rate),
    cu_matchup_barrel = safe_val(p_feat$cu_pct * b_feat$cu_barrel_rate),
    kc_matchup_barrel = safe_val(p_feat$kc_pct * b_feat$kc_barrel_rate),
    ch_matchup_barrel = safe_val(p_feat$ch_pct * b_feat$ch_barrel_rate),
    fs_matchup_barrel = safe_val(p_feat$fs_pct * b_feat$fs_barrel_rate),
    
    ff_velo_advantage = safe_val(p_feat$ff_velo - 93),
    si_velo_advantage = safe_val(p_feat$si_velo - 93),
    sl_velo_advantage = safe_val(p_feat$sl_velo - 84),
    cu_velo_advantage = safe_val(p_feat$cu_velo - 78),
    
    ff_spin_advantage = safe_val(p_feat$ff_spin - 2200),
    sl_spin_advantage = safe_val(p_feat$sl_spin - 2400),
    cu_spin_advantage = safe_val(p_feat$cu_spin - 2500),
    
    exit_velo_vs_chase = safe_val(b_feat$avg_exit_velo * (1 - b_feat$chase_rate)),
    hard_hit_vs_k_diff = safe_val(b_feat$hard_hit_rate * exp(-(p_feat$k_rate_vs_hand - b_feat$k_rate_vs_hand))),
    
    k_rate_diff = safe_val(p_feat$k_rate_vs_hand - b_feat$k_rate_vs_hand),
    bb_rate_diff = safe_val(b_feat$bb_rate_vs_hand),
    
    ff_pct_p = safe_val(p_feat$ff_pct),
    si_pct_p = safe_val(p_feat$si_pct),
    fc_pct_p = safe_val(p_feat$fc_pct),
    sl_pct_p = safe_val(p_feat$sl_pct),
    st_pct_p = safe_val(p_feat$st_pct),
    cu_pct_p = safe_val(p_feat$cu_pct),
    kc_pct_p = safe_val(p_feat$kc_pct),
    ch_pct_p = safe_val(p_feat$ch_pct),
    fs_pct_p = safe_val(p_feat$fs_pct),
    
    chase_rate_b = safe_val(b_feat$chase_rate),
    zone_contact_rate_b = safe_val(b_feat$zone_contact_rate),
    hard_hit_rate_b = safe_val(b_feat$hard_hit_rate),
    barrel_rate_b = safe_val(b_feat$barrel_rate),
    
    same_hand = as.numeric(pitcher_hand == batter_hand)
  )
  
  return(features)
}

# Get ALL 2025 matchups (no PA filter yet)
message("Finding 2025 matchups...")
matchups_2025 <- statcast %>%
  filter(season == 2025, !is.na(events)) %>%
  group_by(pitcher, batter) %>%
  summarize(
    n_pa = n_distinct(paste(game_pk, at_bat_number)),
    hits = sum(events %in% c("single", "double", "triple", "home_run")),
    total_bases = sum(case_when(
      events == "single" ~ 1,
      events == "double" ~ 2,
      events == "triple" ~ 3,
      events == "home_run" ~ 4,
      TRUE ~ 0
    )),
    walks = sum(events == "walk"),
    at_bats = sum(events %in% c("single", "double", "triple", "home_run",
                                "strikeout", "field_out", "grounded_into_double_play",
                                "force_out", "fielders_choice_out", "sac_fly", "double_play",
                                "field_error", "fielders_choice", "strikeout_double_play")),
    .groups = "drop"
  ) %>%
  filter(at_bats > 0) %>%
  mutate(
    obp = (hits + walks) / (at_bats + walks),
    slg = total_bases / at_bats,
    ops = obp + slg
  )

message(sprintf("Found %d total matchups in 2025\n", nrow(matchups_2025)))

# Generate predictions for ALL matchups
message("Generating predictions...")
predictions <- data.frame()

pb <- txtProgressBar(max = nrow(matchups_2025), style = 3)
for (i in 1:nrow(matchups_2025)) {
  setTxtProgressBar(pb, i)
  
  features <- build_matchup_features(matchups_2025$pitcher[i], matchups_2025$batter[i])
  
  if (!is.null(features)) {
    pred_matrix <- xgb.DMatrix(data = as.matrix(features[, feature_cols]))
    pred <- predict(model, pred_matrix)
    
    baseline_runs <- 0.15
    positive_score <- baseline_runs + pred
    index_score <- (positive_score / baseline_runs) * 100
    score <- round(index_score)
    
    # === CALIBRATION ADJUSTMENTS ===
    # 1. Shift to center 100 at league average OPS (0.719)
    score <- score - 12
    
    # 2. Cross-league adjustment (±15 points)
    pitcher_id <- matchups_2025$pitcher[i]
    batter_id <- matchups_2025$batter[i]
    
    # Try 2025 first, fallback to 2024
    p_data <- pitcher_quality %>%
      filter(pitcher == pitcher_id, season == 2025)
    
    if (nrow(p_data) == 0) {
      p_data <- pitcher_quality %>%
        filter(pitcher == pitcher_id, season == 2024)
    }
    
    b_data <- batter_quality %>%
      filter(batter == batter_id, season == 2025)
    
    if (nrow(b_data) == 0) {
      b_data <- batter_quality %>%
        filter(batter == batter_id, season == 2024)
    }
    
    p_level <- "Unknown"
    if (nrow(p_data) > 0) {
      total_pa_p <- sum(p_data$pa)
      mlb_pa_p <- sum(p_data$pa[p_data$level == "MLB"])
      mlb_pct_p <- mlb_pa_p / total_pa_p
      
      if (mlb_pct_p >= 0.40) p_level <- "MLB"
      else if (mlb_pct_p < 0.25) p_level <- "AAA"
      else p_level <- "Both"
    }
    
    b_level <- "Unknown"
    if (nrow(b_data) > 0) {
      total_pa_b <- sum(b_data$pa)
      mlb_pa_b <- sum(b_data$pa[b_data$level == "MLB"])
      mlb_pct_b <- mlb_pa_b / total_pa_b
      
      if (mlb_pct_b >= 0.40) b_level <- "MLB"
      else if (mlb_pct_b < 0.25) b_level <- "AAA"
      else b_level <- "Both"
    }
    
    # Apply penalty if clear cross-league matchup
    if (p_level == "AAA" && b_level == "MLB") {
      score <- score + 15  # MLB batter advantage
    } else if (p_level == "MLB" && b_level == "AAA") {
      score <- score - 15  # MLB pitcher advantage
    }
    
    # 3. Clamp to 50-150
    score <- pmin(pmax(score, 50), 150)
    
    predictions <- rbind(predictions, data.frame(
      pitcher = matchups_2025$pitcher[i],
      batter = matchups_2025$batter[i],
      n_pa = matchups_2025$n_pa[i],
      hits = matchups_2025$hits[i],
      walks = matchups_2025$walks[i],
      at_bats = matchups_2025$at_bats[i],
      total_bases = matchups_2025$total_bases[i],
      actual_ops = matchups_2025$ops[i],
      predicted_score = score
    ))
  }
}
close(pb)

message(sprintf("\nGenerated predictions for %d matchups\n", nrow(predictions)))

# ===== 1. OVERALL CORRELATION (All matchups) =====
message("\n=== OVERALL PERFORMANCE ===")
cor_all <- cor(predictions$predicted_score, predictions$actual_ops)
message(sprintf("Correlation (all matchups): %.3f", cor_all))
message(sprintf("Mean predicted score: %.1f (target: 100)", mean(predictions$predicted_score)))
message(sprintf("SD predicted score: %.1f", sd(predictions$predicted_score)))

# ===== 2. CORRELATION BY PA THRESHOLD =====
message("\n=== Performance by PA Threshold ===")

pa_thresholds <- c(5, 10, 15, 20, 25, 30)
for (pa_min in pa_thresholds) {
  subset <- predictions %>% filter(n_pa >= pa_min)
  if (nrow(subset) >= 10) {
    cor_val <- cor(subset$predicted_score, subset$actual_ops)
    message(sprintf("PA >= %2d: N=%4d | Cor=%.3f", pa_min, nrow(subset), cor_val))
  }
}

# ===== 3. BUCKET VALIDATION (Score ranges vs OPS) =====
message("\n=== Bucket Validation ===")

predictions <- predictions %>%
  mutate(
    score_bucket = case_when(
      predicted_score < 85 ~ "< 85",
      predicted_score < 90 ~ "85-90",
      predicted_score < 95 ~ "90-95",
      predicted_score < 100 ~ "95-100",
      predicted_score < 105 ~ "100-105",
      predicted_score < 110 ~ "105-110",
      predicted_score < 115 ~ "110-115",
      predicted_score < 120 ~ "115-120",
      TRUE ~ "120+"
    )
  ) %>%
  mutate(score_bucket = factor(score_bucket, levels = c(
    "< 85", "85-90", "90-95", "95-100", "100-105", 
    "105-110", "110-115", "115-120", "120+"
  )))

validation_buckets <- predictions %>%
  group_by(score_bucket) %>%
  summarize(
    n_matchups = n(),
    total_pas = sum(n_pa),
    total_hits = sum(hits),
    total_walks = sum(walks),
    total_at_bats = sum(at_bats),
    total_bases = sum(total_bases),
    .groups = "drop"
  ) %>%
  mutate(
    obp = (total_hits + total_walks) / (total_at_bats + total_walks),
    slg = total_bases / total_at_bats,
    ops = obp + slg
  ) %>%
  arrange(score_bucket)

print(validation_buckets, n = 20)

# Bucket correlation
bucket_scores <- predictions %>%
  group_by(score_bucket) %>%
  summarize(avg_score = mean(predicted_score), .groups = "drop")

validation_with_scores <- validation_buckets %>%
  left_join(bucket_scores, by = "score_bucket")

bucket_cor <- cor(validation_with_scores$avg_score, validation_with_scores$ops)
message(sprintf("\nBucket correlation (avg score vs OPS): %.3f", bucket_cor))

# Save results
write.csv(validation_buckets, "validation_holdout_2025.csv", row.names = FALSE)
write.csv(predictions, "validation_holdout_all_matchups.csv", row.names = FALSE)

message("\n=== Files Saved ===")
message("✓ validation_holdout_2025.csv (bucket summary)")
message("✓ validation_holdout_all_matchups.csv (all predictions)")

message("\n=== Summary ===")
message(sprintf("Overall correlation: %.3f", cor_all))
message(sprintf("Bucket correlation: %.3f", bucket_cor))
message(sprintf("Mean score: %.1f (calibration check)", mean(predictions$predicted_score)))
message("\nScore < 100: Pitcher advantage (lower OPS)")
message("Score = 100: League average")  
message("Score > 100: Batter advantage (higher OPS)")