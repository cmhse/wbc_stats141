# Feature Engineering
# Builds pitcher and batter features with handedness splits

library(tidyverse)

message("Building matchup features\n")

statcast <- readRDS("data/statcast_data.rds")
pitcher_quality <- readRDS("models/pitcher_quality.rds")
batter_quality <- readRDS("models/batter_quality.rds")

message(sprintf("Loaded %s pitches (%d-%d)\n",
                format(nrow(statcast), big.mark = ","),
                min(statcast$season), max(statcast$season)))

# Pitcher features by handedness
message("Computing pitcher features...")

pitcher_features <- statcast %>%
  group_by(pitcher, season, p_throws, stand, level) %>%
  summarize(
    pa_vs_hand = n_distinct(paste(game_pk, at_bat_number)),
    
    # Results
    delta_re_vs_hand = mean(delta_run_exp, na.rm = TRUE),
    k_rate_vs_hand = sum(events %in% c("strikeout", "strikeout_double_play"), na.rm = TRUE) / pa_vs_hand,
    
    # Arsenal
    ff_pct = mean(pitch_type == "FF", na.rm = TRUE),
    si_pct = mean(pitch_type == "SI", na.rm = TRUE),
    fc_pct = mean(pitch_type == "FC", na.rm = TRUE),
    sl_pct = mean(pitch_type == "SL", na.rm = TRUE),
    st_pct = mean(pitch_type == "ST", na.rm = TRUE),
    cu_pct = mean(pitch_type == "CU", na.rm = TRUE),
    kc_pct = mean(pitch_type == "KC", na.rm = TRUE),
    ch_pct = mean(pitch_type == "CH", na.rm = TRUE),
    fs_pct = mean(pitch_type == "FS", na.rm = TRUE),
    
    # Run values
    ff_delta_re = mean(delta_run_exp[pitch_type == "FF"], na.rm = TRUE),
    si_delta_re = mean(delta_run_exp[pitch_type == "SI"], na.rm = TRUE),
    fc_delta_re = mean(delta_run_exp[pitch_type == "FC"], na.rm = TRUE),
    sl_delta_re = mean(delta_run_exp[pitch_type == "SL"], na.rm = TRUE),
    st_delta_re = mean(delta_run_exp[pitch_type == "ST"], na.rm = TRUE),
    cu_delta_re = mean(delta_run_exp[pitch_type == "CU"], na.rm = TRUE),
    kc_delta_re = mean(delta_run_exp[pitch_type == "KC"], na.rm = TRUE),
    ch_delta_re = mean(delta_run_exp[pitch_type == "CH"], na.rm = TRUE),
    fs_delta_re = mean(delta_run_exp[pitch_type == "FS"], na.rm = TRUE),
    
    # Whiff rates
    ff_whiff_rate = sum(description %in% c("swinging_strike", "swinging_strike_blocked") & pitch_type == "FF", na.rm = TRUE) / 
      sum(pitch_type == "FF", na.rm = TRUE),
    si_whiff_rate = sum(description %in% c("swinging_strike", "swinging_strike_blocked") & pitch_type == "SI", na.rm = TRUE) / 
      sum(pitch_type == "SI", na.rm = TRUE),
    fc_whiff_rate = sum(description %in% c("swinging_strike", "swinging_strike_blocked") & pitch_type == "FC", na.rm = TRUE) / 
      sum(pitch_type == "FC", na.rm = TRUE),
    sl_whiff_rate = sum(description %in% c("swinging_strike", "swinging_strike_blocked") & pitch_type == "SL", na.rm = TRUE) / 
      sum(pitch_type == "SL", na.rm = TRUE),
    st_whiff_rate = sum(description %in% c("swinging_strike", "swinging_strike_blocked") & pitch_type == "ST", na.rm = TRUE) / 
      sum(pitch_type == "ST", na.rm = TRUE),
    cu_whiff_rate = sum(description %in% c("swinging_strike", "swinging_strike_blocked") & pitch_type == "CU", na.rm = TRUE) / 
      sum(pitch_type == "CU", na.rm = TRUE),
    kc_whiff_rate = sum(description %in% c("swinging_strike", "swinging_strike_blocked") & pitch_type == "KC", na.rm = TRUE) / 
      sum(pitch_type == "KC", na.rm = TRUE),
    ch_whiff_rate = sum(description %in% c("swinging_strike", "swinging_strike_blocked") & pitch_type == "CH", na.rm = TRUE) / 
      sum(pitch_type == "CH", na.rm = TRUE),
    fs_whiff_rate = sum(description %in% c("swinging_strike", "swinging_strike_blocked") & pitch_type == "FS", na.rm = TRUE) / 
      sum(pitch_type == "FS", na.rm = TRUE),
    
    # Velocity
    ff_velo = mean(release_speed[pitch_type == "FF"], na.rm = TRUE),
    si_velo = mean(release_speed[pitch_type == "SI"], na.rm = TRUE),
    fc_velo = mean(release_speed[pitch_type == "FC"], na.rm = TRUE),
    sl_velo = mean(release_speed[pitch_type == "SL"], na.rm = TRUE),
    st_velo = mean(release_speed[pitch_type == "ST"], na.rm = TRUE),
    cu_velo = mean(release_speed[pitch_type == "CU"], na.rm = TRUE),
    kc_velo = mean(release_speed[pitch_type == "KC"], na.rm = TRUE),
    ch_velo = mean(release_speed[pitch_type == "CH"], na.rm = TRUE),
    fs_velo = mean(release_speed[pitch_type == "FS"], na.rm = TRUE),
    
    # Spin
    ff_spin = mean(release_spin_rate[pitch_type == "FF"], na.rm = TRUE),
    si_spin = mean(release_spin_rate[pitch_type == "SI"], na.rm = TRUE),
    fc_spin = mean(release_spin_rate[pitch_type == "FC"], na.rm = TRUE),
    sl_spin = mean(release_spin_rate[pitch_type == "SL"], na.rm = TRUE),
    st_spin = mean(release_spin_rate[pitch_type == "ST"], na.rm = TRUE),
    cu_spin = mean(release_spin_rate[pitch_type == "CU"], na.rm = TRUE),
    kc_spin = mean(release_spin_rate[pitch_type == "KC"], na.rm = TRUE),
    ch_spin = mean(release_spin_rate[pitch_type == "CH"], na.rm = TRUE),
    fs_spin = mean(release_spin_rate[pitch_type == "FS"], na.rm = TRUE),
    
    # Vertical break
    ff_vbreak = mean(pfx_z[pitch_type == "FF"], na.rm = TRUE),
    si_vbreak = mean(pfx_z[pitch_type == "SI"], na.rm = TRUE),
    fc_vbreak = mean(pfx_z[pitch_type == "FC"], na.rm = TRUE),
    sl_vbreak = mean(pfx_z[pitch_type == "SL"], na.rm = TRUE),
    st_vbreak = mean(pfx_z[pitch_type == "ST"], na.rm = TRUE),
    cu_vbreak = mean(pfx_z[pitch_type == "CU"], na.rm = TRUE),
    kc_vbreak = mean(pfx_z[pitch_type == "KC"], na.rm = TRUE),
    ch_vbreak = mean(pfx_z[pitch_type == "CH"], na.rm = TRUE),
    fs_vbreak = mean(pfx_z[pitch_type == "FS"], na.rm = TRUE),
    
    # Horizontal break
    ff_hbreak = mean(pfx_x[pitch_type == "FF"], na.rm = TRUE),
    si_hbreak = mean(pfx_x[pitch_type == "SI"], na.rm = TRUE),
    fc_hbreak = mean(pfx_x[pitch_type == "FC"], na.rm = TRUE),
    sl_hbreak = mean(pfx_x[pitch_type == "SL"], na.rm = TRUE),
    st_hbreak = mean(pfx_x[pitch_type == "ST"], na.rm = TRUE),
    cu_hbreak = mean(pfx_x[pitch_type == "CU"], na.rm = TRUE),
    kc_hbreak = mean(pfx_x[pitch_type == "KC"], na.rm = TRUE),
    ch_hbreak = mean(pfx_x[pitch_type == "CH"], na.rm = TRUE),
    fs_hbreak = mean(pfx_x[pitch_type == "FS"], na.rm = TRUE),
    
    # Release
    extension = mean(release_extension, na.rm = TRUE),
    release_height = mean(release_pos_z, na.rm = TRUE),
    release_side = mean(release_pos_x, na.rm = TRUE),
    
    .groups = "drop"
  ) %>%
  filter(pa_vs_hand >= 20)

message(sprintf("Created %s pitcher profiles", format(nrow(pitcher_features), big.mark = ",")))
message(sprintf("  MLB: %s | AAA: %s", 
                format(sum(pitcher_features$level == "MLB"), big.mark = ","),
                format(sum(pitcher_features$level == "AAA"), big.mark = ",")))

# Batter features by handedness
message("\nComputing batter features...")

batter_features <- statcast %>%
  group_by(batter, season, stand, p_throws, level) %>%
  summarize(
    pa_vs_hand = n_distinct(paste(game_pk, at_bat_number)),
    
    # Results
    delta_re_vs_hand = mean(delta_run_exp, na.rm = TRUE),
    k_rate_vs_hand = sum(events %in% c("strikeout", "strikeout_double_play"), na.rm = TRUE) / pa_vs_hand,
    
    # Run values by pitch
    ff_delta_re = mean(delta_run_exp[pitch_type == "FF"], na.rm = TRUE),
    si_delta_re = mean(delta_run_exp[pitch_type == "SI"], na.rm = TRUE),
    fc_delta_re = mean(delta_run_exp[pitch_type == "FC"], na.rm = TRUE),
    sl_delta_re = mean(delta_run_exp[pitch_type == "SL"], na.rm = TRUE),
    st_delta_re = mean(delta_run_exp[pitch_type == "ST"], na.rm = TRUE),
    cu_delta_re = mean(delta_run_exp[pitch_type == "CU"], na.rm = TRUE),
    kc_delta_re = mean(delta_run_exp[pitch_type == "KC"], na.rm = TRUE),
    ch_delta_re = mean(delta_run_exp[pitch_type == "CH"], na.rm = TRUE),
    fs_delta_re = mean(delta_run_exp[pitch_type == "FS"], na.rm = TRUE),
    
    # Whiff rates by pitch
    ff_whiff_rate = sum(description %in% c("swinging_strike", "swinging_strike_blocked") & pitch_type == "FF", na.rm = TRUE) / 
      sum(pitch_type == "FF" & !is.na(description), na.rm = TRUE),
    si_whiff_rate = sum(description %in% c("swinging_strike", "swinging_strike_blocked") & pitch_type == "SI", na.rm = TRUE) / 
      sum(pitch_type == "SI" & !is.na(description), na.rm = TRUE),
    fc_whiff_rate = sum(description %in% c("swinging_strike", "swinging_strike_blocked") & pitch_type == "FC", na.rm = TRUE) / 
      sum(pitch_type == "FC" & !is.na(description), na.rm = TRUE),
    sl_whiff_rate = sum(description %in% c("swinging_strike", "swinging_strike_blocked") & pitch_type == "SL", na.rm = TRUE) / 
      sum(pitch_type == "SL" & !is.na(description), na.rm = TRUE),
    st_whiff_rate = sum(description %in% c("swinging_strike", "swinging_strike_blocked") & pitch_type == "ST", na.rm = TRUE) / 
      sum(pitch_type == "ST" & !is.na(description), na.rm = TRUE),
    cu_whiff_rate = sum(description %in% c("swinging_strike", "swinging_strike_blocked") & pitch_type == "CU", na.rm = TRUE) / 
      sum(pitch_type == "CU" & !is.na(description), na.rm = TRUE),
    kc_whiff_rate = sum(description %in% c("swinging_strike", "swinging_strike_blocked") & pitch_type == "KC", na.rm = TRUE) / 
      sum(pitch_type == "KC" & !is.na(description), na.rm = TRUE),
    ch_whiff_rate = sum(description %in% c("swinging_strike", "swinging_strike_blocked") & pitch_type == "CH", na.rm = TRUE) / 
      sum(pitch_type == "CH" & !is.na(description), na.rm = TRUE),
    fs_whiff_rate = sum(description %in% c("swinging_strike", "swinging_strike_blocked") & pitch_type == "FS", na.rm = TRUE) / 
      sum(pitch_type == "FS" & !is.na(description), na.rm = TRUE),
    
    # Plate discipline
    bb_rate_vs_hand = sum(events == "walk", na.rm = TRUE) / pa_vs_hand,
    chase_rate = sum(type == "S" & description != "called_strike" & 
                       !is.na(zone) & zone > 9, na.rm = TRUE) /
      sum(!is.na(zone) & zone > 9, na.rm = TRUE),
    zone_contact_rate = sum(description %in% c("foul", "hit_into_play") & !is.na(zone) & zone <= 9, na.rm = TRUE) /
      sum(!is.na(zone) & zone <= 9, na.rm = TRUE),
    
    # Contact quality
    avg_exit_velo = mean(launch_speed[!is.na(launch_speed)], na.rm = TRUE),
    max_exit_velo = max(launch_speed[!is.na(launch_speed)], na.rm = TRUE),
    hard_hit_rate = sum(launch_speed >= 95, na.rm = TRUE) / sum(!is.na(launch_speed), na.rm = TRUE),
    barrel_rate = sum(launch_speed_angle %in% 5:6, na.rm = TRUE) / sum(!is.na(launch_speed_angle), na.rm = TRUE),
    avg_launch_angle = mean(launch_angle[!is.na(launch_angle)], na.rm = TRUE),
    
    .groups = "drop"
  ) %>%
  filter(pa_vs_hand >= 20)

message(sprintf("Created %s batter profiles", format(nrow(batter_features), big.mark = ",")))
message(sprintf("  MLB: %s | AAA: %s\n", 
                format(sum(batter_features$level == "MLB"), big.mark = ","),
                format(sum(batter_features$level == "AAA"), big.mark = ",")))

# Save
saveRDS(pitcher_features, "models/pitcher_features.rds")
saveRDS(batter_features, "models/batter_features.rds")

message("Saved: models/pitcher_features.rds")
message("Saved: models/batter_features.rds")
message(sprintf("\nPitcher features: %d per profile", ncol(pitcher_features) - 5))
message(sprintf("Batter features: %d per profile", ncol(batter_features) - 5))
message("\nNext: 05_train_model.R")