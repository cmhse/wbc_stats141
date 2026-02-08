# Player Quality Metrics
# Calculates overall performance for Log5 baseline

library(tidyverse)

message("Building player quality metrics\n")

statcast <- readRDS("data/statcast_data.rds")
message(sprintf("Loaded %s pitches", format(nrow(statcast), big.mark = ",")))
message(sprintf("Seasons: %s\n", paste(sort(unique(statcast$season)), collapse = ", ")))

# Pitcher quality
message("Computing pitcher quality...")

pitcher_quality <- statcast %>%
  filter(!is.na(delta_run_exp)) %>%
  group_by(pitcher, season, level) %>%
  summarize(
    pa = n_distinct(paste(game_pk, at_bat_number)),
    pitches = n(),
    delta_re_per_pa = mean(delta_run_exp, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  filter(pa >= 50) %>%
  mutate(
    pitcher_quality = 0.5 - (delta_re_per_pa * 2.5),
    pitcher_quality = pmax(0.2, pmin(0.8, pitcher_quality))
  )

message(sprintf("Created %s pitcher profiles", format(nrow(pitcher_quality), big.mark = ",")))
message(sprintf("  MLB: %s | AAA: %s", 
                format(sum(pitcher_quality$level == "MLB"), big.mark = ","),
                format(sum(pitcher_quality$level == "AAA"), big.mark = ",")))

# Batter quality
message("\nComputing batter quality...")

batter_quality <- statcast %>%
  filter(!is.na(delta_run_exp)) %>%
  group_by(batter, season, level) %>%
  summarize(
    pa = n_distinct(paste(game_pk, at_bat_number)),
    pitches = n(),
    delta_re_per_pa = mean(delta_run_exp, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  filter(pa >= 50) %>%
  mutate(
    batter_quality = 0.5 + (delta_re_per_pa * 2.5),
    batter_quality = pmax(0.2, pmin(0.8, batter_quality))
  )

message(sprintf("Created %s batter profiles", format(nrow(batter_quality), big.mark = ",")))
message(sprintf("  MLB: %s | AAA: %s\n", 
                format(sum(batter_quality$level == "MLB"), big.mark = ","),
                format(sum(batter_quality$level == "AAA"), big.mark = ",")))

# Top performers
message("Top pitchers (2024-2025 MLB):")
top_pitchers <- pitcher_quality %>%
  filter(season >= 2024, level == "MLB") %>%
  arrange(desc(pitcher_quality)) %>%
  head(10) %>%
  select(pitcher, season, pa, delta_re_per_pa, pitcher_quality)
print(top_pitchers)

message("\nTop batters (2024-2025 MLB):")
top_batters <- batter_quality %>%
  filter(season >= 2024, level == "MLB") %>%
  arrange(desc(batter_quality)) %>%
  head(10) %>%
  select(batter, season, pa, delta_re_per_pa, batter_quality)
print(top_batters)

# Save
if (!dir.exists("models")) dir.create("models")

saveRDS(pitcher_quality, "models/pitcher_quality.rds")
saveRDS(batter_quality, "models/batter_quality.rds")

message("\nSaved: models/pitcher_quality.rds")
message("Saved: models/batter_quality.rds")
message("\nNext: 04_build_features.R")
