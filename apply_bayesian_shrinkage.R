# Apply Bayesian Shrinkage to Quality Scores
# Regresses small sample scores toward league mean

library(tidyverse)

message("=== Applying Bayesian Shrinkage to Quality Scores ===\n")

# Load quality data
pitcher_quality <- readRDS("models/pitcher_quality.rds")
batter_quality <- readRDS("models/batter_quality.rds")

# Calculate league means for 2025 (our credibility target)
league_means <- list(
  mlb_pitcher = mean(pitcher_quality$pitcher_quality[pitcher_quality$season == 2025 & 
                                                       pitcher_quality$level == "MLB" & 
                                                       pitcher_quality$pa >= 100]),
  aaa_pitcher = mean(pitcher_quality$pitcher_quality[pitcher_quality$season == 2025 & 
                                                       pitcher_quality$level == "AAA" & 
                                                       pitcher_quality$pa >= 100]),
  mlb_batter = mean(batter_quality$batter_quality[batter_quality$season == 2025 & 
                                                     batter_quality$level == "MLB" & 
                                                     batter_quality$pa >= 100]),
  aaa_batter = mean(batter_quality$batter_quality[batter_quality$season == 2025 & 
                                                     batter_quality$level == "AAA" & 
                                                     batter_quality$pa >= 100])
)

message("League means (players with 100+ PAs):")
message(sprintf("  MLB pitchers: %.4f", league_means$mlb_pitcher))
message(sprintf("  AAA pitchers: %.4f", league_means$aaa_pitcher))
message(sprintf("  MLB batters: %.4f", league_means$mlb_batter))
message(sprintf("  AAA batters: %.4f\n", league_means$aaa_batter))

# Shrinkage function
# quality_shrunk = (pa * quality + K * league_mean) / (pa + K)
# where K = credibility constant (PAs needed for full credibility)

apply_shrinkage <- function(quality, pa, league_mean, K = 120) {
  shrunk <- (pa * quality + K * league_mean) / (pa + K)
  return(shrunk)
}

# Apply to pitchers
pitcher_quality_shrunk <- pitcher_quality %>%
  mutate(
    league_mean = case_when(
      level == "MLB" ~ league_means$mlb_pitcher,
      level == "AAA" ~ league_means$aaa_pitcher,
      TRUE ~ 0.5
    ),
    pitcher_quality_original = pitcher_quality,
    pitcher_quality = apply_shrinkage(pitcher_quality, pa, league_mean, K = 120),
    shrinkage_amount = pitcher_quality_original - pitcher_quality
  )

# Apply to batters
batter_quality_shrunk <- batter_quality %>%
  mutate(
    league_mean = case_when(
      level == "MLB" ~ league_means$mlb_batter,
      level == "AAA" ~ league_means$aaa_batter,
      TRUE ~ 0.5
    ),
    batter_quality_original = batter_quality,
    batter_quality = apply_shrinkage(batter_quality, pa, league_mean, K = 120),
    shrinkage_amount = batter_quality_original - batter_quality
  )

# Show examples of shrinkage
message("=== Shrinkage Examples (K = 120 PAs) ===\n")

message("Small sample AAA hitters (original → shrunk):")
examples_small <- batter_quality_shrunk %>%
  filter(season == 2025, level == "AAA", pa < 100, batter_quality_original > 0.55) %>%
  arrange(desc(batter_quality_original)) %>%
  head(10)

print(examples_small %>% 
        select(batter, pa, batter_quality_original, batter_quality, shrinkage_amount))

message("\nLarge sample AAA hitters (barely shrunk):")
examples_large <- batter_quality_shrunk %>%
  filter(season == 2025, level == "AAA", pa >= 200, batter_quality_original > 0.55) %>%
  arrange(desc(batter_quality_original)) %>%
  head(5)

print(examples_large %>% 
        select(batter, pa, batter_quality_original, batter_quality, shrinkage_amount))

# Check Nick Morabito specifically
morabito <- batter_quality_shrunk %>%
  filter(batter == 703492)

if (nrow(morabito) > 0) {
  message("\nNick Morabito shrinkage:")
  print(morabito %>% 
          select(season, level, pa, batter_quality_original, batter_quality, shrinkage_amount))
}

# Summary statistics
message("\n=== Impact Summary ===\n")

pitcher_impact <- pitcher_quality_shrunk %>%
  filter(season == 2025) %>%
  group_by(level, pa_bucket = cut(pa, breaks = c(0, 50, 100, 200, Inf), 
                                    labels = c("30-49", "50-99", "100-199", "200+"))) %>%
  summarize(
    n = n(),
    avg_shrinkage = mean(abs(shrinkage_amount)),
    max_shrinkage = max(abs(shrinkage_amount)),
    .groups = "drop"
  )

message("Pitcher shrinkage by sample size:")
print(pitcher_impact)

batter_impact <- batter_quality_shrunk %>%
  filter(season == 2025) %>%
  group_by(level, pa_bucket = cut(pa, breaks = c(0, 50, 100, 200, Inf), 
                                    labels = c("30-49", "50-99", "100-199", "200+"))) %>%
  summarize(
    n = n(),
    avg_shrinkage = mean(abs(shrinkage_amount)),
    max_shrinkage = max(abs(shrinkage_amount)),
    .groups = "drop"
  )

message("\nBatter shrinkage by sample size:")
print(batter_impact)

# Save shrunk quality files
pitcher_quality_final <- pitcher_quality_shrunk %>%
  select(-league_mean, -pitcher_quality_original, -shrinkage_amount)

batter_quality_final <- batter_quality_shrunk %>%
  select(-league_mean, -batter_quality_original, -shrinkage_amount)

saveRDS(pitcher_quality_final, "models/pitcher_quality.rds")
saveRDS(batter_quality_final, "models/batter_quality.rds")

message("\n✓ Saved shrunk quality scores")
message("✓ Small sample extremes have been regressed toward league mean")
message("✓ Large sample players barely affected")

message("\n=== Next Steps ===")
message("1. Rebuild features: source('04_build_features.R')")
message("2. Restart your app")
message("3. Test Nick Morabito - should no longer grade at 150!")
