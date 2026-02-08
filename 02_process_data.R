# Data Processing
# Cleans raw Statcast data and calculates delta_run_exp

library(tidyverse)
library(data.table)

message("Processing Statcast data\n")

raw_data <- readRDS("data/statcast_raw.rds")
message(sprintf("Loaded %s pitches\n", format(nrow(raw_data), big.mark = ",")))

# Find delta_run_exp column (might have different names)
message("Checking for run expectancy column...")
run_exp_col <- NULL
possible_names <- c("delta_run_exp", "delta_run_exp_bat", "run_exp_change", "delta_home_run_exp")

for (col_name in possible_names) {
  if (col_name %in% names(raw_data)) {
    run_exp_col <- col_name
    message(sprintf("Found: %s", col_name))
    break
  }
}

if (is.null(run_exp_col)) {
  message("No run expectancy column found, creating from events")
}

# Filter and clean
statcast <- as_tibble(raw_data) %>%
  filter(game_type == "R" | is.na(game_type))

# Handle delta_run_exp
if (!is.null(run_exp_col)) {
  statcast <- statcast %>%
    mutate(delta_run_exp = as.numeric(.data[[run_exp_col]]))
} else {
  statcast <- statcast %>%
    mutate(
      delta_run_exp = case_when(
        events == "home_run" ~ 1.4,
        events == "triple" ~ 1.0,
        events == "double" ~ 0.7,
        events == "single" ~ 0.45,
        events == "walk" ~ 0.3,
        events == "hit_by_pitch" ~ 0.3,
        events %in% c("strikeout", "strikeout_double_play") ~ -0.3,
        events %in% c("field_out", "force_out", "grounded_into_double_play") ~ -0.25,
        TRUE ~ 0
      )
    )
}

# Select relevant columns
statcast <- statcast %>%
  select(
    game_pk, game_date, pitcher, batter, at_bat_number,
    any_of(c("inning", "inning_topbot", "outs_when_up", "balls", "strikes",
             "on_3b", "on_2b", "on_1b")),
    any_of(c("pitch_type", "pitch_name", "release_speed", "release_spin_rate",
             "release_extension", "release_pos_x", "release_pos_y", "release_pos_z")),
    any_of(c("pfx_x", "pfx_z", "plate_x", "plate_z")),
    any_of(c("description", "events", "type", "bb_type")),
    any_of(c("launch_speed", "launch_angle", "launch_speed_angle")),
    any_of(c("zone", "stand", "p_throws")),
    delta_run_exp,
    any_of(c("level", "season", "game_type"))
  ) %>%
  mutate(delta_run_exp = if_else(is.na(delta_run_exp), 0, as.numeric(delta_run_exp))) %>%
  filter(!is.na(pitch_type), pitch_type != "")

message(sprintf("Processed: %s pitches\n", format(nrow(statcast), big.mark = ",")))

# Add calculated fields
statcast <- statcast %>%
  mutate(
    p_throws = if ("p_throws" %in% names(.)) toupper(p_throws) else NA_character_,
    stand = if ("stand" %in% names(.)) toupper(stand) else NA_character_,
    ab_id = paste(game_pk, at_bat_number, sep = "_"),
    base_state = if (all(c("on_3b", "on_2b", "on_1b") %in% names(.))) {
      case_when(
        !is.na(on_3b) & !is.na(on_2b) & !is.na(on_1b) ~ "123",
        !is.na(on_3b) & !is.na(on_2b) ~ "23",
        !is.na(on_3b) & !is.na(on_1b) ~ "13",
        !is.na(on_2b) & !is.na(on_1b) ~ "12",
        !is.na(on_3b) ~ "3",
        !is.na(on_2b) ~ "2",
        !is.na(on_1b) ~ "1",
        TRUE ~ "empty"
      )
    } else {
      NA_character_
    },
    level = if ("level" %in% names(.)) if_else(is.na(level), "MLB", level) else "MLB"
  )

# Summary
message("\nData Summary")
message(sprintf("Pitches: %s", format(nrow(statcast), big.mark = ",")))
message(sprintf("Pitchers: %s", format(n_distinct(statcast$pitcher), big.mark = ",")))
message(sprintf("Batters: %s", format(n_distinct(statcast$batter), big.mark = ",")))
message(sprintf("Games: %s", format(n_distinct(statcast$game_pk), big.mark = ",")))

if ("season" %in% names(statcast)) {
  message(sprintf("Seasons: %s", paste(sort(unique(statcast$season)), collapse = ", ")))
}

if ("level" %in% names(statcast)) {
  message("\nBy level:")
  print(statcast %>% count(level, sort = TRUE))
}

if (all(c("season", "level") %in% names(statcast))) {
  message("\nBy season and level:")
  print(statcast %>% 
          count(season, level) %>% 
          pivot_wider(names_from = level, values_from = n, values_fill = 0))
}

message("\nTop pitch types:")
print(statcast %>% count(pitch_type, sort = TRUE) %>% head(10))

# Save
saveRDS(statcast, "data/statcast_data.rds")
message(sprintf("\nSaved: data/statcast_data.rds (%.1f MB)", 
                file.size("data/statcast_data.rds") / 1024^2))

if ("season" %in% names(statcast)) {
  for (yr in unique(statcast$season)) {
    season_data <- statcast %>% filter(season == yr)
    saveRDS(season_data, sprintf("data/statcast_%d.rds", yr))
    message(sprintf("  %d: %s pitches", yr, format(nrow(season_data), big.mark = ",")))
  }
}

message("\nNext: 03_build_quality.R")
