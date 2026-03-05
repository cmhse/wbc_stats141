# Rebuild player_names.csv from actual statcast data
# Simple version that works with any column structure

library(tidyverse)
library(baseballr)

cat("=== Rebuilding player_names.csv ===\n\n")

# Load statcast data
statcast <- readRDS("data/statcast_data.rds")

cat("Loading data...\n")
cat("Total pitches:", format(nrow(statcast), big.mark = ","), "\n\n")

cat("Available columns:\n")
print(names(statcast))
cat("\n")

# Get all unique pitcher IDs
cat("Extracting unique pitcher IDs...\n")
pitcher_ids <- statcast %>%
  filter(!is.na(pitcher)) %>%
  pull(pitcher) %>%
  unique()

cat("  Found", format(length(pitcher_ids), big.mark = ","), "unique pitcher IDs\n")

# Get all unique batter IDs
cat("Extracting unique batter IDs...\n")
batter_ids <- statcast %>%
  filter(!is.na(batter)) %>%
  pull(batter) %>%
  unique()

cat("  Found", format(length(batter_ids), big.mark = ","), "unique batter IDs\n")

# Combine all IDs
cat("\nCombining all unique player IDs...\n")
all_ids <- unique(c(pitcher_ids, batter_ids))
cat("  Total unique player IDs:", format(length(all_ids), big.mark = ","), "\n\n")

# Get names from Chadwick Bureau
cat("Downloading Chadwick Bureau player registry...\n")
cat("(This downloads once and may take 30-60 seconds)\n")

chadwick <- chadwick_player_lu()

cat("  ✓ Downloaded -", format(nrow(chadwick), big.mark = ","), "players in registry\n\n")

# Match our IDs to Chadwick
cat("Matching player IDs to names...\n")

# Create dataframe from our IDs
our_players <- data.frame(id = all_ids, stringsAsFactors = FALSE)

# Join with Chadwick
all_players <- our_players %>%
  left_join(
    chadwick %>% 
      select(id = key_mlbam, 
             name_first, 
             name_last) %>%
      filter(!is.na(id)),
    by = "id"
  ) %>%
  mutate(
    name = ifelse(is.na(name_first), 
                  paste("Player", id),
                  paste(name_first, name_last))
  ) %>%
  select(id, name) %>%
  arrange(name)

# Check how many matched
matched <- sum(!grepl("^Player ", all_players$name))
cat("  ✓ Matched", format(matched, big.mark = ","), "players to names\n")
unmatched <- nrow(all_players) - matched
if (unmatched > 0) {
  cat("  ✗", format(unmatched, big.mark = ","), "players without names (will show as 'Player ID')\n")
}
cat("\n")

# Check for the previously missing players
cat("Checking for previously missing players:\n")
missing_names <- c("Carrillo", "DeLucia", "Armenta", "Gastelum", 
                   "Mineo", "Antonacci", "Fischer", "Morabito", 
                   "Serna", "Ornelas")

for (n in missing_names) {
  matches <- all_players %>% 
    filter(grepl(n, name, ignore.case = TRUE))
  
  if (nrow(matches) > 0) {
    cat("  ✓", n, "- Found", nrow(matches), "match(es):\n")
    for (i in 1:min(nrow(matches), 3)) {
      cat("      ", matches$name[i], "(ID:", matches$id[i], ")\n")
    }
  } else {
    cat("  ✗", n, "- NOT FOUND\n")
  }
}

# Backup old file if it exists
if (file.exists("player_names.csv")) {
  old_count <- nrow(read_csv("player_names.csv", show_col_types = FALSE))
  file.copy("player_names.csv", "player_names_OLD.csv", overwrite = TRUE)
  cat("\n✓ Backed up old player_names.csv to player_names_OLD.csv\n")
  cat("  Old file had", format(old_count, big.mark = ","), "players\n")
}

# Save the new file
cat("\nSaving to player_names.csv...\n")
write_csv(all_players, "player_names.csv")

cat("✓ Done!\n\n")
cat("New player_names.csv has", format(nrow(all_players), big.mark = ","), "players\n")

cat("\n=== NEXT STEPS ===\n")
cat("1. Run: source('check_roster_data.R') to verify players are found\n")
cat("2. Restart your Shiny app\n")
cat("3. Missing players should now appear in the app!\n")
