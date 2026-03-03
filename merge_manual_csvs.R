# Merge Manually Downloaded CSVs
# Combines Baseball Savant CSVs with existing statcast_data.rds

library(tidyverse)
library(data.table)

cat("=== Merging Manual CSV Downloads ===\n\n")

# Create directory for manual downloads if it doesn't exist
if (!dir.exists("data/manual_downloads")) {
  dir.create("data/manual_downloads")
  cat("Created directory: data/manual_downloads/\n")
  cat("Place your downloaded CSVs here!\n\n")
  stop("Please download CSVs and place in data/manual_downloads/ then re-run this script")
}

# Load existing data
cat("Loading existing data...\n")
existing_data <- readRDS("data/statcast_data.rds")
cat(sprintf("Existing data: %s pitches\n\n", format(nrow(existing_data), big.mark = ",")))

# Find all CSV files in manual_downloads
csv_files <- list.files("data/manual_downloads", pattern = "\\.csv$", full.names = TRUE)

if (length(csv_files) == 0) {
  stop("No CSV files found in data/manual_downloads/\nPlease download and place CSV files there")
}

cat(sprintf("Found %d CSV files to merge:\n", length(csv_files)))
for (f in csv_files) {
  cat(sprintf("  - %s\n", basename(f)))
}
cat("\n")

# Load and combine all CSVs
cat("Loading CSV files...\n")
manual_data_list <- list()

for (csv_file in csv_files) {
  cat(sprintf("  Reading %s... ", basename(csv_file)))
  
  tryCatch({
    data <- fread(csv_file, showProgress = FALSE)
    cat(sprintf("%s pitches\n", format(nrow(data), big.mark = ",")))
    manual_data_list[[length(manual_data_list) + 1]] <- data
  }, error = function(e) {
    cat(sprintf("ERROR: %s\n", e$message))
  })
}

if (length(manual_data_list) == 0) {
  stop("Failed to load any CSV files")
}

# Combine all manual CSVs
cat("\nCombining manual CSVs...\n")
manual_data <- rbindlist(manual_data_list, fill = TRUE)
cat(sprintf("Total manual data: %s pitches\n\n", format(nrow(manual_data), big.mark = ",")))

# Standardize column names to match existing data
cat("Standardizing column names...\n")

# Baseball Savant CSVs might have slightly different column names
# Map common variations
column_mapping <- c(
  "player_name" = "batter_name",  # Sometimes it's player_name instead of batter/pitcher
  "pitch_type" = "pitch_type",
  "release_speed" = "release_speed",
  "game_date" = "game_date",
  "game_pk" = "game_pk"
)

# Check which columns we have
cat("Columns in manual CSV:\n")
cat(paste(names(manual_data)[1:min(20, ncol(manual_data))], collapse = ", "), "...\n\n")

# Add level and season if not present
if (!"level" %in% names(manual_data)) {
  cat("Adding 'level' column...\n")
  manual_data <- manual_data %>%
    mutate(
      level = case_when(
        game_pk >= 777000 & game_pk < 778000 ~ "AAA",
        game_pk >= 693000 & game_pk < 694000 ~ "AAA",
        game_pk >= 666000 & game_pk < 667000 ~ "AAA",
        game_pk >= 740000 & game_pk < 760000 ~ "MLB",
        game_pk >= 718000 & game_pk < 719000 ~ "MLB",
        TRUE ~ "AAA"  # Default to AAA for manual downloads
      )
    )
}

if (!"season" %in% names(manual_data)) {
  cat("Adding 'season' column from game_date...\n")
  manual_data <- manual_data %>%
    mutate(season = year(as.Date(game_date)))
}

# Add game_type if not present
if (!"game_type" %in% names(manual_data)) {
  manual_data$game_type <- "R"  # Assume regular season
}

# Select only columns that exist in both datasets
common_cols <- intersect(names(existing_data), names(manual_data))
cat(sprintf("Common columns: %d\n", length(common_cols)))
cat(sprintf("Keeping: %s\n\n", paste(head(common_cols, 10), collapse = ", ")))

# Subset both datasets to common columns
existing_subset <- existing_data %>% select(all_of(common_cols))
manual_subset <- manual_data %>% select(all_of(common_cols))

# Fix date format mismatch
cat("Converting date formats...\n")
if ("game_date" %in% names(existing_subset)) {
  existing_subset <- existing_subset %>%
    mutate(game_date = as.Date(game_date))
}
if ("game_date" %in% names(manual_subset)) {
  manual_subset <- manual_subset %>%
    mutate(game_date = as.Date(game_date))
}

# Remove duplicates BEFORE merging
cat("Checking for duplicates...\n")

# Identify duplicate rows (same game_pk, at_bat_number, pitcher, batter, pitch characteristics)
duplicate_key_cols <- c("game_pk", "at_bat_number", "pitcher", "batter", "pitch_type")
duplicate_key_cols <- intersect(duplicate_key_cols, common_cols)

cat(sprintf("Using columns for duplicate detection: %s\n", paste(duplicate_key_cols, collapse = ", ")))

# Mark which rows from manual data already exist in existing data
manual_subset <- manual_subset %>%
  mutate(row_id = row_number())

existing_keys <- existing_subset %>%
  select(all_of(duplicate_key_cols)) %>%
  distinct() %>%
  mutate(in_existing = TRUE)

manual_with_flag <- manual_subset %>%
  left_join(existing_keys, by = duplicate_key_cols, relationship = "many-to-many")

new_rows <- manual_with_flag %>%
  filter(is.na(in_existing)) %>%
  select(-in_existing, -row_id)

cat(sprintf("Manual data: %s pitches\n", format(nrow(manual_subset), big.mark = ",")))
cat(sprintf("Already in existing data: %s pitches\n", 
            format(nrow(manual_subset) - nrow(new_rows), big.mark = ",")))
cat(sprintf("New pitches to add: %s\n\n", format(nrow(new_rows), big.mark = ",")))

if (nrow(new_rows) == 0) {
  cat("No new data to add - all manual data already exists!\n")
  cat("Your existing data already has everything from the CSVs.\n")
  stop("Nothing to merge")
}

# Combine datasets
cat("Merging datasets...\n")
combined_data <- bind_rows(existing_subset, new_rows)

cat(sprintf("Combined data: %s pitches\n\n", format(nrow(combined_data), big.mark = ",")))

# Final deduplication pass (just in case)
cat("Final deduplication check...\n")
combined_data <- combined_data %>%
  distinct(across(all_of(duplicate_key_cols)), .keep_all = TRUE)

cat(sprintf("After final dedup: %s pitches\n\n", format(nrow(combined_data), big.mark = ",")))

# Add back any missing columns from original data with NA values
missing_cols <- setdiff(names(existing_data), names(combined_data))
if (length(missing_cols) > 0) {
  cat(sprintf("Adding %d missing columns with NA values...\n", length(missing_cols)))
  for (col in missing_cols) {
    combined_data[[col]] <- NA
  }
}

# Reorder columns to match original
combined_data <- combined_data %>% select(all_of(names(existing_data)))

# Summary
cat("\n=== SUMMARY ===\n")
cat(sprintf("Original data: %s pitches\n", format(nrow(existing_data), big.mark = ",")))
cat(sprintf("Added from CSVs: %s pitches\n", format(nrow(new_rows), big.mark = ",")))
cat(sprintf("Final data: %s pitches\n", format(nrow(combined_data), big.mark = ",")))

cat("\nBy level:\n")
print(combined_data %>% count(level, sort = TRUE))

cat("\nBy season:\n")
print(combined_data %>% count(season, sort = TRUE))

# Backup original
cat("\nBacking up original data...\n")
file.copy("data/statcast_data.rds", 
          sprintf("data/statcast_data_backup_%s.rds", format(Sys.Date(), "%Y%m%d")),
          overwrite = TRUE)
cat("✓ Backed up to: statcast_data_backup_YYYYMMDD.rds\n")

# Save merged data
cat("\nSaving merged data...\n")
saveRDS(combined_data, "data/statcast_data.rds")
cat("✓ Saved: data/statcast_data.rds\n")

cat("\n=== NEXT STEPS ===\n")
cat("1. Run: source('03_build_quality.R')\n")
cat("2. Run: source('04_build_features.R')\n")
cat("3. Run: source('05_train_model.R')\n")
cat("4. Run: source('rebuild_player_names_SIMPLE.R')\n")
cat("5. Run: source('check_roster_data.R') to verify\n")
cat("6. Restart your Shiny app!\n")