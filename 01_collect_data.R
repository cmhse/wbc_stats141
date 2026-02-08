# Statcast Data Collection
# Downloads MLB (2020-2025) and AAA (2023-2025) pitch-level data

library(tidyverse)
library(data.table)

if (!dir.exists("data")) dir.create("data")
if (!dir.exists("data/raw")) dir.create("data/raw")

message("Collecting Statcast data\n")

# Download function with retry
pull_statcast_data <- function(start_date, end_date, max_retries = 3) {
  cat(sprintf("%s to %s... ", start_date, end_date))
  
  for (attempt in 1:max_retries) {
    result <- tryCatch({
      url <- paste0(
        "https://baseballsavant.mlb.com/statcast_search/csv?",
        "all=true&hfPT=&hfAB=&hfGT=R%7C&hfPR=&hfZ=&hfBBT=&hfBBL=&",
        "hfNewZones=&hfC=&hfSea=", format(as.Date(start_date), "%Y"), "%7C",
        "&hfSit=&player_type=batter&hfOuts=&opponent=&pitcher_throws=&",
        "batter_stands=&hfSA=&hfInfield=&team=&position=&hfOutfield=&",
        "hfRO=&home_road=&hfFlag=&hfPull=&metric_1=&hfInn=&min_pitches=0&",
        "min_results=0&group_by=name&sort_col=pitches&",
        "player_event_sort=api_p_release_speed&sort_order=desc&min_pas=0&",
        "game_date_gt=", start_date, "&game_date_lt=", end_date, "&type=details"
      )
      
      data <- fread(url, showProgress = FALSE)
      data$download_date <- Sys.Date()
      cat(sprintf("(%s pitches)\n", format(nrow(data), big.mark = ",")))
      return(data)
      
    }, error = function(e) {
      if (attempt < max_retries) {
        cat(sprintf("retry %d... ", attempt))
        Sys.sleep(5)
        return(NULL)
      } else {
        stop(sprintf("Failed: %s", e$message))
      }
    })
    
    if (!is.null(result)) return(result)
  }
}

# Split into 3-day chunks (stays under 25K pitch limit)
create_chunks <- function(start_date, end_date, days = 3) {
  start <- as.Date(start_date)
  end <- as.Date(end_date)
  chunks <- list()
  current <- start
  
  while (current < end) {
    chunk_end <- min(current + days - 1, end)
    chunks[[length(chunks) + 1]] <- list(start = as.character(current), end = as.character(chunk_end))
    current <- chunk_end + 1
  }
  return(chunks)
}

# MLB seasons
message("\nMLB Data")
mlb_seasons <- list(
  list(start = "2020-07-23", end = "2020-09-28", year = 2020),
  list(start = "2021-04-01", end = "2021-10-04", year = 2021),
  list(start = "2022-04-07", end = "2022-10-06", year = 2022),
  list(start = "2023-03-30", end = "2023-10-02", year = 2023),
  list(start = "2024-03-20", end = "2024-09-30", year = 2024),
  list(start = "2025-03-27", end = "2025-09-30", year = 2025)
)

mlb_data_list <- list()

for (s in mlb_seasons) {
  message(sprintf("\n%d", s$year))
  chunks <- create_chunks(s$start, s$end)
  season_data <- list()
  
  for (i in seq_along(chunks)) {
    cat(sprintf("[%d/%d] ", i, length(chunks)))
    data <- pull_statcast_data(chunks[[i]]$start, chunks[[i]]$end)
    data$level <- "MLB"
    data$season <- s$year
    season_data[[i]] <- data
    if (i < length(chunks)) Sys.sleep(2)
  }
  
  combined <- rbindlist(season_data, fill = TRUE)
  mlb_data_list[[as.character(s$year)]] <- combined
  saveRDS(combined, sprintf("data/raw/mlb_%d.rds", s$year))
  message(sprintf("Complete: %s pitches", format(nrow(combined), big.mark = ",")))
}

# AAA seasons
message("\n\nAAA Data")
aaa_seasons <- list(
  list(start = "2023-03-31", end = "2023-09-25", year = 2023),
  list(start = "2024-03-29", end = "2024-09-23", year = 2024),
  list(start = "2025-03-28", end = "2025-09-30", year = 2025)
)

aaa_data_list <- list()

for (s in aaa_seasons) {
  message(sprintf("\n%d", s$year))
  chunks <- create_chunks(s$start, s$end)
  season_data <- list()
  
  for (i in seq_along(chunks)) {
    cat(sprintf("[%d/%d] ", i, length(chunks)))
    data <- pull_statcast_data(chunks[[i]]$start, chunks[[i]]$end)
    data$level <- "AAA"
    data$season <- s$year
    season_data[[i]] <- data
    if (i < length(chunks)) Sys.sleep(2)
  }
  
  combined <- rbindlist(season_data, fill = TRUE)
  aaa_data_list[[as.character(s$year)]] <- combined
  saveRDS(combined, sprintf("data/raw/aaa_%d.rds", s$year))
  message(sprintf("Complete: %s pitches", format(nrow(combined), big.mark = ",")))
}

# Combine
all_data <- rbindlist(c(mlb_data_list, aaa_data_list), fill = TRUE)

message(sprintf("\n\nTotal: %s pitches", format(nrow(all_data), big.mark = ",")))
message(sprintf("MLB: %s | AAA: %s", 
                format(sum(all_data$level == "MLB"), big.mark = ","),
                format(sum(all_data$level == "AAA"), big.mark = ",")))

saveRDS(all_data, "data/statcast_raw.rds")
message("\nSaved: data/statcast_raw.rds")
message("Next: 02_process_data.R")
