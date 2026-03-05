# Matchup Sheet Generator - World Baseball Classic
# Interactive tool for Italy vs opponents

library(shiny)
library(tidyverse)
library(DT)
library(xgboost)
library(openxlsx)  # For Excel with conditional formatting

# Custom CSS for professional WBC/Italia theme
custom_css <- HTML("
<style>
/* Italia/WBC Color Scheme */
body {
  background: linear-gradient(135deg, #F5F7FA 0%, #E8EDF2 100%);
  font-family: 'Segoe UI', Tahoma, Geneva, Verdana, sans-serif;
}

.container-fluid {
  background: white;
  border-radius: 15px;
  box-shadow: 0 8px 32px rgba(0, 0, 0, 0.1);
  margin-top: 20px;
  padding: 30px;
}

/* Logo Header */
.logo-header {
  background: linear-gradient(135deg, #0066B3 0%, #003D82 100%);
  padding: 25px;
  border-radius: 10px;
  margin-bottom: 30px;
  box-shadow: 0 4px 12px rgba(0, 0, 0, 0.15);
  display: flex;
  justify-content: space-between;
  align-items: center;
}

.logo-box {
  background: white;
  padding: 12px 20px;
  border-radius: 8px;
  box-shadow: 0 2px 8px rgba(0, 0, 0, 0.1);
  max-height: 80px;
  display: flex;
  align-items: center;
}

.logo-box img {
  max-height: 60px;
  max-width: 150px;
  object-fit: contain;
}

.main-title {
  color: white;
  font-size: 36px;
  font-weight: 800;
  text-align: center;
  text-shadow: 2px 2px 4px rgba(0, 0, 0, 0.3);
  letter-spacing: 3px;
  flex-grow: 1;
  margin: 0 20px;
  font-family: 'Arial Black', 'Helvetica Neue', Arial, sans-serif;
  text-transform: uppercase;
}

/* Sidebar Styling */
.well {
  background: linear-gradient(135deg, #FFFFFF 0%, #F8F9FA 100%);
  border: 2px solid #0066B3;
  border-radius: 12px;
  box-shadow: 0 4px 12px rgba(0, 102, 179, 0.1);
}

h3, h4 {
  color: #003D82;
  font-weight: 700;
  border-bottom: 3px solid #DC143C;
  padding-bottom: 10px;
  margin-top: 20px;
}

/* Buttons */
.btn-primary {
  background: linear-gradient(135deg, #0066B3 0%, #003D82 100%);
  border: none;
  font-weight: 600;
  text-transform: uppercase;
  letter-spacing: 0.5px;
  padding: 12px;
  border-radius: 8px;
  transition: all 0.3s;
}

.btn-primary:hover {
  background: linear-gradient(135deg, #003D82 0%, #001F41 100%);
  transform: translateY(-2px);
  box-shadow: 0 6px 16px rgba(0, 0, 0, 0.2);
}

.quick-load-btn {
  background: linear-gradient(135deg, #FFFFFF 0%, #F8F9FA 100%);
  color: #003D82;
  border: 2px solid #0066B3;
  font-weight: 700;
  margin: 4px 2px;
  padding: 10px 12px;
  border-radius: 8px;
  font-size: 12px;
  transition: all 0.3s;
  box-shadow: 0 2px 4px rgba(0, 0, 0, 0.1);
  text-transform: uppercase;
  letter-spacing: 0.5px;
}

.quick-load-btn:hover {
  background: linear-gradient(135deg, #0066B3 0%, #003D82 100%);
  color: white;
  border-color: #003D82;
  transform: translateY(-1px);
  box-shadow: 0 4px 8px rgba(0, 102, 179, 0.3);
}

.btn-sm {
  background-color: #E8EDF2;
  color: #555;
  border: none;
  font-weight: 600;
  border-radius: 6px;
}

.btn-sm:hover {
  background-color: #D0D7DE;
}

/* Select inputs */
.selectize-input {
  border: 2px solid #E0E0E0;
  border-radius: 8px;
  transition: all 0.3s;
}

.selectize-input:focus {
  border-color: #0066B3;
  box-shadow: 0 0 0 3px rgba(0, 102, 179, 0.1);
}

/* Info boxes */
.info-box {
  background: #F0F7FF;
  border-left: 4px solid #0066B3;
  padding: 15px;
  border-radius: 6px;
  margin: 15px 0;
  font-size: 13px;
  color: #555;
  box-shadow: 0 2px 4px rgba(0, 0, 0, 0.05);
}

/* Tabs */
.nav-tabs > li.active > a {
  background-color: #0066B3 !important;
  color: white !important;
  font-weight: 600;
  border-radius: 8px 8px 0 0;
}

.nav-tabs > li > a {
  color: #003D82;
  font-weight: 600;
}

.nav-tabs > li > a:hover {
  background-color: #F0F7FF;
}

/* matchup grid title */
h3.matchup-title {
  background: linear-gradient(135deg, #0066B3 0%, #003D82 100%);
  color: white;
  padding: 15px 25px;
  border-radius: 10px;
  text-align: center;
  box-shadow: 0 4px 12px rgba(0, 0, 0, 0.15);
  margin-bottom: 20px;
}

/* Data table */
.dataTables_wrapper {
  background: white;
  padding: 20px;
  border-radius: 12px;
  box-shadow: 0 4px 12px rgba(0, 0, 0, 0.08);
  border: 1px solid #E8EDF2;
}

/* Color legend */
.color-info {
  background: linear-gradient(90deg, #d32f2f 0%, #ffffff 50%, #388e3c 100%);
  height: 8px;
  border-radius: 4px;
  margin: 15px 0;
  box-shadow: 0 2px 4px rgba(0, 0, 0, 0.1);
}

/* Download buttons */
.btn-default {
  background: linear-gradient(135deg, #1A4D2E 0%, #0F3A1F 100%);
  color: white;
  font-weight: 600;
  border: none;
  border-radius: 8px;
  margin: 5px 0;
  transition: all 0.3s;
}

.btn-default:hover {
  background: linear-gradient(135deg, #0F3A1F 0%, #051F10 100%);
  color: white;
  transform: translateY(-2px);
  box-shadow: 0 4px 12px rgba(26, 77, 46, 0.3);
}

/* Instructions */
.instruction-section {
  background: #F8F9FA;
  padding: 25px;
  border-radius: 12px;
  border-left: 5px solid #1A4D2E;
  margin: 20px 0;
}

.instruction-section h3 {
  color: #1A4D2E;
  border-color: #DC143C;
}

.instruction-section ul {
  line-height: 1.9;
}

.instruction-section strong {
  color: #003D82;
}

hr {
  border-top: 2px solid #E8EDF2;
}
</style>
")

# Load model and data
model <- xgb.load("models/matchup_model.json")
feature_cols <- readRDS("models/model_features.rds")
pitcher_features <- readRDS("models/pitcher_features.rds")
batter_features <- readRDS("models/batter_features.rds")
pitcher_quality <- readRDS("models/pitcher_quality.rds")
batter_quality <- readRDS("models/batter_quality.rds")

# Player names
if (file.exists("player_names.csv")) {
  players <- read_csv("player_names.csv", show_col_types = FALSE)
} else {
  stop("Missing player_names.csv")
}

get_player_name <- function(player_id) {
  name <- players %>% filter(id == player_id) %>% pull(name)
  if (length(name) == 0) return(paste("Player", player_id))
  return(normalize_name(name[1]))
}

# Check if player is primarily AAA (>75% of PAs in AAA)
is_primarily_aaa <- function(player_id, player_type = "pitcher") {
  if (player_type == "pitcher") {
    quality_data <- pitcher_quality %>%
      filter(pitcher == player_id, season >= 2024)
  } else {
    quality_data <- batter_quality %>%
      filter(batter == player_id, season >= 2024)
  }
  
  if (nrow(quality_data) == 0) return(FALSE)
  
  # Calculate percentage of PAs in AAA
  total_pa <- sum(quality_data$pa)
  aaa_pa <- sum(quality_data$pa[quality_data$level == "AAA"])
  
  return(aaa_pa / total_pa > 0.75)
}

# Get player name with asterisk if primarily AAA
get_player_name_with_indicator <- function(player_id, player_type = "pitcher") {
  name <- get_player_name(player_id)
  if (is_primarily_aaa(player_id, player_type)) {
    return(paste0(name, "*"))
  }
  return(name)
}

# Italy roster (WBC 2026)
italy_pitchers <- c(
  "Sam Aldegheri", "Dan Altavilla", "Dylan DeLucia", "Alessandro Ercolani",
  "Matt Festa", "Gordon Graceffo", "Alek Jacob", "Joe La Sorsa",
  "Michael Lorenzen", "Ron Marinaccio", "Kyle Nicolas", "Aaron Nola",
  "Adam Ottavino", "Gabriele Quattrini", "Greg Weissert"
)

italy_hitters <- c(
  "Alberto Mineo", "Kyle Teel", "Sam Antonacci", "Jon Berti",
  "Zach Dezenzo", "Andrew Fischer", "Giaconino Lasaracina", "Miles Mastrobuoni",
  "Vinnie Pasquantino", "Thomas Saggese", "Jac Caglianone", "Dominic Canzone",
  "Jakob Marsee", "Nick Morabito", "Dante Nori"
)

# United States roster
usa_pitchers <- c(
  "David Bednar", "Matthew Boyd", "Garrett Cleavinger", "Clay Holmes",
  "Griffin Jax", "Brad Keller", "Clayton Kershaw", "Nolan McLean",
  "Mason Miller", "Joe Ryan", "Paul Skenes", "Tarik Skubal",
  "Gabe Speier", "Michael Wacha", "Logan Webb", "Garrett Whitlock"
)

usa_hitters <- c(
  "Cal Raleigh", "Will Smith", "Alex Bregman", "Ernie Clement",
  "Paul Goldschmidt", "Bryce Harper", "Gunnar Henderson", "Brice Turang",
  "Bobby Witt Jr.", "Roman Anthony", "Byron Buxton", "Pete Crow-Armstrong",
  "Aaron Judge", "Kyle Schwarber"
)

# Mexico roster
mexico_pitchers <- c(
  "Alexander Armenta", "Javier Assad", "Brennan Bernardino", "Taj Bradley",
  "Alex Carrillo", "Jesus Cruz", "Daniel Duarte", "Robert Garcia",
  "Luis Gastelum", "Andres Munoz", "Samy Natera Jr.", "Roel Ramirez",
  "Gerardo Reyes", "Victor Vodnik", "Taijuan Walker"
)

mexico_hitters <- c(
  "Alejandro Kirk", "Alexis Wilson", "Jonathan Aranda", "Nick Gonzales",
  "Joey Meneses", "Joey Ortiz", "Jared Serna", "Rowdy Tellez",
  "Luis Urias", "Ramon Urias", "Randy Arozarena", "Jarren Duran",
  "Julian Ornelas", "Alejandro Osuna", "Alek Thomas"
)

# Great Britain roster
gbr_pitchers <- c(
  "Jack Anderson", "Brendan Beck", "Tristan Beck", "Donovan Benoit",
  "Chavez Fernander", "Gary Gill Hill", "Antonio Knowles", "Miles Langhorne",
  "Ryan Long", "Michael Petersen", "Jack Seppings", "Graham Spraker",
  "Najer Victor", "Tyler Viza", "Nick Wells", "Owen Wild", "Vance Worley"
)

gbr_hitters <- c(
  "Will Cresswell", "Harry Ford", "Jazz Chisholm", "Nate Eaton",
  "Lucius Fox", "Ivan Johnson", "Ian Lewis Jr.", "BJ Murray",
  "Nick Ward", "Matt Koperniak", "Kristian Robinson", "Trayce Thompson",
  "Justin Wylie"
)

# Brazil roster
brazil_pitchers <- c(
  "Pietro Rienzo", "Gabriel Barbosa", "Joseph Contreras", "Tiago Da Silva", 
  "Murilo Gouvea", "Hugo Kanabushi", "Pedro Da Costa Lemos", "Tomas Lopez", 
  "Daniel Missaki", "Oscar Nakaoshi", "Eric Pardinho", "Enzo Sawayama", 
  "Bo Takahashi", "Thyago Vieira", "Hector Villarroel"
)

brazil_hitters <- c(
  "Gabriel Gomes", "Enzo Hayashida", "Matheus Silva", "Gabriel Carmo",
  "Dante Bichette Jr.", "Vitor Ito", "Felipe Koragi", "Felipe Mizukosi",
  "Tiago Nishiyama", "Leonardo Reginatto", "Lucas Rojo", "Osvaldo Carvalho",
  "Gabriel Maciel", "Victor Mascai", "Lucas Ramirez"
)

# Helper: Find player ID from name
find_player_id <- function(player_name) {
  # Normalize the search name
  normalized_search <- normalize_name(player_name)
  
  # Special case for Will Smith - use the catcher (Dodgers)
  if (normalized_search == "Will Smith") {
    # Will Smith catcher ID is 669257, pitcher is 519293
    id <- players %>% filter(normalize_name(name) == normalized_search, id == 669257) %>% pull(id)
    if (length(id) > 0) return(id[1])
  }
  
  # Normalize all player names and match
  id <- players %>% 
    filter(normalize_name(name) == normalized_search) %>% 
    pull(id)
  if (length(id) == 0) return(NA)
  return(id[1])
}

# Get 2024-2025 players
# Normalize names (remove accents: ñ→n, é→e, etc.)
normalize_name <- function(name) {
  # Convert to UTF-8 first
  name <- enc2utf8(name)
  # Replace tilde encoding issues (Mu~noz → Munoz)
  name <- gsub("~n", "n", name, ignore.case = TRUE)
  name <- gsub("~N", "N", name, ignore.case = TRUE)
  # Replace accented characters
  name <- gsub("ñ", "n", name, ignore.case = TRUE)
  name <- gsub("á", "a", name, ignore.case = TRUE)
  name <- gsub("é", "e", name, ignore.case = TRUE)
  name <- gsub("í", "i", name, ignore.case = TRUE)
  name <- gsub("ó", "o", name, ignore.case = TRUE)
  name <- gsub("ú", "u", name, ignore.case = TRUE)
  name <- gsub("ü", "u", name, ignore.case = TRUE)
  # Handle UTF-8 encoded versions
  name <- iconv(name, from = "UTF-8", to = "ASCII//TRANSLIT", sub = "")
  return(name)
}

pitcher_choices_2024 <- pitcher_features %>%
  filter(season >= 2024) %>%
  distinct(pitcher) %>%
  mutate(name = sapply(pitcher, function(id) normalize_name(get_player_name(id)))) %>%
  arrange(name)

batter_choices_2024 <- batter_features %>%
  filter(season >= 2024) %>%
  distinct(batter) %>%
  mutate(name = sapply(batter, function(id) normalize_name(get_player_name(id)))) %>%
  arrange(name)

pitcher_choices <- setNames(pitcher_choices_2024$pitcher, pitcher_choices_2024$name)
batter_choices <- setNames(batter_choices_2024$batter, batter_choices_2024$name)

# Helper: Build features for any pitcher-batter combo
build_matchup_features <- function(pitcher_id, batter_id) {
  # Get most recent pitcher features (prefer 2025, fall back to 2024)
  p_feat <- pitcher_features %>%
    filter(pitcher == pitcher_id, season >= 2024) %>%
    arrange(desc(season)) %>%
    head(1)
  
  # Get most recent batter features
  b_feat <- batter_features %>%
    filter(batter == batter_id, season >= 2024) %>%
    arrange(desc(season)) %>%
    head(1)
  
  # Get quality scores
  p_qual <- pitcher_quality %>%
    filter(pitcher == pitcher_id, season >= 2024) %>%
    arrange(desc(season)) %>%
    head(1)
  
  b_qual <- batter_quality %>%
    filter(batter == batter_id, season >= 2024) %>%
    arrange(desc(season)) %>%
    head(1)
  
  # If no data available, return NULL (player not in system)
  if (nrow(p_feat) == 0 || nrow(b_feat) == 0 || nrow(p_qual) == 0 || nrow(b_qual) == 0) {
    return(NULL)
  }
  
  # Build Log5
  log5 <- function(p, b) {
    num <- p * (1 - b)
    denom <- num + b * (1 - p)
    return(num / denom)
  }
  
  log5_prob <- log5(p_qual$pitcher_quality, b_qual$batter_quality)
  log5_base <- (0.5 - log5_prob) * 0.2
  
  # Helper to replace NA with 0
  safe_val <- function(x) ifelse(is.na(x) | !is.finite(x), 0, x)
  
  # Build all features with NA protection
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
    
    same_hand = as.numeric(p_feat$p_throws == b_feat$stand)
  )
  
  return(features)
}

# Prediction function using actual model
predict_matchup <- function(pitcher_id, batter_id) {
  tryCatch({
    # Build features
    features <- build_matchup_features(pitcher_id, batter_id)
    
    if (is.null(features)) {
      return(list(score = NA, reason = "No data"))
    }
    
    # Ensure correct order
    features <- features[, feature_cols]
    
    # Get prediction (in runs per PA)
    pred_matrix <- xgb.DMatrix(data = as.matrix(features))
    pred <- predict(model, pred_matrix)
    
    # Convert to index scale (100 = average)
    # The prediction is in runs advantage for batter (typically -0.15 to +0.15 range)
    # We need to convert this to a positive scale first, then index it
    
    # CALIBRATION: Center model predictions so league average = 100
    # Model predictions don't average to exactly 0, causing calibration drift
    # Subtract the mean prediction from training/validation to center at 0
    pred_centered <- pred - 0.0018  # Mean from 2025 holdout validation
    
    # Add baseline to make all values positive (league average wOBA ≈ 0.320 runs/PA equivalent)
    # This shifts the scale so 0 becomes a positive baseline
    baseline_runs <- 0.15  # This represents "average" performance
    positive_score <- baseline_runs + pred_centered  # Now centered at baseline
    
    # Apply index scaling: (individual / average) * 100
    # Average positive_score = baseline_runs (0.15), so scores are relative to this
    index_score <- (positive_score / baseline_runs) * 100
    
    score <- round(index_score)
    
    # === CALIBRATION ADJUSTMENTS ===
    # 1. Shift to center 100 at league average OPS (0.719)
    score <- score - 12
    
    # 2. Cross-league adjustment (±15 points)
    # Try 2025 first, fallback to 2024 if no data
    p_data <- pitcher_quality %>%
      filter(pitcher == pitcher_id, season == 2025)
    
    # Fallback to 2024 if no 2025 data
    if (nrow(p_data) == 0) {
      p_data <- pitcher_quality %>%
        filter(pitcher == pitcher_id, season == 2024)
    }
    
    b_data <- batter_quality %>%
      filter(batter == batter_id, season == 2025)
    
    # Fallback to 2024 if no 2025 data
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
    
    # Apply penalty for clear cross-league matchups
    if (p_level == "AAA" && b_level == "MLB") {
      score <- score + 15  # MLB batter advantage
    } else if (p_level == "MLB" && b_level == "AAA") {
      score <- score - 15  # MLB pitcher advantage
    }
    
    # 3. Clamp to 50-150 range
    score <- pmin(pmax(score, 50), 150)
    
    # Now: 100 = league average (OPS 0.719)
    #      >100 = batter advantage
    #      <100 = pitcher advantage
    #      ±15 for cross-league matchups
    
    # Get feature importance-weighted contributions
    # Load global importance scores
    importance_weights <- c(
      log5_base = 0.132, batter_platoon_effect = 0.063, pitcher_platoon_effect = 0.060,
      ch_matchup_runs = 0.034, si_matchup_runs = 0.033, sl_matchup_runs = 0.031,
      ff_matchup_runs = 0.031, fc_matchup_runs = 0.030, ff_matchup_whiff = 0.030,
      cu_matchup_runs = 0.028, k_rate_diff = 0.028, zone_contact_rate_b = 0.026,
      si_matchup_whiff = 0.022, hard_hit_vs_k_diff = 0.021, cu_matchup_whiff = 0.021,
      ch_matchup_whiff = 0.020, st_matchup_runs = 0.019, hard_hit_rate_b = 0.019,
      fc_matchup_whiff = 0.019, chase_rate_b = 0.019, sl_matchup_whiff = 0.014,
      fs_matchup_runs = 0.009, ff_velo_advantage = 0.008, bb_rate_diff = 0.007,
      fs_matchup_whiff = 0.006, k_rate_diff = 0.006, barrel_rate_b = 0.018
    )
    
    # Calculate weighted contributions WITH Z-SCORE STANDARDIZATION
    # Calculate mean and SD for each feature across typical matchups
    # These are approximate population statistics
    feature_means <- c(
      log5_base = 0, pitcher_platoon_effect = 0, batter_platoon_effect = 0,
      ff_matchup_whiff = 0.015, si_matchup_whiff = 0.010, fc_matchup_whiff = 0.008,
      sl_matchup_whiff = 0.012, st_matchup_whiff = 0.005, cu_matchup_whiff = 0.008,
      kc_matchup_whiff = 0.003, ch_matchup_whiff = 0.008, fs_matchup_whiff = 0.003,
      ff_matchup_runs = 0.002, si_matchup_runs = 0.001, fc_matchup_runs = 0.001,
      sl_matchup_runs = 0.001, st_matchup_runs = 0.0005, cu_matchup_runs = 0.001,
      kc_matchup_runs = 0.0003, ch_matchup_runs = 0.001, fs_matchup_runs = 0.0005,
      ff_velo_advantage = 0, si_velo_advantage = 0, sl_velo_advantage = 0, cu_velo_advantage = 0,
      ff_spin_advantage = 0, sl_spin_advantage = 0, cu_spin_advantage = 0,
      exit_velo_vs_chase = 75, hard_hit_vs_k_diff = 0.25,
      k_rate_diff = 0, bb_rate_diff = 0.08,
      ff_pct_p = 0.35, si_pct_p = 0.15, fc_pct_p = 0.05, sl_pct_p = 0.15, st_pct_p = 0.02,
      cu_pct_p = 0.08, kc_pct_p = 0.02, ch_pct_p = 0.10, fs_pct_p = 0.05,
      chase_rate_b = 0.30, zone_contact_rate_b = 0.85, hard_hit_rate_b = 0.40,
      barrel_rate_b = 0.08, same_hand = 0.5
    )
    
    feature_sds <- c(
      log5_base = 0.015, pitcher_platoon_effect = 0.015, batter_platoon_effect = 0.015,
      ff_matchup_whiff = 0.010, si_matchup_whiff = 0.008, fc_matchup_whiff = 0.006,
      sl_matchup_whiff = 0.008, st_matchup_whiff = 0.004, cu_matchup_whiff = 0.006,
      kc_matchup_whiff = 0.003, ch_matchup_whiff = 0.006, fs_matchup_whiff = 0.003,
      ff_matchup_runs = 0.003, si_matchup_runs = 0.002, fc_matchup_runs = 0.002,
      sl_matchup_runs = 0.002, st_matchup_runs = 0.001, cu_matchup_runs = 0.002,
      kc_matchup_runs = 0.001, ch_matchup_runs = 0.002, fs_matchup_runs = 0.001,
      ff_velo_advantage = 2, si_velo_advantage = 2, sl_velo_advantage = 2, cu_velo_advantage = 2,
      ff_spin_advantage = 150, sl_spin_advantage = 150, cu_spin_advantage = 150,
      exit_velo_vs_chase = 15, hard_hit_vs_k_diff = 0.15,
      k_rate_diff = 0.08, bb_rate_diff = 0.04,
      ff_pct_p = 0.15, si_pct_p = 0.10, fc_pct_p = 0.05, sl_pct_p = 0.10, st_pct_p = 0.03,
      cu_pct_p = 0.06, kc_pct_p = 0.03, ch_pct_p = 0.08, fs_pct_p = 0.04,
      chase_rate_b = 0.08, zone_contact_rate_b = 0.06, hard_hit_rate_b = 0.08,
      barrel_rate_b = 0.04, same_hand = 0.5
    )
    
    contributions <- sapply(feature_cols, function(fname) {
      weight <- ifelse(fname %in% names(importance_weights), importance_weights[fname], 0.001)
      mean_val <- ifelse(fname %in% names(feature_means), feature_means[fname], 0)
      sd_val <- ifelse(fname %in% names(feature_sds), feature_sds[fname], 1)
      # Z-score: (value - mean) / sd, then weight by importance
      z_score <- abs(features[[fname]] - mean_val) / sd_val
      z_score * weight
    })
    names(contributions) <- feature_cols
    
    # Filter: Only show features favoring the winning side
    score_favors_batter <- score > 100
    
    if (score_favors_batter) {
      # Batter favored (score > 100): only show POSITIVE contributions
      filtered_contribs <- contributions[contributions > 0]
    } else {
      # Pitcher favored (score < 100): only show NEGATIVE contributions  
      filtered_contribs <- contributions[contributions < 0]
    }
    
    # Get top 5 by absolute value
    if (length(filtered_contribs) >= 5) {
      top_5_names <- names(sort(abs(filtered_contribs), decreasing = TRUE))[1:5]
      top_5_idx <- match(top_5_names, feature_cols)
    } else if (length(filtered_contribs) > 0) {
      # Less than 5 contributions in favored direction
      top_5_names <- names(sort(abs(filtered_contribs), decreasing = TRUE))
      top_5_idx <- match(top_5_names, feature_cols)
    } else {
      # Edge case: no contributions in favored direction (neutral matchup)
      top_5_idx <- order(abs(contributions), decreasing = TRUE)[1:5]
    }
    
    top_5_features <- feature_cols[top_5_idx]
    
    # Create readable labels
    label_feature <- function(fname) {
      case_when(
        grepl("ff_matchup_whiff", fname) ~ "FF-Whiff",
        grepl("si_matchup_whiff", fname) ~ "SI-Whiff",
        grepl("sl_matchup_whiff", fname) ~ "SL-Whiff",
        grepl("cu_matchup_whiff", fname) ~ "CB-Whiff",
        grepl("ch_matchup_whiff", fname) ~ "CH-Whiff",
        grepl("fc_matchup_whiff", fname) ~ "FC-Whiff",
        grepl("st_matchup_whiff", fname) ~ "ST-Whiff",
        grepl("fs_matchup_whiff", fname) ~ "FS-Whiff",
        grepl("ff_matchup_hard_hit", fname) ~ "FF-HardHit",
        grepl("si_matchup_hard_hit", fname) ~ "SI-HardHit",
        grepl("sl_matchup_hard_hit", fname) ~ "SL-HardHit",
        grepl("cu_matchup_hard_hit", fname) ~ "CB-HardHit",
        grepl("ch_matchup_hard_hit", fname) ~ "CH-HardHit",
        grepl("fc_matchup_hard_hit", fname) ~ "FC-HardHit",
        grepl("st_matchup_hard_hit", fname) ~ "ST-HardHit",
        grepl("fs_matchup_hard_hit", fname) ~ "FS-HardHit",
        grepl("ff_matchup_barrel", fname) ~ "FF-Barrel",
        grepl("si_matchup_barrel", fname) ~ "SI-Barrel",
        grepl("sl_matchup_barrel", fname) ~ "SL-Barrel",
        grepl("cu_matchup_barrel", fname) ~ "CB-Barrel",
        grepl("ch_matchup_barrel", fname) ~ "CH-Barrel",
        grepl("fc_matchup_barrel", fname) ~ "FC-Barrel",
        grepl("st_matchup_barrel", fname) ~ "ST-Barrel",
        grepl("fs_matchup_barrel", fname) ~ "FS-Barrel",
        grepl("ff_matchup_runs", fname) ~ "FF-Runs",
        grepl("si_matchup_runs", fname) ~ "SI-Runs",
        grepl("sl_matchup_runs", fname) ~ "SL-Runs",
        grepl("cu_matchup_runs", fname) ~ "CB-Runs",
        grepl("ch_matchup_runs", fname) ~ "CH-Runs",
        grepl("fc_matchup_runs", fname) ~ "FC-Runs",
        grepl("st_matchup_runs", fname) ~ "ST-Runs",
        grepl("fs_matchup_runs", fname) ~ "FS-Runs",
        grepl("ff_velo", fname) ~ "P-FFVelo",
        grepl("si_velo", fname) ~ "P-SIVelo",
        grepl("sl_velo", fname) ~ "P-SLVelo",
        grepl("cu_velo", fname) ~ "P-CBVelo",
        grepl("ff_spin", fname) ~ "P-FFSpin",
        grepl("sl_spin", fname) ~ "P-SLSpin",
        grepl("cu_spin", fname) ~ "P-CBSpin",
        grepl("hard_hit_rate", fname) ~ "B-HardHit",
        grepl("barrel", fname) ~ "B-Barrel",
        grepl("chase_rate_b", fname) ~ "B-Chase",
        grepl("zone_contact", fname) ~ "B-ZnCont",
        grepl("k_rate_diff", fname) ~ "K-Rate",
        grepl("bb_rate", fname) ~ "B-BB",
        grepl("exit_velo", fname) ~ "B-ExitVel",
        grepl("hard_hit_vs_k", fname) ~ "HH-vs-K",
        grepl("log5", fname) ~ "Talent",
        grepl("pitcher_platoon", fname) ~ "P-Plat",
        grepl("batter_platoon", fname) ~ "B-Plat",
        grepl("same_hand", fname) ~ "SameHand",
        grepl("ff_pct", fname) ~ "P-FF%",
        grepl("si_pct", fname) ~ "P-SI%",
        grepl("sl_pct", fname) ~ "P-SL%",
        grepl("cu_pct", fname) ~ "P-CB%",
        grepl("ch_pct", fname) ~ "P-CH%",
        grepl("fc_pct", fname) ~ "P-FC%",
        grepl("st_pct", fname) ~ "P-ST%",
        grepl("fs_pct", fname) ~ "P-FS%",
        grepl("kc_pct", fname) ~ "P-KC%",
        TRUE ~ substr(fname, 1, 7)
      )
    }
    
    # Create reason strings - newlines for matchup grid, commas for CSV
    reason_labels <- sapply(top_5_features, label_feature)
    reason_grid <- paste(reason_labels, collapse = "\n")
    reason_csv <- paste(reason_labels, collapse = ", ")
    
    return(list(score = score, reason = reason_csv, reason_grid = reason_grid))
    
  }, error = function(e) {
    return(list(score = NA, reason = "Error"))
  })
}

# UI
ui <- fluidPage(
  custom_css,
  
  # Professional header with actual logo images
  div(class = "logo-header",
      div(class = "logo-box",
          tags$img(src = "Italia.jpg", height = "60px", alt = "Team Italia")
      ),
      div(class = "main-title",
          "ITALY MATCHUP ANALYTICS"
      ),
      div(class = "logo-box",
          tags$img(src = "WBC.jpg", height = "60px", alt = "World Baseball Classic")
      )
  ),
  
  theme = bslib::bs_theme(bootswatch = "flatly"),
  
  # Main tabs at the top level
  tabsetPanel(
    id = "main_tabs",
    
    # TAB 1: GENERAL MATCHUPS
    tabPanel("General Matchups",
             br(),
             sidebarLayout(
               sidebarPanel(
                 width = 3,
                 
                 h4("Quick Load Teams"),
                 fluidRow(
                   column(6, actionButton("load_italy_bat", "Italy Batters", class = "quick-load-btn btn-block")),
                   column(6, actionButton("load_italy_pitch", "Italy Pitchers", class = "quick-load-btn btn-block"))
                 ),
                 fluidRow(
                   column(6, actionButton("load_usa_bat", "USA Batters", class = "quick-load-btn btn-block")),
                   column(6, actionButton("load_usa_pitch", "USA Pitchers", class = "quick-load-btn btn-block"))
                 ),
                 fluidRow(
                   column(6, actionButton("load_mexico_bat", "Mexico Batters", class = "quick-load-btn btn-block")),
                   column(6, actionButton("load_mexico_pitch", "Mexico Pitchers", class = "quick-load-btn btn-block"))
                 ),
                 fluidRow(
                   column(6, actionButton("load_gbr_bat", "GBR Batters", class = "quick-load-btn btn-block")),
                   column(6, actionButton("load_gbr_pitch", "GBR Pitchers", class = "quick-load-btn btn-block"))
                 ),
                 fluidRow(
                   column(6, actionButton("load_brazil_bat", "Brazil Batters", class = "quick-load-btn btn-block")),
                   column(6, actionButton("load_brazil_pitch", "Brazil Pitchers", class = "quick-load-btn btn-block"))
                 ),
                 
                 hr(),
                 
                 h4("Pitchers"),
                 selectInput("pitchers", 
                             NULL,
                             choices = pitcher_choices,
                             multiple = TRUE,
                             selectize = TRUE),
                 actionButton("clear_pitchers", "Clear", class = "btn-sm"),
                 
                 hr(),
                 
                 h4("Batters"),
                 selectInput("batters",
                             NULL,
                             choices = batter_choices,
                             multiple = TRUE,
                             selectize = TRUE),
                 actionButton("clear_batters", "Clear", class = "btn-sm"),
                 
                 hr(),
                 
                 actionButton("generate", "Generate Matchup Sheet", 
                              class = "btn-primary btn-lg btn-block"),
                 
                 br(),
                 
                 downloadButton("download_csv", "Download CSV", class = "btn-block"),
                 downloadButton("download_excel", "Download Excel", class = "btn-block"),
                 downloadButton("download_pdf", "Download PDF", class = "btn-block")
               ),
               
               mainPanel(
                 width = 9,
                 uiOutput("sheet_title"),
                 plotOutput("heatmap", height = "auto"),
                 br(),
                 DTOutput("detail_table")
               )
             )
    ),
    
    # TAB 2: ITALY SCOUTING REPORT
    tabPanel("Italy Scouting Report",
             br(),
             sidebarLayout(
               sidebarPanel(
                 width = 3,
                 
                 div(class = "info-box",
                     strong("Generate comprehensive scouting reports:"),
                     br(), br(),
                     "• Italy Pitchers vs Opponent Batters", br(),
                     "• Opponent Pitchers vs Italy Batters"
                 ),
                 
                 hr(),
                 
                 h4("Opponent Team"),
                 radioButtons("opponent_team", NULL,
                              choices = c("USA" = "usa",
                                          "Mexico" = "mexico",
                                          "Great Britain" = "gbr",
                                          "Brazil" = "brazil"),
                              selected = "usa"),
                 
                 hr(),
                 
                 actionButton("generate_scout", "Generate Scouting Report", 
                              class = "btn-primary btn-lg btn-block"),
                 
                 br(),
                 
                 downloadButton("download_scouting_excel", "Download Report Excel", class = "btn-block"),
                 downloadButton("download_scouting_pdf", "Download Report PDF", class = "btn-block")
               ),
               
               mainPanel(
                 width = 9,
                 uiOutput("scouting_status"),
                 br(),
                 h3("Italy Pitchers vs Opponent Batters", class = "matchup-title"),
                 plotOutput("scout_pitch_heatmap", height = "800px"),
                 br(),
                 DTOutput("scout_pitch_table"),
                 downloadButton("download_scout_pitch_csv", "Download Italy Pitching CSV", class = "btn-sm"),
                 br(), br(),
                 h3("Opponent Pitchers vs Italy Batters", class = "matchup-title"),
                 plotOutput("scout_bat_heatmap", height = "800px"),
                 br(),
                 DTOutput("scout_bat_table"),
                 downloadButton("download_scout_bat_csv", "Download Italy Batting CSV", class = "btn-sm")
               )
             )
    ),
    
    # TAB 3: INSTRUCTIONS
    # TAB 3: MODEL VALIDATION
    tabPanel("Model Validation",
             br(),
             h3("2025 Holdout Validation Results"),
             div(style = "background-color: #d1ecf1; padding: 15px; border-left: 5px solid #0c5460; margin-bottom: 20px;",
                 p(strong("Model Performance:")),
                 tags$ul(
                   tags$li("Model trained on 2020-2024 data"),
                   tags$li("Validated on 2025 holdout data (unseen)"),
                   tags$li("No data leakage"),
                   tags$li("Production model uses all data (2020-2025)")
                 )
             ),
             plotOutput("validation_plot", height = "500px"),
             br(),
             DTOutput("validation_table"),
             br(),
             h4("Key Results"),
             tags$ul(
               tags$li(strong("Bucket correlation:"), textOutput("validation_correlation_text", inline = TRUE)),
               tags$li(strong("Score interpretation:"), "High scores = high OPS (batter advantage)"),
               tags$li(strong("Score 100:"), "Neutral/average performance"),
               tags$li(strong("Score 120+:"), "Strong batter advantage"),
               tags$li(strong("Score < 85:"), "Strong pitcher advantage"),
               tags$li(strong("Each ~8-10 points:"), "Approximately 0.100 OPS difference")
             ),
             p(em("Note: Correlation calculated from validation bucket data"))
    ),
    
    # TAB 4: INSTRUCTIONS
    tabPanel("Instructions",
             br(),
             h3("Quick Start"),
             tags$ol(
               tags$li(strong("General Matchups:"), "Select pitchers and batters → Generate Matchup Sheet"),
               tags$li(strong("Italy Scouting:"), "Select opponent team → Generate Scouting Report"),
               tags$li(strong("Download:"), "Export as CSV, Excel, or PDF")
             ),
             br(),
             h3("How the Model Works"),
             p("Predicts delta run expectancy per plate appearance"),
             tags$ul(
               tags$li(strong("What it measures:"), "Change in expected runs based on game state (runners, outs) before vs after the PA"),
               tags$li(strong("From batter's perspective:"), "Positive prediction = good for batter, negative = good for pitcher"),
               tags$li(strong("Examples of run values:"), "Home run ≈ +1.4 to +2.2, Walk ≈ +0.3, Strikeout ≈ -0.25 to -0.3"),
               tags$li(strong("Converted to 100-point scale:"), "score = (0.15 + prediction) / 0.15 × 100"),
               tags$ul(
                 tags$li("Baseline of 0.15 shifts predictions to positive range"),
                 tags$li("Then indexed relative to 100 = average")
               ),
               tags$li(strong("Score 100:"), "Neutral (0 run advantage)"),
               tags$li(strong("Score interpretation:"), "Low = pitcher wins, High = batter wins"),
               tags$li(strong("Each ~8-10 points:"), "Approximately 0.100 OPS difference")
             ),
             br(),
             h3("Reading the Grid"),
             tags$ul(
               tags$li(strong("Colors:"), "Blue-to-Red scale"),
               tags$li(strong("Blue (low scores):"), "Pitcher advantage"),
               tags$li(strong("Red (high scores):"), "Batter advantage"),
               tags$li(strong("All matchups:"), "Consistent batter-centric view"),
               tags$li(strong("Asterisk (*):"), "Player has >75% of plate appearances in AAA (limited MLB data)")
             ),
             br(),
             h3("Key Features"),
             tags$ul(
               tags$li(strong("Talent (Log5):"), "Overall quality comparison using Log5 formula"),
               tags$li(strong("Platoon (P-Plat, B-Plat):"), "Performance vs handedness"),
               tags$li(strong("Pitch-specific (FF-Whiff, SL-Whiff):"), "Whiff rate interactions by pitch type"),
               tags$li(strong("Contact quality (B-Barrel, Hard-Hit):"), "Optimal contact and hard-hit interactions"),
               tags$li(strong("K-Rate:"), "Strikeout differential (pitcher K% - batter K%)")
             )
    )
  )
)

# Server
server <- function(input, output, session) {
  
  matchup_results <- reactiveVal(NULL)
  
  # Load validation data (prefer holdout if available)
  validation_data <- tryCatch({
    if (file.exists("validation_holdout_2025.csv")) {
      read.csv("validation_holdout_2025.csv", stringsAsFactors = FALSE)
    } else {
      read.csv("validation_by_score_tier.csv", stringsAsFactors = FALSE)
    }
  }, error = function(e) NULL)
  
  # Calculate correlation if validation data exists
  validation_correlation <- NULL
  if (!is.null(validation_data) && nrow(validation_data) > 0) {
    # Extract numeric scores from bucket labels (use midpoint)
    validation_data$score_numeric <- sapply(validation_data$score_bucket, function(x) {
      if (grepl("<", x)) return(82)  # < 85 → 82
      if (grepl("\\+", x)) return(122)  # 120+ → 122
      # For ranges like "85-90", take midpoint
      nums <- as.numeric(strsplit(gsub("[^0-9-]", "", x), "-")[[1]])
      return(mean(nums))
    })
    validation_correlation <- cor(validation_data$score_numeric, validation_data$ops)
  }
  
  # Validation plot
  output$validation_plot <- renderPlot({
    if (is.null(validation_data)) {
      plot.new()
      text(0.5, 0.5, "Validation data not found.\nPlace validation_holdout_2025.csv in app directory.", cex = 1.5)
      return()
    }
    
    # Create plot
    par(mar = c(5, 5, 3, 2))
    plot(1:nrow(validation_data), validation_data$ops,
         type = "b", pch = 19, cex = 1.5, col = "#2c3e50", lwd = 2,
         xlab = "Score Bucket (Low → High)", ylab = "Actual OPS",
         main = "Score Buckets vs Actual OPS (Higher Scores = Higher OPS)",
         xaxt = "n", ylim = c(0.3, 1.0), cex.main = 1.3, cex.lab = 1.2)
    
    axis(1, at = 1:nrow(validation_data), labels = validation_data$score_bucket, cex.axis = 0.9)
    abline(h = 0.719, lty = 2, col = "gray50", lwd = 1.5)
    text(5, 0.75, "2025 League Avg (0.719)", col = "gray30", cex = 0.9)
    grid(NA, NULL, lty = 1, col = "gray90")
  })
  
  # Validation correlation text
  output$validation_correlation_text <- renderText({
    if (is.null(validation_correlation)) {
      return("Run validate_model_FIXED.R to generate results")
    }
    sprintf("%.3f (positive relationship: high scores = high OPS)", validation_correlation)
  })
  
  # Validation table
  output$validation_table <- renderDT({
    if (is.null(validation_data)) {
      return(data.frame(Message = "Validation data not found. Run validate_model.R"))
    }
    
    display_data <- validation_data %>%
      select(score_bucket, n_matchups, total_pas, obp, slg, ops) %>%
      rename(
        "Score Bucket" = score_bucket,
        "N Matchups" = n_matchups,
        "Total PAs" = total_pas,
        "OBP" = obp,
        "SLG" = slg,
        "OPS" = ops
      )
    
    datatable(display_data,
              options = list(pageLength = 10, dom = 't'),
              rownames = FALSE) %>%
      formatRound(c("OBP", "SLG", "OPS"), 3)
  })
  
  # Quick load buttons
  observeEvent(input$load_italy_bat, {
    ids <- sapply(italy_hitters, find_player_id)
    ids <- ids[!is.na(ids)]
    updateSelectInput(session, "batters", selected = ids)
  })
  
  observeEvent(input$load_italy_pitch, {
    ids <- sapply(italy_pitchers, find_player_id)
    ids <- ids[!is.na(ids)]
    updateSelectInput(session, "pitchers", selected = ids)
  })
  
  observeEvent(input$load_gbr_bat, {
    ids <- sapply(gbr_hitters, find_player_id)
    ids <- ids[!is.na(ids)]
    updateSelectInput(session, "batters", selected = ids)
  })
  
  observeEvent(input$load_gbr_pitch, {
    ids <- sapply(gbr_pitchers, find_player_id)
    ids <- ids[!is.na(ids)]
    updateSelectInput(session, "pitchers", selected = ids)
  })
  
  observeEvent(input$load_usa_bat, {
    ids <- sapply(usa_hitters, find_player_id)
    ids <- ids[!is.na(ids)]
    updateSelectInput(session, "batters", selected = ids)
  })
  
  observeEvent(input$load_usa_pitch, {
    ids <- sapply(usa_pitchers, find_player_id)
    ids <- ids[!is.na(ids)]
    updateSelectInput(session, "pitchers", selected = ids)
  })
  
  observeEvent(input$load_mexico_bat, {
    ids <- sapply(mexico_hitters, find_player_id)
    ids <- ids[!is.na(ids)]
    updateSelectInput(session, "batters", selected = ids)
  })
  
  observeEvent(input$load_mexico_pitch, {
    ids <- sapply(mexico_pitchers, find_player_id)
    ids <- ids[!is.na(ids)]
    updateSelectInput(session, "pitchers", selected = ids)
  })
  
  observeEvent(input$load_brazil_bat, {
    ids <- sapply(brazil_hitters, find_player_id)
    ids <- ids[!is.na(ids)]
    updateSelectInput(session, "batters", selected = ids)
  })
  
  observeEvent(input$load_brazil_pitch, {
    ids <- sapply(brazil_pitchers, find_player_id)
    ids <- ids[!is.na(ids)]
    updateSelectInput(session, "pitchers", selected = ids)
  })
  
  observeEvent(input$clear_pitchers, {
    updateSelectInput(session, "pitchers", selected = character(0))
  })
  
  observeEvent(input$clear_batters, {
    updateSelectInput(session, "batters", selected = character(0))
  })
  
  # Generate sheet
  observeEvent(input$generate, {
    req(input$pitchers, input$batters)
    
    withProgress(message = 'Generating...', {
      
      matchups <- expand.grid(
        pitcher = as.numeric(input$pitchers),
        batter = as.numeric(input$batters),
        stringsAsFactors = FALSE
      )
      
      incProgress(0.3)
      
      # Get predictions
      results <- lapply(1:nrow(matchups), function(i) {
        predict_matchup(matchups$pitcher[i], matchups$batter[i])
      })
      
      matchups$score <- sapply(results, function(x) x$score)
      matchups$reason <- sapply(results, function(x) x$reason)
      matchups$reason_grid <- sapply(results, function(x) x$reason_grid)
      
      incProgress(0.7)
      
      matchups$pitcher_name <- sapply(matchups$pitcher, function(id) get_player_name_with_indicator(id, "pitcher"))
      matchups$batter_name <- sapply(matchups$batter, function(id) get_player_name_with_indicator(id, "batter"))
      
      matchup_results(matchups)
      
      incProgress(1.0)
    })
  })
  
  # Reactive for scouting report data
  scouting_results <- reactiveVal(NULL)
  
  # Generate Italy Scouting Report
  observeEvent(input$generate_scout, {
    req(input$opponent_team)
    
    withProgress(message = 'Generating Italy Scouting Report...', {
      
      # Get opponent batters and pitchers
      opponent_batters <- switch(input$opponent_team,
                                 "usa" = sapply(usa_hitters, find_player_id),
                                 "mexico" = sapply(mexico_hitters, find_player_id),
                                 "gbr" = sapply(gbr_hitters, find_player_id),
                                 "brazil" = sapply(brazil_hitters, find_player_id)
      )
      opponent_batters <- opponent_batters[!is.na(opponent_batters)]
      
      opponent_pitchers <- switch(input$opponent_team,
                                  "usa" = sapply(usa_pitchers, find_player_id),
                                  "mexico" = sapply(mexico_pitchers, find_player_id),
                                  "gbr" = sapply(gbr_pitchers, find_player_id),
                                  "brazil" = sapply(brazil_pitchers, find_player_id)
      )
      opponent_pitchers <- opponent_pitchers[!is.na(opponent_pitchers)]
      
      # Get Italy players
      italy_batters <- sapply(italy_hitters, find_player_id)
      italy_batters <- italy_batters[!is.na(italy_batters)]
      
      italy_pitchers_ids <- sapply(italy_pitchers, find_player_id)
      italy_pitchers_ids <- italy_pitchers_ids[!is.na(italy_pitchers_ids)]
      
      incProgress(0.2)
      
      # Generate both matchup sets
      matchups_all <- rbind(
        expand.grid(
          pitcher = italy_pitchers_ids,
          batter = opponent_batters,
          stringsAsFactors = FALSE
        ),
        expand.grid(
          pitcher = opponent_pitchers,
          batter = italy_batters,
          stringsAsFactors = FALSE
        )
      )
      
      incProgress(0.4)
      
      # Get predictions
      results <- lapply(1:nrow(matchups_all), function(i) {
        predict_matchup(matchups_all$pitcher[i], matchups_all$batter[i])
      })
      
      matchups_all$score <- sapply(results, function(x) x$score)
      matchups_all$reason <- sapply(results, function(x) x$reason)
      matchups_all$reason_grid <- sapply(results, function(x) x$reason_grid)
      
      incProgress(0.8)
      
      matchups_all$pitcher_name <- sapply(matchups_all$pitcher, function(id) get_player_name_with_indicator(id, "pitcher"))
      matchups_all$batter_name <- sapply(matchups_all$batter, function(id) get_player_name_with_indicator(id, "batter"))
      
      scouting_results(matchups_all)
      
      incProgress(1.0)
    })
  })
  
  output$scouting_status <- renderUI({
    if (is.null(scouting_results())) {
      div(class = "info-box", style = "background: #FFF3CD; border-left: 4px solid #FFA500;",
          strong("⚠️ No scouting report generated yet."), 
          " Click 'Generate Scouting Report' in the left sidebar after selecting an opponent team.")
    } else {
      opponent_name <- switch(input$opponent_team,
                              "usa" = "USA",
                              "mexico" = "Mexico",
                              "gbr" = "Great Britain",
                              "brazil" = "Brazil"
      )
      div(class = "info-box", style = "background: #D1F2EB; border-left: 4px solid #28A745;",
          strong(sprintf("✓ Italy vs %s Scouting Report Generated", opponent_name)))
    }
  })
  
  output$sheet_title <- renderUI({
    req(matchup_results())
    data <- matchup_results()
    
    h3(sprintf("Matchup Sheet %s", "(Red = Good for hitters)"), class = "matchup-title")
  })
  
  output$heatmap <- renderPlot({
    req(matchup_results())
    
    data <- matchup_results()
    
    # Detect if Italy is batting or pitching
    italy_ids_bat <- sapply(italy_hitters, find_player_id)
    italy_ids_pitch <- sapply(italy_pitchers, find_player_id)
    italy_ids_bat <- italy_ids_bat[!is.na(italy_ids_bat)]
    italy_ids_pitch <- italy_ids_pitch[!is.na(italy_ids_pitch)]
    
    italy_batting <- any(data$batter %in% italy_ids_bat)
    italy_pitching <- any(data$pitcher %in% italy_ids_pitch)
    
    # NO FLIPPING - high scores always = good for batter
    
    mat <- data %>%
      select(pitcher_name, batter_name, score) %>%
      pivot_wider(names_from = pitcher_name, values_from = score) %>%  # Pitchers as columns
      column_to_rownames("batter_name") %>%  # Batters as rows
      as.matrix()
    
    # Dynamic sizing based on matrix dimensions
    n_batters <- nrow(mat)  # Batters now rows
    n_pitchers <- ncol(mat)  # Pitchers now columns
    
    # Calculate optimal margins
    bottom_mar <- max(12, min(20, n_pitchers * 0.8))  # Pitchers on X
    left_mar <- max(12, min(22, n_batters * 0.9))  # Batters on Y
    
    par(mar = c(bottom_mar, left_mar, 4, 2))
    
    # Blue-red color scheme: blue (low) = bad for hitter, red (high) = good for hitter
    col_fun <- colorRampPalette(c("#2166ac", "#f7f7f7", "#b2182b"))(100)
    
    # Handle case where all values are NA
    if (all(is.na(mat))) {
      plot.new()
      text(0.5, 0.5, "No matchup data available for these players\nTry selecting players with MLB/AAA experience", 
           cex = 1.5, col = "red")
      return()
    }
    
    # Center on 100 (league average OPS+)
    min_val <- min(mat, na.rm = TRUE)
    max_val <- max(mat, na.rm = TRUE)
    max_dist <- max(abs(min_val - 100), abs(max_val - 100))
    if (!is.finite(max_dist) || max_dist == 0) {
      max_dist <- 20
    }
    breaks <- seq(100 - max_dist, 100 + max_dist, length.out = 101)
    
    image(1:ncol(mat), 1:nrow(mat), t(mat),
          col = col_fun,
          breaks = breaks,
          xlab = "", ylab = "",
          axes = FALSE,
          main = "Red = Hitter Advantage | Blue = Pitcher Advantage",
          cex.main = 1.6)
    
    # Add grid lines ONLY where we have data
    for (i in 1:nrow(mat)) {
      for (j in 1:ncol(mat)) {
        if (!is.na(mat[i, j])) {
          rect(j - 0.5, i - 0.5, j + 0.5, i + 0.5, border = "gray50", lwd = 1.2)
        }
      }
    }
    
    # Add ONLY scores - cleaner, more readable (OPS+ format)
    text_scale <- min(1, 10 / max(n_pitchers, n_batters))
    score_cex <- max(1.8, min(2.5, 2.2 * text_scale))
    
    for (i in 1:nrow(mat)) {
      for (j in 1:ncol(mat)) {
        if (!is.na(mat[i, j])) {
          # Just score (OPS+), large and centered
          text(j, i, sprintf("%d", round(mat[i, j])), 
               cex = score_cex, font = 2,
               col = ifelse(abs(mat[i, j] - 100) > 10, "white", "black"))
        }
      }
    }
    
    # Dynamic label sizing
    label_cex <- max(1.0, min(1.8, 1.5 * text_scale))
    
    axis(1, at = 1:ncol(mat), labels = colnames(mat), las = 2, cex.axis = label_cex)
    axis(2, at = 1:nrow(mat), labels = rownames(mat), las = 2, cex.axis = label_cex)
    
    mtext("Pitchers", side = 1, line = bottom_mar - 2, font = 2, cex = 1.4)  # X-axis
    mtext("Batters", side = 2, line = left_mar - 2, font = 2, cex = 1.4)  # Y-axis
  }, height = function() {
    req(matchup_results())
    data <- matchup_results()
    n_batters <- length(unique(data$batter_name))  # Now rows
    n_pitchers <- length(unique(data$pitcher_name))  # Now columns
    # Increase to 60px per batter for better spacing
    max(700, min(1600, 60 * n_batters + 250))
  })
  
  output$detail_table <- renderDT({
    req(matchup_results())
    
    data <- matchup_results()
    
    # Detect if Italy is batting or pitching
    italy_ids_bat <- sapply(italy_hitters, find_player_id)
    italy_ids_pitch <- sapply(italy_pitchers, find_player_id)
    italy_ids_bat <- italy_ids_bat[!is.na(italy_ids_bat)]
    italy_ids_pitch <- italy_ids_pitch[!is.na(italy_ids_pitch)]
    
    italy_batting <- any(data$batter %in% italy_ids_bat)
    italy_pitching <- any(data$pitcher %in% italy_ids_pitch)
    
    # NO FLIPPING - high scores always = good for batter
    
    data %>%
      select(Pitcher = pitcher_name, 
             Batter = batter_name, 
             Score = score,
             Why = reason) %>%
      arrange(desc(Score)) %>%
      datatable(
        options = list(
          pageLength = 50,
          lengthMenu = c(25, 50, 100, 200),
          scrollY = "600px",
          scrollCollapse = TRUE
        ),
        rownames = FALSE
      ) %>%
      formatStyle('Score',
                  backgroundColor = styleInterval(
                    c(90, 95, 100, 105, 110),
                    c('#2166ac', '#4393c3', '#d1e5f0', '#fddbc7', '#f4a582', '#b2182b')  # Blue to red
                  ),
                  fontWeight = 'bold')
  })
  
  # Helper function to create scouting matchup grid
  # PDF-specific matchup grid with reasons in cells
  create_pdf_grid <- function(data, italy_ids, is_pitching = TRUE, title = "") {
    if (is_pitching) {
      scout_data <- data %>% 
        filter(pitcher %in% italy_ids)
      # NO FLIP
    } else {
      scout_data <- data %>% 
        filter(batter %in% italy_ids)
      # NO FLIP
    }
    
    # Filter out rows/columns with all NAs
    if (nrow(scout_data) == 0) {
      plot.new()
      text(0.5, 0.5, "No Italy players in current matchups", cex = 1.5, col = "#666")
      return()
    }
    
    # Scores: high = good for Italy on both pitching and batting
    
    mat <- scout_data %>%
      select(pitcher_name, batter_name, score) %>%
      pivot_wider(names_from = pitcher_name, values_from = score) %>%  # Pitchers as columns
      column_to_rownames("batter_name") %>%  # Batters as rows
      as.matrix()
    
    # Remove rows with all NAs (batters)
    keep_rows <- apply(mat, 1, function(x) !all(is.na(x)))
    mat <- mat[keep_rows, , drop = FALSE]
    
    # Remove columns with all NAs (pitchers)
    keep_cols <- apply(mat, 2, function(x) !all(is.na(x)))
    mat <- mat[, keep_cols, drop = FALSE]
    
    if (nrow(mat) == 0 || ncol(mat) == 0) {
      plot.new()
      text(0.5, 0.5, "Insufficient data for matchups", cex = 1.5, col = "#666")
      return()
    }
    
    n_bat <- nrow(mat)  # Batters now rows
    n_pitch <- ncol(mat)  # Pitchers now columns
    
    par(mar = c(12, 14, 3, 2))
    
    # Blue-red color scheme: blue (low) = pitcher wins, red (high) = batter wins
    col_fun <- colorRampPalette(c("#2166ac", "#f7f7f7", "#b2182b"))(100)
    # Center on 100 (league average OPS+)
    min_val <- min(mat, na.rm = TRUE)
    max_val <- max(mat, na.rm = TRUE)
    max_dist <- max(abs(min_val - 100), abs(max_val - 100))
    if (!is.finite(max_dist) || max_dist == 0) max_dist <- 20
    breaks <- seq(100 - max_dist, 100 + max_dist, length.out = 101)
    
    image(1:ncol(mat), 1:nrow(mat), t(mat),
          col = col_fun, breaks = breaks,
          xlab = "", ylab = "", axes = FALSE,
          main = title, cex.main = 1.5)
    
    # Add ONLY scores - clean for PDF printing
    for (i in 1:nrow(mat)) {
      for (j in 1:ncol(mat)) {
        if (!is.na(mat[i, j])) {
          rect(j - 0.5, i - 0.5, j + 0.5, i + 0.5, border = "gray40", lwd = 1.5)
          
          # Just the score, large and centered
          text(j, i, sprintf("%d", round(mat[i, j])), 
               cex = 2.2, font = 2,
               col = ifelse(abs(mat[i, j] - 100) > 10, "white", "black"))
        }
      }
    }
    
    label_cex <- min(1.4, 12 / max(n_pitch, n_bat))
    axis(1, at = 1:ncol(mat), labels = colnames(mat), las = 2, cex.axis = label_cex)
    axis(2, at = 1:nrow(mat), labels = rownames(mat), las = 2, cex.axis = label_cex)
    
    mtext("Pitchers", side = 1, line = 10, font = 2, cex = 1.4)  # X-axis
    mtext("Batters", side = 2, line = 12, font = 2, cex = 1.4)  # Y-axis
    
  }
  
  create_scout_grid <- function(scout_data, italy_ids, is_pitching = TRUE, title = "") {
    # Data is already filtered by the caller
    
    if (nrow(scout_data) == 0) {
      plot.new()
      text(0.5, 0.5, "No Italy players in current matchups", cex = 1.5, col = "#666")
      return()
    }
    
    # Scores are already flipped appropriately by the caller
    # Italy pitching: scores were flipped (200 - score)
    # Italy batting: scores NOT flipped (low = good for Italy)
    
    mat <- scout_data %>%
      select(pitcher_name, batter_name, score) %>%
      pivot_wider(names_from = pitcher_name, values_from = score) %>%  # Pitchers as columns
      column_to_rownames("batter_name") %>%  # Batters as rows
      as.matrix()
    
    # Remove rows with all NAs (batters with no data)
    keep_rows <- apply(mat, 1, function(x) !all(is.na(x)))
    mat <- mat[keep_rows, , drop = FALSE]
    
    # Remove columns with all NAs (pitchers with no data)
    keep_cols <- apply(mat, 2, function(x) !all(is.na(x)))
    mat <- mat[, keep_cols, drop = FALSE]
    
    if (nrow(mat) == 0 || ncol(mat) == 0) {
      plot.new()
      text(0.5, 0.5, "Insufficient data for matchups", cex = 1.5, col = "#666")
      return()
    }
    
    n_bat <- nrow(mat)  # Batters now rows
    n_pitch <- ncol(mat)  # Pitchers now columns
    
    # Larger margins for readability
    par(mar = c(12, 14, 3, 2))
    
    # Blue-red color scheme: blue (low) = pitcher wins, red (high) = batter wins
    col_fun <- colorRampPalette(c("#2166ac", "#f7f7f7", "#b2182b"))(100)
    
    # Center on 100 (league average OPS+)
    min_val <- min(mat, na.rm = TRUE)
    max_val <- max(mat, na.rm = TRUE)
    max_dist <- max(abs(min_val - 100), abs(max_val - 100))
    if (!is.finite(max_dist) || max_dist == 0) max_dist <- 20
    breaks <- seq(100 - max_dist, 100 + max_dist, length.out = 101)
    
    image(1:ncol(mat), 1:nrow(mat), t(mat),
          col = col_fun,
          breaks = breaks,
          xlab = "", ylab = "",
          axes = FALSE,
          main = title,
          cex.main = 1.5)
    
    # Draw ONLY scores - no reasons in cells
    for (i in 1:nrow(mat)) {
      for (j in 1:ncol(mat)) {
        if (!is.na(mat[i, j])) {
          rect(j - 0.5, i - 0.5, j + 0.5, i + 0.5, border = "gray40", lwd = 1.5)
          
          # Just the score (OPS+ format), large and bold
          text(j, i, sprintf("%d", round(mat[i, j])), 
               cex = 2.2, font = 2,
               col = ifelse(abs(mat[i, j] - 100) > 10, "white", "black"))
        }
      }
    }
    
    # Larger axis labels
    label_cex <- min(1.4, 12 / max(n_pitch, n_bat))
    axis(1, at = 1:ncol(mat), labels = colnames(mat), las = 2, cex.axis = label_cex)
    axis(2, at = 1:nrow(mat), labels = rownames(mat), las = 2, cex.axis = label_cex)
    
    # Simple text labels without special characters
    mtext("Pitchers", side = 1, line = 10, font = 2, cex = 1.4)  # X-axis
    mtext("Batters", side = 2, line = 12, font = 2, cex = 1.4)  # Y-axis
  }
  
  output$scout_pitch_heatmap <- renderPlot({
    req(scouting_results())
    data <- scouting_results()
    
    italy_ids_pitch <- sapply(italy_pitchers, find_player_id)
    italy_ids_pitch <- italy_ids_pitch[!is.na(italy_ids_pitch)]
    
    # NO FLIPPING - high scores = good for batter
    scout_data <- data %>% filter(pitcher %in% italy_ids_pitch)
    
    create_scout_grid(scout_data, italy_ids_pitch, is_pitching = TRUE, 
                      title = "Italy Pitchers vs Opponent Batters (Red = Batter Advantage)")
  })
  
  output$scout_bat_heatmap <- renderPlot({
    req(scouting_results())
    data <- scouting_results()
    
    italy_ids_bat <- sapply(italy_hitters, find_player_id)
    italy_ids_bat <- italy_ids_bat[!is.na(italy_ids_bat)]
    
    # DON'T flip - use reversed colors in create_scout_grid
    scout_data <- data %>% filter(batter %in% italy_ids_bat)
    
    create_scout_grid(scout_data, italy_ids_bat, is_pitching = FALSE,
                      title = "Opponent Pitchers vs Italy Batters (Red = Batter Advantage)")
  })
  
  # Data tables for scouting report
  output$scout_pitch_table <- renderDT({
    req(scouting_results())
    data <- scouting_results()
    
    italy_ids_pitch <- sapply(italy_pitchers, find_player_id)
    italy_ids_pitch <- italy_ids_pitch[!is.na(italy_ids_pitch)]
    
    scout_data <- data %>% 
      filter(pitcher %in% italy_ids_pitch) %>%
      select(Pitcher = pitcher_name, Batter = batter_name, Score = score, Why = reason) %>%
      arrange(desc(Score))  # High score first = good for batter
    
    datatable(scout_data,
              options = list(pageLength = 25, scrollX = TRUE),
              rownames = FALSE) %>%
      formatStyle('Score',
                  backgroundColor = styleInterval(
                    cuts = c(90, 95, 100, 105, 110),
                    values = c('#2166ac', '#4393c3', '#d1e5f0', '#fddbc7', '#f4a582', '#b2182b')  # Blue to red
                  ),
                  fontWeight = 'bold')
  })
  
  output$scout_bat_table <- renderDT({
    req(scouting_results())
    data <- scouting_results()
    
    italy_ids_bat <- sapply(italy_hitters, find_player_id)
    italy_ids_bat <- italy_ids_bat[!is.na(italy_ids_bat)]
    
    # High scores = good for Italy batters
    scout_data <- data %>% 
      filter(batter %in% italy_ids_bat) %>%
      select(Pitcher = pitcher_name, Batter = batter_name, Score = score, Why = reason) %>%
      arrange(desc(Score))  # High score first = good for Italy batters
    
    datatable(scout_data,
              options = list(pageLength = 25, scrollX = TRUE),
              rownames = FALSE) %>%
      formatStyle('Score',
                  backgroundColor = styleInterval(
                    cuts = c(90, 95, 100, 105, 110),
                    values = c('#2166ac', '#4393c3', '#d1e5f0', '#fddbc7', '#f4a582', '#b2182b')  # Blue to red
                  ),
                  fontWeight = 'bold')
  })
  
  # CSV downloads for scout tables
  output$download_scout_pitch_csv <- downloadHandler(
    filename = function() {
      opponent <- switch(input$opponent_team, "usa"="USA", "mexico"="Mexico", "gbr"="GBR", "brazil"="Brazil")
      sprintf("italy_pitching_vs_%s_%s.csv", opponent, Sys.Date())
    },
    content = function(file) {
      req(scouting_results())
      data <- scouting_results()
      italy_ids_pitch <- sapply(italy_pitchers, find_player_id)
      italy_ids_pitch <- italy_ids_pitch[!is.na(italy_ids_pitch)]
      
      scout_data <- data %>% 
        filter(pitcher %in% italy_ids_pitch) %>%
        select(Pitcher = pitcher_name, Batter = batter_name, Score = score, Why = reason)
      
      write.csv(scout_data, file, row.names = FALSE)
    }
  )
  
  output$download_scout_bat_csv <- downloadHandler(
    filename = function() {
      opponent <- switch(input$opponent_team, "usa"="USA", "mexico"="Mexico", "gbr"="GBR", "brazil"="Brazil")
      sprintf("italy_batting_vs_%s_%s.csv", opponent, Sys.Date())
    },
    content = function(file) {
      req(scouting_results())
      data <- scouting_results()
      italy_ids_bat <- sapply(italy_hitters, find_player_id)
      italy_ids_bat <- italy_ids_bat[!is.na(italy_ids_bat)]
      
      scout_data <- data %>% 
        filter(batter %in% italy_ids_bat) %>%
        select(Pitcher = pitcher_name, Batter = batter_name, Score = score, Why = reason)
      
      write.csv(scout_data, file, row.names = FALSE)
    }
  )
  
  # Excel download for scouting report
  output$download_scouting_excel <- downloadHandler(
    filename = function() {
      opponent_name <- switch(input$opponent_team,
                              "usa" = "USA",
                              "mexico" = "Mexico",
                              "gbr" = "GBR",
                              "brazil" = "Brazil"
      )
      sprintf("italy_vs_%s_scouting_%s.xlsx", tolower(opponent_name), Sys.Date())
    },
    content = function(file) {
      req(scouting_results())
      
      data <- scouting_results()
      
      italy_ids_pitch <- sapply(italy_pitchers, find_player_id)
      italy_ids_bat <- sapply(italy_hitters, find_player_id)
      italy_ids_pitch <- italy_ids_pitch[!is.na(italy_ids_pitch)]
      italy_ids_bat <- italy_ids_bat[!is.na(italy_ids_bat)]
      
      # Sheet 1: Italy Pitchers vs Opponent Batters
      pitch_data <- data %>% 
        filter(pitcher %in% italy_ids_pitch)
      # NO FLIP - high = good for batter
      
      pitch_scores <- pitch_data %>%
        select(Pitcher = pitcher_name, Batter = batter_name, Score = score) %>%
        pivot_wider(names_from = Pitcher, values_from = Score)  # Pitchers as columns, batters as rows
      
      pitch_why <- pitch_data %>%
        select(Pitcher = pitcher_name, Batter = batter_name, Why = reason) %>%
        pivot_wider(names_from = Pitcher, values_from = Why)  # Pitchers as columns, batters as rows
      
      # Sheet 2: Opponent Pitchers vs Italy Batters
      bat_data <- data %>% 
        filter(batter %in% italy_ids_bat)
      # DON'T flip - low = good for Italy batters
      
      bat_scores <- bat_data %>%
        select(Pitcher = pitcher_name, Batter = batter_name, Score = score) %>%
        pivot_wider(names_from = Pitcher, values_from = Score)  # Pitchers as columns, batters as rows
      
      bat_why <- bat_data %>%
        select(Pitcher = pitcher_name, Batter = batter_name, Why = reason) %>%
        pivot_wider(names_from = Pitcher, values_from = Why)  # Pitchers as columns, batters as rows
      
      # Write to Excel with conditional formatting
      wb <- createWorkbook()
      
      # Italy Pitching Scores with color scale
      addWorksheet(wb, "Italy Pitching Scores")
      writeData(wb, "Italy Pitching Scores", pitch_scores)
      conditionalFormatting(wb, "Italy Pitching Scores",
                            cols = 2:ncol(pitch_scores),
                            rows = 2:(nrow(pitch_scores) + 1),
                            type = "colourScale",
                            style = c("#2166ac", "#f7f7f7", "#b2182b"),
                            rule = c(80, 100, 120))
      
      # Italy Pitching Reasons
      addWorksheet(wb, "Italy Pitching Reasons")
      writeData(wb, "Italy Pitching Reasons", pitch_why)
      
      # Italy Batting Scores with color scale
      addWorksheet(wb, "Italy Batting Scores")
      writeData(wb, "Italy Batting Scores", bat_scores)
      conditionalFormatting(wb, "Italy Batting Scores",
                            cols = 2:ncol(bat_scores),
                            rows = 2:(nrow(bat_scores) + 1),
                            type = "colourScale",
                            style = c("#2166ac", "#f7f7f7", "#b2182b"),
                            rule = c(80, 100, 120))
      
      # Italy Batting Reasons
      addWorksheet(wb, "Italy Batting Reasons")
      writeData(wb, "Italy Batting Reasons", bat_why)
      
      saveWorkbook(wb, file, overwrite = TRUE)
    }
  )
  
  output$download_scouting_pdf <- downloadHandler(
    filename = function() {
      opponent_name <- switch(input$opponent_team,
                              "usa" = "USA",
                              "mexico" = "Mexico",
                              "gbr" = "GBR",
                              "brazil" = "Brazil"
      )
      sprintf("italy_vs_%s_scouting_%s.pdf", tolower(opponent_name), Sys.Date())
    },
    content = function(file) {
      req(scouting_results())
      
      data <- scouting_results()
      
      italy_ids_pitch <- sapply(italy_pitchers, find_player_id)
      italy_ids_bat <- sapply(italy_hitters, find_player_id)
      italy_ids_pitch <- italy_ids_pitch[!is.na(italy_ids_pitch)]
      italy_ids_bat <- italy_ids_bat[!is.na(italy_ids_bat)]
      
      opponent_name <- switch(input$opponent_team,
                              "usa" = "USA",
                              "mexico" = "Mexico",
                              "gbr" = "Great Britain",
                              "brazil" = "Brazil"
      )
      
      pdf(file, width = 14, height = 10)
      
      # Page 1: Italy Pitchers with reasons in cells
      create_pdf_grid(data, italy_ids_pitch, is_pitching = TRUE,
                      title = sprintf("Italy Pitchers vs %s Batters - %s", opponent_name, Sys.Date()))
      
      # Page 2: Italy Batters with reasons in cells
      create_pdf_grid(data, italy_ids_bat, is_pitching = FALSE,
                      title = sprintf("%s Pitchers vs Italy Batters - %s", opponent_name, Sys.Date()))
      
      dev.off()
    }
  )
  
  output$download_csv <- downloadHandler(
    filename = function() {
      sprintf("matchup_sheet_%s.csv", Sys.Date())
    },
    content = function(file) {
      req(matchup_results())
      
      data <- matchup_results()
      
      # Detect if Italy is batting or pitching
      italy_ids_bat <- sapply(italy_hitters, find_player_id)
      italy_ids_pitch <- sapply(italy_pitchers, find_player_id)
      italy_ids_bat <- italy_ids_bat[!is.na(italy_ids_bat)]
      italy_ids_pitch <- italy_ids_pitch[!is.na(italy_ids_pitch)]
      
      italy_batting <- any(data$batter %in% italy_ids_bat)
      italy_pitching <- any(data$pitcher %in% italy_ids_pitch)
      
      # NO FLIPPING - high scores = good for batter
      
      mat <- data %>%
        select(Pitcher = pitcher_name, Batter = batter_name, Score = score, Why = reason) %>%
        pivot_wider(names_from = Batter, values_from = c(Score, Why))
      
      write.csv(mat, file, row.names = FALSE)
    }
  )
  
  # Excel download for general matchups
  output$download_excel <- downloadHandler(
    filename = function() {
      sprintf("matchup_sheet_%s.xlsx", Sys.Date())
    },
    content = function(file) {
      req(matchup_results())
      
      data <- matchup_results()
      
      # Detect if Italy is batting or pitching
      italy_ids_bat <- sapply(italy_hitters, find_player_id)
      italy_ids_pitch <- sapply(italy_pitchers, find_player_id)
      italy_ids_bat <- italy_ids_bat[!is.na(italy_ids_bat)]
      italy_ids_pitch <- italy_ids_pitch[!is.na(italy_ids_pitch)]
      
      italy_batting <- any(data$batter %in% italy_ids_bat)
      italy_pitching <- any(data$pitcher %in% italy_ids_pitch)
      
      # NO FLIPPING - high scores = good for batter
      
      # Create wide format - batters as rows, pitchers as columns
      score_mat <- data %>%
        select(Pitcher = pitcher_name, Batter = batter_name, Score = score) %>%
        pivot_wider(names_from = Pitcher, values_from = Score)  # Pitchers as columns
      
      why_mat <- data %>%
        select(Pitcher = pitcher_name, Batter = batter_name, Why = reason) %>%
        pivot_wider(names_from = Pitcher, values_from = Why)  # Pitchers as columns
      
      # Write to Excel with conditional formatting
      wb <- createWorkbook()
      
      # Scores sheet with color scale
      addWorksheet(wb, "Scores")
      writeData(wb, "Scores", score_mat)
      
      # Apply conditional formatting (blue-white-red scale centered on 100)
      conditionalFormatting(wb, "Scores",
                            cols = 2:ncol(score_mat),
                            rows = 2:(nrow(score_mat) + 1),
                            type = "colourScale",
                            style = c("#2166ac", "#f7f7f7", "#b2182b"),
                            rule = c(80, 100, 120))
      
      # Reasons sheet
      addWorksheet(wb, "Reasons")
      writeData(wb, "Reasons", why_mat)
      
      saveWorkbook(wb, file, overwrite = TRUE)
    }
  )
  
  output$download_pdf <- downloadHandler(
    filename = function() {
      sprintf("matchup_sheet_%s.pdf", Sys.Date())
    },
    content = function(file) {
      req(matchup_results())
      
      pdf(file, width = 14, height = 10)
      
      data <- matchup_results()
      
      # Detect if Italy is batting or pitching
      italy_ids_bat <- sapply(italy_hitters, find_player_id)
      italy_ids_pitch <- sapply(italy_pitchers, find_player_id)
      italy_ids_bat <- italy_ids_bat[!is.na(italy_ids_bat)]
      italy_ids_pitch <- italy_ids_pitch[!is.na(italy_ids_pitch)]
      
      italy_batting <- any(data$batter %in% italy_ids_bat)
      italy_pitching <- any(data$pitcher %in% italy_ids_pitch)
      
      # NO FLIPPING - high scores always = good for batter
      
      # Determine title
      if (italy_batting) {
        title_text <- "Italy Batters Matchup Sheet"
      } else if (italy_pitching) {
        title_text <- "Italy Pitchers Matchup Sheet"
      } else {
        title_text <- "Matchup Sheet (Batter-Centric)"
      }
      
      mat <- data %>%
        select(pitcher_name, batter_name, score) %>%
        pivot_wider(names_from = pitcher_name, values_from = score) %>%  # Pitchers as columns
        column_to_rownames("batter_name") %>%  # Batters as rows
        as.matrix()
      
      par(mar = c(10, 12, 3, 3))
      
      # Blue-red color scheme: blue (low) = pitcher wins, red (high) = batter wins
      col_fun <- colorRampPalette(c("#2166ac", "#f7f7f7", "#b2182b"))(100)
      
      # Center color scale on 100
      min_val <- min(mat, na.rm = TRUE)
      max_val <- max(mat, na.rm = TRUE)
      max_dist <- max(abs(min_val - 100), abs(max_val - 100))
      if (!is.finite(max_dist) || max_dist == 0) max_dist <- 20
      breaks <- seq(100 - max_dist, 100 + max_dist, length.out = 101)
      
      image(1:ncol(mat), 1:nrow(mat), t(mat),
            col = col_fun,
            breaks = breaks,
            xlab = "", ylab = "",
            axes = FALSE,
            main = sprintf("%s - %s", title_text, Sys.Date()))
      
      # Draw grid and scores ONLY (no reasons)
      for (i in 1:nrow(mat)) {
        for (j in 1:ncol(mat)) {
          if (!is.na(mat[i, j])) {
            rect(j - 0.5, i - 0.5, j + 0.5, i + 0.5, border = "gray80", lwd = 0.5)
            
            # Just the score
            text(j, i, sprintf("%d", round(mat[i, j])), 
                 cex = 1.2, font = 2,
                 col = ifelse(abs(mat[i, j] - 100) > 10, "white", "black"))
          }
        }
      }
      
      label_cex <- min(1.0, 10 / max(ncol(mat), nrow(mat)))
      axis(1, at = 1:ncol(mat), labels = colnames(mat), las = 2, cex.axis = label_cex)
      axis(2, at = 1:nrow(mat), labels = rownames(mat), las = 2, cex.axis = label_cex)
      
      mtext("Pitchers", side = 1, line = 8, font = 2, cex = 1.2)  # X-axis
      mtext("Batters", side = 2, line = 10, font = 2, cex = 1.2)  # Y-axis
      
      dev.off()
    }
  )
}

shinyApp(ui = ui, server = server)