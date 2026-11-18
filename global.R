# app/global.R — robust globals for UI & server

suppressPackageStartupMessages({
  library(dplyr)
  library(stringr)
  library(tibble)
  library(DT)
})

# safety helper used by server.R (idempotent)
if (!exists("safe_numeric")) safe_numeric <- function(x) suppressWarnings(as.numeric(x))

# ---- UPDATED DATA LOADING FOR PUBLISHING ----
# 1. First, try to load from the www/ directory (for published app)
# 2. Fall back to local data/processed/ path (for local development)
data_path <- NULL

if (file.exists("www/players_clean.rds")) {
  data_path <- "www/players_clean.rds"
  cat("✅ Found data in www/ directory\n")
} else if (file.exists("data/processed/players_clean.rds")) {
  data_path <- "data/processed/players_clean.rds" 
  cat("✅ Found data in local data/processed directory\n")
} else if (file.exists("../data/processed/players_clean.rds")) {
  data_path <- "../data/processed/players_clean.rds"
  cat("✅ Found data in parent data/processed directory\n")
}

# Load data if found, otherwise create empty dataset
if (!is.null(data_path) && file.exists(data_path)) {
  players <- readRDS(data_path)
  cat("✅ Successfully loaded data from:", data_path, "\n")
  cat("Rows:", nrow(players), "| Seasons:", paste(sort(unique(players$season)), collapse = ", "), "\n")
} else {
  warning("❌ Could not find players_clean.rds in any expected location. Using empty dataset.")
  players <- tibble::tibble(
    player_name = character(), team_name = character(), season = character(),
    age = numeric(), position_bucket = character(), minutes = numeric(),
    market_value_mill_gbp = numeric(),
    goal_contrib_p90 = numeric(), pass_accuracy_pct = numeric(),
    tackles_p90 = numeric(), interceptions_p90 = numeric()
  )
}

# ---- derived choices & ranges with safe fallbacks ----
# Seasons
seasons_avail <- players %>% pull(season) %>% unique() %>% sort()
if (length(seasons_avail) == 0) seasons_avail <- c("2024", "2025")

# Positions - Simplified to match your CSV data
pos_levels <- players %>% 
  pull(position_bucket) %>% 
  unique() %>% 
  sort()

# If no positions found in data, use the broad categories from your CSV
if (length(pos_levels) == 0 || all(is.na(pos_levels))) {
  pos_levels <- c("GK", "DF", "MF", "FW", "OTHER")
} else {
  # Ensure we have the main categories
  main_positions <- c("GK", "DF", "MF", "FW", "OTHER")
  pos_levels <- sort(unique(c(pos_levels, main_positions)))
  # Remove any NA values
  pos_levels <- pos_levels[!is.na(pos_levels)]
}

# helper to compute numeric range safely
range_safe <- function(x, fallback_min, fallback_max) {
  x <- suppressWarnings(as.numeric(x))
  if (any(is.finite(x))) {
    c(floor(min(x, na.rm = TRUE)), ceiling(max(x, na.rm = TRUE)))
  } else {
    c(fallback_min, fallback_max)
  }
}

# Age range
age_rng  <- range_safe(players$age, 16, 38)
age_min  <- age_rng[1]; age_max <- age_rng[2]

# Minutes range
mins_rng <- range_safe(players$minutes, 0, 3000)
mins_min <- mins_rng[1]; mins_max <- mins_rng[2]

# Market value range
mv_rng   <- range_safe(players$market_value_mill_gbp, 0, 80)
mv_min   <- mv_rng[1]; mv_max <- mv_rng[2]

# quick console log
cat(
  "Global init → seasons:", paste(seasons_avail, collapse = ", "),
  "| pos_levels:", paste(head(pos_levels, 5), collapse = ", "),
  if (length(pos_levels) > 5) "…" else "",
  "| mins:", mins_min, "-", mins_max,
  "| mv:", mv_min, "-", mv_max, "\n"
)