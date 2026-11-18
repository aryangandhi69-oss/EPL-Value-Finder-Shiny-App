# app/R/helpers.R
# Small utilities to keep server.R & global.R clean (ISAR style)

# Safely coerce to numeric (returns NA on bad input without noisy warnings)
safe_numeric <- function(x) suppressWarnings(as.numeric(x))

# Clamp values to [0, 1] range (useful before radars)
clamp01 <- function(x) pmin(pmax(safe_numeric(x), 0), 1)

# Simple formatting helpers
fmt_money_gbp <- function(x) {
  x <- safe_numeric(x)
  ifelse(is.na(x), NA_character_, paste0("£", format(round(x, 1), big.mark = ","), "m"))
}

fmt_pct <- function(x, digits = 1) {
  x <- safe_numeric(x)
  ifelse(is.na(x), NA_character_, paste0(round(x, digits), "%"))
}

# Safe range with fallback (handy for sliders)
safe_range <- function(x, fallback = c(0, 1), round_fun = identity) {
  x <- safe_numeric(x)
  x <- x[is.finite(x)]
  if (length(x) == 0) return(round_fun(fallback))
  round_fun(c(min(x), max(x)))
}

# (Optional) cosine similarity for future “find similars” feature
cosine_sim <- function(a, b) {
  a <- safe_numeric(a); b <- safe_numeric(b)
  if (any(is.na(c(a, b)))) return(NA_real_)
  denom <- sqrt(sum(a^2)) * sqrt(sum(b^2))
  if (denom == 0) return(NA_real_)
  sum(a * b) / denom
}

