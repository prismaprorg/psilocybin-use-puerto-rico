# 00_setup.R -- configuration and helpers shared by all scripts.
# Source this first (run_all.R does).

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
})

# ---- Configuration ---------------------------------------------------------
# Participant-level REDCap export. NOT stored in this repo. Override with the
# environment variable PSILOCYBIN_DATA_CSV.
DEFAULT_DATA_CSV <- file.path(
  "C:/Users/jeanv/OneDrive/Documents/PRISMA/Research/claude-scientific-writer",
  "projects/lsd-use-in-puerto-rico/analysis/data",
  "EstudioEpidemiologic_DATA_2023-06-28_1313.csv"
)
DATA_CSV <- Sys.getenv("PSILOCYBIN_DATA_CSV", unset = DEFAULT_DATA_CSV)

# Repo root = parent of R/ (works when run via Rscript from anywhere).
find_root <- function() {
  args <- commandArgs(trailingOnly = FALSE)
  f <- sub("^--file=", "", args[grepl("^--file=", args)])
  start <- if (length(f)) dirname(normalizePath(f[1])) else getwd()
  p <- start
  for (i in 1:4) {
    if (file.exists(file.path(p, "run_all.R"))) return(p)
    p <- dirname(p)
  }
  getwd()
}
ROOT <- find_root()
OUT  <- file.path(ROOT, "output")
dir.create(OUT, showWarnings = FALSE)

# ---- Reproduced-value registry ---------------------------------------------
# Every number the paper prints is stored here under a stable id; 04_compare.R
# joins this registry to published_values.csv.
RESULTS <- new.env()
RESULTS$tab <- data.frame(id = character(), value = numeric(),
                          alt = numeric(), note = character(),
                          stringsAsFactors = FALSE)
rec <- function(id, value, alt = NA_real_, note = "") {
  stopifnot(length(id) == length(value))
  RESULTS$tab <- rbind(RESULTS$tab,
    data.frame(id = id, value = as.numeric(value), alt = as.numeric(alt),
               note = note, stringsAsFactors = FALSE))
  invisible(NULL)
}

# Percentage helper (1 decimal precision kept; rounding is done at comparison).
pct <- function(n, d) 100 * n / d

# Significance stars from a p-value, as in the paper's tables.
stars <- function(p) {
  ifelse(is.na(p), NA_character_,
         ifelse(p < 0.001, "***", ifelse(p < 0.01, "**", ifelse(p < 0.05, "*", ""))))
}
sig_level <- function(p) ifelse(p < 0.001, 3, ifelse(p < 0.01, 2, ifelse(p < 0.05, 1, 0)))

say <- function(...) cat(sprintf(...), "\n", sep = "")
