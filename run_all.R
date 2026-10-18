# run_all.R -- regenerate every number in the paper from the raw REDCap export.
#   Rscript run_all.R
# Raw data path: env var PSILOCYBIN_DATA_CSV (default set in R/00_setup.R).
# Output: output/*.csv, output/*.png, output/comparison.csv
local({
  here <- dirname(normalizePath(sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE)[1])))
  setwd(here)
})
for (f in c("00_setup", "01_clean", "02_tables", "03_models", "04_compare")) {
  cat(sprintf("\n---- %s ----\n", f))
  source(file.path("R", paste0(f, ".R")), echo = FALSE)
}
