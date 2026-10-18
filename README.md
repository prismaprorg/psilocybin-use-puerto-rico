# Psilocybin use in Puerto Rico: reproduction of the published results

Regenerates the numbers in Vélez Rodríguez et al., "Psilocybin use in Puerto Rico: Patterns,
motivations, and personality correlates from an online survey", *Journal of Psychedelic Studies*
2026;10(3):250-260, doi:10.1556/2054.2026.00519, from the raw REDCap export, and compares every
printed value with the reproduced one.

Local repository only, no remote. The participant-level CSV is **not** in this repo.

## Run

```powershell
# from the repo root, in PowerShell (Rscript segfaults under Git Bash on this machine)
& "C:\Program Files\R\R-4.5.2\bin\Rscript.exe" run_all.R
```

- First run on a new machine: `renv::restore()` (packages pinned in `renv.lock`, R 4.5.2).
- Raw data path: environment variable `PSILOCYBIN_DATA_CSV`; default is
  `...\projects\lsd-use-in-puerto-rico\analysis\data\EstudioEpidemiologic_DATA_2023-06-28_1313.csv`
  (set in `R/00_setup.R`).
- `run_all.R` sources `R/00_setup.R` (config, helpers) -> `01_clean.R` (sample flow, recoding, hard
  `stopifnot` checks) -> `02_tables.R` (flow, Tables 1-3, TIPI, in-text numbers) -> `03_models.R`
  (logistic model, fit, diagnostics, figures) -> `04_compare.R`.
- Everything the paper prints is registered under a stable id and compared to `published_values.csv`
  (296 targets transcribed from the PDF). `discrepancy_notes.csv` holds the explanation attached to
  each mismatch (matched by regex on id); a mismatch with no note prints `UNEXPLAINED`.

## Outputs (`output/`, aggregates only)

| File | Content |
|---|---|
| `comparison.csv` | One row per published value: published, reproduced, status (match/differs), distance in last-digit units, `alt_reproduced` + `alt_status` (a second plausible computation, see below), category, explanation |
| `comparison_summary.csv` | Match/differ counts per table |
| `reproduced_not_published.csv` | Reproduced numbers the paper does not print (regression n, other model terms, ...) |
| `flow.csv`, `table1_demographics.csv`, `table1_chisq.csv`, `table2_patterns_users.csv`, `table3_substances.csv`, `tipi_mann_whitney.csv` | Reproduced tables (counts and percentages) |
| `logistic_regression_aOR.csv`, `model_fit_and_diagnostics.csv`, `model_numeric_predictor_correlations.csv` | Model results |
| `figure1_residuals_leverage.png`, `figure2_odds_ratios.png` | Re-drawn Figures 1 and 2 (Fig. 1 plots anonymous per-observation diagnostics, as the paper does) |

The paper has Tables 1-3 and no Table 4; the model results are in Fig. 2 and the text.

## The sample-flow rule

Only this rule reproduces the published 343 -> 312 -> 304 -> 253 flow (asserted with `stopifnot`):

1. Keep rows with `vive_enpr != 2 & contestado_anteriormente != 1 & mayor_edad != 2`. As in
   `dplyr::filter`, a row with NA on any of these is dropped. 343 -> **312**.
2. De-duplicate on `apodo` + `palabra_secreta` after `trimws()` and `tolower()`, keeping the first
   record. 312 -> **304** (8 dropped).
3. Drop rows where `consumo` is not 1 or 2 (NA or 99). 304 -> **253**, of whom **133** have
   `consumo == 1` (52.57%, printed 52.6%) and 120 do not. LSD (`sustancias___2`) is 90/253.

Notes on the flow:

- `Raking Only.R` uses exact-match `distinct(apodo, palabra_secreta)`, which finds only 6 duplicates
  (n = 254, 134 users) and hard-codes 253 in `prop.test`. Normalising case and whitespace is a
  defensible fix (a nickname typed "Ana " or "ana" is the same respondent) and is the only
  de-duplication that gives 8.
- The paper calls the 31 screened-out records "mostly incomplete". In the export, 29 of them have a
  missing answer on at least one of the three screening items and 2 answered an eligibility item with
  an ineligible response. The 51 "missing outcome" are 49 NA and 2 "prefer not to answer".
- The Table 1 caption ("before and after imputation and raking") is a leftover. The analysis is
  unweighted complete-case; no imputation or raking is run here.

## Result: flow reproduces exactly

312 / 304 / 253 / 133 / 90 all reproduce. Of 296 published values compared, **249 match and 47
differ** (rounding to the printed precision, round-half-up). By table:

| Part | Compared | Match | Differ |
|---|---|---|---|
| Sample flow | 10 | 10 | 0 |
| Table 1 (demographics, n per variable, chi-square stars) | 78 | 70 | 8 |
| Table 2 (patterns among users, belief chi-squares) | 54 | 48 | 6 |
| Table 3 (substances, chi-square stars) | 64 | 52 | 12 |
| TIPI Mann-Whitney (r and p) | 10 | 8 | 2 |
| Logistic model (aORs, CIs, fit, diagnostics) | 40 | 37 | 3 |
| In-text statistics (Abstract, Results, Discussion, Conclusion) | 40 | 24 | 16 |

The regression is based on n = 224 complete cases (not stated in the paper). McFadden R2 = 0.3055,
log-likelihoods -155.26 / -107.83, LR chi-square 94.86, max leverage 0.213, max Cook's D 0.049 all
reproduce.

## Methodological choices (made before looking at the published numbers, none tuned)

- Recoding follows `Raking Only.R` lines 10-140: TIPI reverse-keying (8 - x) for reservado, colerico,
  desorganizado, ansioso, tradicional; religion Christian (code 4) vs all others (atheist, agnostic,
  spiritual, Judaism, Hinduism, Buddhism, other); orientation for the model Heterosexual (reference),
  Bisexual, Gay, All Others (asexual, lesbian, pansexual, demisexual, other); gender only man/woman.
- Table 1 percentages are cell percentages of each variable's valid n (total = users + non-users),
  which is what the printed numbers are, although the table note says "column percentages". Table 3
  likewise prints percentages of the total n = 253. Table 2 is true column percentages of 133.
- Table 2 primary denominator is 133 as printed in the header; `alt` uses valid answers only
  (single-choice) or n/253 (multiple-choice, the old script's hard-coded denominator).
- Chi-square: `chisq.test` default (Yates correction for 2x2). The paper does not say how categories
  were collapsed for orientation and education; I collapsed orientation to the model's four levels and
  education to four. `table1_chisq.csv` shows the verdict (significant or not) is the same under the
  other collapsings I tried.
- Relationship status: "Married/Living Together" = married + cohabiting; separated (n = 2 users) is
  put with "Single/Divorced/Widowed" as a priori. The alternative is shown in `alt`.
- Odds-ratio CIs are profile-likelihood (`exp(confint(glm))`), Wald in `alt`.
- TIPI effect size is `rcompanion::wilcoxonR` on the analysis sample (N = 249 complete cases).

## What does not match

All 47 are in `output/comparison.csv` with the exact numbers. Grouped by cause:

**Likely errors in the paper**

- **TIPI effect sizes (openness, agreeableness).** Reproduced r = -0.385 and -0.172; printed -0.35 and
  -0.16. All five printed r values are reproduced exactly only if N = 304 enters Z/sqrt(N): the
  original script called `wilcoxonR` on the 304-row data frame still containing the 51 rows with a
  missing outcome. p-values and the three non-significant r are unaffected. The two printed effect
  sizes are understated (about 9% and 7%).
- **Table 2 labels transposed.** "Addictive": printed Maybe 1 / Yes 8; the REDCap codes give Yes
  (code 1) 0.8% and Maybe (code 3) 8.3%. "Experience": printed Unpleasant 2 / Very unpleasant 1; codes
  give very unpleasant (code 1) 1.5% and unpleasant (code 2) 0.8%. "Harmful" is correct.
- **Table 2, Purposes "Other" = 9** is 6.0% (8 users). 9 is the value of Methods "Other"; likely a copy
  error.
- **Table 3 omits significance stars** for DMT, Salvia, Ayahuasca, Mescaline (chi-square p < 0.001;
  Fisher's exact also < 0.05), Ketamine (p = 0.037) and Peyote (p = 0.010). Sparse cells do not
  explain it (minimum expected count 8-10 for the first four; alcohol, with minimum expected 2.9, is
  starred). The Results text lists only alcohol, marijuana, LSD, MDMA, cocaine and poppers as
  significant, omitting cigarettes, which the table stars.
- **Internal inconsistencies:** Abstract aOR for gay 0.14 vs Results 0.13 (reproduced 0.133);
  Discussion prevalence 52.4% vs 52.6% everywhere else (reproduced 52.57%; 52.4% is not reproduced by
  either de-duplication variant); Results text "52% and 48%" of the total sample hold non-addictive /
  non-harmful beliefs vs Abstract 61% and 57% (reproduced 61.3% and 56.9%); curiosity 43% (text and
  Abstract, truncated from 43.6) vs 44 in Table 2; integration 56% (text) vs 57 (Table 2); LSD 35%
  (text, truncated from 35.57) vs 36 in Table 3; "88%" pleasant or very pleasant (88.7% truncated; the
  table's own 67+22 = 89); Table 1 transgender "Yes" cells print 5 + 5 = 10 against a printed total of
  9.
- **Table 1 label problems (not number problems):** the header says "no missing data" but each
  variable has its own valid n (235-253) and only 224 respondents are complete for the model; the
  "Non-Christian (Spiritual, Judaism, Buddhism)" row also contains atheists, agnostics, Hindus and
  others; the note says "column percentages" but the values are cell percentages. The Methods say
  t-tests were used for personality; the Results (and the code) use Mann-Whitney. The Methods cite the
  `survey` package, which this analysis does not use.
- **Fig. 2 caption** lists the significant predictors without "non-heterosexual (All Others)"
  (aOR 4.33, p = 0.023, which the Abstract does report) and the non-significant ones without
  transgender (p = 0.66).

**Rounding and small-cell differences (one unit of the last printed digit)**

- Table 1: orientation "Other" 1.2% printed 2 (total and users); transgender "Yes" users and non-users
  4.4% printed 5; education "11th grade and below" 0.4% printed 1 (total, non-users); relationship
  status single users 37.9 vs 37 and non-users 29.4 vs 30. Cause unknown; separated coded as married
  fixes only one of the two relationship cells.
- Table 3 totals: cigarettes 66.4 vs 67, cocaine 28.5 vs 29, poppers 21.7 vs 21, ayahuasca 6.7 vs 6,
  heroin 2.8 vs 2, and MDMA users 24.51 vs 24. The printed totals look like sums of the rounded user and
  non-user cells (or truncations), not roundings of the exact totals. No coding difference.
- Table 2 integration "Yes" 56.4 vs 57: excluding the one "prefer not to answer" gives 56.8 and
  matches, but that denominator is not used for other rows.
- Male aOR upper CI 14.4995 vs printed 14.49; Abstract openness lower CI 1.4963 vs 1.49 (Results text
  prints 1.5, which matches). Sub-0.001 numerical difference; the paper used R 4.2.2, this runs on
  R 4.5.2 where `confint.glm` moved from MASS to stats. I could not confirm this with R 4.2.2.

**Unexplained**

- **In-text polysubstance counts** (Results, "Polysubstance use"): marijuana among users 98% (n = 127),
  LSD among users 65% (n = 84), marijuana among non-users 57% (n = 67), no other substances among
  non-users 41% (n = 48). Reproduced 97.0% (129), 63.9% (85), 54.2% (65), 41.7% (50). These also
  disagree with Table 3's own columns, so they probably come from an earlier version of the sample;
  no variant I tested reproduces them.

## Environment

R 4.5.2 (paper: 4.2.2). Packages pinned in `renv.lock`: dplyr 1.2.1 (paper 1.1.4), tidyr 1.3.2,
rcompanion 2.5.4, pscl 1.5.9, broom 1.0.13, ggplot2 4.0.3 (88 packages including dependencies). All
installed from CRAN binaries or source without problems; `finalfit` (used for the paper's Fig. 2)
and `mice`/`survey` (imputation/raking, not part of the published analysis) were deliberately not
installed.

## Privacy

`data/`, `*.csv` inputs, `.Rhistory` and `.RData` are git-ignored; only `published_values.csv`,
`discrepancy_notes.csv` and `output/*.csv` (aggregates) are tracked. No script prints, writes or
views individual rows, and `apodo` and `palabra_secreta` are used only to build an in-memory
de-duplication key.
