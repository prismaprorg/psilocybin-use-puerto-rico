# 01_clean.R -- raw REDCap export -> analysis sample `d` (participant-level, kept
# in memory only; never written to disk or printed).
#
# Sample-flow rule (see README): verified to be the only rule that reproduces
# the published 343 -> 312 -> 304 -> 253 flow.

stopifnot(file.exists(DATA_CSV))
raw <- read.csv(DATA_CSV, na.strings = "NA", stringsAsFactors = FALSE)

# ---- Sample flow -----------------------------------------------------------
n_raw <- nrow(raw)

# Step 1: screening. As in dplyr::filter, rows with NA on any criterion drop.
s1 <- raw %>% filter(vive_enpr != 2, contestado_anteriormente != 1, mayor_edad != 2)
n_s1 <- nrow(s1)

# Step 2: duplicates on nickname + secret word, normalised with trimws+tolower,
# first record kept. (The original script's exact-match distinct() finds only 6.)
key <- paste(tolower(trimws(s1$apodo)), tolower(trimws(s1$palabra_secreta)), sep = "\r")
s2 <- s1[!duplicated(key), ]
n_s2 <- nrow(s2)
n_dup_exact <- nrow(s1) - nrow(s1 %>% distinct(apodo, palabra_secreta, .keep_all = TRUE))
# Diagnostic: the exact-match de-duplication of Raking Only.R gives n = 254, not 253.
s3_exact <- s1 %>% distinct(apodo, palabra_secreta, .keep_all = TRUE) %>% filter(consumo %in% c(1, 2))
rec("diag.exact_distinct_final_n", nrow(s3_exact))
rec("diag.exact_distinct_users", sum(s3_exact$consumo == 1))
rec("diag.exact_distinct_users_pct", pct(sum(s3_exact$consumo == 1), nrow(s3_exact)))
n_blank_keys_s1 <- sum(trimws(s1$apodo) == "" & trimws(s1$palabra_secreta) == "", na.rm = TRUE)

# Step 3: primary outcome (consumo) must be 1 (yes) or 2 (no); NA / 99 dropped.
s3 <- s2 %>% filter(consumo %in% c(1, 2))
n_s3 <- nrow(s3)
n_users <- sum(s3$consumo == 1)

# Composition of the exclusions (diagnostic only; helps judge the paper's wording
# "31 records mostly incomplete" / "51 missing the primary outcome").
scr_fail <- raw %>% filter(is.na(vive_enpr) | is.na(contestado_anteriormente) | is.na(mayor_edad) |
                           vive_enpr == 2 | contestado_anteriormente == 1 | mayor_edad == 2)
excl31 <- c(any_screening_item_NA = sum(is.na(scr_fail$vive_enpr) | is.na(scr_fail$contestado_anteriormente) | is.na(scr_fail$mayor_edad)),
            answered_ineligible   = sum(!(is.na(scr_fail$vive_enpr) | is.na(scr_fail$contestado_anteriormente) | is.na(scr_fail$mayor_edad))))
excl51 <- c(consumo_NA = sum(is.na(s2$consumo)), consumo_99 = sum(s2$consumo == 99, na.rm = TRUE))

flow <- c(raw = n_raw, after_screening = n_s1, after_dedup = n_s2,
          after_outcome = n_s3, users = n_users)

# Hard assertions: fail loudly if the rule stops reproducing the published flow.
stopifnot(n_raw == 343, n_s1 == 312, n_s2 == 304, n_s3 == 253, n_users == 133,
          sum(s3$sustancias___2 == 1, na.rm = TRUE) == 90)

rec("flow.raw", n_raw)
rec("flow.screened_out", n_raw - n_s1)
rec("flow.after_screening", n_s1)
rec("flow.duplicates", n_s1 - n_s2)
rec("flow.after_dedup", n_s2)
rec("flow.missing_outcome", n_s2 - n_s3)
rec("flow.final_n", n_s3)
rec("flow.users", n_users)
rec("flow.users_pct", pct(n_users, n_s3))
rec("flow.nonusers", n_s3 - n_users)

# ---- Recoding (mirrors Raking Only.R lines 10-140) -------------------------
rev7 <- function(x) 8 - x   # 1<->7, 2<->6, ...

d_pre <- s2 %>%
  mutate(
    psilocybin_use = case_when(consumo == 1 ~ 1L, consumo == 2 ~ 0L),
    # TIPI-SPA: reverse-keyed items then pair means
    per_reservado     = rev7(per_reservado),
    per_colerico      = rev7(per_colerico),
    per_desorganizado = rev7(per_desorganizado),
    per_ansioso       = rev7(per_ansioso),
    per_tradicional   = rev7(per_tradicional),
    openness            = (per_abierto + per_tradicional) / 2,
    extraversion        = (per_extrovertido + per_reservado) / 2,
    agreeableness       = (per_comprensivo + per_colerico) / 2,
    conscientiousness   = (per_fiable + per_desorganizado) / 2,
    emotional_stability = (per_sereno + per_ansioso) / 2,
    age = edad,
    # Gender: only man/woman analysed; non-binary (3), other (55), decline (99) -> NA
    gender = factor(case_when(identidad_de_genero == 1 ~ "Male",
                              identidad_de_genero == 2 ~ "Female"),
                    levels = c("Female", "Male")),
    # Sexual orientation, full (REDCap codes 1 asexual, 2 bi, 3 gay, 4 het,
    # 5 lesbian, 6 pansexual, 7 demisexual, 55 other, 99 decline)
    orientation_full = factor(case_when(orientacion_sexual == 4 ~ "Heterosexual",
                                        orientacion_sexual == 1 ~ "Asexual",
                                        orientacion_sexual == 2 ~ "Bisexual",
                                        orientacion_sexual == 7 ~ "Demisexual",
                                        orientacion_sexual == 3 ~ "Gay",
                                        orientacion_sexual == 5 ~ "Lesbian",
                                        orientacion_sexual == 6 ~ "Pansexual",
                                        orientacion_sexual == 55 ~ "Other"),
                              levels = c("Heterosexual", "Asexual", "Bisexual", "Demisexual",
                                         "Gay", "Lesbian", "Pansexual", "Other")),
    # Collapsed orientation used in the regression (Raking Only.R `orientation`)
    orientation = factor(case_when(orientacion_sexual == 4 ~ "Heterosexual",
                                   orientacion_sexual == 2 ~ "Bisexual",
                                   orientacion_sexual == 3 ~ "Gay",
                                   orientacion_sexual %in% c(1, 5, 6, 7, 55) ~ "All Others"),
                         levels = c("Heterosexual", "All Others", "Bisexual", "Gay")),
    transgender = factor(case_when(persona_trans == 1 ~ "Yes", persona_trans == 2 ~ "No"),
                         levels = c("No", "Yes")),
    education = case_when(nivel_acad == 1 ~ "11th grade & below",
                          nivel_acad == 2 ~ "High School",
                          nivel_acad == 3 ~ "Associate's degree",
                          nivel_acad == 4 ~ "Bachelor's degree",
                          nivel_acad == 5 ~ "Master's degree",
                          nivel_acad == 6 ~ "Doctorate's degree"),
    # Religion: Christianity (4) vs everything else (atheist, agnostic, spiritual,
    # Judaism, Hinduism, Buddhism, other)
    religion = factor(case_when(afiliacion_religiosa == 4 ~ "Christian",
                                afiliacion_religiosa %in% c(1, 2, 3, 5, 6, 7, 55) ~ "Non-Christian"),
                      levels = c("Christian", "Non-Christian")),
    # Relationship status. 2 married, 5 living together -> "Married/Living together";
    # 1 never married, 3 divorced, 4 widowed, 6 separated -> "Single/Divorced/Widowed"
    relationship = case_when(estado_civil %in% c(2, 5) ~ "Married/Living Together",
                             estado_civil %in% c(1, 3, 4, 6) ~ "Single/Divorced/Widowed")
  )

# `d_pre` (304 rows, outcome NA for 51) is kept only to replicate the original
# script's TIPI effect-size call (see 02_tables.R). All analyses use `d`.
d <- d_pre %>% filter(!is.na(psilocybin_use))
stopifnot(nrow(d_pre) == 304, nrow(d) == 253, sum(d$psilocybin_use) == 133)
say("[01_clean] flow: %s", paste(names(flow), flow, sep = "=", collapse = ", "))
say("[01_clean] 31 screened-out: %s", paste(names(excl31), excl31, sep = "=", collapse = ", "))
say("[01_clean] 51 missing outcome: %s", paste(names(excl51), excl51, sep = "=", collapse = ", "))
say("[01_clean] exact-match distinct() would drop %d duplicates (not 8); blank-key rows in step 1: %d",
    n_dup_exact, n_blank_keys_s1)
