# 02_tables.R -- sample flow, Tables 1-3, TIPI tests and in-text statistics.
# Aggregates only: counts, percentages, test statistics. No participant rows.

u <- d$psilocybin_use          # 1 = lifetime psilocybin user, 0 = non-user
n_all <- nrow(d); n_u <- sum(u == 1); n_n <- sum(u == 0)

rec_df <- function(df) rec(df$id, df$value, df$alt, df$note)

chisq_p <- function(x, g) {
  t <- table(x, g)
  if (nrow(t) < 2 || ncol(t) < 2) return(c(p = NA, min_exp = NA))
  ct <- suppressWarnings(chisq.test(t))     # default: Yates correction for 2x2
  c(p = ct$p.value, min_exp = min(ct$expected))
}

# ============================================================================
# Sample flow (diagnostic output)
# ============================================================================
flow_tab <- data.frame(
  step = c("Records in REDCap export", "Excluded at screening (vive_enpr/contestado_anteriormente/mayor_edad)",
           "After screening", "Duplicates (apodo + palabra_secreta, trimws+tolower)", "After de-duplication",
           "Missing primary outcome (consumo NA or 99)", "Analysis sample", "Lifetime psilocybin users",
           "Non-users"),
  n = c(n_raw, n_raw - n_s1, n_s1, n_s1 - n_s2, n_s2, n_s2 - n_s3, n_s3, n_users, n_s3 - n_users))
write.csv(flow_tab, file.path(OUT, "flow.csv"), row.names = FALSE)
print(flow_tab, row.names = FALSE)

# ============================================================================
# Table 1 -- demographics. Cell percentages of the variable's valid n
# (total / users / non-users columns sum to the total column).
# ============================================================================
d$orientation_t1 <- d$orientation                       # collapsed: Het / Bisexual / Gay / All Others
d$education_t1 <- factor(case_when(
  d$education %in% c("11th grade & below", "High School") ~ "HS or below",
  d$education == "Associate's degree" ~ "Associate",
  d$education == "Bachelor's degree" ~ "Bachelor",
  d$education %in% c("Master's degree", "Doctorate's degree") ~ "Graduate"))
# alternative relationship coding: separated (estado_civil 6) counted as married
d$relationship_alt <- case_when(d$estado_civil %in% c(2, 5, 6) ~ "Married/Living Together",
                                d$estado_civil %in% c(1, 3, 4) ~ "Single/Divorced/Widowed")

t1_spec <- list(
  list(key = "Gender",              var = "gender",           test = "gender",
       lv = c("Male", "Female")),
  list(key = "Sexual Orientation",  var = "orientation_full", test = "orientation_t1",
       lv = c("Heterosexual", "Asexual", "Bisexual", "Demisexual", "Gay", "Lesbian", "Pansexual", "Other")),
  list(key = "Transgender",         var = "transgender",      test = "transgender",
       lv = c("Yes", "No")),
  list(key = "Highest Education",   var = "education",        test = "education_t1",
       lv = c("11th grade & below", "High School", "Associate's degree", "Bachelor's degree",
              "Master's degree", "Doctorate's degree")),
  list(key = "Religion",            var = "religion",         test = "religion",
       lv = c("Christian", "Non-Christian")),
  list(key = "Relationship Status", var = "relationship",     test = "relationship",
       lv = c("Married/Living Together", "Single/Divorced/Widowed"), alt = "relationship_alt")
)

t1_rows <- list(); t1_test <- list()
for (s in t1_spec) {
  x <- as.character(d[[s$var]]); ok <- !is.na(x); nv <- sum(ok)
  xa <- if (!is.null(s$alt)) as.character(d[[s$alt]]) else NULL
  nva <- if (!is.null(xa)) sum(!is.na(xa)) else NA
  for (lv in s$lv) {
    cells <- c(total = sum(x == lv, na.rm = TRUE),
               users = sum(x == lv & u == 1, na.rm = TRUE),
               nonusers = sum(x == lv & u == 0, na.rm = TRUE))
    cells_alt <- if (!is.null(xa)) c(total = sum(xa == lv, na.rm = TRUE),
                                     users = sum(xa == lv & u == 1, na.rm = TRUE),
                                     nonusers = sum(xa == lv & u == 0, na.rm = TRUE)) else rep(NA, 3)
    for (cn in names(cells)) {
      t1_rows[[length(t1_rows) + 1]] <- data.frame(
        id = sprintf("T1.%s.%s.%s", s$key, lv, cn), variable = s$key, level = lv, column = cn,
        n = cells[[cn]], valid_n = nv, value = pct(cells[[cn]], nv),
        alt = if (!is.null(xa)) pct(cells_alt[[cn]], nva) else NA_real_,
        note = if (!is.null(xa)) "alt = separated counted as married" else "", stringsAsFactors = FALSE)
    }
  }
  pr <- chisq_p(d[[s$test]], u)
  t1_test[[length(t1_test) + 1]] <- data.frame(variable = s$key, test_variable = s$test,
                                               p = pr[["p"]], min_expected = pr[["min_exp"]])
  rec(sprintf("T1.%s.n", s$key), nv)
  rec(sprintf("T1.%s.sig", s$key), sig_level(pr[["p"]]), note = sprintf("chi-square p = %.3g", pr[["p"]]))
}
t1 <- do.call(rbind, t1_rows); rec_df(t1)
write.csv(t1[, c("variable", "level", "column", "n", "valid_n", "value")],
          file.path(OUT, "table1_demographics.csv"), row.names = FALSE)

# Robustness: chi-square p-values under alternative category collapsing (paper does not
# say how categories were collapsed). The significant / non-significant verdict is
# the same under every variant.
d$orient_het2 <- ifelse(is.na(d$orientation_full), NA, ifelse(d$orientation_full == "Heterosexual", "Het", "Non-het"))
variants <- list(c("Sexual Orientation", "orientation_full"), c("Sexual Orientation", "orient_het2"),
                 c("Highest Education", "education"))
t1_var <- do.call(rbind, lapply(variants, function(v) {
  pr <- chisq_p(d[[v[2]]], u)
  data.frame(variable = v[1], test_variable = v[2], p = pr[["p"]], min_expected = pr[["min_exp"]])
}))
write.csv(rbind(do.call(rbind, t1_test), t1_var), file.path(OUT, "table1_chisq.csv"), row.names = FALSE)

# ============================================================================
# Table 2 -- patterns among psilocybin users (n = 133). Primary denominator = 133.
# alt (single-choice) = % of valid answers excluding "prefer not to answer"/NA.
# alt (multiple-choice) = n / 253, the hard-coded denominator in Raking Only.R.
# ============================================================================
m <- d[u == 1, ]
t2_single <- list(
  Motivation  = list(var = "motivacion", map = c(Curiosity = 1, Introspection = 4, Friends = 7,
                       `Personal Healing` = 6, `Mystical experiences` = 3, `To feel drugged` = 8,
                       Spirituality = 2, `Increasing Creativity` = 5)),
  Dosage      = list(var = "dosis", map = c(`<0.5g` = 1, `0.5-1.5g` = 2, `1.5-2.5g` = 3, `2.5-3.5g` = 4,
                       `>3.5g` = 5, `Don't know` = 88)),
  Frequency   = list(var = "frecuencia", map = c(`<5 times in lifetime` = 1, Annual = 4, Monthly = 3,
                       Weekly = 2, Other = 55)),
  Company     = list(var = "compania", map = c(Accompanied = 2, Alone = 1, Other = 55)),
  Place       = list(var = "lugar", map = c(Nature = 1, Home = 2, Party = 4, Ceremony = 3, Other = 55)),
  Preparation = list(var = "consumo_preparacion", map = c(No = 2, Yes = 1)),
  Integration = list(var = "consumo_integracion", map = c(No = 2, Yes = 1)),
  Experience  = list(var = "consumo_experiencia", map = c(`Very pleasant` = 5, Pleasant = 4, Neutral = 3,
                       Unpleasant = 2, `Very unpleasant` = 1)),
  Addictive   = list(var = "hongos_adictivos", map = c(No = 2, Maybe = 3, Yes = 1)),
  Harmful     = list(var = "hongos_perjudicial", map = c(No = 2, Maybe = 3, Yes = 1))
)
t2_multi <- list(
  Methods  = list(`Dried Mushrooms` = "metodo___1", `Tea or coffee` = "metodo___2", Capsules = "metodo___4",
                  Chocolate = "metodo___3", Other = "metodo___55"),
  Purposes = list(Recreational = "proposito___2", Creativity = "proposito___4", Medicinal = "proposito___3",
                  Spiritual = "proposito___1", Other = "proposito___55")
)
t2_rows <- list()
for (k in names(t2_single)) {
  s <- t2_single[[k]]; x <- m[[s$var]]
  valid <- sum(!is.na(x) & x != 99)
  for (lab in names(s$map)) {
    n <- sum(x == s$map[[lab]], na.rm = TRUE)
    t2_rows[[length(t2_rows) + 1]] <- data.frame(
      id = sprintf("T2.%s.%s", k, lab), variable = k, level = lab, n = n,
      value = pct(n, n_u), alt = pct(n, valid),
      note = "alt = % of valid answers (excl. prefer-not-to-answer / NA)", stringsAsFactors = FALSE)
  }
}
for (k in names(t2_multi)) {
  for (lab in names(t2_multi[[k]])) {
    n <- sum(m[[t2_multi[[k]][[lab]]]] == 1, na.rm = TRUE)
    t2_rows[[length(t2_rows) + 1]] <- data.frame(
      id = sprintf("T2.%s.%s", k, lab), variable = k, level = lab, n = n,
      value = pct(n, n_u), alt = pct(n, 253),
      note = "alt = n/253, the hard-coded denominator in Raking Only.R", stringsAsFactors = FALSE)
  }
}
t2 <- do.call(rbind, t2_rows); rec_df(t2)
write.csv(t2[, c("variable", "level", "n", "value")], file.path(OUT, "table2_patterns_users.csv"), row.names = FALSE)

# Chi-square (users vs non-users) for the belief items, whole sample
for (k in c("Addictive", "Harmful")) {
  pr <- chisq_p(d[[t2_single[[k]]$var]], u)
  rec(sprintf("T2.%s.sig", k), sig_level(pr[["p"]]), note = sprintf("chi-square p = %.3g", pr[["p"]]))
}
# Beliefs in the total sample (abstract: 61% non-addictive, 57% safe; Results text: 52%, 48%)
rec("TX.not_addictive_total", pct(sum(d$hongos_adictivos == 2, na.rm = TRUE), n_all))
rec("TX.not_harmful_total",   pct(sum(d$hongos_perjudicial == 2, na.rm = TRUE), n_all))
rec("TX.not_addictive_total_abstract", pct(sum(d$hongos_adictivos == 2, na.rm = TRUE), n_all))
rec("TX.not_harmful_total_abstract",   pct(sum(d$hongos_perjudicial == 2, na.rm = TRUE), n_all))

# ============================================================================
# Table 3 -- lifetime use of each substance. Printed values are cell percentages
# of the total sample (n = 253): users% + non-users% = total%. alt = true column
# percentages (users of 133, non-users of 120; total of valid answers).
# ============================================================================
sub_spec <- c(Alcohol = "sustancias_alcohol", Cigarettes = "sustancias_cigarrillo",
              Marihuana = "sustancias___1", LSD = "sustancias___2", Cocaine = "sustancias___12",
              MDMA = "sustancias___3", `Amyl Nitrate/Poppers` = "sustancias___14", DMT = "sustancias___4",
              Salvia = "sustancias___11", Ayahuasca = "sustancias___5", Mescaline = "sustancias___6",
              Ketamine = "sustancias___10", Peyote = "sustancias___7", Heroin = "sustancias___13",
              `5-MEO-DMT` = "sustancias___8", `2-CB` = "sustancias___9")
t3_rows <- list(); t3_p <- list()
for (s in names(sub_spec)) {
  x <- d[[sub_spec[[s]]]]; yes <- x == 1
  valid <- sum(!is.na(x) & x != 99)
  n_tot <- sum(yes, na.rm = TRUE); n_us <- sum(yes & u == 1, na.rm = TRUE); n_no <- sum(yes & u == 0, na.rm = TRUE)
  for (cn in c("total", "users", "nonusers")) {
    nn <- c(total = n_tot, users = n_us, nonusers = n_no)[[cn]]
    alt <- c(total = pct(n_tot, valid), users = pct(n_us, n_u), nonusers = pct(n_no, n_n))[[cn]]
    t3_rows[[length(t3_rows) + 1]] <- data.frame(
      id = sprintf("T3.%s.%s", s, cn), substance = s, column = cn, n = nn,
      value = pct(nn, n_all), alt = alt,
      note = "alt = column % (users/133, non-users/120; total/valid answers)", stringsAsFactors = FALSE)
  }
  pr <- chisq_p(x, u)
  fp <- if (nlevels(factor(x)) == 2) fisher.test(table(x, u))$p.value else NA_real_
  t3_p[[length(t3_p) + 1]] <- data.frame(substance = s, p = pr[["p"]], min_expected = pr[["min_exp"]],
                                         stars = stars(pr[["p"]]), fisher_p = fp)
  rec(sprintf("T3.%s.sig", s), sig_level(pr[["p"]]),
      note = sprintf("chi-square p = %.3g; min expected count %.1f", pr[["p"]], pr[["min_exp"]]))
}
t3 <- do.call(rbind, t3_rows); rec_df(t3)
t3p <- do.call(rbind, t3_p)
write.csv(merge(t3[, c("substance", "column", "n", "value")], t3p, by = "substance"),
          file.path(OUT, "table3_substances.csv"), row.names = FALSE)

# In-text polysubstance statements (Results, "Polysubstance use")
yes1 <- function(v) d[[v]] == 1
none <- d$sustancias___15 == 1
rec("TX.mj_users_pct",   pct(sum(yes1("sustancias___1") & u == 1), n_u))
rec("TX.mj_users_n",     sum(yes1("sustancias___1") & u == 1))
rec("TX.lsd_users_pct",  pct(sum(yes1("sustancias___2") & u == 1), n_u))
rec("TX.lsd_users_n",    sum(yes1("sustancias___2") & u == 1))
rec("TX.mj_nonusers_pct", pct(sum(yes1("sustancias___1") & u == 0), n_n))
rec("TX.mj_nonusers_n",   sum(yes1("sustancias___1") & u == 0))
rec("TX.none_nonusers_pct", pct(sum(none & u == 0), n_n))
rec("TX.none_nonusers_n",   sum(none & u == 0))
rec("TX.alcohol_total_pct", pct(sum(yes1("sustancias_alcohol"), na.rm = TRUE), n_all))
rec("TX.cigarettes_total_pct", pct(sum(yes1("sustancias_cigarrillo"), na.rm = TRUE), n_all))
rec("TX.mj_total_pct",  pct(sum(yes1("sustancias___1")), n_all))
rec("TX.lsd_total_pct", pct(sum(yes1("sustancias___2")), n_all))
rec("T3.LSD.n_total", sum(yes1("sustancias___2")))   # sample-filter check: 90 of 253

# ============================================================================
# TIPI -- Mann-Whitney U (users vs non-users) with rcompanion::wilcoxonR
# ============================================================================
suppressPackageStartupMessages(library(rcompanion))
traits <- c(Openness = "openness", `Emotional stability` = "emotional_stability",
            Agreeableness = "agreeableness", Extraversion = "extraversion",
            Conscientiousness = "conscientiousness")
tipi_rows <- list()
for (tr in names(traits)) {
  v <- traits[[tr]]
  w <- suppressWarnings(wilcox.test(d[[v]] ~ d$psilocybin_use))
  r <- wilcoxonR(x = d[[v]], g = d$psilocybin_use)
  # What the original script effectively did: it called wilcoxonR on the 304-row
  # data frame (51 rows with a missing outcome were never dropped), so the N inside
  # r = Z / sqrt(N) was 304.
  r_orig <- suppressWarnings(wilcoxonR(x = d_pre[[v]], g = d_pre$psilocybin_use))
  n_used <- sum(!is.na(d[[v]]))
  tipi_rows[[length(tipi_rows) + 1]] <- data.frame(
    trait = tr, W = unname(w$statistic), p = w$p.value, r = as.numeric(r), r_if_N304 = as.numeric(r_orig),
    n_valid = n_used, mean_users = mean(d[[v]][u == 1], na.rm = TRUE),
    mean_nonusers = mean(d[[v]][u == 0], na.rm = TRUE), stringsAsFactors = FALSE)
  rec(sprintf("TIPI.%s.r", tr), as.numeric(r), alt = as.numeric(r_orig),
      note = "alt = wilcoxonR on the 304-row frame (N = 304 in Z/sqrt(N)), as in Raking Only.R")
  rec(sprintf("TIPI.%s.p", tr), w$p.value)
}
tipi <- do.call(rbind, tipi_rows)
write.csv(tipi, file.path(OUT, "tipi_mann_whitney.csv"), row.names = FALSE)

# ============================================================================
# Other in-text statistics (Results, Discussion, Conclusion)
# ============================================================================
rec("TX.prevalence_pct", pct(n_u, n_all))
rec("TX.prevalence_pct_discussion", pct(n_u, n_all))
pt <- prop.test(n_users, 253)
rec("TX.prevalence_ci_lo", 100 * pt$conf.int[1]); rec("TX.prevalence_ci_hi", 100 * pt$conf.int[2])
rec("TX.curiosity_pct_text", pct(sum(m$motivacion == 1), n_u))
rec("TX.curiosity_pct_abstract", pct(sum(m$motivacion == 1), n_u))
rec("TX.integration_pct_text", pct(sum(m$consumo_integracion == 1), n_u))
rec("TX.preparation_pct_text", pct(sum(m$consumo_preparacion == 1), n_u))
rec("TX.dose_lt25_pct", pct(sum(m$dosis %in% 1:3), n_u))
rec("TX.dose_unknown_pct", pct(sum(m$dosis == 88), n_u))
rec("TX.lt5_times_pct", pct(sum(m$frecuencia == 1), n_u))
rec("TX.accompanied_pct", pct(sum(m$compania == 2), n_u))
rec("TX.nature_pct", pct(sum(m$lugar == 1), n_u))
rec("TX.dried_pct", pct(sum(m$metodo___1 == 1), n_u))
rec("TX.recreational_pct", pct(sum(m$proposito___2 == 1), n_u))
rec("TX.nonrecreational_pct", pct(sum(m$proposito___1 == 1 | m$proposito___3 == 1 | m$proposito___4 == 1), n_u))
rec("TX.pleasant_pct", pct(sum(m$consumo_experiencia == 4), n_u))
rec("TX.very_pleasant_pct", pct(sum(m$consumo_experiencia == 5), n_u))
rec("TX.pleasant_or_very_pleasant_abstract", pct(sum(m$consumo_experiencia %in% 4:5), n_u))
rec("TX.not_addictive_users", pct(sum(m$hongos_adictivos == 2), n_u))
rec("TX.not_harmful_users", pct(sum(m$hongos_perjudicial == 2), n_u))
rec("TX.first_use_min_age", min(m$consumo_edad))
rec("TX.first_use_lt18_pct", pct(sum(m$consumo_edad < 18), n_u))
rec("TX.first_use_lt21_pct", pct(sum(m$consumo_edad < 21), n_u))
fu <- m %>% group_by(orientation_full) %>% summarise(n = n(), mean_age_first_use = mean(consumo_edad), .groups = "drop") %>%
  filter(!is.na(orientation_full))
# Suppress means for cells with n < 5 in the written file (a 1-person mean is that person's value);
# the comparisons below still use the unsuppressed `fu`.
fu_out <- fu
fu_out$mean_age_first_use[fu_out$n < 5] <- NA
write.csv(fu_out, file.path(OUT, "first_use_age_by_orientation.csv"), row.names = FALSE)
write.csv(data.frame(statistic = c("n", "mean", "sd", "median", "min", "max"),
                     value = c(n_u, mean(m$consumo_edad), sd(m$consumo_edad), median(m$consumo_edad),
                               min(m$consumo_edad), max(m$consumo_edad))),
          file.path(OUT, "first_use_age_users.csv"), row.names = FALSE)
mean_het <- fu$mean_age_first_use[fu$orientation_full == "Heterosexual"]
rec("TX.first_use_younger_bi", as.numeric(fu$mean_age_first_use[fu$orientation_full == "Bisexual"] < mean_het))
rec("TX.first_use_younger_lesbian", as.numeric(fu$mean_age_first_use[fu$orientation_full == "Lesbian"] < mean_het))
rec("TX.first_use_younger_pansexual", as.numeric(fu$mean_age_first_use[fu$orientation_full == "Pansexual"] < mean_het))
