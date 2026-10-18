# 03_models.R -- multivariable logistic regression, fit statistics, influence
# diagnostics and the two figures. Aggregates / coefficients only.

suppressPackageStartupMessages({ library(pscl); library(ggplot2); library(broom) })

fit <- glm(psilocybin_use ~ gender + orientation + transgender + age + religion +
             openness + agreeableness + extraversion + conscientiousness + emotional_stability,
           data = d, family = binomial())
n_fit <- nobs(fit)
rec("M.n_fit", n_fit)

# Odds ratios with profile-likelihood 95% CI (what exp(confint(fit)) gives) and Wald CI.
ci_prof <- suppressMessages(confint(fit))
ci_wald <- confint.default(fit)
sm <- summary(fit)$coefficients
coef_tab <- data.frame(term = rownames(sm), OR = exp(coef(fit)),
                       CI_lo_profile = exp(ci_prof[, 1]), CI_hi_profile = exp(ci_prof[, 2]),
                       CI_lo_wald = exp(ci_wald[, 1]), CI_hi_wald = exp(ci_wald[, 2]),
                       p = sm[, 4], row.names = NULL)
write.csv(coef_tab, file.path(OUT, "logistic_regression_aOR.csv"), row.names = FALSE)

term_map <- c(Male = "genderMale", Bisexual = "orientationBisexual", Gay = "orientationGay",
              `Non-heterosexual (All Others)` = "orientationAll Others", Transgender = "transgenderYes",
              Age = "age", `Non-Christian` = "religionNon-Christian", Openness = "openness",
              Agreeableness = "agreeableness", Extraversion = "extraversion",
              Conscientiousness = "conscientiousness", `Emotional stability` = "emotional_stability")
for (lab in names(term_map)) {
  r <- coef_tab[coef_tab$term == term_map[[lab]], ]
  rec(sprintf("M.%s.or", lab), r$OR, alt = NA)
  rec(sprintf("M.%s.lo", lab), r$CI_lo_profile, alt = r$CI_lo_wald, note = "alt = Wald CI")
  rec(sprintf("M.%s.hi", lab), r$CI_hi_profile, alt = r$CI_hi_wald, note = "alt = Wald CI")
  rec(sprintf("M.%s.p", lab), r$p)
}

# Fit statistics. Null model must use the same rows as the full model.
mf <- fit$model
fit0 <- glm(psilocybin_use ~ 1, data = mf, family = binomial())
ll_full <- as.numeric(logLik(fit)); ll_null <- as.numeric(logLik(fit0))
pr2 <- suppressMessages(pscl::pR2(fit))
rec("M.mcfadden", unname(pr2["McFadden"]))
rec("M.ll_null", -ll_null)       # paper prints the absolute value
rec("M.ll_full", -ll_full)
rec("M.lrt", 2 * (ll_full - ll_null))
stopifnot(abs(ll_null - as.numeric(pr2["llhNull"])) < 1e-6)

# Influence diagnostics
aug <- broom::augment(fit)
rec("M.max_leverage", max(aug$.hat))
rec("M.max_cooks", max(aug$.cooksd))
rec("M.share_std_resid_within_2", 100 * mean(abs(aug$.std.resid) <= 2))
write.csv(data.frame(statistic = c("n_model", "max_leverage", "max_cooks_distance", "min_std_resid",
                                   "max_std_resid", "n_abs_std_resid_gt_2"),
                     value = c(n_fit, max(aug$.hat), max(aug$.cooksd), min(aug$.std.resid),
                               max(aug$.std.resid), sum(abs(aug$.std.resid) > 2))),
          file.path(OUT, "model_fit_and_diagnostics.csv"), row.names = FALSE)

# Multicollinearity (paper: "assumptions ... were evaluated and met"); GVIF-free check
# using the correlation among the five TIPI scores and age (no extra package needed).
write.csv(round(cor(mf[, c("age", "openness", "agreeableness", "extraversion", "conscientiousness",
                           "emotional_stability")]), 3),
          file.path(OUT, "model_numeric_predictor_correlations.csv"))

# ---- Figures (re-drawn; the paper's Fig. 1 and Fig. 2 were made with ggplot2 / finalfit) ----
p1 <- ggplot(aug, aes(x = .hat, y = .std.resid)) +
  geom_point(aes(size = .cooksd), alpha = 0.6) +
  geom_hline(yintercept = c(-2, 2), linetype = "dashed") +
  labs(title = "Model residual analysis",
       subtitle = "Standardized residuals, leverage and Cook's D",
       x = "Leverage", y = "Standardized residuals", size = "Cook's D") + theme_minimal()
ggsave(file.path(OUT, "figure1_residuals_leverage.png"), p1, width = 8, height = 4, dpi = 200)

forest <- coef_tab[coef_tab$term != "(Intercept)", ]
forest$term <- factor(forest$term, levels = rev(forest$term))
p2 <- ggplot(forest, aes(x = OR, y = term)) +
  geom_vline(xintercept = 1, linetype = "dashed") +
  geom_linerange(aes(xmin = CI_lo_profile, xmax = CI_hi_profile), orientation = "y") +
  geom_point(size = 2) + scale_x_log10() +
  labs(title = "Adjusted odds ratios for lifetime psilocybin use",
       x = "Odds ratio (log scale), 95% profile-likelihood CI", y = NULL) + theme_minimal()
ggsave(file.path(OUT, "figure2_odds_ratios.png"), p2, width = 8, height = 4, dpi = 200)

say("[03_models] regression n = %d; McFadden = %.4f; LL null = %.2f; LL full = %.2f; LRT = %.2f",
    n_fit, pr2[["McFadden"]], ll_null, ll_full, 2 * (ll_full - ll_null))
