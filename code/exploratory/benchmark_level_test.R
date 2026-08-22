# Benchmark-level test: does the LEVEL of the comparison bar teach respondents
# about the COALITION, over and above where it places their own municipality?
#
# Motivation. The theory in latex/main.tex treats the benchmark as a device for
# differencing out the common shock: only d = r_home - r_bench is informative,
# and with a party-level competence component z_P a SAME-coalition comparison
# differences that out too. So the pure model says a same-coalition benchmark
# cannot teach a voter where their coalition generally sits. An alternative is
# that respondents read the benchmark's LEVEL as a draw of same-coalition
# performance, which would move coalition beliefs directly.
#
# IDENTIFICATION CAVEAT (why this script is built the way it is). Within an arm,
# conditional on home_rate, the benchmark level B and the difference d are
# perfectly collinear (d = home_rate - B). Regressing an outcome on B while
# controlling for home_rate therefore CANNOT separate a "level" channel from a
# "difference" channel — the coefficient is the same object either way. Do not
# add both B and d to one model and read the two coefficients.
#
# The discriminating variation is instead WHICH OUTCOME moves in WHICH ARM. The
# comparison municipalities are party-labelled in T3 (other coalition) and T4
# (same coalition), unlabelled in T2, and the bars show rainfall, not crime, in
# control2. That yields a 2x2 the pure-difference account cannot produce:
#
#   arm       outcome moved by B under party learning     pure-difference model
#   -------   ----------------------------------------    ---------------------
#   T4        home-party rating DOWN; other-coalition      B shifts only relative
#             rating ~0; incumbent rating UP               standing, so the SAME
#   T3        other-coalition rating DOWN; home-party      pattern in T3 and T4
#             rating ~0; incumbent rating UP               and no home/other
#   T2        neither party rating moves (no labels)       asymmetry anywhere
#   control2  nothing moves (rainfall bars)
#
# So the test is a sign CONTRAST across outcomes and arms, not the sign of any
# single coefficient. A significant B on relative standing alone proves nothing.
#
# Identifying assumption: given the home municipality, which comparison
# municipalities were drawn from the Mahalanobis-matched pool is random, so B is
# exogenous conditional on home_rate. home_rate is controlled for throughout
# because matching on log population/area/distance makes comparison rates
# correlate with the home rate mechanically.
#
# Exploratory and underpowered by design: roughly 385 respondents per arm, one
# continuous regressor, noisy 0-100 sliders. Read the intervals, not the stars.
#
# Outputs:
#   latex/images/benchmark_level_test.pdf
#   data/derived/benchmark_level_test.csv

library(estimatr)
library(dplyr)
library(ggplot2)
library(broom)
library(tidyr)

load("data/derived/survey_panel_dataset.Rdata")

if (!exists("ci_alpha")) {
  ci_alpha <- 0.05
}

num <- function(x) suppressWarnings(as.numeric(x))

# Okabe-Ito, matching vote_update_analysis.R / home_party_update_analysis.R
arm_colors <- c(
  control2 = "#999999",
  T2 = "#009E73",
  T3 = "#E69F00",
  T4 = "#0072B2"
)

panel <- filter(panel, muni_changed == 0, Attention_Check == "somewhat_agree")

# ── 1. Outcome construction (mirrors home_party_update_analysis.R) ────────────

coalition_post_rating <- cbind(
  "MORENA/PVEM/PT" = num(panel$MORENA_Crime_Rating_Post),
  "PAN/PRI/PRD" = num(panel$Coalition_PAN_PRI_PRD_Crime_Rating_Post),
  "MC" = num(panel$MC_Crime_Rating_Post)
)

coalition_pre_rating <- cbind(
  "MORENA/PVEM/PT" = num(panel$MORENA_Crime_Rating_Pre),
  "PAN/PRI/PRD" = num(panel$Coalition_PAN_PRI_PRD_Crime_Rating_Pre),
  "MC" = num(panel$MC_Crime_Rating_Pre)
)

# Average rating of the coalitions that do NOT govern the home municipality.
opp_avg <- t(vapply(
  seq_len(nrow(panel)),
  function(i) {
    hc <- panel$home_coalition[i]
    if (is.na(hc)) {
      return(c(post = NA_real_, pre = NA_real_))
    }
    keep <- colnames(coalition_pre_rating) != hc
    post <- coalition_post_rating[i, keep]
    pre <- coalition_pre_rating[i, keep]
    c(
      post = if (all(is.na(post))) NA_real_ else mean(post, na.rm = TRUE),
      pre = if (all(is.na(pre))) NA_real_ else mean(pre, na.rm = TRUE)
    )
  },
  numeric(2)
))

panel$opp_avg_post <- opp_avg[, "post"]
panel$opp_avg_pre <- opp_avg[, "pre"]

panel$inc_post <- num(panel$Home_Crime_Handling_Post)
panel$inc_pre <- num(panel$Home_Crime_Handling_Pre)
panel$party_post <- num(panel$Home_Party_Crime_Handling_Post)
panel$party_pre <- num(panel$Home_Party_Crime_Handling_Pre)
panel$inc_minus_opp_avg_post <- panel$inc_post - panel$opp_avg_post

# ── 2. Benchmark level B ──────────────────────────────────────────────────────
# Mean robbery rate per 100k of the four comparison municipalities the
# respondent was shown. comp_rate_1..4 are built in create_panel_dataset.R.

comp_rate_cols <- paste0("comp_rate_", 1:4)
stopifnot(all(comp_rate_cols %in% names(panel)))

comp_mat <- as.matrix(panel[comp_rate_cols])
panel$n_comp_obs <- rowSums(!is.na(comp_mat))
panel$bench_rate <- ifelse(
  panel$n_comp_obs == 0,
  NA_real_,
  rowMeans(comp_mat, na.rm = TRUE)
)

# Rates are right-skewed; log both sides so the control absorbs the
# matching-induced correlation on the same scale as the regressor.
panel$log_bench <- log1p(panel$bench_rate)
panel$log_home_rate <- log1p(panel$home_rate)

# Coverage diagnostic. Whether comparison municipalities were SAMPLED for arms
# that never displayed them (control, T1) decides if a pure placebo arm is
# available: if control has a defined bench_rate, B there was never seen by the
# respondent and must have no effect.
coverage <- panel %>%
  group_by(Treatment_Group) %>%
  summarise(
    n = n(),
    n_bench_defined = sum(!is.na(bench_rate)),
    median_bench = median(bench_rate, na.rm = TRUE),
    sd_log_bench = sd(log_bench, na.rm = TRUE),
    .groups = "drop"
  )

cat("\n== Benchmark-level coverage by arm ==\n")
print(as.data.frame(coverage))
cat(
  "\nArms with n_bench_defined ~ 0 cannot be tested. If control/T1 are",
  "\npopulated, they are placebo arms: comparisons sampled but never shown.\n"
)

# Standardize B on the arms that actually displayed a comparison chart, so one
# SD means the same thing across arms.
shown_arms <- c("control2", "T2", "T3", "T4")
bench_sd <- sd(
  panel$log_bench[panel$Treatment_Group %in% shown_arms],
  na.rm = TRUE
)
cat(sprintf("\nSD of log benchmark level (comparison arms): %.3f\n", bench_sd))

# ── 3. Per-arm models ─────────────────────────────────────────────────────────
# One model per (arm, outcome). Within an arm there is no arm interaction, so
# the gaps enter as plain controls. Each outcome is pre-adjusted: post level on
# the corresponding pre level(s).

outcome_specs <- list(
  list(
    name = "inc_post",
    label = "Incumbent rating",
    pre = c("inc_pre")
  ),
  list(
    name = "party_post",
    label = "Home-party rating",
    pre = c("party_pre")
  ),
  list(
    name = "opp_avg_post",
    label = "Other-coalition rating",
    pre = c("opp_avg_pre")
  ),
  list(
    name = "inc_minus_opp_avg_post",
    label = "Incumbent vs. other coalitions",
    pre = c("inc_pre", "opp_avg_pre")
  )
)

fit_bench <- function(outcome, pre_vars, arm_data) {
  fml <- as.formula(paste0(
    outcome,
    " ~ log_bench + log_home_rate + ",
    paste(pre_vars, collapse = " + "),
    " + log_crime_gap + rank_gap + coalition_pre"
  ))
  lm_robust(fml, alpha = ci_alpha, data = arm_data, se_type = "HC2")
}

results <- list()

for (arm in shown_arms) {
  arm_data <- filter(panel, Treatment_Group == arm)
  if (sum(!is.na(arm_data$log_bench)) < 30) {
    cat(sprintf("\nSkipping %s: too few defined benchmark levels.\n", arm))
    next
  }
  for (spec in outcome_specs) {
    m <- fit_bench(spec$name, spec$pre, arm_data)
    tid <- tidy(m, conf.int = TRUE) %>% filter(term == "log_bench")
    if (nrow(tid) == 0) {
      next
    }
    results[[length(results) + 1]] <- tid %>%
      transmute(
        arm = arm,
        outcome = spec$label,
        n = m$nobs,
        # Scale to a 1 SD increase in the benchmark level.
        estimate = estimate * bench_sd,
        std.error = std.error * bench_sd,
        conf.low = conf.low * bench_sd,
        conf.high = conf.high * bench_sd,
        p.value
      )
  }
}

coefs <- bind_rows(results) %>%
  mutate(
    arm = factor(arm, levels = shown_arms),
    outcome = factor(outcome, levels = vapply(outcome_specs, `[[`, "", "label"))
  ) %>%
  arrange(outcome, arm)

cat("\n== Effect of a 1 SD higher benchmark level, by arm and outcome ==\n")
print(as.data.frame(coefs), digits = 3)

# ── 4. The discriminating contrasts ───────────────────────────────────────────
# Party learning lives in the ASYMMETRY between the home-party and
# other-coalition ratings, and in that asymmetry flipping between T4 and T3.
# Both are estimated on the same respondents, so the two coefficients are
# correlated; a difference is tested by stacking the outcomes rather than by
# comparing intervals by eye. Here the outcome difference IS a variable, so the
# contrast is just a model on (party_post - opp_avg_post).

panel$party_minus_opp_post <- panel$party_post - panel$opp_avg_post

cat("\n== Home-party minus other-coalition rating (the asymmetry) ==\n")
asym <- list()
for (arm in shown_arms) {
  arm_data <- filter(panel, Treatment_Group == arm)
  if (sum(!is.na(arm_data$log_bench)) < 30) {
    next
  }
  m <- fit_bench(
    "party_minus_opp_post",
    c("party_pre", "opp_avg_pre"),
    arm_data
  )
  tid <- tidy(m, conf.int = TRUE) %>% filter(term == "log_bench")
  asym[[length(asym) + 1]] <- tid %>%
    transmute(
      arm = arm,
      n = m$nobs,
      estimate = estimate * bench_sd,
      std.error = std.error * bench_sd,
      conf.low = conf.low * bench_sd,
      conf.high = conf.high * bench_sd,
      p.value
    )
}
asym <- bind_rows(asym)
print(as.data.frame(asym), digits = 3)

# Formal T4-vs-T3 difference in that asymmetry: pooled model, log_bench x arm.
pooled_data <- filter(panel, Treatment_Group %in% c("T3", "T4"))
m_pooled <- lm_robust(
  party_minus_opp_post ~
    log_bench * Treatment_Group +
    log_home_rate * Treatment_Group +
    party_pre + opp_avg_pre + log_crime_gap + rank_gap + coalition_pre,
  alpha = ci_alpha,
  data = pooled_data,
  se_type = "HC2"
)

int_term <- grep("^log_bench:Treatment_Group", names(coef(m_pooled)), value = TRUE)
if (length(int_term) == 1) {
  est <- coef(m_pooled)[[int_term]] * bench_sd
  se <- sqrt(vcov(m_pooled)[int_term, int_term]) * bench_sd
  cat(sprintf(
    paste0(
      "\nT4 vs T3 difference in the benchmark-level effect on the",
      " home-party/other-coalition asymmetry:\n  %.2f (SE %.2f), 95%% CI",
      " [%.2f, %.2f], p = %.3f\n"
    ),
    est,
    se,
    est - 1.96 * se,
    est + 1.96 * se,
    2 * pnorm(-abs(est / se))
  ))
  cat(
    "  Party learning predicts this to be negative: a high same-coalition\n",
    " benchmark should depress the home party specifically, which T3's\n",
    " other-coalition benchmark should not do.\n"
  )
} else {
  cat("\nCould not locate the log_bench x arm interaction term.\n")
}

# ── 5. Plot ───────────────────────────────────────────────────────────────────

bench_plot <- ggplot(coefs, aes(y = arm, x = estimate, color = arm)) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey50") +
  geom_errorbar(
    aes(xmin = conf.low, xmax = conf.high),
    orientation = "y",
    width = 0.15,
    linewidth = 0.7
  ) +
  geom_point(size = 2.2) +
  facet_wrap(~outcome, ncol = 2, scales = "free_x") +
  scale_color_manual(values = arm_colors, guide = "none") +
  labs(
    title = "Effect of the benchmark's level on crime-handling ratings",
    subtitle = paste(
      "Party learning predicts, in T4 only: home-party rating down,",
      "other-coalition rating flat"
    ),
    x = "Change per 1 SD increase in mean comparison robbery rate (0-100 scale)",
    y = NULL,
    caption = paste0(
      "Per-arm OLS, HC2. Controls: home robbery rate, pre-treatment level(s),",
      " both perception gaps,\npre-treatment coalition preference. Bars are ",
      100 * (1 - ci_alpha),
      "% CIs. Conditional on the home rate the\nbenchmark level and the ",
      "home-benchmark difference are collinear: read the cross-outcome and\n",
      "cross-arm sign contrasts, not any single coefficient."
    )
  ) +
  theme_minimal() +
  theme(plot.caption = element_text(hjust = 0, size = 7))

print(bench_plot)

ggsave(
  "latex/images/benchmark_level_test.pdf",
  plot = bench_plot,
  width = 9,
  height = 6
)
cat("\nWrote latex/images/benchmark_level_test.pdf\n")

write.csv(
  coefs,
  "data/derived/benchmark_level_test.csv",
  row.names = FALSE
)
cat("Wrote data/derived/benchmark_level_test.csv\n")