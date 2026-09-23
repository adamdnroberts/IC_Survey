# Does the wave 1 -> wave 2 gap moderate anything?
#
# QUESTION. Respondents took wave 2 anywhere from a few days to several weeks
# after wave 1. If the elapsed gap matters -- priors go stale, the wave-1
# municipality choice stops describing where they live, attention decays --
# then the effects estimated in vote_update_analysis.R and
# belief_update_analysis.R are averages over a moderator we have never looked
# at. This script tests that.
#
# THE POWER PROBLEM, AND WHAT THIS SCRIPT DOES ABOUT IT. The obvious way to ask
# the question is to split the sample at the median gap and re-run the main
# specification in each half. That is the WORST available test: it halves n
# twice over, discards the ordering information in days_between, and the
# published spec already spends its degrees of freedom on a 5-arm factor
# interacted with two gap regressors. A split-sample comparison of those
# interactions has essentially no power to detect anything short of a sign flip.
#
# So the tests here are ordered from most to least powerful, and the headline
# test is the most powerful one:
#
#   1. PRIMARY (1 df). Pool the information arms into a single indicator and
#      test ONE triple-interaction coefficient: log_crime_gap x treated x gap_z.
#      Pooling arms and keeping days_between continuous means the whole sample
#      informs a single parameter. This is the test to report.
#   2. SECONDARY (2 df). Add the rank_gap triple and test both jointly (Wald).
#   3. OMNIBUS (many df). The arm-by-arm three-way interaction, tested jointly.
#      Reported for completeness and expected to be null by construction -- it
#      is the low-power version of test 1, and a null here is NOT evidence of
#      no moderation.
#   4. AUXILIARY (1 df each). Direct, high-power checks that do not go through
#      the treatment interaction at all: does the gap predict the outcome
#      level, does it predict cross-wave measurement inconsistency, and is it
#      balanced across arms.
#   5. EQUIVALENCE (TOST). Tests 1-4 all have "no moderation" as the null, so
#      failing to reject them is not evidence of no moderation. Test 5 flips the
#      null -- it asks whether we can positively reject that the moderation is
#      LARGE -- and reports the smallest equivalence bound this sample could
#      ever reject, which is the assumption-free answer to "can I rule a
#      difference out?". Bounds are stated as fractions of the pooled treatment
#      slope, never in outcome SDs -- see the note above frac_bound.
#
# Tests 1 and 4 are where the power is. If the gap matters at all it should
# show up there first.
#
# MULTIPLICITY. Two primary tests (one per outcome). Bonferroni-adjusted
# threshold 0.025 is printed alongside the raw p-values; nothing else here is
# confirmatory.
#
# SAMPLE. create_panel_dataset.R already drops days_between <= 4 (and NA), so
# the moderator is truncated from below: this tests variation ABOVE that floor,
# not the full support. Otherwise the estimation sample matches the published
# specs -- muni_changed == 0, passed attention check, control2 dropped.
#
# Usage:
#   "/c/Program Files/R/R-4.5.1/bin/Rscript.exe" --vanilla \
#     code/exploratory/wave_gap_moderation.R

library(estimatr)
library(dplyr)
library(ggplot2)
library(broom)
library(car)

load("data/derived/survey_panel_dataset.Rdata")

ci_alpha <- 0.05
bonferroni_alpha <- 0.025 # two primary tests

# ── Sample and variable construction ─────────────────────────────────────────

panel_full <- panel

# Both slider columns arrive as character from the response pull (see
# belief_update_analysis.R); convert before any subsetting.
panel_full$inc_post <- as.numeric(panel_full$Home_Crime_Handling_Post)
panel_full$inc_pre <- as.numeric(panel_full$Home_Crime_Handling_Pre)

panel_full$Vote_home_post <- as.integer(
  !is.na(panel_full$coalition_post) &
    !is.na(panel_full$home_coalition) &
    panel_full$home_coalition == panel_full$coalition_post
)

panel_full$coalition_pre_f <- panel_full$coalition_pre
panel_full$coalition_pre_f[is.na(panel_full$coalition_pre_f)] <- "Other"
panel_full$inc_vote <- as.numeric(
  panel_full$coalition_pre_f == panel_full$home_coalition
)

# Cross-wave measurement consistency: the four Netquest panel demographics are
# asked in both waves and should not change. Disagreement is a proxy for a
# degraded link or a degraded respondent, and is the auxiliary outcome in
# test 4b. Built on panel_full (before the attention-check filter) so it is not
# conditioned on the very attentiveness it is meant to measure.
panel_full$demo_inconsistent <- as.integer(
  panel_full$NQ_Age_w1 != panel_full$NQ_Age_w2 |
    panel_full$NQ_Sex_w1 != panel_full$NQ_Sex_w2 |
    panel_full$NQ_Region_w1 != panel_full$NQ_Region_w2 |
    panel_full$NQ_SEL_w1 != panel_full$NQ_SEL_w2
)

panel_full$muni_changed_num <- as.integer(panel_full$muni_changed)
panel_full$attn_pass <- as.integer(
  panel_full$Attention_Check == "somewhat_agree"
)

# The moderator. Standardized so the triple interaction reads "per 1 SD of
# elapsed days" and so the two-way terms stay interpretable at the mean gap
# rather than at zero days (which is outside the support). A log version is run
# as a robustness check: if the gap matters, a week early on plausibly matters
# more than a week late, and the linear form would understate that.
gap_mean <- mean(panel_full$days_between, na.rm = TRUE)
gap_sd <- sd(panel_full$days_between, na.rm = TRUE)
panel_full$gap_z <- (panel_full$days_between - gap_mean) / gap_sd
panel_full$gap_log_z <- with(
  panel_full,
  (log(days_between) - mean(log(days_between), na.rm = TRUE)) /
    sd(log(days_between), na.rm = TRUE)
)

# Pooled information indicator for the primary test: every arm that was shown
# real robbery numbers, against the plain-control placebo. control2 is dropped
# to match the published estimation sample.
panel_full$treated <- as.integer(panel_full$Treatment_Group != "control")

panel_with_failures <- filter(panel_full, muni_changed == 0)
panel_est <- filter(
  panel_with_failures,
  Attention_Check == "somewhat_agree" & Treatment_Group != "control2"
)

cat("\n================ SAMPLE ================\n")
cat(sprintf(
  "Estimation sample: n = %d (of %d linked panel rows)\n",
  nrow(panel_est),
  nrow(panel_full)
))
cat(sprintf(
  "days_between: mean %.1f, median %.1f, sd %.1f, range %.1f-%.1f\n",
  gap_mean,
  median(panel_full$days_between, na.rm = TRUE),
  gap_sd,
  min(panel_full$days_between, na.rm = TRUE),
  max(panel_full$days_between, na.rm = TRUE)
))
print(round(quantile(
  panel_est$days_between,
  c(0.1, 0.25, 0.5, 0.75, 0.9),
  na.rm = TRUE
), 1))

# A moderator with no spread cannot be detected as a moderator. If the IQR of
# the gap is a couple of days the whole exercise is underpowered by design, and
# that should be reported rather than discovered later.
gap_iqr <- diff(quantile(panel_est$days_between, c(0.25, 0.75), na.rm = TRUE))
if (gap_iqr < 3) {
  cat(sprintf(
    "\nWARNING: interquartile range of days_between is only %.1f days.\n",
    gap_iqr
  ))
  cat("There may be too little variation in the gap to detect moderation.\n")
}

# ── 1-2. Primary and secondary tests: pooled arms, continuous gap ────────────
#
# Form: outcome ~ (pre-treatment level) + gap_regressor * treated * gap_z +
#       rank_gap * treated * gap_z + controls.
#
# The coefficient of interest is log_crime_gap:treated:gap_z -- how the
# treatment's crime-gap response changes per SD of elapsed days. All lower-order
# terms are included automatically by the * expansion; dropping any of them
# would make the triple uninterpretable.

# Term names from a three-way expansion depend on the order R happens to build
# them in; resolve against the fitted names instead of hard-coding, and fail
# loudly if a rename ever breaks the match (cf. joint_gap_test in
# belief_update_analysis.R).
find_term <- function(model, parts) {
  nms <- names(coef(model))
  hit <- nms[vapply(
    nms,
    function(n) {
      bits <- strsplit(n, ":", fixed = TRUE)[[1]]
      length(bits) == length(parts) && setequal(bits, parts)
    },
    logical(1)
  )]
  stopifnot(length(hit) == 1)
  hit
}

run_moderation <- function(
  data,
  outcome,
  pre_control,
  extra_controls,
  gap_var,
  moderator,
  label
) {
  fml <- as.formula(paste0(
    outcome,
    " ~ ",
    paste(c(pre_control, extra_controls), collapse = " + "),
    " + ",
    gap_var,
    " * treated * ",
    moderator,
    " + rank_gap * treated * ",
    moderator
  ))

  m <- lm_robust(fml, data = data, se_type = "HC2", alpha = ci_alpha)

  cg3 <- find_term(m, c(gap_var, "treated", moderator))
  rg3 <- find_term(m, c("rank_gap", "treated", moderator))

  cat("\n---------------- ", label, " ----------------\n", sep = "")
  cat(sprintf("n = %d\n", m$nobs))

  est <- tidy(m) %>% filter(term %in% c(cg3, rg3))
  print(est, digits = 4)

  cat("\nPRIMARY (1 df): ", cg3, " = 0\n", sep = "")
  p_primary <- est$p.value[est$term == cg3]
  cat(sprintf(
    "  p = %.4f  (Bonferroni threshold %.3f -> %s)\n",
    p_primary,
    bonferroni_alpha,
    if (p_primary < bonferroni_alpha) "REJECT" else "fail to reject"
  ))

  cat("\nSECONDARY (2 df): both triples = 0\n")
  print(
    linearHypothesis(m, c(paste(cg3, "= 0"), paste(rg3, "= 0"))),
    digits = 4
  )

  invisible(list(model = m, p_primary = p_primary, terms = c(cg3, rg3)))
}

cat("\n================ TESTS 1-2: POOLED-ARM MODERATION ================\n")

belief_mod <- run_moderation(
  data = panel_est,
  outcome = "inc_post",
  pre_control = "inc_pre",
  extra_controls = "as.factor(coalition_pre_f)",
  gap_var = "log_crime_gap",
  moderator = "gap_z",
  label = "Belief (incumbent crime-handling post, ANCOVA)"
)

vote_mod <- run_moderation(
  data = panel_est,
  outcome = "Vote_home_post",
  pre_control = "inc_vote",
  extra_controls = c("as.factor(coalition_pre_f)", "as.factor(actual_rank)"),
  gap_var = "log_crime_gap",
  moderator = "gap_z",
  label = "Vote (incumbent-coalition vote post)"
)

cat("\n================ ROBUSTNESS: LOG GAP, CAPPED CRIME GAP ================\n")

invisible(run_moderation(
  data = panel_est,
  outcome = "inc_post",
  pre_control = "inc_pre",
  extra_controls = "as.factor(coalition_pre_f)",
  gap_var = "log_crime_gap",
  moderator = "gap_log_z",
  label = "Belief, log(days) moderator"
))

invisible(run_moderation(
  data = panel_est,
  outcome = "Vote_home_post",
  pre_control = "inc_vote",
  extra_controls = c("as.factor(coalition_pre_f)", "as.factor(actual_rank)"),
  gap_var = "log_crime_gap",
  moderator = "gap_log_z",
  label = "Vote, log(days) moderator"
))

invisible(run_moderation(
  data = panel_est,
  outcome = "inc_post",
  pre_control = "inc_pre",
  extra_controls = "as.factor(coalition_pre_f)",
  gap_var = "crime_gap_capped",
  moderator = "gap_z",
  label = "Belief, capped (not logged) crime gap"
))

# ── 3. Omnibus: arm-by-arm three-way, tested jointly ────────────────────────
#
# This is the specification the question naturally suggests -- let every arm's
# response to the information vary with the gap -- and it is deliberately
# reported LAST. With 4 arms x 2 gap regressors it spends 8 degrees of freedom
# on interactions that the pooled test spends 1 on. Read a null here as "no
# power", not as "no moderation".

omnibus_joint <- function(model, gap_var, moderator, arms) {
  triples <- unlist(lapply(arms, function(a) {
    arm_term <- paste0("as.factor(Treatment_Group)", a)
    c(
      find_term(model, c(gap_var, arm_term, moderator)),
      find_term(model, c("rank_gap", arm_term, moderator))
    )
  }))
  linearHypothesis(model, paste(triples, "= 0"))
}

cat("\n================ TEST 3: ARM-BY-ARM OMNIBUS ================\n")

m_omni <- lm_robust(
  inc_post ~
    inc_pre +
    log_crime_gap * as.factor(Treatment_Group) * gap_z +
    rank_gap * as.factor(Treatment_Group) * gap_z +
    as.factor(coalition_pre_f),
  data = panel_est,
  se_type = "HC2",
  alpha = ci_alpha
)

arms_present <- setdiff(sort(unique(panel_est$Treatment_Group)), "control")
cat("\nBelief: all arm x gap-regressor x gap_z interactions = 0\n")
print(omnibus_joint(m_omni, "log_crime_gap", "gap_z", arms_present), digits = 4)

m_omni_vote <- lm_robust(
  Vote_home_post ~
    inc_vote +
    log_crime_gap * as.factor(Treatment_Group) * gap_z +
    rank_gap * as.factor(Treatment_Group) * gap_z +
    as.factor(coalition_pre_f) +
    as.factor(actual_rank),
  data = panel_est,
  se_type = "HC2",
  alpha = ci_alpha
)

cat("\nVote: all arm x gap-regressor x gap_z interactions = 0\n")
print(
  omnibus_joint(m_omni_vote, "log_crime_gap", "gap_z", arms_present),
  digits = 4
)

# ── 4. Auxiliary checks (1 df each, full sample, no interaction penalty) ─────

cat("\n================ TEST 4: AUXILIARY ================\n")

# 4a. Does the gap move the outcomes directly, holding the pre level fixed?
# A negative coefficient on gap_z here would say beliefs drift (or the pre
# measure decays as a predictor) with elapsed time, independent of treatment.
cat("\n4a. Direct effect of the gap on the outcome levels\n")
print(
  tidy(lm_robust(
    inc_post ~ inc_pre + gap_z + as.factor(coalition_pre_f),
    data = panel_est,
    se_type = "HC2"
  )) %>% filter(term == "gap_z"),
  digits = 4
)
print(
  tidy(lm_robust(
    Vote_home_post ~ inc_vote + gap_z + as.factor(coalition_pre_f),
    data = panel_est,
    se_type = "HC2"
  )) %>% filter(term == "gap_z"),
  digits = 4
)

# Interaction with the pre level: does the wave-1 measure predict the wave-2
# measure less well as the gap grows? This is the cleanest possible statement
# of "priors go stale", it costs 1 df, and it uses every row.
cat("\n4a-bis. Does the pre-treatment level predict the post level less well\n")
cat("        as the gap grows? (inc_pre x gap_z)\n")
print(
  tidy(lm_robust(
    inc_post ~ inc_pre * gap_z + as.factor(coalition_pre_f),
    data = panel_est,
    se_type = "HC2"
  )) %>% filter(term == "inc_pre:gap_z"),
  digits = 4
)

# 4b. Data quality. Built on the pre-filter panel so the attention check and
# the municipality change are themselves outcomes rather than sample filters.
cat("\n4b. Does cross-wave data quality degrade with the gap?\n")
for (dv in c("demo_inconsistent", "muni_changed_num", "attn_pass")) {
  fit <- lm_robust(
    as.formula(paste0(dv, " ~ gap_z")),
    data = panel_full,
    se_type = "HC2"
  )
  cat(sprintf("  %-18s ", dv))
  print(tidy(fit) %>% filter(term == "gap_z"), digits = 4)
}

# 4c. Balance. Treatment is assigned inside wave 2, so the gap CANNOT be caused
# by the arm; a large imbalance here means something is wrong with the link or
# the fielding batches, not with the experiment.
cat("\n4c. Is the gap balanced across arms? (F-test, should be null)\n")
print(
  anova(lm(days_between ~ as.factor(Treatment_Group), data = panel_est)),
  digits = 4
)

# ── Figures ─────────────────────────────────────────────────────────────────
#
# The tertile plot is DESCRIPTIVE ONLY. It is the picture of the split-sample
# analysis this script deliberately does not use as a test -- shown so the shape
# of the pooled interaction is visible, with intervals wide enough to make the
# power cost of splitting obvious.

gap_hist <- ggplot(panel_est, aes(x = days_between)) +
  geom_histogram(binwidth = 1, fill = "#0072B2", color = "white") +
  geom_vline(
    xintercept = median(panel_est$days_between, na.rm = TRUE),
    linetype = "dashed",
    color = "grey30"
  ) +
  labs(
    x = "Days between wave 1 and wave 2",
    y = "Respondents",
    caption = paste0(
      "N = ",
      nrow(panel_est),
      "; dashed line = median. Panel is filtered to gaps > 4 days."
    )
  ) +
  theme_minimal()

ggsave(
  "latex/images/wave_gap_distribution.pdf",
  plot = gap_hist,
  width = 6,
  height = 4
)

panel_est$gap_tertile <- cut(
  panel_est$days_between,
  breaks = quantile(panel_est$days_between, c(0, 1 / 3, 2 / 3, 1), na.rm = TRUE),
  labels = c("Short gap", "Medium gap", "Long gap"),
  include.lowest = TRUE
)

tertile_est <- lapply(levels(panel_est$gap_tertile), function(g) {
  d <- filter(panel_est, gap_tertile == g)
  fit <- lm_robust(
    inc_post ~
      inc_pre + log_crime_gap * treated + rank_gap * treated,
    data = d,
    se_type = "HC2"
  )
  tidy(fit) %>%
    filter(term == find_term(fit, c("log_crime_gap", "treated"))) %>%
    mutate(gap_tertile = g, n = fit$nobs)
}) %>%
  bind_rows()

tertile_plot <- ggplot(
  tertile_est,
  aes(x = estimate, y = gap_tertile)
) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey50") +
  geom_errorbar(
    aes(xmin = conf.low, xmax = conf.high),
    orientation = "y",
    width = 0,
    linewidth = 0.5
  ) +
  geom_point() +
  labs(
    x = "CG x treated interaction (belief outcome)",
    y = NULL,
    caption = paste0(
      "Descriptive only -- the reported test is the continuous ",
      "triple interaction.\nn per tertile: ",
      paste(tertile_est$n, collapse = ", "),
      "; bars 95% CI."
    )
  ) +
  theme_minimal()

ggsave(
  "latex/images/wave_gap_tertile_descriptive.pdf",
  plot = tertile_plot,
  width = 6,
  height = 4
)

# ── 5. Equivalence: can we reject the null that the gap DOES matter? ─────────
#
# Tests 1-4 have the conventional null (no moderation), so failing to reject
# them is not evidence that the gap is irrelevant -- only that we could not show
# it is. Reversing the burden requires an equivalence test (TOST): the null is
# that the moderation is LARGE, |beta| >= delta, and rejecting it licenses the
# positive claim "any moderation is smaller than delta". Same logic as
# code/equivalence_tests.R, but on a single coefficient rather than an omnibus
# ANOVA, so it is two one-sided t-tests done by hand.
#
# Operationally, TOST at alpha rejects if and only if the (1 - 2*alpha)
# confidence interval for beta lies entirely inside (-delta, +delta). That
# equivalence is what makes the "minimum defensible bound" below meaningful:
#
#   delta_min = |beta_hat| + t_{1-alpha, df} * se
#
# is the SMALLEST delta this sample could ever reject. It is a property of the
# data, not of any judgment call, and it is the honest answer to "can I reject
# that they differ?" -- if delta_min is larger than any moderation you would
# call substantively meaningful, the answer is no, and no choice of bound
# rescues it.
#
# TWO BOUNDS ARE REPORTED, because there is no single right delta:
#   (a) RELATIVE. delta = frac_bound x |pooled treatment slope| in the same
#       model. "Moderation is less than half the size of the effect it
#       moderates" is a claim readers can evaluate without knowing the scale.
#   (b) PERMISSIVE. delta = 1.0 x |pooled treatment slope|: "moderation is no
#       larger than the effect it moderates". A weak claim, but it is the
#       weakest claim still worth making, so it is the one most likely to be
#       attainable at this n.
#
# Both bounds are RELATIVE to the pooled slope on purpose. An earlier version
# used an absolute bound of 0.1 x SD(outcome), borrowed from the d = 0.2 balance
# bound in equivalence_tests.R, and it was meaningless here: that bound scales
# the OUTCOME, but the quantity being bounded is a slope-on-a-slope. It came out
# 11-20x larger than the main effect itself and "rejected" at p < 1e-40 while
# establishing nothing. Do not reintroduce it.
#
# A bound chosen AFTER seeing delta_min is not a test. Set these first.

frac_bound <- 0.5
permissive_bound <- 1.0

tost_coef <- function(
  model,
  term,
  delta,
  alpha = 0.05,
  label = "",
  outcome = NA_character_,
  main_slope = NA_real_
) {
  b <- coef(model)[[term]]
  se <- sqrt(diag(vcov(model)))[[term]]
  df <- model$df.residual

  # Two one-sided tests; the binding (larger) p-value is the TOST p-value.
  t_lower <- (b + delta) / se # H0: beta <= -delta
  t_upper <- (b - delta) / se # H0: beta >= +delta
  p_tost <- max(pt(t_lower, df, lower.tail = FALSE), pt(t_upper, df))

  crit <- qt(1 - alpha, df)
  ci90 <- b + c(-1, 1) * crit * se # the (1 - 2*alpha) interval
  delta_min <- abs(b) + crit * se

  cat(sprintf("\n  %s\n", label))
  cat(sprintf(
    "    beta = %.5f (se %.5f), %g%% CI [%.5f, %.5f]\n",
    b,
    se,
    100 * (1 - 2 * alpha),
    ci90[1],
    ci90[2]
  ))
  cat(sprintf("    equivalence bound delta = %.5f\n", delta))
  cat(sprintf(
    "    TOST p = %.4f -> %s\n",
    p_tost,
    if (p_tost < alpha) {
      "REJECT 'moderation is large': equivalence established"
    } else {
      "CANNOT reject: sample is consistent with meaningful moderation"
    }
  ))
  cat(sprintf(
    "    smallest bound this sample could reject: delta_min = %.5f\n",
    delta_min
  ))

  invisible(data.frame(
    outcome = outcome,
    label = label,
    main_slope = main_slope,
    beta = b,
    se = se,
    delta = delta,
    p_tost = p_tost,
    delta_min = delta_min,
    # Both ratios are signed against the main slope. A triple interaction's own
    # sign says nothing on its own -- +0.005 amplifies a positive slope and
    # cancels a negative one -- so the ratio, not the coefficient, is what gets
    # read out loud.
    beta_over_slope = b / main_slope,
    delta_min_over_slope = delta_min / abs(main_slope)
  ))
}

cat("\n================ TEST 5: EQUIVALENCE (TOST) ================\n")
cat("Null is that the gap moderation IS large; rejecting it is the positive\n")
cat("claim that the wave gap does not meaningfully change the effect.\n")

equiv_rows <- list()

for (spec in list(
  list(fit = belief_mod, name = "Belief"),
  list(fit = vote_mod, name = "Vote")
)) {
  m <- spec$fit$model
  triple <- spec$fit$terms[1]
  main_slope <- coef(m)[[find_term(m, c("log_crime_gap", "treated"))]]
  triple_b <- coef(m)[[triple]]

  cat(sprintf(
    "\n%s -- triple interaction %s\n",
    spec$name,
    triple
  ))
  cat(sprintf(
    "  pooled treatment slope = %+.5f (more surprising news -> %s outcome)\n",
    main_slope,
    if (main_slope < 0) "LOWER" else "HIGHER"
  ))
  cat(sprintf(
    "  triple / slope = %+.3f: 1 SD more elapsed time %s the response by %.0f%%\n",
    triple_b / main_slope,
    if (triple_b / main_slope > 0) "AMPLIFIES" else "OFFSETS",
    100 * abs(triple_b / main_slope)
  ))

  for (bnd in c(frac_bound, permissive_bound)) {
    equiv_rows[[length(equiv_rows) + 1]] <- tost_coef(
      m,
      triple,
      delta = bnd * abs(main_slope),
      label = sprintf("%g x |pooled treatment slope|", bnd),
      outcome = spec$name,
      main_slope = main_slope
    )
  }
}

equiv_table <- bind_rows(equiv_rows)

# delta_min expressed against the pooled slope makes the verdict readable
# without the scale: "we can only rule out moderation bigger than X% of the
# effect itself". Above ~100% the equivalence test has told you nothing.
cat("\nVerdict per outcome:\n")
for (o in unique(equiv_table$outcome)) {
  r <- equiv_table[equiv_table$outcome == o, ][1, ]
  cat(sprintf(
    "  %-7s smallest rejectable bound = %.0f%% of the pooled slope -> %s\n",
    o,
    100 * r$delta_min_over_slope,
    if (r$delta_min_over_slope > 1) {
      "CANNOT rule out moderation as large as the effect itself"
    } else {
      "CAN rule out moderation as large as the effect itself"
    }
  ))
}

# ── Summary ─────────────────────────────────────────────────────────────────

cat("\n================ SUMMARY ================\n")
cat(sprintf(
  "Primary p-values (Bonferroni threshold %.3f): belief %.4f, vote %.4f\n",
  bonferroni_alpha,
  belief_mod$p_primary,
  vote_mod$p_primary
))
cat("\nEquivalence (TOST) results:\n")
print(
  equiv_table[, c(
    "outcome",
    "label",
    "beta",
    "se",
    "delta",
    "p_tost",
    "delta_min",
    "beta_over_slope",
    "delta_min_over_slope"
  )],
  digits = 3,
  row.names = FALSE
)
cat("\nFigures written to latex/images/wave_gap_*.pdf\n")
