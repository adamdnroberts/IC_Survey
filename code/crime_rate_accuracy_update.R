library(dplyr)
library(ggplot2)
library(readxl)
library(estimatr)

load("~/IC_Survey/data/survey_panel_dataset.Rdata")

panel$Vote_home_post <- as.integer(
  !is.na(panel$coalition_post) &
    !is.na(panel$home_coalition) &
    panel$home_coalition == panel$coalition_post
)

panel_full <- panel

panel_with_failures <- filter(panel_full, muni_changed == 0)
panel <- filter(panel_with_failures, Attention_Check == "somewhat_agree")

# ── 3. Crime-rate accuracy measures ───────────────────────────────────────────
# home_rate is already carried by the panel (robo join in create_panel_dataset.R).
# Robbery_Estimate is the wave-1 (pre) estimate; Robbery_Estimate_Post is the
# wave-2 (post) estimate. Negative absolute error => higher is more accurate.

panel <- panel %>%
  mutate(
    est_pre = as.numeric(Robbery_Estimate),
    est_post = as.numeric(Robbery_Estimate_Post),
    # Raw-level absolute error (robustness only — heavily right-skewed).
    gap_pre = -abs(home_rate - est_pre),
    gap_post = -abs(home_rate - est_post),
    delta_gap = gap_post - gap_pre,
    # Log-ratio accuracy: 0 = exact, -1 = off by 10x. Compresses the skew that
    # leaves the level measure underpowered. +1 guards against zeros.
    lg_pre = -abs(log10((est_pre + 1) / (home_rate + 1))),
    lg_post = -abs(log10((est_post + 1) / (home_rate + 1))),
    d_lg = lg_post - lg_pre,
    # Interpretable binary: estimate within a factor of 2 of the true rate.
    within2x_pre = as.integer(abs(log2((est_pre + 1) / (home_rate + 1))) <= 1),
    within2x_post = as.integer(abs(log2((est_post + 1) / (home_rate + 1))) <= 1)
  )

panel <- panel %>%
  mutate(
    Treatment_Group = factor(
      Treatment_Group,
      levels = c("control", "control2", "T1", "T2", "T3", "T4")
    ),
    control2 = as.integer(Treatment_Group == "control2"),
    T1 = as.integer(Treatment_Group == "T1"),
    T2 = as.integer(Treatment_Group == "T2"),
    T3 = as.integer(Treatment_Group == "T3"),
    T4 = as.integer(Treatment_Group == "T4")
  )

fit_eq1 <- function(outcome, data) {
  sub <- data[!is.na(data[[outcome]]), ]
  fmla <- as.formula(paste0(outcome, " ~ control2 + T1 + T2 + T3 + T4"))
  lm_robust(fmla, data = sub, se_type = "HC2")
}

# Primary outcomes: log-ratio accuracy (compresses the heavy right skew that
# makes the raw-level |error| measure underpowered) plus an interpretable
# within-factor-of-2 binary. Raw-level gap_post / delta_gap kept as robustness.
outcomes_primary <- c("lg_post", "d_lg", "within2x_post")
outcomes_robust <- c("gap_post", "delta_gap")
outcomes <- c(outcomes_primary, outcomes_robust)
models <- setNames(lapply(outcomes, fit_eq1, data = panel), outcomes)

# ── 7. Two-sided tests for comparison arms (T2, T3, T4) ───────────────────────

extract_results <- function(outcome) {
  m <- models[[outcome]]
  b <- coef(m)[c("T2", "T3", "T4")]
  se <- sqrt(diag(vcov(m)))[c("T2", "T3", "T4")]
  z <- b / se
  data.frame(
    outcome = outcome,
    arm = c("T2", "T3", "T4"),
    estimate = b,
    se = se,
    p_two_sided = 2 * pnorm(-abs(z)),
    row.names = NULL
  )
}

results_df <- do.call(rbind, lapply(outcomes, extract_results))

# ── 8. Summary statistics by arm ──────────────────────────────────────────────

arm_summary <- panel %>%
  group_by(Treatment_Group) %>%
  summarize(
    n = n(),
    lg_post = round(mean(lg_post, na.rm = TRUE), 3),
    d_lg = round(mean(d_lg, na.rm = TRUE), 3),
    within2x_post = round(mean(within2x_post, na.rm = TRUE), 3),
    gap_post = round(mean(gap_post, na.rm = TRUE), 1),
    delta_gap = round(mean(delta_gap, na.rm = TRUE), 1),
    .groups = "drop"
  )

# ── 9. Output ─────────────────────────────────────────────────────────────────

cat("\n=== Sample sizes ===\n")
cat("Total matched respondents (post attn-check filter):", nrow(panel), "\n")
print(table(panel$Treatment_Group))

cat("\n=== Mean crime-rate accuracy by treatment arm ===\n")
print(arm_summary)

cat("\n=== Equation 1 estimates (HC2 SEs) ===\n")
for (nm in outcomes) {
  cat("\nOutcome:", nm, "  (n =", models[[nm]]$nobs, ")\n")
  m <- models[[nm]]
  res <- data.frame(
    estimate = round(coef(m), 4),
    se = round(sqrt(diag(vcov(m))), 4),
    p = round(m$p.value, 4)
  )
  print(res)
}

cat("\n=== Two-sided tests: comparison arms vs. control ===\n")
print(results_df, digits = 3, row.names = FALSE)

# ── 10. Coefficient plot ──────────────────────────────────────────────────────

outcome_labels <- c(
  lg_post = "Log-ratio accuracy (post): -|log10(est/true)|",
  d_lg = "Change in log-ratio accuracy (post - pre)",
  within2x_post = "Within a factor of 2 of true rate (post)",
  gap_post = "Level accuracy (post): -|true - post|",
  delta_gap = "Change in level accuracy (post - pre)"
)

arm_colors <- c(
  control2 = "#999999",
  T1 = "#56B4E9",
  T2 = "#009E73",
  T3 = "#E69F00",
  T4 = "#0072B2"
)

plot_coef_df <- do.call(
  rbind,
  lapply(outcomes_primary, function(nm) {
    m <- models[[nm]]
    b <- coef(m)[c("control2", "T1", "T2", "T3", "T4")]
    se <- sqrt(diag(vcov(m)))[c("control2", "T1", "T2", "T3", "T4")]
    data.frame(
      outcome = nm,
      arm = c("control2", "T1", "T2", "T3", "T4"),
      estimate = b,
      lo95 = b - 1.96 * se,
      hi95 = b + 1.96 * se,
      lo99 = b - 2.576 * se,
      hi99 = b + 2.576 * se,
      row.names = NULL
    )
  })
) %>%
  mutate(
    outcome = factor(
      outcome,
      levels = outcomes_primary,
      labels = outcome_labels[outcomes_primary]
    ),
    arm = factor(arm, levels = c("control2", "T1", "T2", "T3", "T4"))
  )

crime_rate_acc_plot <- ggplot(
  dplyr::filter(
    plot_coef_df,
    outcome == "Change in log-ratio accuracy (post - pre)" & arm != "control2"
  ),
  aes(x = estimate, y = arm, color = arm)
) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey50") +
  geom_linerange(aes(xmin = lo99, xmax = hi99), linewidth = 0.5) +
  geom_linerange(aes(xmin = lo95, xmax = hi95), linewidth = 2, alpha = 0.4) +
  geom_point(size = 2.5) +
  scale_color_manual(values = arm_colors, guide = "none") +
  #facet_wrap(~outcome, scales = "free_x", ncol = 3) +
  labs(
    x = "Change in Accuracy (Post - Pre)",
    y = NULL,
    title = "Crime rate accuracy",
    caption = paste0(
      #Baseline = control (home-only, weather placebo). ",
      #"Pre = wave 1 estimate, post = wave 2 estimate. ",
      "Thick bar 95% CI, thin 99% CI. n = ",
      nrow(panel)
    )
  ) +
  theme_minimal() +
  theme(
    strip.text = element_text(size = 9),
    axis.text.y = element_text(size = 9)
  )

print(crime_rate_acc_plot)

ggsave(
  "latex/images/crime_rate_accuracy_coef_plot.pdf",
  plot = crime_rate_acc_plot,
  width = 3.5,
  height = 4
)
