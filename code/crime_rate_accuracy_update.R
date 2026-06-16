# crime_rate_accuracy_update.R
# Crime-RATE (level) accuracy: does the treatment improve respondents'
# numeric robbery-rate estimate for their home municipality?
#
# Two outcomes (both higher-is-better, negative absolute error):
#   gap_post  = -|true_rate - post estimate|        (level accuracy after treatment)
#   delta_gap = gap_post - gap_pre                  (change in accuracy, post - pre)
#
# The PRE estimate is the wave-1 robbery estimate and the POST estimate is the
# wave-2 post-treatment robbery estimate (identical question wording), so this
# requires the cross-wave PID merge. (manipulation_check.R only has gap_post
# because it uses wave 2 alone, which has no pre-treatment numeric estimate.)
#
# Estimating equation (PAP Eq. 1), control = baseline:
#   Outcome_i = mu + lambda0*control2 + lambda1*T1 + lambda2*T2
#                  + lambda3*T3 + lambda4*T4 + e_i

library(dplyr)
library(ggplot2)
library(readxl)
library(estimatr)

# ── 1. Load data ──────────────────────────────────────────────────────────────

wave1 <- readRDS("data/wave1_responses.rds")
wave2 <- readRDS("data/wave2_responses.rds")

# Drop within-wave duplicate respondents (same Netquest_PID submitted more than
# once) so the cross-wave join is strictly one-to-one. Keeps the first row.
wave1 <- distinct(wave1, Netquest_PID, .keep_all = TRUE)
wave2 <- distinct(wave2, Netquest_PID, .keep_all = TRUE)

match_ids <- read_excel("data/Match ID.xlsx")
match_ids <- janitor::clean_names(match_ids)
# Columns are now snake_case: wave_1, wave_2, status_wave_2

match_ids2 <- read.csv(
  "data/wave_match_ids.csv",
  stringsAsFactors = FALSE,
  fileEncoding = "UTF-8-BOM"
)

# ── 2. Cross-wave PID merge (same logic as belief_update_analysis.R) ──────────

match_ids <- match_ids %>%
  rename(
    pid_w1 = wave_1,
    pid_w2 = wave_2,
    status_w2 = status_wave_2
  ) %>%
  select(pid_w2, pid_w1) %>%
  mutate(across(c(pid_w2, pid_w1), as.character))

match_ids2 <- match_ids2 %>%
  rename(
    pid_w1 = Wave.1,
    pid_w2 = Wave.2
  ) %>%
  select(pid_w2, pid_w1) %>%
  mutate(across(c(pid_w2, pid_w1), as.character))

# Append the second match table, drop exact duplicate pairs, then keep only
# clean one-to-one matches before merging the two waves.
match_ids_new <- bind_rows(match_ids, match_ids2) %>%
  distinct(pid_w2, pid_w1) %>%
  add_count(pid_w2, name = "n_w2") %>%
  add_count(pid_w1, name = "n_w1")

test <- wave2 %>%
  full_join(match_ids_new, by = c("Netquest_PID" = "pid_w2")) %>%
  full_join(
    wave1,
    by = c("pid_w1" = "Netquest_PID"),
    suffix = c("_w2", "_w1")
  ) %>%
  # select(-pid_w1) %>%
  filter(
    Found_Municipality_ID_w2 == Found_Municipality_ID_w1 &
      NQ_Region_w2 == NQ_Region_w1 &
      NQ_SEL_w2 == NQ_SEL_w1 &
      (as.numeric(NQ_Age_w2) == as.numeric(NQ_Age_w1) + 1 |
        NQ_Age_w2 == NQ_Age_w1)
  ) %>%
  # # Found_Municipality_ID is in both waves, so it was suffixed; rebuild the
  # # unified column the downstream code expects.
  mutate(
    Found_Municipality_ID = Found_Municipality_ID_w2,
    # Days between wave-1 and wave-2 responses (timestamps are character strings
    # like "2026-04-10 14:57:42.651886").
    ts_w1 = as.POSIXct(Timestamp_w1, format = "%Y-%m-%d %H:%M:%OS", tz = "UTC"),
    ts_w2 = as.POSIXct(Timestamp_w2, format = "%Y-%m-%d %H:%M:%OS", tz = "UTC"),
    days_between = as.numeric(difftime(ts_w2, ts_w1, units = "days"))
  )

test_dupes <- filter(test, n_w2 > 1)
test_dupes$wrong_mun <- test_dupes$Found_Municipality_ID_w2 !=
  test_dupes$Found_Municipality_ID_w1
test_dupes$wrong_region <- test_dupes$NQ_Region_w2 != test_dupes$NQ_Region_w1
test_dupes$wrong_age <- test_dupes$NQ_Age_w2 != test_dupes$NQ_Age_w1
test_dupes$wrong_sel <- test_dupes$NQ_SEL_w2 != test_dupes$NQ_SEL_w1
test_dupes$wrong_sex <- test_dupes$NQ_Sex_w2 != test_dupes$NQ_Sex_w1


table(test_dupes$wrong_mun, test_dupes$wrong_sel)
table(test_dupes$wrong_mun, test_dupes$wrong_age)
table(test_dupes$wrong_mun, test_dupes$wrong_sel)
table(test_dupes$wrong_mun, test_dupes$wrong_sex)
# ── 3. True home robbery rate ─────────────────────────────────────────────────

robo <- readRDS("data/robo_2025.rds") %>%
  mutate(Cve..Municipio = sprintf("%05d", as.integer(Cve..Municipio))) %>%
  select(Cve..Municipio, rate_per_100k)

test <- test %>%
  left_join(robo, by = c("Found_Municipality_ID" = "Cve..Municipio")) %>%
  rename(home_rate = rate_per_100k)

# ── 4. Crime-rate accuracy measures ───────────────────────────────────────────
# Robbery_Estimate is the wave-1 (pre) estimate; Robbery_Estimate_Post is the
# wave-2 (post) estimate. Negative absolute error => higher is more accurate.

est_cap <- 100000 # drop implausible estimates, matching manipulation_check.R

test <- test %>%
  mutate(
    est_pre = as.numeric(Robbery_Estimate),
    est_post = as.numeric(Robbery_Estimate_Post),
    est_pre = if_else(!is.na(est_pre) & est_pre > est_cap, NA_real_, est_pre),
    est_post = if_else(
      !is.na(est_post) & est_post > est_cap,
      NA_real_,
      est_post
    ),
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

# ── 5. Sample filter and treatment indicators ─────────────────────────────────

test <- filter(test, Attention_Check == "somewhat_agree")

# Require at least 6 days between the two waves (drop fast re-takes / mismatches).
test <- filter(test, !is.na(days_between) & days_between >= 6)

test <- test %>%
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

# ── 6. Fit Equation 1 for each outcome ────────────────────────────────────────

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
models <- setNames(lapply(outcomes, fit_eq1, data = test), outcomes)

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

arm_summary <- test %>%
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
cat("Total matched respondents (post attn-check filter):", nrow(test), "\n")
print(table(test$Treatment_Group))

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
  plot_coef_df,
  aes(x = estimate, y = arm, color = arm)
) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey50") +
  geom_linerange(aes(xmin = lo95, xmax = hi95), linewidth = 0.7) +
  geom_point(size = 2.5) +
  scale_color_manual(values = arm_colors, guide = "none") +
  facet_wrap(~outcome, scales = "free_x", ncol = 3) +
  labs(
    x = "Estimate relative to control (HC2 SEs, 95% CI)",
    y = NULL,
    title = "Crime-rate accuracy: treatment effects",
    caption = paste0(
      "Baseline = control (home-only, weather placebo). ",
      "Pre = wave 1 estimate, post = wave 2 estimate. n = ",
      nrow(test)
    )
  ) +
  theme_classic() +
  theme(
    strip.text = element_text(size = 9),
    axis.text.y = element_text(size = 9)
  )

print(crime_rate_acc_plot)

ggsave(
  "latex/images/crime_rate_accuracy_coef_plot.pdf",
  plot = crime_rate_acc_plot,
  width = 11,
  height = 4.5
)
