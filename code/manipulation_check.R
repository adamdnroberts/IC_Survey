# manipulation_check.R
# Manipulation check analysis per PAP §"Manipulation Check"
#
# Two checks:
#   1. Comparative accuracy check — do comparison arms (T2, T3, T4) improve
#      respondents' post-treatment beliefs about relative robbery rankings?
#      Outcomes: tau_b (Kendall concordance), accuracy count acc_i (correct
#      classifications out of 4); both in levels (post) and as change (post - pre).
#   2. Placebo check — does the weather chart alone (control2) produce
#      crime-driven belief updating on the home-municipality slider?
#
# Estimating equation (PAP Eq. 1):
#   Outcome_i = mu + lambda0*control2 + lambda1*T1 + lambda2*T2 + lambda3*T3 + lambda4*T4 + e_i
#
# Both outcomes are higher-is-better. Two-sided tests throughout.
#
# Data source: data/wave2_responses.rds (has Comparison_Muni_*_ID columns)

library(dplyr)
library(estimatr)

# ── 1. Load data ──────────────────────────────────────────────────────────────

if (!file.exists("data/wave2_responses.rds")) {
  stop("data/wave2_responses.rds not found. Run pull_responses.R first.")
}

d <- readRDS("data/wave2_responses.rds")

d <- filter(d, Attention_Check == "somewhat_agree")
d <- filter(
  d,
  is.na(Robbery_Estimate_Post) | as.numeric(Robbery_Estimate_Post) <= 100000
)


robo_rate <- readRDS("data/robo_2025.rds") |>
  mutate(
    CVEGEO = formatC(Cve..Municipio, width = 5, flag = "0", format = "d")
  ) |>
  select(CVEGEO, rate_per_100k)

# ── 2. Filter valid respondents ───────────────────────────────────────────────

d <- d |>
  filter(!is.na(Found_Municipality_ID) & Found_Municipality_ID != "")

# ── 3. Standardize CVEGEOs and join robbery rates ─────────────────────────────

pad5 <- function(x) {
  suppressWarnings(
    ifelse(
      is.na(x) | x == "",
      NA_character_,
      formatC(as.integer(x), width = 5, flag = "0", format = "d")
    )
  )
}

d <- d |>
  mutate(
    home_id = pad5(Found_Municipality_ID),
    comp_id1 = pad5(Comparison_Muni_1_ID),
    comp_id2 = pad5(Comparison_Muni_2_ID),
    comp_id3 = pad5(Comparison_Muni_3_ID),
    comp_id4 = pad5(Comparison_Muni_4_ID)
  )

rate_lookup <- setNames(robo_rate$rate_per_100k, robo_rate$CVEGEO)

d <- d |>
  mutate(
    rate_home = rate_lookup[home_id],
    rate_comp1 = rate_lookup[comp_id1],
    rate_comp2 = rate_lookup[comp_id2],
    rate_comp3 = rate_lookup[comp_id3],
    rate_comp4 = rate_lookup[comp_id4],
    ratio1 = rate_comp1 / rate_home,
    ratio2 = rate_comp2 / rate_home,
    ratio3 = rate_comp3 / rate_home,
    ratio4 = rate_comp4 / rate_home
  )

# ── 4. Helper functions ───────────────────────────────────────────────────────

# Perceived direction: +1 = comp has more crime, 0 = same, -1 = less
rank_to_dir <- function(x) {
  case_when(
    x %in% c("more_than_double", "more") ~ 1L,
    x == "same" ~ 0L,
    x %in% c("fewer", "less_than_half") ~ -1L,
    TRUE ~ NA_integer_
  )
}

# Perceived numeric score for tau-b (higher = more crime perceived)
rank_to_score <- function(x) {
  case_when(
    x == "more_than_double" ~ 2L,
    x == "more" ~ 1L,
    x == "same" ~ 0L,
    x == "fewer" ~ -1L,
    x == "less_than_half" ~ -2L,
    TRUE ~ NA_integer_
  )
}

# True direction based on ratio C_j / C_home, with bandwidth `thresh`
true_dir <- function(ratio, thresh) {
  case_when(
    ratio > 1 + thresh ~ 1L,
    ratio < 1 - thresh ~ -1L,
    !is.na(ratio) ~ 0L,
    TRUE ~ NA_integer_
  )
}

# Kendall's tau-b between perceived scores and actual ratios (4 munis per respondent)
kendall_tb <- function(s1, s2, s3, s4, r1, r2, r3, r4) {
  scores <- c(s1, s2, s3, s4)
  ratios <- c(r1, r2, r3, r4)
  ok <- !is.na(scores) & !is.na(ratios)
  if (sum(ok) < 2) {
    return(NA_real_)
  }
  suppressWarnings(cor(scores[ok], ratios[ok], method = "kendall"))
}

# Accuracy count: number of correctly classified pairs (higher = better).
# Returns NA if respondent provided no perceived directions.
acc_count <- function(d1, d2, d3, d4, t1, t2, t3, t4) {
  dirs <- c(d1, d2, d3, d4)
  trues <- c(t1, t2, t3, t4)
  if (all(is.na(dirs))) {
    return(NA_integer_)
  }
  ok <- !is.na(dirs) & !is.na(trues)
  sum(dirs[ok] == trues[ok])
}

# ── 5. Compute accuracy measures for a given threshold ────────────────────────

compute_accuracy <- function(data, thresh = 0.25) {
  data |>
    rowwise() |>
    mutate(
      score_pre1 = rank_to_score(Crime_Rank_Comp_1),
      score_pre2 = rank_to_score(Crime_Rank_Comp_2),
      score_pre3 = rank_to_score(Crime_Rank_Comp_3),
      score_pre4 = rank_to_score(Crime_Rank_Comp_4),
      score_post1 = rank_to_score(Crime_Rank_Comp_1_Post),
      score_post2 = rank_to_score(Crime_Rank_Comp_2_Post),
      score_post3 = rank_to_score(Crime_Rank_Comp_3_Post),
      score_post4 = rank_to_score(Crime_Rank_Comp_4_Post),
      dir_pre1 = rank_to_dir(Crime_Rank_Comp_1),
      dir_pre2 = rank_to_dir(Crime_Rank_Comp_2),
      dir_pre3 = rank_to_dir(Crime_Rank_Comp_3),
      dir_pre4 = rank_to_dir(Crime_Rank_Comp_4),
      dir_post1 = rank_to_dir(Crime_Rank_Comp_1_Post),
      dir_post2 = rank_to_dir(Crime_Rank_Comp_2_Post),
      dir_post3 = rank_to_dir(Crime_Rank_Comp_3_Post),
      dir_post4 = rank_to_dir(Crime_Rank_Comp_4_Post),
      true1 = true_dir(ratio1, thresh),
      true2 = true_dir(ratio2, thresh),
      true3 = true_dir(ratio3, thresh),
      true4 = true_dir(ratio4, thresh),
      tau_pre = kendall_tb(
        score_pre1,
        score_pre2,
        score_pre3,
        score_pre4,
        ratio1,
        ratio2,
        ratio3,
        ratio4
      ),
      tau_post = kendall_tb(
        score_post1,
        score_post2,
        score_post3,
        score_post4,
        ratio1,
        ratio2,
        ratio3,
        ratio4
      ),
      delta_tau = tau_post - tau_pre,
      acc_pre = acc_count(
        dir_pre1,
        dir_pre2,
        dir_pre3,
        dir_pre4,
        true1,
        true2,
        true3,
        true4
      ),
      acc_post = acc_count(
        dir_post1,
        dir_post2,
        dir_post3,
        dir_post4,
        true1,
        true2,
        true3,
        true4
      ),
      delta_acc = acc_post - acc_pre
    ) |>
    ungroup()
}

d <- compute_accuracy(d, thresh = 0.25)

# ── 6. Treatment indicators (control = baseline) ──────────────────────────────

d <- d |>
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

# ── 7. Robbery estimate gap (true - post estimate) ───────────────────────────

d <- d |>
  mutate(gap_post = -abs(rate_home - as.numeric(Robbery_Estimate_Post)))

# ── 8. Fit Equation 1 for each accuracy outcome ───────────────────────────────

fit_eq1 <- function(outcome, data) {
  sub <- data[!is.na(data[[outcome]]), ]
  fmla <- as.formula(paste0(outcome, " ~ control2 + T1 + T2 + T3 + T4"))
  lm_robust(fmla, data = sub, se_type = "HC2")
}

rank_outcomes <- c("tau_post", "acc_post", "delta_tau", "delta_acc")
rob_outcomes <- c("gap_post")
acc_outcomes <- c(rank_outcomes, rob_outcomes)
models_acc <- setNames(lapply(acc_outcomes, fit_eq1, data = d), acc_outcomes)

# ── 9. Two-sided p-values for comparison arms ────────────────────────────────

extract_results <- function(outcome) {
  m <- models_acc[[outcome]]
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

results_df <- do.call(rbind, lapply(acc_outcomes, extract_results))

# ── 10. Sensitivity checks (±20% and ±33% thresholds, ranking outcomes only) ──

sensitivity_results <- lapply(c(0.20, 0.33), function(thresh) {
  d_s <- compute_accuracy(d, thresh = thresh)
  d_s$gap_post <- -abs(d_s$rate_home - as.numeric(d_s$Robbery_Estimate_Post))
  fits <- setNames(lapply(rank_outcomes, fit_eq1, data = d_s), rank_outcomes)
  do.call(
    rbind,
    lapply(rank_outcomes, function(nm) {
      m <- fits[[nm]]
      b <- coef(m)[c("T2", "T3", "T4")]
      se <- sqrt(diag(vcov(m)))[c("T2", "T3", "T4")]
      data.frame(
        thresh = thresh,
        outcome = nm,
        arm = c("T2", "T3", "T4"),
        estimate = b,
        se = se,
        p_two_sided = 2 * pnorm(-abs(b / se)),
        row.names = NULL
      )
    })
  )
})
sensitivity_df <- do.call(rbind, sensitivity_results)

# ── 10. Summary statistics by arm ─────────────────────────────────────────────

arm_summary <- d |>
  group_by(Treatment_Group) |>
  summarize(
    n = n(),
    tau_pre = round(mean(tau_pre, na.rm = TRUE), 3),
    tau_post = round(mean(tau_post, na.rm = TRUE), 3),
    delta_tau = round(mean(delta_tau, na.rm = TRUE), 3),
    acc_pre = round(mean(acc_pre, na.rm = TRUE), 3),
    acc_post = round(mean(acc_post, na.rm = TRUE), 3),
    delta_acc = round(mean(delta_acc, na.rm = TRUE), 3),
    gap_post = round(mean(gap_post, na.rm = TRUE), 1),
    .groups = "drop"
  )

# ── 11. Output ────────────────────────────────────────────────────────────────

cat("\n=== Sample sizes ===\n")
cat("Total respondents:", nrow(d), "\n")
cat("By treatment arm:\n")
print(table(d$Treatment_Group))

cat("\n=== Mean accuracy by treatment arm (threshold = ±25%) ===\n")
print(arm_summary)

cat("\n=== Equation 1 estimates (HC2 SEs) ===\n")
for (nm in acc_outcomes) {
  cat("\nOutcome:", nm, "\n")
  m <- models_acc[[nm]]
  res <- data.frame(
    estimate = round(coef(m), 4),
    se = round(sqrt(diag(vcov(m))), 4),
    p = round(m$p.value, 4)
  )
  print(res)
}

cat("\n=== Two-sided tests: comparison arms vs. control ===\n")
print(results_df, digits = 3, row.names = FALSE)

cat("\n=== Sensitivity: ±20% and ±33% thresholds ===\n")
print(sensitivity_df, digits = 3, row.names = FALSE)

# ── 12. Plots ─────────────────────────────────────────────────────────────────

library(ggplot2)

outcome_labels <- c(
  tau_post = "Kendall tau-b (post)",
  acc_post = "Accuracy count (post)",
  delta_tau = "Change in Kendall tau-b (post - pre)",
  delta_acc = "Change in accuracy count (post - pre)",
  gap_post = "Robbery estimate accuracy: -|true - post|"
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
  lapply(acc_outcomes, function(nm) {
    m <- models_acc[[nm]]
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
) |>
  mutate(
    outcome = factor(
      outcome,
      levels = acc_outcomes,
      labels = outcome_labels[acc_outcomes]
    ),
    arm = factor(arm, levels = c("control2", "T1", "T2", "T3", "T4"))
  )

manip_coef_plot <- ggplot(
  plot_coef_df,
  aes(x = estimate, y = arm, color = arm)
) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey50") +
  geom_linerange(aes(xmin = lo95, xmax = hi95), linewidth = 0.7) +
  geom_point(size = 2.5) +
  scale_color_manual(values = arm_colors, guide = "none") +
  facet_wrap(~outcome, scales = "free", ncol = 2) +
  labs(
    x = "Estimate relative to control (HC2 SEs, 95% CI)",
    y = NULL,
    title = "Manipulation check estimates",
    caption = paste0(
      "Baseline = control (home-only, weather placebo). Threshold = ±25%. n = ",
      nrow(d)
    )
  ) +
  theme_classic() +
  theme(
    strip.text = element_text(size = 9),
    axis.text.y = element_text(size = 9)
  )

print(manip_coef_plot)

ggsave(
  "latex/images/manip_check_coef_plot.pdf",
  plot = manip_coef_plot,
  width = 8,
  height = 5
)

# ── Threshold sensitivity plot ────────────────────────────────────────────────

threshold_labels <- c("0.2" = "±20%", "0.25" = "±25%", "0.33" = "±33%")

rank_outcomes <- c("acc_post", "delta_acc")

sensitivity_plot_df <- bind_rows(
  results_df %>%
    filter(outcome %in% rank_outcomes) %>%
    mutate(thresh = "0.25"),
  sensitivity_df %>%
    mutate(thresh = as.character(thresh))
) %>%
  mutate(
    lo95 = estimate - 1.96 * se,
    hi95 = estimate + 1.96 * se,
    thresh = factor(
      thresh,
      levels = c("0.2", "0.25", "0.33"),
      labels = threshold_labels
    ),
    outcome = factor(
      outcome,
      levels = rank_outcomes,
      labels = outcome_labels[rank_outcomes]
    ),
    arm = factor(arm, levels = c("T2", "T3", "T4"))
  )

sensitivity_plot <- ggplot(
  subset(sensitivity_plot_df, !is.na(outcome)),
  aes(x = estimate, xmin = lo95, xmax = hi95, y = thresh, color = arm)
) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey50") +
  geom_pointrange(position = position_dodge(width = 0.5)) +
  scale_color_manual(values = arm_colors[c("T2", "T3", "T4")]) +
  facet_wrap(~outcome, scales = "free_x", ncol = 2) +
  labs(
    x = "Estimate relative to control (95% CI)",
    y = "Threshold",
    color = "Treatment arm",
    title = "Sensitivity to accuracy threshold definition",
    caption = paste0("n = ", nrow(d))
  ) +
  theme_classic() +
  theme(strip.text = element_text(size = 9))

print(sensitivity_plot)

ggsave(
  "latex/images/manip_check_sensitivity_plot.pdf",
  plot = sensitivity_plot,
  width = 8,
  height = 5
)
