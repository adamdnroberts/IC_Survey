library(dplyr)

# Loads `panel` and attaches the comparison-importance index and Vote_home_post.
# Shared with index_similarity_linear_models.R so both scripts run standalone.
source("code/index_similarity/build_importance_index.R")

# ── Mean similarity score by treatment arm ────────────────────────────────────
# Comparison municipalities are drawn at random within arm, so these means
# should be close to equal across arms. A systematic difference would mean the
# arms were shown differently salient comparisons, which would confound the
# importance interaction estimated in the GAM below. Reported on the analysis
# sample (muni unchanged, attention check passed) so it describes the
# respondents the models actually use.
similarity_sample <- panel %>%
  filter(
    muni_changed == 0,
    Attention_Check == "somewhat_agree",
    !is.na(comparison_importance_lp)
  )

similarity_stats <- function(df) {
  summarise(
    df,
    n = n(),
    mean_lp = mean(comparison_importance_lp),
    sd_lp = sd(comparison_importance_lp),
    mean_prob = mean(comparison_importance_prob),
    .groups = "drop"
  )
}

similarity_by_arm <- bind_rows(
  similarity_stats(group_by(similarity_sample, Arm = Treatment_Group)),
  mutate(similarity_stats(similarity_sample), Arm = "All", .before = 1)
)

cat("\n=== Mean comparison-similarity score by treatment arm ===\n")
cat("(comparison_importance_lp: benchmark-model log-odds, averaged over the\n")
cat(" comparisons each respondent was shown; mean_prob is the probability scale)\n\n")
print(as.data.frame(similarity_by_arm), row.names = FALSE, digits = 3)

# ── GAM: does the RG × treatment effect vary with comparison importance? ───────
# Mirrors vote_update_gam.R (same outcome, sample restrictions, and nuisance
# controls), but replaces the 1-D by-arm smooth s(rank_gap, by = arm_group) with
# a 2-D tensor-product smooth te(rank_gap, comparison_importance_lp, by =
# arm_group). This lets the rank_gap updating slope bend as comparison
# importance changes, and estimates that bending separately by arm — i.e. how
# the RG × treatment effect on incumbent vote moves across comparison_importance_lp.
library(ggplot2)
library(mgcv)

# Sample restrictions mirror vote_update_gam.R; the outcome Vote_home_post comes
# from build_importance_index.R.
gam_panel <- panel %>%
  filter(muni_changed == 0, Attention_Check == "somewhat_agree")

gam_panel$coalition_pre[is.na(gam_panel$coalition_pre)] <- "Other"
gam_panel$inc_vote <- as.numeric(
  gam_panel$coalition_pre == gam_panel$home_coalition
)

curve_arms <- c("control", "T1", "T2", "T3", "T4")
arm_group_levels <- c("control", "T1", "Comparison")
coalition_pre_mode <- names(which.max(table(gam_panel$coalition_pre)))

# Pool T2–T4 as "Comparison" (as in vote_update_gam.R). Control is the reference
# arm; both it and the Comparison arm now carry comparison_importance_lp.
gam_data <- gam_panel %>%
  filter(
    Treatment_Group %in% curve_arms,
    !is.na(rank_gap),
    !is.na(comparison_importance_lp)
  ) %>%
  mutate(
    arm_group = factor(
      case_when(
        Treatment_Group == "control" ~ "control",
        Treatment_Group == "T1" ~ "T1",
        TRUE ~ "Comparison"
      ),
      levels = arm_group_levels
    ),
    coalition_pre = factor(coalition_pre)
  )

# rank_gap is discrete (integers), so k = 5 on that margin, as in vote_update_gam.R.
m_vote_gam_ci <- gam(
  Vote_home_post ~
    arm_group +
    s(rank_gap, k = 5) +
    te(rank_gap, comparison_importance_lp, by = arm_group, k = c(5, 5)) +
    s(log_crime_gap) +
    te(log_crime_gap, comparison_importance_lp, by = arm_group, k = c(5, 5)) +
    #coalition_pre +
    inc_vote,
  family = binomial(),
  data = gam_data,
  method = "REML"
)

summary(m_vote_gam_ci)


# ── Plot: Comparison − control difference in P(incumbent vote) over comparison ─
# importance, with one line per rank_gap value (accurate prior RG = 0, and
# optimistic / pessimistic priors RG = ±2). Both arms are evaluated at the same
# rank_gap and importance, so the difference isolates the treatment effect and
# how it shifts across comparison importance. crime_gap_capped at its mean,
# coalition_pre at its mode, inc_vote = 1. Probability-scale SEs via delta method
# from the model lpmatrix + covariance (as in vote_update_gam.R's contrast).
crit95 <- qnorm(0.975)
b <- coef(m_vote_gam_ci)
V <- vcov(m_vote_gam_ci)

lp_row <- function(rg, ci, arm, iv = 1) {
  predict(
    m_vote_gam_ci,
    newdata = data.frame(
      rank_gap = rg,
      comparison_importance_lp = ci,
      arm_group = factor(arm, levels = arm_group_levels),
      crime_gap_capped = mean(gam_data$crime_gap_capped, na.rm = TRUE),
      coalition_pre = factor(
        coalition_pre_mode,
        levels = levels(gam_data$coalition_pre)
      ),
      inc_vote = iv
    ),
    type = "lpmatrix"
  )
}

# x-axis: sequence over the comparison-importance range seen in the Comparison arm.
comp_ci <- gam_data$comparison_importance_lp[gam_data$arm_group == "Comparison"]
ci_seq <- seq(
  quantile(comp_ci, 0.05, na.rm = TRUE),
  quantile(comp_ci, 0.95, na.rm = TRUE),
  length.out = 80
)

# lines: a few representative rank_gap values (discrete integers).
rank_levels <- c(-2, 0, 2)

diff_curve <- bind_rows(lapply(rank_levels, function(rg) {
  bind_rows(lapply(ci_seq, function(ci_val) {
    Xc <- lp_row(rg, ci_val, "Comparison")
    X0 <- lp_row(rg, ci_val, "control")
    pc <- plogis(as.vector(Xc %*% b))
    p0 <- plogis(as.vector(X0 %*% b))
    d <- pc - p0
    grad <- (pc * (1 - pc)) * Xc - (p0 * (1 - p0)) * X0
    se <- sqrt(as.vector(grad %*% V %*% t(grad)))
    data.frame(
      comparison_importance_lp = ci_val,
      rank_gap = rg,
      diff = d,
      lwr95 = d - crit95 * se,
      upr95 = d + crit95 * se
    )
  }))
}))

diff_curve$rank_gap <- factor(
  diff_curve$rank_gap,
  levels = rank_levels,
  labels = c(
    "RG = -2 (pessimistic)",
    "RG = 0 (accurate)",
    "RG = +2 (optimistic)"
  )
)

rg_colors <- c(
  "RG = -2 (pessimistic)" = "#0072B2",
  "RG = 0 (accurate)" = "#999999",
  "RG = +2 (optimistic)" = "#E69F00"
)

vote_update_curve_importance <- ggplot(
  diff_curve,
  aes(
    x = comparison_importance_lp,
    y = diff,
    color = rank_gap,
    fill = rank_gap
  )
) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +
  geom_ribbon(aes(ymin = lwr95, ymax = upr95), alpha = 0.15, color = NA) +
  geom_line(linewidth = 0.8) +
  scale_color_manual(values = rg_colors) +
  scale_fill_manual(values = rg_colors) +
  labs(
    y = "P(incumbent vote): Comparison − control",
    x = "Comparison importance (LP)",
    color = "Rank gap (RG)",
    fill = "Rank gap (RG)",
    caption = "ribbon 95% CI; importance shown over 5th–95th percentile range"
  ) +
  facet_wrap(~rank_gap) +
  theme_minimal()

print(vote_update_curve_importance)

# ggsave(
#   "latex/images/vote_update_curve_importance.pdf",
#   plot = vote_update_curve_importance,
#   width = 7,
#   height = 4.5
# )

# ── Robustness: keep T2 / T3 / T4 separate (not pooled) ───────────────────────
# Same specification as the headline, but arm_group is the full Treatment_Group
# level, so each comparison arm gets its own tensor surface. control and T1 stay
# in the fit for the reference/nuisance terms but are not plotted. Cells are
# thinner here (and hence CIs wider); this is a robustness check on the pooled
# "Comparison" result above.
arm_levels_sep <- c("control", "T1", "T2", "T3", "T4")
sep_groups <- c("T2", "T3", "T4")

gam_data_sep <- gam_panel %>%
  filter(
    Treatment_Group %in% arm_levels_sep,
    !is.na(rank_gap),
    !is.na(comparison_importance_lp)
  ) %>%
  mutate(
    arm_group = factor(Treatment_Group, levels = arm_levels_sep),
    coalition_pre = factor(coalition_pre)
  )

m_vote_gam_ci_sep <- gam(
  Vote_home_post ~
    arm_group +
    te(rank_gap, comparison_importance_lp, by = arm_group, k = c(5, 5)) +
    s(crime_gap_capped, k = 5) +
    coalition_pre +
    inc_vote,
  family = binomial(),
  data = gam_data_sep,
  method = "REML"
)

summary(m_vote_gam_ci_sep)

b_sep <- coef(m_vote_gam_ci_sep)
V_sep <- vcov(m_vote_gam_ci_sep)

lp_row_sep <- function(rg, ci, arm, iv = 1) {
  predict(
    m_vote_gam_ci_sep,
    newdata = data.frame(
      rank_gap = rg,
      comparison_importance_lp = ci,
      arm_group = factor(arm, levels = arm_levels_sep),
      crime_gap_capped = mean(gam_data_sep$crime_gap_capped, na.rm = TRUE),
      coalition_pre = factor(
        coalition_pre_mode,
        levels = levels(gam_data_sep$coalition_pre)
      ),
      inc_vote = iv
    ),
    type = "lpmatrix"
  )
}

# Each arm gets its own comparison-importance range (5th–95th pctile) so we don't
# extrapolate an arm past the comparisons it actually showed.
diff_curve_sep <- bind_rows(lapply(sep_groups, function(arm) {
  arm_ci <- gam_data_sep$comparison_importance_lp[
    gam_data_sep$arm_group == arm
  ]
  ci_seq_a <- seq(
    quantile(arm_ci, 0.05, na.rm = TRUE),
    quantile(arm_ci, 0.95, na.rm = TRUE),
    length.out = 80
  )
  bind_rows(lapply(rank_levels, function(rg) {
    bind_rows(lapply(ci_seq_a, function(ci_val) {
      Xa <- lp_row_sep(rg, ci_val, arm)
      X0 <- lp_row_sep(rg, ci_val, "control")
      pa <- plogis(as.vector(Xa %*% b_sep))
      p0 <- plogis(as.vector(X0 %*% b_sep))
      d <- pa - p0
      grad <- (pa * (1 - pa)) * Xa - (p0 * (1 - p0)) * X0
      se <- sqrt(as.vector(grad %*% V_sep %*% t(grad)))
      data.frame(
        arm = arm,
        comparison_importance_lp = ci_val,
        rank_gap = rg,
        diff = d,
        lwr95 = d - crit95 * se,
        upr95 = d + crit95 * se
      )
    }))
  }))
}))

diff_curve_sep$rank_gap <- factor(
  diff_curve_sep$rank_gap,
  levels = rank_levels,
  labels = c(
    "RG = -2 (pessimistic)",
    "RG = 0 (accurate)",
    "RG = +2 (optimistic)"
  )
)
diff_curve_sep$arm <- factor(diff_curve_sep$arm, levels = sep_groups)

vote_update_curve_importance_sep <- ggplot(
  diff_curve_sep,
  aes(
    x = comparison_importance_lp,
    y = diff,
    color = rank_gap,
    fill = rank_gap
  )
) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +
  geom_ribbon(aes(ymin = lwr95, ymax = upr95), alpha = 0.12, color = NA) +
  geom_line(linewidth = 0.8) +
  scale_color_manual(values = rg_colors) +
  scale_fill_manual(values = rg_colors) +
  facet_grid(arm ~ rank_gap) +
  labs(
    y = "P(incumbent vote): arm − control",
    x = "Comparison importance (LP)",
    color = "Rank gap (RG)",
    fill = "Rank gap (RG)",
    caption = "ribbon 95% CI; importance over each arm's 5th–95th percentile range"
  ) +
  theme_minimal()

print(vote_update_curve_importance_sep)

# ggsave(
#   "latex/images/vote_update_curve_importance_byarm.pdf",
#   plot = vote_update_curve_importance_sep,
#   width = 9,
#   height = 7
# )
