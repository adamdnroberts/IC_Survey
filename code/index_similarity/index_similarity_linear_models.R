# ── LPM twin of index_comparisons.R's headline GAM (m_vote_gam_ci) ────────────
# Same outcome, sample, and Comparison-control importance plot as the GAM, but
# the tensor smooths te(rank_gap, importance) / te(log_crime_gap, importance) are
# replaced by linear interactions: arm_group fully interacted with rank_gap,
# log_crime_gap, and comparison_importance_lp. Fit by OLS with HC2 SEs (a linear
# probability model). Runs after index_comparisons.R (uses panel$Vote_home_post,
# comparison_importance_lp, log_crime_gap, rank_gap, home_coalition, coalition_pre).
library(dplyr)
library(ggplot2)
library(estimatr)
if (!exists("ci_alpha")) ci_alpha <- 0.01

# ── Analysis sample: identical construction to gam_data in index_comparisons.R ─
# drop muni movers + failed attention check, pool T2-T4 into "Comparison",
# require non-NA rank_gap and comparison_importance_lp.
lm_panel <- panel %>%
  filter(muni_changed == 0, Attention_Check == "somewhat_agree")

lm_panel$coalition_pre[is.na(lm_panel$coalition_pre)] <- "Other"
lm_panel$inc_vote <- as.numeric(
  lm_panel$coalition_pre == lm_panel$home_coalition
)

arm_group_levels <- c("control", "T1", "Comparison")

lm_data <- lm_panel %>%
  filter(
    Treatment_Group %in% c("control", "T1", "T2", "T3", "T4"),
    !is.na(rank_gap),
    !is.na(comparison_importance_lp),
    !is.na(log_crime_gap)
  ) %>%
  mutate(
    arm_group = factor(
      case_when(
        Treatment_Group == "control" ~ "control",
        Treatment_Group == "T1" ~ "T1",
        TRUE ~ "Comparison"
      ),
      levels = arm_group_levels
    )
  )

# Linear-probability analogue of the tensor GAM: arm_group interacted with the
# updating variables (rank_gap, log_crime_gap) and with comparison_importance_lp.
m_log <- lm_robust(
  Vote_home_post ~
    #arm_group * rank_gap + # * comparison_importance_lp +
    arm_group * log_crime_gap * comparison_importance_lp +
    inc_vote,
  alpha = ci_alpha,
  data = lm_data,
  se_type = "HC2"
)

summary(m_log)

# ── Net importance-modulation of the bad-news (crime_gap) slope, by arm ────────
# The raw triple coefficient is only the Comparison-vs-control DIFFERENCE in how
# comparison importance bends the crime_gap slope. The substantive quantity is
# the cross-partial in each arm:
#   d^2 P(inc vote) / d log_crime_gap d importance
#     = beta[importance:crime]                              (control baseline)
#     + beta[armX:importance:crime]                         (arm's extra bending)
# A negative value = bad news reduces incumbent vote MORE when the comparison is
# more expected/representative (your hypothesis); positive = it reduces it less.
b <- coef(m_log)
V <- vcov(m_log)
cn <- names(b)

# Robustly locate coefficients by the tokens in their (order-independent) names.
find_coef <- function(tokens, exclude = character(0)) {
  hit <- vapply(cn, function(n) {
    all(vapply(tokens, grepl, logical(1), x = n, fixed = TRUE)) &&
      !any(vapply(exclude, grepl, logical(1), x = n, fixed = TRUE))
  }, logical(1))
  out <- cn[hit]
  if (length(out) != 1L) {
    stop(sprintf(
      "expected 1 coef for tokens {%s}, found %d: %s",
      paste(tokens, collapse = ", "), length(out), paste(out, collapse = " | ")
    ))
  }
  out
}

base_nm <- find_coef(
  c("log_crime_gap", "comparison_importance_lp"),
  exclude = c("arm_group", "rank_gap")
)
comp_nm <- find_coef(
  c("arm_groupComparison", "log_crime_gap", "comparison_importance_lp"),
  exclude = "rank_gap"
)
t1_nm <- find_coef(
  c("arm_groupT1", "log_crime_gap", "comparison_importance_lp"),
  exclude = "rank_gap"
)

# Linear contrasts L on b: control = base term; each arm = base + its triple.
contrast <- function(names_in) {
  L <- setNames(numeric(length(b)), cn)
  L[names_in] <- 1
  est <- as.numeric(L %*% b)
  se <- sqrt(as.numeric(t(L) %*% V %*% L))
  z <- est / se
  crit <- qnorm(1 - ci_alpha / 2)
  data.frame(
    estimate = est,
    std.error = se,
    statistic = z,
    p.value = 2 * pnorm(-abs(z)),
    conf.low = est - crit * se,
    conf.high = est + crit * se
  )
}

crosspartial_by_arm <- bind_rows(
  cbind(arm = "control (baseline)", contrast(base_nm)),
  cbind(arm = "T1", contrast(c(base_nm, t1_nm))),
  cbind(arm = "Comparison", contrast(c(base_nm, comp_nm)))
)

cat("\nNet importance-modulation of the crime_gap slope",
    " [d^2 P(inc vote) / d log_crime_gap d importance], by arm:\n", sep = "")
print(crosspartial_by_arm, row.names = FALSE)

# ── Same, but with T2 / T3 / T4 kept SEPARATE (not pooled) ────────────────────
# Refit the LPM with arm_group = full Treatment_Group (control / T1 / T2 / T3 /
# T4), mirroring the sep GAM in index_comparisons.R, then print the net crime_gap
# x importance cross-partial for each comparison arm on its own. Cells are thinner
# here, so SEs are wider than the pooled "Comparison" estimate above.
arm_levels_sep <- c("control", "T1", "T2", "T3", "T4")

lm_data_sep <- lm_data %>%
  mutate(arm_group = factor(Treatment_Group, levels = arm_levels_sep))

m_log_sep <- lm_robust(
  Vote_home_post ~
    #arm_group * rank_gap * comparison_importance_lp +
    arm_group * log_crime_gap * comparison_importance_lp +
    inc_vote,
  alpha = ci_alpha,
  data = lm_data_sep,
  se_type = "HC2"
)

b_sep <- coef(m_log_sep)
V_sep <- vcov(m_log_sep)
cn_sep <- names(b_sep)

# Same token-matching + contrast machinery as above, on the sep model.
find_coef_sep <- function(tokens, exclude = character(0)) {
  hit <- vapply(cn_sep, function(n) {
    all(vapply(tokens, grepl, logical(1), x = n, fixed = TRUE)) &&
      !any(vapply(exclude, grepl, logical(1), x = n, fixed = TRUE))
  }, logical(1))
  out <- cn_sep[hit]
  if (length(out) != 1L) {
    stop(sprintf(
      "expected 1 coef for tokens {%s}, found %d: %s",
      paste(tokens, collapse = ", "), length(out), paste(out, collapse = " | ")
    ))
  }
  out
}

contrast_sep <- function(names_in) {
  L <- setNames(numeric(length(b_sep)), cn_sep)
  L[names_in] <- 1
  est <- as.numeric(L %*% b_sep)
  se <- sqrt(as.numeric(t(L) %*% V_sep %*% L))
  z <- est / se
  crit <- qnorm(1 - ci_alpha / 2)
  data.frame(
    estimate = est,
    std.error = se,
    statistic = z,
    p.value = 2 * pnorm(-abs(z)),
    conf.low = est - crit * se,
    conf.high = est + crit * se
  )
}

base_nm_sep <- find_coef_sep(
  c("log_crime_gap", "comparison_importance_lp"),
  exclude = c("arm_group", "rank_gap")
)
# control = base term; each other arm = base + its own triple interaction.
crosspartial_by_arm_sep <- bind_rows(
  cbind(arm = "control (baseline)", contrast_sep(base_nm_sep)),
  bind_rows(lapply(c("T1", "T2", "T3", "T4"), function(a) {
    triple <- find_coef_sep(
      c(paste0("arm_group", a), "log_crime_gap", "comparison_importance_lp"),
      exclude = "rank_gap"
    )
    cbind(arm = a, contrast_sep(c(base_nm_sep, triple)))
  }))
)

cat("\nNet importance-modulation of the crime_gap slope",
    " [d^2 P(inc vote) / d log_crime_gap d importance], separate arms:\n", sep = "")
print(crosspartial_by_arm_sep, row.names = FALSE)

# ── Plot: Comparison - control difference in P(incumbent vote) over comparison ─
# importance, one line/facet per rank_gap value (RG = -2, 0, +2). Both arms are
# evaluated at the same rank_gap and importance, so the difference isolates the
# treatment effect and how it shifts across comparison importance. log_crime_gap
# at its mean, inc_vote = 1. Because this is a linear (identity-link) model the
# Comparison-control difference and its SE are exact contrasts on the design
# matrix: diff = (Xc - X0) b, se = sqrt((Xc - X0) V (Xc - X0)').
crit95 <- qnorm(0.975)
b <- coef(m_log)
V <- vcov(m_log)
mt <- delete.response(terms(m_log))
crime_mean <- mean(lm_data$log_crime_gap, na.rm = TRUE)

# One design-matrix row for a given rank_gap, importance, and arm. Columns are
# aligned to the fitted coefficient names so the contrast lines up with b/V.
lp_row <- function(rg, ci, arm, iv = 1) {
  nd <- data.frame(
    rank_gap = rg,
    comparison_importance_lp = ci,
    log_crime_gap = crime_mean,
    arm_group = factor(arm, levels = arm_group_levels),
    inc_vote = iv
  )
  # model.matrix needs all factor levels present to build every interaction
  # column; supply a dummy row per level, then keep the requested arm's row.
  filler <- data.frame(
    rank_gap = rg,
    comparison_importance_lp = ci,
    log_crime_gap = crime_mean,
    arm_group = factor(arm_group_levels, levels = arm_group_levels),
    inc_vote = iv
  )
  mm <- model.matrix(mt, rbind(nd, filler))
  mm[1, names(b), drop = FALSE]
}

# x-axis: importance range actually seen in the Comparison arm (5th-95th pctile).
comp_ci <- lm_data$comparison_importance_lp[lm_data$arm_group == "Comparison"]
ci_seq <- seq(
  quantile(comp_ci, 0.05, na.rm = TRUE),
  quantile(comp_ci, 0.95, na.rm = TRUE),
  length.out = 80
)

rank_levels <- c(-2, 0, 2)

diff_curve <- bind_rows(lapply(rank_levels, function(rg) {
  bind_rows(lapply(ci_seq, function(ci_val) {
    Xc <- lp_row(rg, ci_val, "Comparison")
    X0 <- lp_row(rg, ci_val, "control")
    dX <- Xc - X0
    d <- as.vector(dX %*% b)
    se <- sqrt(as.vector(dX %*% V %*% t(dX)))
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

vote_update_curve_importance_lm <- ggplot(
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
    y = "P(incumbent vote): Comparison - control",
    x = "Comparison importance (LP)",
    color = "Rank gap (RG)",
    fill = "Rank gap (RG)",
    caption = "linear probability model; ribbon 95% CI; importance over 5th-95th percentile range"
  ) +
  facet_wrap(~rank_gap) +
  theme_minimal()

print(vote_update_curve_importance_lm)

# ggsave(
#   "latex/images/vote_update_curve_importance_lm.pdf",
#   plot = vote_update_curve_importance_lm,
#   width = 7,
#   height = 4.5
# )
