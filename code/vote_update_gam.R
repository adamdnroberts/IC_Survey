# GAM updating curves for incumbent vote over the rank gap.
# Split out from vote_update_analysis.R. Self-contained: builds the same panel,
# outcome, and controls, then fits logit GAMs with a by-arm smooth on rank_gap
# and plots fitted incumbent-vote curves.

library(dplyr)
library(ggplot2)
library(mgcv)

load("~/IC_Survey/data/survey_panel_dataset.Rdata")

# Colorblind-friendly (Okabe-Ito) palette, matching crime_rate_accuracy_update.R
arm_colors <- c(
  control2 = "#999999",
  T1 = "#56B4E9",
  T2 = "#009E73",
  T3 = "#E69F00",
  T4 = "#0072B2"
)

panel$Vote_home_post <- as.integer(
  !is.na(panel$coalition_post) &
    !is.na(panel$home_coalition) &
    panel$home_coalition == panel$coalition_post
)

panel <- panel %>%
  filter(muni_changed == 0, Attention_Check == "somewhat_agree")

panel$coalition_pre[is.na(panel$coalition_pre)] <- "Other"

panel$inc_vote <- as.numeric(panel$coalition_pre == panel$home_coalition)

# ── Update curve relative to control: pooled comparison arms (T2–T4) ───────────
# Logit GAM of incumbent-vote probability over rank_gap for three groups:
# control (weather placebo, home-only), T1 (plain info), and Comparison
# (T2/T3/T4 pooled), with a by-group smooth on rank_gap plus s(crime_gap_wins)
# and coalition_pre. T1 is kept as its own arm in the fit but not plotted (its
# updating is essentially identical to control). We plot the Comparison group's
# *difference* in predicted probability from control, P(group) - P(control), as
# rank_gap varies (control is the zero line). crime_gap_wins is held at its mean
# and coalition_pre at its mode. rank_gap is discrete (integers -4..4), so k = 5.
curve_arms <- c("control", "T1", "T2", "T3", "T4")
arm_group_levels <- c("control", "T1", "Comparison")
diff_groups <- c("Comparison")
coalition_pre_mode <- names(which.max(table(panel$coalition_pre)))

gam_data <- panel %>%
  filter(Treatment_Group %in% curve_arms, !is.na(rank_gap)) %>%
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

m_vote_gam <- gam(
  Vote_home_post ~
    arm_group +
    s(rank_gap, by = arm_group, k = 5) +
    s(crime_gap_wins, by = arm_group, k = 5) +
    coalition_pre +
    inc_vote,
  family = binomial(),
  data = gam_data,
  method = "REML"
)

summary(m_vote_gam)

# Fitted incumbent-vote probability for the Comparison group over rank_gap, with
# crime_gap_wins at its mean and coalition_pre at its mode. 95% and 99% CIs from
# the link scale, back-transformed through the logit link.
crit95 <- qnorm(0.975)
crit99 <- qnorm(0.995)

# Two curves per arm: one for non-incumbent-voters (inc_vote = 0) and one for
# incumbent-voters (inc_vote = 1), rather than holding inc_vote at its mean.
inc_vals <- c(0, 1)
fit_curve <- bind_rows(lapply(diff_groups, function(g) {
  # Predict only across the range where this arm actually has data, so the curve
  # isn't extrapolated into rank_gap values the arm never took.
  g_rank <- gam_data$rank_gap[gam_data$arm_group == g]
  rank_seq <- seq(
    min(g_rank, na.rm = TRUE),
    max(g_rank, na.rm = TRUE),
    length.out = 100
  )
  bind_rows(lapply(inc_vals, function(iv) {
    nd <- data.frame(
      rank_gap = rank_seq,
      arm_group = factor(g, levels = arm_group_levels),
      crime_gap_wins = mean(gam_data$crime_gap_wins, na.rm = TRUE),
      coalition_pre = factor(
        coalition_pre_mode,
        levels = levels(gam_data$coalition_pre)
      ),
      inc_vote = iv
    )
    pr <- predict(m_vote_gam, newdata = nd, type = "link", se.fit = TRUE)
    data.frame(
      rank_gap = rank_seq,
      arm_group = factor(g, levels = diff_groups),
      inc_vote = factor(iv, levels = inc_vals),
      fit = plogis(pr$fit),
      lwr95 = plogis(pr$fit - crit95 * pr$se.fit),
      upr95 = plogis(pr$fit + crit95 * pr$se.fit),
      lwr99 = plogis(pr$fit - crit99 * pr$se.fit),
      upr99 = plogis(pr$fit + crit99 * pr$se.fit)
    )
  }))
}))

inc_colors <- c("0" = "grey55", "1" = "black")
inc_linetypes <- c("0" = "dashed", "1" = "solid")

# Marginal histogram of the Comparison group's rank_gap values, drawn as a thin
# strip just below the curve (rank_gap is discrete, so one bar per integer).
rug_counts <- gam_data %>%
  filter(arm_group == "Comparison") %>%
  count(rank_gap)

y_lo <- min(fit_curve$lwr95)
y_hi <- max(fit_curve$upr95)
strip_h <- 0.15 * (y_hi - y_lo)
floor_y <- y_lo - 0.02 * (y_hi - y_lo) - strip_h
rug_counts <- rug_counts %>%
  mutate(
    ymin = floor_y,
    ymax = floor_y + strip_h * n / max(n)
  )

# In-plot labels for each curve (placed at the left end), in lieu of a legend.
inc_curve_labels <- fit_curve %>%
  group_by(inc_vote) %>%
  filter(rank_gap == min(rank_gap)) %>%
  ungroup() %>%
  mutate(label = paste0("Prior incumbent vote = ", inc_vote))

vote_update_curve_rank <- ggplot(
  fit_curve,
  aes(
    x = rank_gap,
    y = fit,
    color = inc_vote,
    fill = inc_vote,
    linetype = inc_vote
  )
) +
  geom_rect(
    data = rug_counts,
    aes(xmin = rank_gap - 0.4, xmax = rank_gap + 0.4, ymin = ymin, ymax = ymax),
    inherit.aes = FALSE,
    fill = "grey60",
    color = NA
  ) +
  geom_ribbon(aes(ymin = lwr95, ymax = upr95), alpha = 0.18, color = NA) +
  geom_line(linewidth = 0.8) +
  geom_text(
    data = inc_curve_labels,
    aes(label = label),
    hjust = -1,
    vjust = -0.8,
    size = 5,
    show.legend = FALSE
  ) +
  scale_color_manual(values = inc_colors, guide = "none") +
  scale_fill_manual(values = inc_colors, guide = "none") +
  scale_linetype_manual(values = inc_linetypes, guide = "none") +
  scale_x_continuous(breaks = -4:4) +
  labs(
    y = "P(Vote for incumbent)",
    x = "RG",
    caption = "ribbon 95% CI"
  ) +
  theme_minimal()

print(vote_update_curve_rank)

ggsave(
  "C:/Users/adamd/Dropbox/Apps/Overleaf/PolMeth 2026 Poster/figures/vote_update_curve_rank.pdf",
  plot = vote_update_curve_rank,
  width = 7,
  height = 4.5
)

# ── Same fitted curves, but T2, T3, T4 each fit and plotted separately ─────────
# Re-fit the GAM with a by-arm smooth at the full Treatment_Group level (control
# and T1 stay in the fit for common reference/nuisance terms but are not plotted)
# and show each comparison arm's fitted P(incumbent vote) in its own panel, with
# the same rug histogram and 95%/99% CI bands as above.
arm_levels_sep <- c("control", "T1", "T2", "T3", "T4")
sep_groups <- c("T3", "T4")

gam_data_sep <- panel %>%
  filter(Treatment_Group %in% arm_levels_sep, !is.na(rank_gap)) %>%
  mutate(
    arm_group = factor(Treatment_Group, levels = arm_levels_sep),
    coalition_pre = factor(coalition_pre)
  )

m_vote_gam_sep <- gam(
  Vote_home_post ~
    arm_group +
    s(rank_gap, by = arm_group, k = 5) +
    s(crime_gap_wins, k = 5) +
    coalition_pre +
    inc_vote,
  family = binomial(),
  data = gam_data_sep,
  method = "REML"
)

summary(m_vote_gam_sep)

fit_curve_sep <- bind_rows(lapply(sep_groups, function(g) {
  # Predict only across the range where this arm actually has data, floored at
  # rank_gap = -2.
  g_rank <- gam_data_sep$rank_gap[gam_data_sep$arm_group == g]
  rank_seq <- seq(
    max(-2, min(g_rank, na.rm = TRUE)),
    max(g_rank, na.rm = TRUE),
    length.out = 100
  )
  nd <- data.frame(
    rank_gap = rank_seq,
    arm_group = factor(g, levels = arm_levels_sep),
    crime_gap_wins = mean(gam_data_sep$crime_gap_wins, na.rm = TRUE),
    coalition_pre = factor(
      coalition_pre_mode,
      levels = levels(gam_data_sep$coalition_pre)
    ),
    inc_vote = 1 #mean(gam_data_sep$inc_vote, na.rm = TRUE)
  )
  pr <- predict(m_vote_gam_sep, newdata = nd, type = "link", se.fit = TRUE)
  data.frame(
    rank_gap = rank_seq,
    arm_group = factor(g, levels = sep_groups),
    fit = plogis(pr$fit),
    lwr95 = plogis(pr$fit - crit95 * pr$se.fit),
    upr95 = plogis(pr$fit + crit95 * pr$se.fit),
    lwr99 = plogis(pr$fit - crit99 * pr$se.fit),
    upr99 = plogis(pr$fit + crit99 * pr$se.fit)
  )
}))

# Per-arm rug histogram, on a common floor below the lowest 99% band across arms.
y_lo_sep <- min(fit_curve_sep$lwr95)
y_hi_sep <- max(fit_curve_sep$upr95)
strip_h_sep <- 0.15 * (y_hi_sep - y_lo_sep)
floor_y_sep <- y_lo_sep - 0.02 * (y_hi_sep - y_lo_sep) - strip_h_sep
# Per-arm rug histogram (one strip per panel).
rug_counts_sep <- gam_data_sep %>%
  filter(arm_group %in% sep_groups, rank_gap >= -2) %>%
  mutate(arm_group = factor(as.character(arm_group), levels = sep_groups)) %>%
  count(arm_group, rank_gap) %>%
  group_by(arm_group) %>%
  mutate(ymin = floor_y_sep, ymax = floor_y_sep + strip_h_sep * n / max(n)) %>%
  ungroup()

vote_update_curve_rank_sep <- ggplot(
  fit_curve_sep,
  aes(x = rank_gap, y = fit, color = arm_group, fill = arm_group)
) +
  geom_rect(
    data = rug_counts_sep,
    aes(xmin = rank_gap - 0.4, xmax = rank_gap + 0.4, ymin = ymin, ymax = ymax),
    inherit.aes = FALSE,
    fill = "grey60",
    color = NA
  ) +
  geom_ribbon(aes(ymin = lwr95, ymax = upr95), alpha = 0.20, color = NA) +
  geom_line(linewidth = 0.8) +
  facet_wrap(~arm_group) +
  scale_color_manual(values = arm_colors[sep_groups], guide = "none") +
  scale_fill_manual(values = arm_colors[sep_groups], guide = "none") +
  scale_x_continuous(breaks = -4:4) +
  labs(
    y = "Predicted Incumbent Vote",
    x = "RG",
    caption = "ribbon 95% CI"
  ) +
  theme_minimal()

print(vote_update_curve_rank_sep)

ggsave(
  "C:/Users/adamd/Dropbox/Apps/Overleaf/PolMeth 2026 Poster/figures/vote_update_curve_rank_byarm.pdf",
  plot = vote_update_curve_rank_sep,
  width = 9,
  height = 4.5
)

# ── Within-arm contrast: accurate (rank_gap = 0) vs very optimistic prior (= 4) ─
# Evaluates one arm's fitted smooth at rank_gap 0 and 4 and differences them.
# Because it is the same arm and the same covariate values, everything except
# the rank_gap smooth (crime_gap_wins, coalition_pre, inc_vote, arm intercept)
# cancels, so the contrast is f_g(4) - f_g(0) with an exact SE from the model
# covariance. Reported on the logit scale (with odds ratio) and, via the delta
# method, as a difference in predicted probability.
# The logit-scale contrast (diff_logit, odds ratio) does not depend on inc_vote:
# inc_vote enters linearly, so it cancels in f(4) - f(0). The probability-scale
# difference does depend on it, because the baseline P differs. We therefore
# report the probability contrast at both inc_vote = 0 and inc_vote = 1.
contrast_0_vs_4 <- function(model, dat, arm, iv) {
  lp_row <- function(x) {
    predict(
      model,
      newdata = data.frame(
        rank_gap = x,
        arm_group = factor(arm, levels = levels(dat$arm_group)),
        crime_gap_wins = mean(dat$crime_gap_wins, na.rm = TRUE),
        coalition_pre = factor(
          coalition_pre_mode,
          levels = levels(dat$coalition_pre)
        ),
        inc_vote = iv
      ),
      type = "lpmatrix"
    )
  }
  b <- coef(model)
  V <- vcov(model)
  X0 <- lp_row(0)
  X4 <- lp_row(4)

  # Logit-scale difference f(4) - f(0) (identical across inc_vote).
  Xd <- X4 - X0
  d_logit <- as.vector(Xd %*% b)
  se_logit <- sqrt(as.vector(Xd %*% V %*% t(Xd)))

  # Probability-scale difference P(4) - P(0), delta method (depends on inc_vote).
  p0 <- plogis(as.vector(X0 %*% b))
  p4 <- plogis(as.vector(X4 %*% b))
  grad <- (p4 * (1 - p4)) * X4 - (p0 * (1 - p0)) * X0
  se_p <- sqrt(as.vector(grad %*% V %*% t(grad)))

  data.frame(
    arm = arm,
    inc_vote = iv,
    p_rank0 = p0,
    p_rank4 = p4,
    diff_prob = p4 - p0,
    se_prob = se_p,
    p_value_prob = 2 * pnorm(-abs((p4 - p0) / se_p)),
    diff_logit = d_logit,
    odds_ratio = exp(d_logit),
    or_lwr = exp(d_logit - qnorm(0.975) * se_logit),
    or_upr = exp(d_logit + qnorm(0.975) * se_logit),
    p_value_logit = 2 * pnorm(-abs(d_logit / se_logit))
  )
}

contrast_spec <- list(
  list(model = m_vote_gam, dat = gam_data, arm = "Comparison"),
  list(model = m_vote_gam_sep, dat = gam_data_sep, arm = "T3"),
  list(model = m_vote_gam_sep, dat = gam_data_sep, arm = "T4")
)

rank_0_vs_4 <- bind_rows(lapply(contrast_spec, function(s) {
  bind_rows(lapply(c(0, 1), function(iv) {
    contrast_0_vs_4(s$model, s$dat, s$arm, iv)
  }))
}))

cat(
  "\nContrast: P(incumbent vote) at rank_gap = 4 (very optimistic prior) vs 0 (accurate)\n"
)
print(rank_0_vs_4, row.names = FALSE, digits = 3)
