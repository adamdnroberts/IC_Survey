# GAM updating curves for incumbent vote over the (log) crime gap.
# Split out from vote_update_analysis.R. Self-contained: builds the same panel,
# outcome, and controls, then fits logit GAMs with by-arm smooths on rank_gap and
# log_crime_gap and plots fitted incumbent-vote curves over log_crime_gap (with
# rank_gap held at its mean).

library(dplyr)
library(ggplot2)
library(mgcv)

load("data/derived/survey_panel_dataset.Rdata")

# These curves are poster figures. They always land in latex/images; set
# POSTER_FIG_DIR in .Renviron to also copy them into the poster project.
poster_fig_dir <- Sys.getenv("POSTER_FIG_DIR")
fig_dirs <- c(
  "latex/images",
  if (nzchar(poster_fig_dir) && dir.exists(poster_fig_dir)) poster_fig_dir
)

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

# ── Update curves: pooled comparison arms (T2–T4) ─────────────────────────
# Logit GAM of incumbent-vote probability for three groups: control (weather
# placebo, home-only), T1 (plain info), and Comparison (T2/T3/T4 pooled), with a
# by-group smooth on rank_gap, a by-group smooth on log_crime_gap, coalition_pre
# and inc_vote. T1 and control are kept as their own arms in the fit but are not
# plotted (T1's updating is essentially identical to control's).
#
# NOTE ON WHAT IS PLOTTED: the figure shows the Comparison group's *level*,
# P(incumbent vote | Comparison), over log_crime_gap — not a difference from
# control. Because log_crime_gap is a function of the respondent's own prior and
# is therefore NOT randomized, the level curve mixes the treatment response with
# selection into optimistic vs pessimistic priors. Reading a slope here as a
# causal dose-response requires the arm-minus-control difference at each gap
# value, which this script does not yet compute.
#
# rank_gap is held at its mean and coalition_pre at its mode. rank_gap is
# discrete (integers -4..4), so k = 5.
#
# SIGN CONVENTION (see code/create_panel_dataset.R):
#   crime_gap = home_rate - Robbery_Estimate, and
#   rank_gap  = actual_rank - rank_prior, where rank 1 = lowest-crime.
# Both are POSITIVE when reality is worse than the respondent believed, i.e.
# positive = negative news, negative = positive news.
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
    s(log_crime_gap, by = arm_group, k = 5) +
    coalition_pre +
    inc_vote,
  family = binomial(),
  data = gam_data,
  method = "REML"
)

summary(m_vote_gam)

# Fitted incumbent-vote probability for the Comparison group over log_crime_gap,
# with rank_gap at its mean and coalition_pre at its mode. 95% and 99% CIs from
# the link scale, back-transformed through the logit link.
crit95 <- qnorm(0.975)
crit99 <- qnorm(0.995)

# Two curves per arm: one for non-incumbent-voters (inc_vote = 0) and one for
# incumbent-voters (inc_vote = 1), rather than holding inc_vote at its mean.
inc_vals <- c(0, 1)
fit_curve <- bind_rows(lapply(diff_groups, function(g) {
  # Predict only across the range where this arm actually has data, so the curve
  # isn't extrapolated into log_crime_gap values the arm never took.
  g_cg <- gam_data$log_crime_gap[gam_data$arm_group == g]
  cg_seq <- seq(
    min(g_cg, na.rm = TRUE),
    max(g_cg, na.rm = TRUE),
    length.out = 100
  )
  bind_rows(lapply(inc_vals, function(iv) {
    nd <- data.frame(
      log_crime_gap = cg_seq,
      rank_gap = mean(gam_data$rank_gap, na.rm = TRUE),
      arm_group = factor(g, levels = arm_group_levels),
      coalition_pre = factor(
        coalition_pre_mode,
        levels = levels(gam_data$coalition_pre)
      ),
      inc_vote = iv
    )
    pr <- predict(m_vote_gam, newdata = nd, type = "link", se.fit = TRUE)
    data.frame(
      log_crime_gap = cg_seq,
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

# Marginal histogram of the Comparison group's log_crime_gap values, drawn as a
# thin strip just below the curve (log_crime_gap is continuous, so bin it).
cg_vals <- gam_data$log_crime_gap[gam_data$arm_group == "Comparison"]
cg_vals <- cg_vals[!is.na(cg_vals)]
h_cg <- hist(cg_vals, breaks = 30, plot = FALSE)
rug_counts <- data.frame(
  xmin = h_cg$breaks[-length(h_cg$breaks)],
  xmax = h_cg$breaks[-1],
  n = h_cg$counts
)

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
  filter(log_crime_gap == min(log_crime_gap)) %>%
  ungroup() %>%
  mutate(label = paste0("Prior incumbent vote = ", inc_vote))

vote_update_curve_rank <- ggplot(
  fit_curve,
  aes(
    x = log_crime_gap,
    y = fit,
    color = inc_vote,
    fill = inc_vote,
    linetype = inc_vote
  )
) +
  geom_rect(
    data = rug_counts,
    aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
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
  labs(
    y = "P(Vote for incumbent)",
    x = "log(crime gap)",
    caption = "ribbon 95% CI"
  ) +
  theme_minimal()

print(vote_update_curve_rank)

for (dir in fig_dirs) {
  ggsave(
    file.path(dir, "vote_update_curve_rank.pdf"),
    plot = vote_update_curve_rank,
    width = 7,
    height = 4.5
  )
}

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
    s(log_crime_gap, k = 5) +
    coalition_pre +
    inc_vote,
  family = binomial(),
  data = gam_data_sep,
  method = "REML"
)

summary(m_vote_gam_sep)

fit_curve_sep <- bind_rows(lapply(sep_groups, function(g) {
  # Predict only across the range where this arm actually has log_crime_gap data.
  g_cg <- gam_data_sep$log_crime_gap[gam_data_sep$arm_group == g]
  cg_seq <- seq(
    min(g_cg, na.rm = TRUE),
    max(g_cg, na.rm = TRUE),
    length.out = 100
  )
  nd <- data.frame(
    log_crime_gap = cg_seq,
    rank_gap = mean(gam_data_sep$rank_gap, na.rm = TRUE),
    arm_group = factor(g, levels = arm_levels_sep),
    coalition_pre = factor(
      coalition_pre_mode,
      levels = levels(gam_data_sep$coalition_pre)
    ),
    inc_vote = 1 #mean(gam_data_sep$inc_vote, na.rm = TRUE)
  )
  pr <- predict(m_vote_gam_sep, newdata = nd, type = "link", se.fit = TRUE)
  data.frame(
    log_crime_gap = cg_seq,
    arm_group = factor(g, levels = sep_groups),
    fit = plogis(pr$fit),
    lwr95 = plogis(pr$fit - crit95 * pr$se.fit),
    upr95 = plogis(pr$fit + crit95 * pr$se.fit),
    lwr99 = plogis(pr$fit - crit99 * pr$se.fit),
    upr99 = plogis(pr$fit + crit99 * pr$se.fit)
  )
}))

# Per-arm rug histogram, on a common floor below the lowest 95% band across arms.
y_lo_sep <- min(fit_curve_sep$lwr95)
y_hi_sep <- max(fit_curve_sep$upr95)
strip_h_sep <- 0.15 * (y_hi_sep - y_lo_sep)
floor_y_sep <- y_lo_sep - 0.02 * (y_hi_sep - y_lo_sep) - strip_h_sep
# Per-arm binned histogram of log_crime_gap (one strip per panel), on a shared
# set of bin breaks across the plotted arms.
sep_cg <- gam_data_sep %>%
  filter(arm_group %in% sep_groups, !is.na(log_crime_gap)) %>%
  mutate(arm_group = factor(as.character(arm_group), levels = sep_groups))
cg_breaks_sep <- seq(
  min(sep_cg$log_crime_gap),
  max(sep_cg$log_crime_gap),
  length.out = 31
)
bin_w_sep <- diff(cg_breaks_sep)[1]
rug_counts_sep <- sep_cg %>%
  mutate(
    bin = cut(log_crime_gap, cg_breaks_sep, include.lowest = TRUE, labels = FALSE)
  ) %>%
  count(arm_group, bin) %>%
  group_by(arm_group) %>%
  mutate(
    xmin = cg_breaks_sep[bin],
    xmax = cg_breaks_sep[bin] + bin_w_sep,
    ymin = floor_y_sep,
    ymax = floor_y_sep + strip_h_sep * n / max(n)
  ) %>%
  ungroup()

vote_update_curve_rank_sep <- ggplot(
  fit_curve_sep,
  aes(x = log_crime_gap, y = fit, color = arm_group, fill = arm_group)
) +
  geom_rect(
    data = rug_counts_sep,
    aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
    inherit.aes = FALSE,
    fill = "grey60",
    color = NA
  ) +
  geom_ribbon(aes(ymin = lwr95, ymax = upr95), alpha = 0.20, color = NA) +
  geom_line(linewidth = 0.8) +
  facet_wrap(~arm_group) +
  scale_color_manual(values = arm_colors[sep_groups], guide = "none") +
  scale_fill_manual(values = arm_colors[sep_groups], guide = "none") +
  labs(
    y = "Predicted Incumbent Vote",
    x = "log(crime gap)",
    caption = "ribbon 95% CI"
  ) +
  theme_minimal()

print(vote_update_curve_rank_sep)

for (dir in fig_dirs) {
  ggsave(
    file.path(dir, "vote_update_curve_rank_byarm.pdf"),
    plot = vote_update_curve_rank_sep,
    width = 9,
    height = 4.5
  )
}

# ── Per-arm crime-gap model (contrasts only; no figure depends on it) ────
# m_vote_gam_sep above fits s(log_crime_gap) WITHOUT by = arm_group, so T3 and T4
# share a single crime-gap smooth. That is fine for the arm intercepts it was
# built for, but it makes per-arm crime-gap contrasts impossible: the smooth is
# common by construction, so any "difference" between arms would be identically
# zero. Refit with a by-arm crime-gap smooth for the contrast tables below. This
# is a separate object on purpose — m_vote_gam_sep still backs the existing
# byarm figure, so re-specifying it here would silently change that figure.
m_vote_gam_sep_cg <- gam(
  Vote_home_post ~
    arm_group +
    s(rank_gap, by = arm_group, k = 5) +
    s(log_crime_gap, by = arm_group, k = 5) +
    coalition_pre +
    inc_vote,
  family = binomial(),
  data = gam_data_sep,
  method = "REML"
)

summary(m_vote_gam_sep_cg)

# ── Symmetric contrasts on either gap measure ────────────────────────────
# Evaluates one arm's fitted smooth in `var` at a pair of values and differences
# them, holding the OTHER gap measure at its mean, coalition_pre at its mode and
# inc_vote at the supplied value. Because it is the same arm and the same
# covariates, everything except the `var` smooth cancels, so the contrast is
# f_g(v1) - f_g(v0) with an exact SE from the model covariance. Reported on the
# logit scale (with odds ratio) and, via the delta method, as a difference in
# predicted probability.
#
# The logit-scale contrast does not depend on inc_vote (it enters linearly, so it
# cancels); the probability-scale one does, because the baseline P differs.
#
# SIGN CONVENTION for both measures (see code/create_panel_dataset.R):
#   rank_gap      = actual_rank - rank_prior      (rank 1 = lowest crime)
#   crime_gap     = home_rate - Robbery_Estimate  (both per 100,000)
#   log_crime_gap = sign(crime_gap) * log(1 + |crime_gap|)
# POSITIVE = reality is worse than the respondent believed = negative news.
# NOTE the two measures need not agree in sign for a given respondent: someone
# can overestimate the absolute robbery rate (positive news on crime_gap) while
# underestimating their municipality's rank among the five shown (negative news
# on rank_gap). Which measure is used therefore defines what "news" means here.
gap_vars <- c("rank_gap", "log_crime_gap")

make_nd <- function(dat, arm, iv, var, x) {
  other <- setdiff(gap_vars, var)
  nd <- data.frame(
    arm_group = factor(arm, levels = levels(dat$arm_group)),
    coalition_pre = factor(
      coalition_pre_mode,
      levels = levels(dat$coalition_pre)
    ),
    inc_vote = iv
  )
  nd[[var]] <- x
  nd[[other]] <- mean(dat[[other]], na.rm = TRUE)
  nd
}

# Raw per-100k robbery gap behind a log_crime_gap value, for interpretation.
unlog_gap <- function(x) sign(x) * (exp(abs(x)) - 1)

contrast_smooth <- function(model, dat, arm, iv, var, v0, v1) {
  lp_row <- function(x) {
    predict(model, newdata = make_nd(dat, arm, iv, var, x), type = "lpmatrix")
  }
  b <- coef(model)
  V <- vcov(model)
  X0 <- lp_row(v0)
  X1 <- lp_row(v1)

  # Logit-scale difference f(v1) - f(v0) (identical across inc_vote).
  Xd <- X1 - X0
  d_logit <- as.vector(Xd %*% b)
  se_logit <- sqrt(as.vector(Xd %*% V %*% t(Xd)))

  # Probability-scale difference P(v1) - P(v0), delta method (depends on inc_vote).
  p0 <- plogis(as.vector(X0 %*% b))
  p1 <- plogis(as.vector(X1 %*% b))
  grad <- (p1 * (1 - p1)) * X1 - (p0 * (1 - p0)) * X0
  se_p <- sqrt(as.vector(grad %*% V %*% t(grad)))

  data.frame(
    arm = arm,
    var = var,
    inc_vote = iv,
    v_from = v0,
    v_to = v1,
    raw_to = if (var == "log_crime_gap") unlog_gap(v1) else v1,
    news = if (v1 > v0) "negative" else "positive",
    p_from = p0,
    p_to = p1,
    diff_prob = p1 - p0,
    se_prob = se_p,
    p_value_prob = 2 * pnorm(-abs((p1 - p0) / se_p)),
    diff_logit = d_logit,
    odds_ratio = exp(d_logit),
    or_lwr = exp(d_logit - qnorm(0.975) * se_logit),
    or_upr = exp(d_logit + qnorm(0.975) * se_logit),
    p_value_logit = 2 * pnorm(-abs(d_logit / se_logit))
  )
}

# ── Asymmetry test: does bad news move votes more than good news? ────────
# Under a symmetric response (in particular a smooth that is linear through 0, as
# the linear gap x Treatment interactions in vote_update_analysis.R assume):
#
#   [f(r) - f(0)]  =  -[f(-r) - f(0)]   <=>   f(r) + f(-r) - 2 f(0) = 0
#
# so the SUM of the two symmetric contrasts is the asymmetry statistic, zero
# under symmetry. It is a second difference: the arm intercept and every term
# linear in the held-fixed covariates cancel exactly.
#   sum < 0  bad news costs the incumbent more than good news gains
#   sum > 0  good news helps more than bad news hurts
#   sum ~ 0  symmetric response
# IMPORTANT: this has power only if the fitted smooth has edf > 1. When REML
# shrinks a smooth to edf = 1 the fit is a straight line, f(r) + f(-r) - 2f(0) is
# zero by algebra, and the test returns ~0 with p ~ 1 regardless of the data.
# Always read the edf column of summary() alongside this table: an edf of exactly
# 1.000 means "no curvature was identified", NOT "the response is symmetric".
asymmetry_smooth <- function(model, dat, arm, iv, var, r) {
  lp_row <- function(x) {
    predict(model, newdata = make_nd(dat, arm, iv, var, x), type = "lpmatrix")
  }
  b <- coef(model)
  V <- vcov(model)
  Xm <- lp_row(-r)
  X0 <- lp_row(0)
  Xp <- lp_row(r)

  # Logit scale: f(r) + f(-r) - 2 f(0).
  Xd <- Xp + Xm - 2 * X0
  d_logit <- as.vector(Xd %*% b)
  se_logit <- sqrt(as.vector(Xd %*% V %*% t(Xd)))

  # Probability scale: P(r) + P(-r) - 2 P(0), delta method.
  pm <- plogis(as.vector(Xm %*% b))
  p0 <- plogis(as.vector(X0 %*% b))
  pp <- plogis(as.vector(Xp %*% b))
  grad <- (pp * (1 - pp)) * Xp +
    (pm * (1 - pm)) * Xm -
    2 * (p0 * (1 - p0)) * X0
  se_p <- sqrt(as.vector(grad %*% V %*% t(grad)))

  d_prob <- pp + pm - 2 * p0
  data.frame(
    arm = arm,
    var = var,
    inc_vote = iv,
    r = r,
    raw_r = if (var == "log_crime_gap") unlog_gap(r) else r,
    bad_news_prob = pp - p0,
    good_news_prob = pm - p0,
    asym_prob = d_prob,
    se_prob = se_p,
    p_value_prob = 2 * pnorm(-abs(d_prob / se_p)),
    asym_logit = d_logit,
    se_logit = se_logit,
    p_value_logit = 2 * pnorm(-abs(d_logit / se_logit))
  )
}

# control is included alongside the treated arms so the treated-arm contrasts can
# be read against the placebo arm's own gap slope: respondents with optimistic
# priors may differ from pessimistic ones whether or not they were shown any
# crime information. The pooled model supplies control/Comparison; the by-arm
# crime-gap model supplies T2/T3/T4. T2 matters here because it is the only
# arm whose crime-gap smooth kept any curvature (edf ~1.9); every other arm was
# shrunk to a straight line, where the asymmetry test is degenerate.
contrast_spec <- list(
  list(model = m_vote_gam, dat = gam_data, arm = "control"),
  list(model = m_vote_gam, dat = gam_data, arm = "Comparison"),
  list(model = m_vote_gam_sep_cg, dat = gam_data_sep, arm = "T2"),
  list(model = m_vote_gam_sep_cg, dat = gam_data_sep, arm = "T3"),
  list(model = m_vote_gam_sep_cg, dat = gam_data_sep, arm = "T4")
)

# Symmetric points about an accurate prior. rank_gap is integer (-4..4). For
# log_crime_gap, +-2/+-4/+-6 correspond to raw robbery-rate gaps of roughly
# +-6, +-54 and +-402 per 100,000 (see unlog_gap).
target_grid <- list(
  rank_gap = c(-4, -2, 2, 4),
  log_crime_gap = c(-6, -4, -2, 2, 4, 6)
)
asym_grid <- list(
  rank_gap = c(2, 4),
  log_crime_gap = c(2, 4, 6)
)

run_grid <- function(fun, grid) {
  bind_rows(lapply(contrast_spec, function(s) {
    bind_rows(lapply(gap_vars, function(v) {
      bind_rows(lapply(c(0, 1), function(iv) {
        bind_rows(lapply(grid[[v]], function(x) fun(s, v, iv, x)))
      }))
    }))
  }))
}

gap_contrasts <- run_grid(
  function(s, v, iv, x) {
    contrast_smooth(s$model, s$dat, s$arm, iv, v, v0 = 0, v1 = x)
  },
  target_grid
)

gap_asymmetry <- run_grid(
  function(s, v, iv, x) asymmetry_smooth(s$model, s$dat, s$arm, iv, v, x),
  asym_grid
)

# ── Support on each side of zero ─────────────────────────────────────────
# A flat good-news half of any curve is only informative if the negative cells
# are populated. Check this before reading any contrast as an absence of effect.
cat("\nSupport for rank_gap by arm (pooled model):\n")
print(
  as.data.frame(
    gam_data %>%
      filter(!is.na(rank_gap)) %>%
      count(arm_group, rank_gap) %>%
      tidyr::pivot_wider(
        names_from = rank_gap,
        values_from = n,
        values_fill = 0
      )
  ),
  row.names = FALSE
)

cat("\nSupport for crime_gap by arm (sign of the gap):\n")
print(
  as.data.frame(
    gam_data %>%
      filter(!is.na(log_crime_gap)) %>%
      mutate(
        side = case_when(
          crime_gap < 0 ~ "positive news (overestimated)",
          crime_gap > 0 ~ "negative news (underestimated)",
          TRUE ~ "exact"
        )
      ) %>%
      count(arm_group, side) %>%
      tidyr::pivot_wider(names_from = side, values_from = n, values_fill = 0)
  ),
  row.names = FALSE
)

cat("\nlog_crime_gap quantiles by arm:\n")
print(
  as.data.frame(
    gam_data %>%
      filter(!is.na(log_crime_gap)) %>%
      group_by(arm_group) %>%
      summarise(
        n = n(),
        p05 = quantile(log_crime_gap, 0.05),
        p25 = quantile(log_crime_gap, 0.25),
        p50 = quantile(log_crime_gap, 0.50),
        p75 = quantile(log_crime_gap, 0.75),
        p95 = quantile(log_crime_gap, 0.95),
        .groups = "drop"
      )
  ),
  row.names = FALSE,
  digits = 3
)

# How often the two measures disagree about the direction of the news. If this is
# large, "positive vs negative news" is not well defined without naming a measure.
cat("\nAgreement between rank_gap and crime_gap on the sign of the news:\n")
print(
  as.data.frame(
    gam_data %>%
      filter(
        !is.na(rank_gap),
        !is.na(crime_gap),
        rank_gap != 0,
        crime_gap != 0
      ) %>%
      count(
        rank_news = if_else(rank_gap > 0, "negative", "positive"),
        crime_news = if_else(crime_gap > 0, "negative", "positive")
      )
  ),
  row.names = FALSE
)

cat(
  "\nContrasts in P(incumbent vote) vs an accurate prior (gap = 0).\n",
  "v_to > 0 = negative news (worse than believed); < 0 = positive news.\n",
  "raw_to gives the per-100,000 robbery gap for log_crime_gap rows.\n",
  "Prefer odds_ratio / p_value_logit: the delta-method probability SE is\n",
  "unreliable where fitted P is near 0 or 1.\n",
  sep = ""
)
# max = 1e6 defeats the default max.print truncation: this table is ~100 rows
# wide enough that R silently drops the tail otherwise.
print(gap_contrasts, row.names = FALSE, digits = 3, max = 1e6)

cat(
  "\nAsymmetry test: f(r) + f(-r) - 2*f(0), zero under a symmetric response.\n",
  "Negative => bad news costs the incumbent more than good news gains.\n",
  "Only meaningful where the corresponding smooth has edf > 1 (see summary()).\n",
  sep = ""
)
print(gap_asymmetry, row.names = FALSE, digits = 3, max = 1e6)
