# Moderated-mediation figure: the T4-vs-control effect on incumbent vote,
# decomposed into the part that flows THROUGH the incumbent-minus-opposition
# crime rating (ACME, "mediated") and the part that does not (ADE, "direct"),
# estimated across the range of log_crime_gap with rank_gap held at its mean.
#
# The mediated curve is small and directional -- positive (reward) at good-news
# gaps, negative (punishment) at bad-news gaps, crossing zero around
# log_crime_gap ~ 3, which is why the AVERAGE ACME is ~0. The direct curve is
# always positive and several times larger, so most of T4's (crime-gap
# contingent) vote effect runs through channels other than this rating.
#
# The outcome model interacts the mediator with arm; see the comment on
# m_outcome for why the additive form cannot be used here.
#
# Self-contained: rebuilds panel_med and the pooled mediator/outcome models
# (m_mediator, m_outcome) inline. (An earlier comment here pointed at
# code/mediation_analysis.R as the model of record; no such file exists.)
# Output: latex/images/t4_mediated_direct_by_crimegap.pdf

library(dplyr)
library(ggplot2)
library(mediation)

load("data/derived/survey_panel_dataset.Rdata")

# Sample: drop muni movers + failed attention check (as in mediation_analysis.R).
panel <- panel %>%
  filter(muni_changed == 0, Attention_Check == "somewhat_agree")

# ── Mediator: incumbent minus opposition-average crime rating (post) ──────────
# inc_minus_opp_avg_post = Home_Crime_Handling_Post - (average post-treatment
# crime rating across the coalitions that do NOT govern the home municipality).
num <- function(x) suppressWarnings(as.numeric(x))

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

# Per-respondent opposition benchmark: average across the non-home coalitions,
# for both post and pre ratings. NA home_coalition or all-NA ratings -> NA.
opp_benchmarks <- t(vapply(
  seq_len(nrow(panel)),
  function(i) {
    hc <- panel$home_coalition[i]
    if (is.na(hc)) {
      return(c(avg_post = NA_real_, avg_pre = NA_real_))
    }
    keep <- colnames(coalition_post_rating) != hc
    post <- coalition_post_rating[i, keep]
    pre <- coalition_pre_rating[i, keep]
    c(
      avg_post = if (all(is.na(post))) NA_real_ else mean(post, na.rm = TRUE),
      avg_pre = if (all(is.na(pre))) NA_real_ else mean(pre, na.rm = TRUE)
    )
  },
  numeric(2)
))

panel$opp_avg_post <- opp_benchmarks[, "avg_post"]
panel$opp_avg_pre <- opp_benchmarks[, "avg_pre"]
panel$inc_post <- num(panel$Home_Crime_Handling_Post)
panel$inc_pre <- num(panel$Home_Crime_Handling_Pre)
panel$inc_minus_opp_avg_post <- panel$inc_post - panel$opp_avg_post

panel$Vote_home_post <- as.integer(
  !is.na(panel$coalition_post) &
    !is.na(panel$home_coalition) &
    panel$home_coalition == panel$coalition_post
)

# ── Pooled mediator & outcome models (all arms, control2 excluded) ────────────
panel_med <- filter(panel, Treatment_Group != "control2")
panel_med$Treatment_Group <- relevel(
  droplevels(factor(panel_med$Treatment_Group)),
  ref = "control"
)

m_mediator <- lm(
  inc_minus_opp_avg_post ~
    inc_pre +
    opp_avg_pre +
    log_crime_gap * Treatment_Group +
    rank_gap * Treatment_Group +
    coalition_pre,
  data = panel_med
)

# The mediator is interacted with arm. Without that term the outcome model
# imposes a single mediator coefficient tau for every arm, which forces
# ACME(g) = tau * (delta_4 + beta_4 * g) -- the mediated curve is then the
# mediator equation's T4 coefficient rescaled by a constant, so its slope and
# its zero crossing are algebraic consequences of the specification rather than
# findings. It is also substantively wrong here: the paper's theory is precisely
# that the belief-to-vote mapping differs by benchmark type, which the additive
# form assumes away. With the interaction present, mediate() detects it and
# returns separate ACMEs under control (d0) and treatment (d1).
m_outcome <- lm(
  Vote_home_post ~
    inc_minus_opp_avg_post * Treatment_Group +
    inc_pre +
    opp_avg_pre +
    log_crime_gap * Treatment_Group +
    rank_gap * Treatment_Group +
    coalition_pre,
  data = panel_med
)

set.seed(1)
n_grid <- 13
sims <- 1000

rg_hold <- mean(panel_med$rank_gap, na.rm = TRUE)
cg_grid <- seq(
  quantile(panel_med$log_crime_gap, 0.05, na.rm = TRUE),
  quantile(panel_med$log_crime_gap, 0.95, na.rm = TRUE),
  length.out = n_grid
)

# T4 ACME (mediated) and ADE (direct) at each log_crime_gap value.
rows <- lapply(cg_grid, function(g) {
  m <- mediate(
    m_mediator,
    m_outcome,
    treat = "Treatment_Group",
    mediator = "inc_minus_opp_avg_post",
    control.value = "control",
    treat.value = "T4",
    covariates = list(log_crime_gap = g, rank_gap = rg_hold),
    robustSE = TRUE,
    sims = sims
  )
  # With the treat x mediator interaction the ACME differs by condition, so d0
  # (under control) and d1 (under T4) are no longer equal. The plotted curve is
  # the average of the two; d0 and d1 are kept so the console output shows how
  # far apart they are -- if they diverge sharply, the additive model the paper
  # currently reports was hiding real moderation.
  data.frame(
    log_crime_gap = g,
    acme = m$d.avg,
    acme_lo = m$d.avg.ci[1],
    acme_hi = m$d.avg.ci[2],
    ade = m$z.avg,
    ade_lo = m$z.avg.ci[1],
    ade_hi = m$z.avg.ci[2],
    acme_d0 = m$d0,
    acme_d1 = m$d1,
    acme_p = m$d.avg.p
  )
})
df <- do.call(rbind, rows)
print(df, row.names = FALSE)

med_lab <- "Mediated (via inc−opp rating)"
dir_lab <- "Direct (other channels)"
sel <- function(d, cols) {
  setNames(d[, cols], c("log_crime_gap", "est", "lo", "hi"))
}
plot_df <- rbind(
  transform(
    sel(df, c("log_crime_gap", "acme", "acme_lo", "acme_hi")),
    effect = med_lab
  ),
  transform(
    sel(df, c("log_crime_gap", "ade", "ade_lo", "ade_hi")),
    effect = dir_lab
  )
)
plot_df$effect <- factor(plot_df$effect, levels = c(dir_lab, med_lab))

effect_colors <- setNames(c("#D55E00", "#0072B2"), c(dir_lab, med_lab))

# y-axis range spanning BOTH curves' ribbons, so the mediated-only plot (p2) can
# reuse the same y scale as the main plot (p).
y_full_range <- range(c(plot_df$lo, plot_df$hi), na.rm = TRUE)

# Direct in-plot line labels (used instead of a legend): anchored at the right
# end of each curve, nudged apart vertically.
label_df <- plot_df %>%
  group_by(effect) %>%
  slice_max(log_crime_gap, n = 1) %>%
  ungroup() %>%
  mutate(
    y_lab = if_else(
      effect == dir_lab,
      hi + 0.16 * diff(y_full_range),
      lo - 0.04 * diff(y_full_range)
    )
  )

# Mediated-only plot (p2) label: placed ABOVE the CI near a nominal crime gap of
# -300 (i.e. log_crime_gap = cg_fwd(-300)), rather than at the right end.
x_med_lab <- (function(g) sign(g) * log(abs(g)))(-300)
med_curve <- filter(plot_df, effect == med_lab)
label_df_p2 <- data.frame(
  log_crime_gap = x_med_lab,
  effect = factor(med_lab, levels = levels(plot_df$effect)),
  y_lab = approx(med_curve$log_crime_gap, med_curve$hi, xout = x_med_lab)$y +
    0.04 * diff(y_full_range)
)

# x axis stays on the log_crime_gap scale (that is what the models use) but is
# labelled with the nominal crime gap, i.e. actual home robbery rate minus the
# respondent's estimate. log_crime_gap = sign(gap) * log(|gap|), so the inverse
# is sign(x) * exp(|x|). Breaks are round nominal values inside the grid range.
cg_fwd <- function(g) sign(g) * log(abs(g))
nominal_breaks <- c(
  -c(10000, 3000, 1000, 300, 100, 30, 10, 3),
  c(3, 10, 30, 100, 300, 1000, 3000, 10000)
)
nominal_breaks <- nominal_breaks[
  cg_fwd(nominal_breaks) >= min(cg_grid) &
    cg_fwd(nominal_breaks) <= max(cg_grid)
]

p <- ggplot(
  filter(plot_df, log_crime_gap > -8),
  aes(log_crime_gap, est, color = effect, fill = effect)
) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +
  geom_ribbon(aes(ymin = lo, ymax = hi), alpha = 0.15, color = NA) +
  geom_line(linewidth = 0.9) +
  geom_text(
    data = filter(label_df, effect == dir_lab),
    aes(y = y_lab, label = effect),
    hjust = 1,
    size = 3.5,
    show.legend = FALSE
  ) +
  geom_text(
    data = label_df_p2,
    aes(y = y_lab, label = effect),
    hjust = 0.5,
    size = 3.5,
    show.legend = FALSE
  ) +
  scale_color_manual(values = effect_colors) +
  scale_fill_manual(values = effect_colors) +
  scale_x_continuous(
    breaks = cg_fwd(nominal_breaks),
    labels = format(nominal_breaks, big.mark = ",", trim = TRUE)
  ) +
  labs(
    #title = "T4 effect on incumbent vote: mediated vs. direct, by crime gap",
    #subtitle = "rank_gap held at its mean; bands are 95% quasi-Bayesian CIs",
    x = "Crime gap: actual − estimated robbery rate (log spacing)",
    y = "Effect on P(vote incumbent), T4 − control",
    color = NULL,
    fill = NULL
  ) +
  theme_minimal(base_size = 12) +
  theme(legend.position = "none")

print(p)

ggsave(
  "latex/images/t4_mediated_direct_by_crimegap_full.pdf",
  plot = p,
  width = 8,
  height = 4.5
)

p2 <- ggplot(
  filter(plot_df, log_crime_gap > -8 & effect != "Direct (other channels)"),
  aes(log_crime_gap, est, color = effect, fill = effect)
) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +
  geom_ribbon(aes(ymin = lo, ymax = hi), alpha = 0.15, color = NA) +
  geom_line(linewidth = 0.9) +
  geom_text(
    data = label_df_p2,
    aes(y = y_lab, label = effect),
    hjust = 0.5,
    size = 3.5,
    show.legend = FALSE
  ) +
  scale_color_manual(values = effect_colors) +
  scale_fill_manual(values = effect_colors) +
  scale_x_continuous(
    breaks = cg_fwd(nominal_breaks),
    labels = format(nominal_breaks, big.mark = ",", trim = TRUE)
  ) +
  coord_cartesian(ylim = y_full_range) +
  labs(
    #title = "T4 effect on incumbent vote: mediated vs. direct, by crime gap",
    #subtitle = "rank_gap held at its mean; bands are 95% quasi-Bayesian CIs",
    x = "Crime gap: actual − estimated robbery rate (log spacing)",
    y = "Effect on P(vote incumbent), T4 − control",
    color = NULL,
    fill = NULL
  ) +
  theme_minimal(base_size = 12) +
  theme(legend.position = "none")

print(p2)

ggsave(
  "latex/images/t4_mediated_only_by_crimegap.pdf",
  plot = p2,
  width = 8,
  height = 4.5
)
