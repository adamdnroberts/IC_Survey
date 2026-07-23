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
# Self-contained: rebuilds panel_med and the pooled mediator/outcome models
# (m_mediator, m_outcome) inline, mirroring code/mediation_analysis.R, so this
# script no longer sources it (and skips that script's slow mediate() diagnostics).
# Keep this setup block in sync with mediation_analysis.R if the models change.
# Output: latex/images/t4_mediated_direct_by_crimegap.pdf

library(dplyr)
library(ggplot2)
library(mediation)

load("~/IC_Survey/data/survey_panel_dataset.Rdata")

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

m_outcome <- lm(
  Vote_home_post ~
    inc_minus_opp_avg_post +
    inc_pre +
    opp_avg_pre +
    log_crime_gap * Treatment_Group +
    rank_gap * Treatment_Group +
    coalition_pre,
  data = panel_med
)

set.seed(1)
n_grid <- 13
sims <- 500

rg_mean <- mean(panel_med$rank_gap, na.rm = TRUE)
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
    covariates = list(log_crime_gap = g, rank_gap = rg_mean),
    robustSE = TRUE,
    sims = sims
  )
  data.frame(
    log_crime_gap = g,
    acme = m$d0, acme_lo = m$d0.ci[1], acme_hi = m$d0.ci[2],
    ade = m$z0, ade_lo = m$z0.ci[1], ade_hi = m$z0.ci[2]
  )
})
df <- do.call(rbind, rows)
print(df, row.names = FALSE)

med_lab <- "Mediated (via inc−opp rating)"
dir_lab <- "Direct (other channels)"
sel <- function(d, cols) setNames(d[, cols], c("log_crime_gap", "est", "lo", "hi"))
plot_df <- rbind(
  transform(sel(df, c("log_crime_gap", "acme", "acme_lo", "acme_hi")), effect = med_lab),
  transform(sel(df, c("log_crime_gap", "ade", "ade_lo", "ade_hi")), effect = dir_lab)
)
plot_df$effect <- factor(plot_df$effect, levels = c(dir_lab, med_lab))

effect_colors <- setNames(c("#D55E00", "#0072B2"), c(dir_lab, med_lab))

p <- ggplot(plot_df, aes(log_crime_gap, est, color = effect, fill = effect)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +
  geom_ribbon(aes(ymin = lo, ymax = hi), alpha = 0.15, color = NA) +
  geom_line(linewidth = 0.9) +
  scale_color_manual(values = effect_colors) +
  scale_fill_manual(values = effect_colors) +
  labs(
    title = "T4 effect on incumbent vote: mediated vs. direct, by crime gap",
    subtitle = "rank_gap held at its mean; bands are 95% quasi-Bayesian CIs",
    x = "log_crime_gap (prior surprise about crime level)",
    y = "Effect on P(vote incumbent), T4 − control",
    color = NULL,
    fill = NULL
  ) +
  theme_minimal(base_size = 12) +
  theme(legend.position = "top")

print(p)

ggsave(
  "latex/images/t4_mediated_direct_by_crimegap.pdf",
  plot = p,
  width = 7,
  height = 4.5
)
