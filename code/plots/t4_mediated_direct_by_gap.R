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
# Sources code/mediation_analysis.R for the reconciled pooled models
# (m_mediator, m_outcome) and panel_med. NOTE: that source also runs the script's
# own mediate() diagnostics, so this takes a couple of minutes to run.
# Output: latex/images/t4_mediated_direct_by_crimegap.pdf

source("~/IC_Survey/code/mediation_analysis.R")
library(ggplot2)

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
