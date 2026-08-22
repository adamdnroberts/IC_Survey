# Re-estimates the m_log vote-update model from code/vote_update_analysis.R with
# control2 (weather placebo + comparison bar chart) as the omitted baseline
# instead of control (weather placebo, home municipality only).
#
# This is the mirror image of vote_update_analysis.R, which drops control2 and
# leaves control as the baseline. Benchmarking against control2 nets out the
# "saw a comparison chart" component of the treatment, so the T2-T4 interaction
# coefficients isolate the effect of the chart's INFORMATION content rather than
# its presence. Everything else -- sample filters, outcome construction,
# controls, standardization, and the coefficient plot -- matches the source
# script, so estimates are directly comparable across the two baselines, with one
# exception: for control2 rows only, actual_rank (and hence rank_gap) is built
# from the PRECIPITATION values those respondents were actually shown, rather
# than from robbery rates. See the block below.

library(estimatr)
library(dplyr)
library(ggplot2)
library(broom)

load("data/derived/survey_panel_dataset.Rdata")

if (!exists("ci_alpha")) {
  ci_alpha <- 0.01
}

# Colorblind-friendly (Okabe-Ito) palette, matching vote_update_analysis.R.
# control is the dropped arm here, so it needs no entry.
arm_colors <- c(
  T1 = "#56B4E9",
  T2 = "#009E73",
  T3 = "#D55E00",
  T4 = "#0072B2"
)

panel$Vote_home_post <- as.integer(
  !is.na(panel$coalition_post) &
    !is.na(panel$home_coalition) &
    panel$home_coalition == panel$coalition_post
)

panel_full <- panel

panel_full$coalition_pre[is.na(panel_full$coalition_pre)] <- "Other"

panel_full$inc_vote <- as.numeric(
  panel_full$coalition_pre == panel_full$home_coalition
)

# ── control2: rank the comparison municipalities on RAINFALL, not crime ───────
# control2 respondents see a bar chart of annual precipitation for their home
# municipality and the same four comparison municipalities that the treatment
# arms see robbery rates for. The rank they are actually shown is therefore a
# rainfall rank, so for these rows actual_rank is rebuilt from precip_mm using
# the identical counting rule as the crime version in create_panel_dataset.R
# (1 + the number of comparison municipalities BELOW the home value). All other
# arms keep their crime-based actual_rank untouched.
precip <- readRDS("data/precip_data.rds")

precip_of <- function(ids) {
  precip$precip_mm[match(ids, precip$muni_id)]
}

panel_full$home_precip <- precip_of(panel_full$Found_Municipality_ID)

comp_precip <- sapply(1:4, function(i) {
  precip_of(panel_full[[paste0("Comparison_Muni_", i, "_ID")]])
})

panel_full$actual_rank_precip <- 1 +
  rowSums(comp_precip < panel_full$home_precip, na.rm = TRUE)

is_c2 <- panel_full$Treatment_Group == "control2"
panel_full$actual_rank[is_c2] <- panel_full$actual_rank_precip[is_c2]

# rank_gap must be recomputed from the swapped actual_rank. rank_prior is
# unchanged: it is the respondent's pre-treatment CRIME ranking, which is what
# the rainfall chart is (as a placebo) failing to speak to.
panel_full$rank_gap <- panel_full$actual_rank - panel_full$rank_prior

# Sample: same filters as vote_update_analysis.R, except the EXCLUDED control
# arm is "control" rather than "control2".
panel_with_failures <- filter(panel_full, muni_changed == 0)
panel <- filter(
  panel_with_failures,
  Attention_Check == "somewhat_agree" & Treatment_Group != "control"
)

# relevel() makes control2 the omitted category; without this, as.factor() would
# sort "T1" after "control2" and pick control2 anyway, but being explicit keeps
# the baseline correct if the arm labels ever change.
panel$Treatment_Group <- relevel(
  droplevels(factor(panel$Treatment_Group)),
  ref = "control2"
)

log_crime_gap_sd <- sd(panel$log_crime_gap, na.rm = TRUE)
rank_gap_sd <- sd(panel$rank_gap, na.rm = TRUE)

m_log_control2 <- lm_robust(
  Vote_home_post ~
    log_crime_gap *
    Treatment_Group +
    rank_gap * Treatment_Group +
    as.factor(coalition_pre) +
    inc_vote,
  alpha = ci_alpha,
  data = panel,
  se_type = "HC2"
)

summary(m_log_control2)

coef_plot_data_log_control2 <- tidy(m_log_control2, conf.int = TRUE) %>%
  filter(grepl("Treatment_Group", term) & grepl(":", term)) %>%
  mutate(
    group = case_when(
      grepl("^log_crime_gap:", term) ~ "CG × Treatment",
      TRUE ~ "RG × Treatment"
    ),
    treatment = sub(".*Treatment_Group", "", term) %>% sub(":.*$", "", .),
    sd = if_else(group == "CG × Treatment", log_crime_gap_sd, rank_gap_sd),
    across(c(estimate, conf.low, conf.high, std.error), ~ . * sd),
    conf.low95 = estimate - qt(0.975, df) * std.error,
    conf.high95 = estimate + qt(0.975, df) * std.error
  ) %>%
  dplyr::select(-sd)

vote_coef_update_log_control2 <- ggplot(
  coef_plot_data_log_control2,
  aes(y = treatment, x = estimate, color = treatment)
) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey50") +
  geom_errorbar(
    aes(xmin = conf.low95, xmax = conf.high95),
    orientation = "y",
    width = 0,
    linewidth = 0.5,
    position = position_dodge(width = 0.5)
  ) +
  geom_point(position = position_dodge(width = 0.5)) +
  scale_color_manual(values = arm_colors, guide = "none") +
  facet_wrap(~group, scales = "free_x") +
  labs(
    y = "Treatment group",
    x = "Standardized coefficient (1 SD increase in predictor)",
    caption = paste0(
      "Baseline = control2, N = ",
      m_log_control2$nobs,
      ", bars 95% CI"
    )
  ) +
  theme_minimal()

print(vote_coef_update_log_control2)

ggsave(
  "latex/images/vote_coef_update_log_control2.pdf",
  plot = vote_coef_update_log_control2,
  width = 7,
  height = 4.5
)

# Same coefficients, but one standalone plot per gap measure instead of a
# two-panel facet (mirrors the split in vote_update_analysis.R).
build_control2_gap_plot <- function(group_label, x_label) {
  ggplot(
    subset(coef_plot_data_log_control2, group == group_label),
    aes(y = treatment, x = estimate, color = treatment)
  ) +
    geom_vline(xintercept = 0, linetype = "dashed", color = "grey50") +
    geom_errorbar(
      aes(xmin = conf.low95, xmax = conf.high95),
      orientation = "y",
      width = 0,
      linewidth = 0.5,
      position = position_dodge(width = 0.5)
    ) +
    geom_point(position = position_dodge(width = 0.5)) +
    scale_color_manual(values = arm_colors, guide = "none") +
    labs(
      y = "Treatment group",
      x = x_label,
      title = group_label,
      caption = paste0(
        "Baseline = control2, N = ",
        m_log_control2$nobs,
        ", bars 95% CI"
      )
    ) +
    theme_minimal()
}

vote_coef_update_log_control2_cg <- build_control2_gap_plot(
  "CG × Treatment",
  "Standardized coefficient (1 SD increase in crime gap)"
)

vote_coef_update_log_control2_rg <- build_control2_gap_plot(
  "RG × Treatment",
  "Standardized coefficient (1 SD increase in rank gap)"
)

print(vote_coef_update_log_control2_cg)
print(vote_coef_update_log_control2_rg)

ggsave(
  "latex/images/vote_coef_update_log_control2_cg.pdf",
  plot = vote_coef_update_log_control2_cg,
  width = 5.5,
  height = 4.5
)

ggsave(
  "latex/images/vote_coef_update_log_control2_rg.pdf",
  plot = vote_coef_update_log_control2_rg,
  width = 5.5,
  height = 4.5
)
