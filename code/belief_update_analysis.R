# Belief update: incumbent (home-government) crime-handling rating.
#
# Spec form matches code/plots/t4_belief_updates_comparison.R: every model
# regresses the POST-treatment level on the PRE-treatment level (ANCOVA) rather
# than differencing them into a change score. The t4 script additionally
# controls for the opposition pre level and forces a common estimation sample,
# both of which exist there only to make its decomposition identity hold; there
# is no such identity here, so this script keeps its own samples and per-model
# listwise deletion.

library(estimatr)
library(modelsummary)
library(dplyr)
library(ggplot2)
library(broom)

load("data/derived/survey_panel_dataset.Rdata")

panel_full <- panel

# Both slider columns arrive as character from the response pull. Convert once
# here, before any subsetting, so every sample below inherits them.
panel_full$inc_post <- as.numeric(panel_full$Home_Crime_Handling_Post)
panel_full$inc_pre <- as.numeric(panel_full$Home_Crime_Handling_Pre)

# Outer (thin) interval on the plots; the thick bar is the 90% interval built
# from the same standard errors in extract_coef_plot().
ci_alpha <- 0.05

robbery_cap_mult <- 2000

# Colorblind-friendly (Okabe-Ito) palette, matching vote_update_analysis.R
arm_colors <- c(
  control2 = "#999999",
  T1 = "#56B4E9",
  T2 = "#009E73",
  T3 = "#D55E00",
  T4 = "#0072B2"
)

panel_with_failures <- filter(panel_full, muni_changed == 0)
panel <- filter(
  panel_with_failures,
  Attention_Check == "somewhat_agree" & Treatment_Group != "control2"
)

m_capped <- lm_robust(
  inc_post ~
    inc_pre +
    crime_gap_capped *
      as.factor(Treatment_Group) +
    rank_gap * as.factor(Treatment_Group) +
    coalition_pre,
  alpha = ci_alpha,
  data = panel,
  se_type = "HC2"
)

panel_exclude_extreme <- subset(
  panel,
  as.numeric(Robbery_Estimate) <=
    max(panel$home_rate, na.rm = TRUE) * robbery_cap_mult
)

m_exclude_extreme <- lm_robust(
  inc_post ~
    inc_pre +
    log_crime_gap *
      as.factor(Treatment_Group) +
    rank_gap * as.factor(Treatment_Group) +
    coalition_pre,
  alpha = ci_alpha,
  data = panel_exclude_extreme,
  se_type = "HC2"
)

m_log <- lm_robust(
  inc_post ~
    inc_pre +
    log_crime_gap *
      as.factor(Treatment_Group) +
    rank_gap * as.factor(Treatment_Group) +
    coalition_pre,
  alpha = ci_alpha,
  data = panel,
  se_type = "HC2"
)

log_crime_gap_sd <- sd(panel$log_crime_gap, na.rm = TRUE)
crime_gap_capped_sd <- sd(panel$crime_gap_capped, na.rm = TRUE)
log_crime_gap_exclude_sd <- sd(
  panel_exclude_extreme$log_crime_gap,
  na.rm = TRUE
)
rank_gap_sd <- sd(panel$rank_gap, na.rm = TRUE)

extract_coef_plot <- function(model, cg_pattern, model_label, cg_sd, rg_sd) {
  tidy(model, conf.int = TRUE) %>%
    filter(grepl(
      paste0(
        cg_pattern,
        ":as\\.factor|as\\.factor.*:rank_gap(?!:)|(?<=:)rank_gap:as\\.factor"
      ),
      term,
      perl = TRUE
    )) %>%
    filter(!grepl(paste0(cg_pattern, ":rank_gap:as\\.factor"), term)) %>%
    mutate(
      model = model_label,
      group = case_when(
        grepl(paste0("^", cg_pattern, ":as\\.factor"), term) ~ "CG × Treatment",
        TRUE ~ "RG × Treatment"
      ),
      treatment = sub(".*as\\.factor\\([^)]*\\)", "", term) %>%
        sub(":.*$", "", .)
    ) %>%
    mutate(
      sd = if_else(group == "CG × Treatment", cg_sd, rg_sd),
      across(c(estimate, conf.low, conf.high, std.error), ~ . * sd),
      conf.low90 = estimate - qt(0.95, df) * std.error,
      conf.high90 = estimate + qt(0.95, df) * std.error
    ) %>%
    dplyr::select(-sd)
}

coef_plot_both <- bind_rows(
  extract_coef_plot(
    m_capped,
    "crime_gap_capped",
    "m_capped",
    crime_gap_capped_sd,
    rank_gap_sd
  ),
  extract_coef_plot(
    m_exclude_extreme,
    "log_crime_gap",
    "m_exclude_extreme",
    log_crime_gap_exclude_sd,
    rank_gap_sd
  ),
  extract_coef_plot(
    m_log,
    "log_crime_gap",
    "m_log",
    log_crime_gap_sd,
    rank_gap_sd
  )
)

inc_update_coef_plot <- ggplot(
  subset(
    coef_plot_both,
    model == "m_log" &
      treatment == "T4"
  ),
  aes(
    y = treatment,
    x = estimate,
    color = treatment
  )
) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey50") +
  geom_errorbar(
    aes(xmin = conf.low, xmax = conf.high),
    orientation = "y",
    width = 0,
    linewidth = 0.5,
    position = position_dodge(width = 0.5)
  ) +
  geom_errorbar(
    aes(xmin = conf.low90, xmax = conf.high90),
    orientation = "y",
    width = 0,
    linewidth = 2,
    alpha = 0.4,
    position = position_dodge(width = 0.5)
  ) +
  geom_point(position = position_dodge(width = 0.5)) +
  scale_color_manual(values = arm_colors, guide = "none") +
  facet_wrap(~group, scales = "free_x") +
  labs(
    title = "Belief update: main spec (log gap, control2 excluded)",
    y = "Treatment group",
    x = "Standardized coefficient (1 SD increase in predictor)",
    caption = paste0(
      "N = ",
      m_log$nobs,
      ", thick bar 90% CI, thin 95% CI"
    )
  ) +
  theme_minimal()

print(inc_update_coef_plot)

ggsave(
  "latex/images/inc_update_coef_plot.pdf",
  plot = inc_update_coef_plot,
  width = 7,
  height = 4.5
)

# ── Joint test: does an arm respond to the information at all? ───────────────
# The figure shows each arm's two interactions one at a time. This is the joint
# (Wald) test that BOTH are zero for a given arm -- i.e. that the arm's belief
# update is unrelated to the crime-level gap AND to the rank gap. HC2 SEs come
# from the fit, so car::linearHypothesis uses the same variance-covariance
# matrix as the plotted intervals. The test is invariant to the SD rescaling
# used in the figures, so it is run on the raw coefficients.
#
# Note the two interaction terms are named asymmetrically: log_crime_gap is
# entered as log_crime_gap * arm (gap first) and rank_gap as rank_gap * arm
# (arm first), so the term strings differ in order. The stopifnot catches a
# rename before the test silently fails.
joint_gap_test <- function(model, arm) {
  cg <- paste0("log_crime_gap:as.factor(Treatment_Group)", arm)
  rg <- paste0("as.factor(Treatment_Group)", arm, ":rank_gap")
  stopifnot(all(c(cg, rg) %in% names(coef(model))))
  car::linearHypothesis(model, c(paste(cg, "= 0"), paste(rg, "= 0")))
}

for (arm in c("T2", "T4")) {
  cat(
    "\nJoint test (m_log): (CG x ",
    arm,
    ") = (RG x ",
    arm,
    ") = 0\n",
    sep = ""
  )
  print(joint_gap_test(m_log, arm), digits = 4)
}

spec_differences <- ggplot(
  coef_plot_both,
  aes(
    y = treatment,
    x = estimate,
    color = model
  )
) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey50") +
  geom_errorbar(
    aes(xmin = conf.low, xmax = conf.high),
    orientation = "y",
    width = 0,
    linewidth = 0.5,
    position = position_dodge(width = 0.5)
  ) +
  geom_errorbar(
    aes(xmin = conf.low90, xmax = conf.high90),
    orientation = "y",
    width = 0,
    linewidth = 2,
    alpha = 0.4,
    position = position_dodge(width = 0.5)
  ) +
  geom_point(position = position_dodge(width = 0.5)) +
  facet_wrap(~group, scales = "free_x") +
  scale_color_brewer(
    palette = "Dark2",
    breaks = c("m_capped", "m_log", "m_exclude_extreme"),
    labels = c(
      "m_capped" = "Capped level gap",
      "m_log" = "Log gap (all)",
      "m_exclude_extreme" = "Log gap (extremes dropped)"
    )
  ) +
  labs(
    title = "Belief update: comparison across gap specifications",
    y = "Treatment group",
    x = "Standardized coefficient (1 SD increase in predictor)",
    color = "Specification",
    caption = paste0(
      "N = ",
      m_capped$nobs,
      ", thick bar 90% CI, thin 95% CI"
    )
  ) +
  theme_minimal()

print(spec_differences)

ggsave(
  "latex/images/inc_update_spec_differences_plot.pdf",
  plot = spec_differences,
  width = 7,
  height = 4.5
)

# Attention-check robustness: the only thing that should differ between these
# two fits is the sample, so both use the main log-gap spec (m_log) rather than
# the capped gap. Standardized by the main-sample SDs in both cases, so the two
# sets of bars share a scale.
m_attn_all <- lm_robust(
  inc_post ~
    inc_pre +
    log_crime_gap *
      as.factor(Treatment_Group) +
    rank_gap * as.factor(Treatment_Group) +
    coalition_pre,
  alpha = ci_alpha,
  data = panel_with_failures,
  se_type = "HC2"
)

attn_check_compare <- bind_rows(
  extract_coef_plot(
    m_log,
    "log_crime_gap",
    "Excludes attn-check failures",
    log_crime_gap_sd,
    rank_gap_sd
  ),
  extract_coef_plot(
    m_attn_all,
    "log_crime_gap",
    "Includes attn-check failures",
    log_crime_gap_sd,
    rank_gap_sd
  )
)

attn_check_coef_compare <- ggplot(
  subset(attn_check_compare, treatment != "control2"),
  aes(
    y = treatment,
    x = estimate,
    color = model
  )
) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey50") +
  geom_errorbar(
    aes(xmin = conf.low, xmax = conf.high),
    orientation = "y",
    width = 0,
    linewidth = 0.5,
    position = position_dodge(width = 0.5)
  ) +
  geom_errorbar(
    aes(xmin = conf.low90, xmax = conf.high90),
    orientation = "y",
    width = 0,
    linewidth = 2,
    alpha = 0.4,
    position = position_dodge(width = 0.5)
  ) +
  geom_point(position = position_dodge(width = 0.5)) +
  facet_wrap(~group, scales = "free_x") +
  scale_color_brewer(palette = "Dark2") +
  labs(
    title = "Belief update: including vs. excluding attn-check failures",
    y = "Treatment group",
    x = "Standardized coefficient (1 SD increase in predictor)",
    color = "Sample",
    caption = paste0(
      "N = ",
      m_log$nobs,
      " (excl. failures) vs ",
      m_attn_all$nobs,
      " (incl. failures)",
      ", thick bar 90% CI, thin 95% CI"
    )
  ) +
  theme_minimal()

print(attn_check_coef_compare)

ggsave(
  "latex/images/inc_update_incl_failed_attn_plot.pdf",
  plot = attn_check_coef_compare,
  width = 7,
  height = 4.5
)

panel$t_pooled_control <- panel$Treatment_Group
panel$t_pooled_control[panel$Treatment_Group == "control2"] <- "control"

m_log_pooled <- lm_robust(
  inc_post ~
    inc_pre +
    log_crime_gap *
      as.factor(t_pooled_control) +
    rank_gap * as.factor(t_pooled_control) +
    coalition_pre,
  alpha = ci_alpha,
  data = panel,
  se_type = "HC2"
)

coef_plot_pooled <- extract_coef_plot(
  m_log_pooled,
  "log_crime_gap",
  "m_log_pooled",
  log_crime_gap_sd,
  rank_gap_sd
)

inc_update_coef_plot_pooled <- ggplot(
  coef_plot_pooled,
  aes(
    y = treatment,
    x = estimate,
    color = treatment
  )
) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey50") +
  geom_errorbar(
    aes(xmin = conf.low, xmax = conf.high),
    orientation = "y",
    width = 0,
    linewidth = 0.5,
    position = position_dodge(width = 0.5)
  ) +
  geom_errorbar(
    aes(xmin = conf.low90, xmax = conf.high90),
    orientation = "y",
    width = 0,
    linewidth = 2,
    alpha = 0.4,
    position = position_dodge(width = 0.5)
  ) +
  geom_point(position = position_dodge(width = 0.5)) +
  scale_color_manual(values = arm_colors, guide = "none") +
  facet_wrap(~group, scales = "free_x") +
  labs(
    title = "Belief update: control2 pooled into control",
    y = "Treatment group",
    x = "Standardized coefficient (1 SD increase in predictor)",
    caption = paste0(
      "N = ",
      m_log_pooled$nobs,
      ", thick bar 90% CI, thin 95% CI"
    )
  ) +
  theme_minimal()

print(inc_update_coef_plot_pooled)

ggsave(
  "latex/images/incumbent_rank_coef_update_pooled.pdf",
  plot = inc_update_coef_plot_pooled,
  width = 7,
  height = 4.5
)

rg_thresh <- 0.25

panel$actual_rank_25 <- 1 +
  rowSums(
    sapply(paste0("comp_rate_", 1:4), function(col) {
      panel[[col]] < (1 - rg_thresh) * panel$home_rate
    }),
    na.rm = TRUE
  )

panel$rank_gap_25 <- panel$actual_rank_25 - panel$rank_prior

rank_gap_25_sd <- sd(panel$rank_gap_25, na.rm = TRUE)

m_rg25 <- lm_robust(
  inc_post ~
    inc_pre +
    log_crime_gap *
      as.factor(Treatment_Group) +
    rank_gap_25 * as.factor(Treatment_Group) +
    coalition_pre,
  alpha = ci_alpha,
  data = panel,
  se_type = "HC2"
)

coef_plot_rg25 <- extract_coef_plot(
  m_rg25,
  "log_crime_gap",
  "m_rg25",
  log_crime_gap_sd,
  rank_gap_25_sd
)

inc_update_coef_plot_rg25 <- ggplot(
  coef_plot_rg25,
  aes(
    y = treatment,
    x = estimate,
    color = treatment
  )
) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey50") +
  geom_errorbar(
    aes(xmin = conf.low, xmax = conf.high),
    orientation = "y",
    width = 0,
    linewidth = 0.5,
    position = position_dodge(width = 0.5)
  ) +
  geom_errorbar(
    aes(xmin = conf.low90, xmax = conf.high90),
    orientation = "y",
    width = 0,
    linewidth = 2,
    alpha = 0.4,
    position = position_dodge(width = 0.5)
  ) +
  geom_point(position = position_dodge(width = 0.5)) +
  scale_color_manual(values = arm_colors, guide = "none") +
  facet_wrap(~group, scales = "free_x") +
  labs(
    title = "Belief update: alternative rank gap (25% threshold)",
    y = "Treatment group",
    x = "Standardized coefficient (1 SD increase in predictor)",
    caption = paste0("N = ", m_rg25$nobs, ", thick bar 90% CI, thin 95% CI")
  ) +
  theme_minimal()

print(inc_update_coef_plot_rg25)

ggsave(
  "latex/images/inc_update_alt_rank_gap_plot.pdf",
  plot = inc_update_coef_plot_rg25,
  width = 7,
  height = 4.5
)

# control2 is now excluded from the panel (see filter above), so this control2
# vs. T3 contrast is no longer estimable.
# car::linearHypothesis(
#   m_rg25,
#   "as.factor(Treatment_Group)control2:rank_gap_25 = as.factor(Treatment_Group)T3:rank_gap_25"
# )
