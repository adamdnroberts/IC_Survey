# Vote-intention analogue of the spec-comparison figure in
# belief_update_analysis.R (-> inc_update_spec_differences_plot.pdf).
#
# Same question -- does the handling of extreme perception gaps drive the
# result? -- asked of the vote outcome instead of the belief outcome. Three
# specifications of the crime-level gap, overlaid on one set of axes:
#
#   m_capped           crime_gap_capped (robbery estimate top-coded upstream)
#   m_log              log_crime_gap, all respondents
#   m_exclude_extreme  log_crime_gap, respondents with extreme estimates dropped
#
# Outcome, sample and controls follow m_log in vote_update_analysis.R, so the CG
# and RG interactions here line up with vote_coef_update_log_cg.pdf.

library(estimatr)
library(dplyr)
library(ggplot2)
library(broom)

load("data/derived/survey_panel_dataset.Rdata")

if (!exists("ci_alpha")) {
  ci_alpha <- 0.01
}

# Multiplier defining the "extreme estimate" cutoff for m_exclude_extreme:
# respondents whose robbery estimate exceeds vote_exclude_mult x the maximum
# observed home rate are dropped. Matches belief_update_analysis.R so the two
# alt-spec figures answer the same question the same way.
#
# NOTE: create_panel_dataset.R already sets estimates above
# robbery_implausible_max (100,000) to NA, so this cutoff only bites if
# vote_exclude_mult * max(home_rate) falls below that. The count printed below
# reports how many respondents it actually removes -- if that is 0, this spec is
# identical to m_log and the third series is redundant.
vote_exclude_mult <- 2000

# Colorblind-friendly (Okabe-Ito) palette, matching vote_update_analysis.R
arm_colors <- c(
  control2 = "#999999",
  T1 = "#56B4E9",
  T2 = "#009E73",
  T3 = "#E69F00",
  T4 = "#0072B2"
)

# Outcome and controls constructed exactly as in vote_update_analysis.R.
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

panel_with_failures <- filter(panel_full, muni_changed == 0)
panel <- filter(
  panel_with_failures,
  Attention_Check == "somewhat_agree" & Treatment_Group != "control2"
)

# Shared right-hand side; only the crime-level-gap term varies across specs.
vote_formula <- function(cg_term) {
  as.formula(paste0(
    "Vote_home_post ~ ",
    cg_term,
    " * as.factor(Treatment_Group) +",
    " rank_gap * as.factor(Treatment_Group) +",
    " as.factor(coalition_pre) + inc_vote"
  ))
}

m_capped <- lm_robust(
  vote_formula("crime_gap_capped"),
  alpha = ci_alpha,
  data = panel,
  se_type = "HC2"
)

m_log <- lm_robust(
  vote_formula("log_crime_gap"),
  alpha = ci_alpha,
  data = panel,
  se_type = "HC2"
)

extreme_cutoff <- max(panel$home_rate, na.rm = TRUE) * vote_exclude_mult
panel_exclude_extreme <- subset(
  panel,
  as.numeric(Robbery_Estimate) <= extreme_cutoff
)

# Report what the exclusion actually did, so a cutoff that never binds is
# visible in the run log rather than hidden behind an unchanged figure.
n_dropped <- sum(
  !is.na(as.numeric(panel$Robbery_Estimate)) &
    as.numeric(panel$Robbery_Estimate) > extreme_cutoff
)
cat(sprintf(
  "m_exclude_extreme: cutoff %.0f drops %d of %d respondents with a non-missing estimate\n",
  extreme_cutoff,
  n_dropped,
  sum(!is.na(as.numeric(panel$Robbery_Estimate)))
))

m_exclude_extreme <- lm_robust(
  vote_formula("log_crime_gap"),
  alpha = ci_alpha,
  data = panel_exclude_extreme,
  se_type = "HC2"
)

crime_gap_capped_sd <- sd(panel$crime_gap_capped, na.rm = TRUE)
log_crime_gap_sd <- sd(panel$log_crime_gap, na.rm = TRUE)
log_crime_gap_exclude_sd <- sd(
  panel_exclude_extreme$log_crime_gap,
  na.rm = TRUE
)
rank_gap_sd <- sd(panel$rank_gap, na.rm = TRUE)

# Same extraction as belief_update_analysis.R: keep the CG x arm and RG x arm
# interactions, rescale each to a 1 SD increase in its own predictor, and add
# 95% bounds alongside the model's ci_alpha (99%) bounds.
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
      conf.low95 = estimate - qt(0.975, df) * std.error,
      conf.high95 = estimate + qt(0.975, df) * std.error
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
) %>%
  filter(treatment != "control2")

vote_spec_differences <- ggplot(
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
    aes(xmin = conf.low95, xmax = conf.high95),
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
    title = "Vote intention: comparison across gap specifications",
    y = "Treatment group",
    x = "Standardized coefficient (1 SD increase in predictor)",
    color = "Specification",
    caption = paste0(
      "N = ",
      m_log$nobs,
      ", thick bar 95% CI, thin 99% CI"
    )
  ) +
  theme_minimal()

print(vote_spec_differences)

ggsave(
  "latex/images/vote_update_spec_differences_plot.pdf",
  plot = vote_spec_differences,
  width = 7,
  height = 4.5
)
