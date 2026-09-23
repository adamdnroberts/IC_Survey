# Robustness of the incumbent-vote updating result to an alternative rank-gap
# measure. Split out from vote_update_analysis.R. Self-contained: builds the
# same panel, outcome and controls, fits the main log-crime-gap model and its
# 25%-threshold counterpart, and plots the two sets of interaction coefficients
# side by side.
#
# The main rank gap counts a comparison municipality as having "fewer" robberies
# whenever its rate is below the home rate. The alternative measure (matching
# belief_update_analysis.R) requires the comparison rate to be at least 25%
# below the home rate before it counts, so it is robust to respondents treating
# near-ties as ties. rank_gap_25 = actual_rank_25 - rank_prior.

library(estimatr)
library(dplyr)
library(ggplot2)

load("data/derived/survey_panel_dataset.Rdata")

if (!exists("ci_alpha")) {
  ci_alpha <- 0.01
}

# Fraction by which a comparison rate must fall below the home rate to count.
rg_thresh <- 0.25

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

panel$actual_rank_25 <- 1 +
  rowSums(
    sapply(paste0("comp_rate_", 1:4), function(col) {
      panel[[col]] < (1 - rg_thresh) * panel$home_rate
    }),
    na.rm = TRUE
  )
panel$rank_gap_25 <- panel$actual_rank_25 - panel$rank_prior

log_crime_gap_sd <- sd(panel$log_crime_gap, na.rm = TRUE)
rank_gap_sd <- sd(panel$rank_gap, na.rm = TRUE)
rank_gap_25_sd <- sd(panel$rank_gap_25, na.rm = TRUE)

# ── Main model (same specification as m_log in vote_update_analysis.R) ────────
m_log <- lm_robust(
  Vote_home_post ~
    log_crime_gap *
      as.factor(Treatment_Group) +
    rank_gap * as.factor(Treatment_Group) +
    as.factor(coalition_pre) +
    as.factor(actual_rank) +
    inc_vote,
  alpha = ci_alpha,
  data = panel,
  se_type = "HC2"
)

# ── 25% threshold counterpart ─────────────────────────────────────────────────
# NOTE: this drops as.factor(coalition_pre) and as.factor(actual_rank), matching
# the specification the block used in vote_update_analysis.R. The two models
# therefore differ in controls as well as in the rank-gap measure; see the
# caveat printed at the end of this script.
m_vote_25 <- lm_robust(
  Vote_home_post ~
    log_crime_gap *
      as.factor(Treatment_Group) +
    rank_gap_25 * as.factor(Treatment_Group) +
    inc_vote,
  alpha = ci_alpha,
  data = panel,
  se_type = "HC2"
)

summary(m_log)
summary(m_vote_25)

# Standardized CG×Treatment and RG×Treatment interaction coefficients for one
# model. rg_sd is the SD of that model's rank-gap predictor; the crime-gap
# coefficients are always scaled by log_crime_gap_sd because both models use
# log_crime_gap as the crime-gap predictor.
build_interaction_coefs <- function(model, rg_sd, model_label) {
  tidy(model, conf.int = TRUE) %>%
    filter(grepl("Treatment_Group", term) & grepl(":", term)) %>%
    mutate(
      group = if_else(
        grepl("^log_crime_gap:", term),
        "CG × Treatment",
        "RG × Treatment"
      ),
      treatment = sub(".*Treatment_Group\\)", "", term) %>% sub(":.*$", "", .),
      sd = if_else(group == "CG × Treatment", log_crime_gap_sd, rg_sd),
      across(c(estimate, conf.low, conf.high, std.error), ~ . * sd),
      conf.low95 = estimate - qt(0.975, df) * std.error,
      conf.high95 = estimate + qt(0.975, df) * std.error,
      model = model_label
    ) %>%
    filter(treatment != "control2") %>%
    dplyr::select(-sd)
}

coef_compare_25 <- bind_rows(
  build_interaction_coefs(m_log, rank_gap_sd, "Original rank gap"),
  build_interaction_coefs(m_vote_25, rank_gap_25_sd, "25% threshold")
) %>%
  mutate(
    model = factor(model, levels = c("Original rank gap", "25% threshold"))
  )

vote_coef_compare_25 <- ggplot(
  coef_compare_25,
  aes(y = treatment, x = estimate, color = model)
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
  scale_color_manual(
    values = c("Original rank gap" = "#000000", "25% threshold" = "#D55E00"),
    name = "Rank gap measure"
  ) +
  facet_wrap(~group, scales = "free_x") +
  labs(
    y = "Treatment group",
    x = "Standardized coefficient (1 SD increase in predictor)",
    caption = paste0(
      "N = ",
      m_log$nobs,
      " (original) / ",
      m_vote_25$nobs,
      " (25%)",
      ", bars 95% CI"
    )
  ) +
  theme_minimal()

print(vote_coef_compare_25)

ggsave(
  "latex/images/vote_coef_compare_rg25.pdf",
  plot = vote_coef_compare_25,
  width = 7,
  height = 4.5
)
cat("\nWrote latex/images/vote_coef_compare_rg25.pdf\n")

cat(sprintf(
  paste0(
    "\nrank_gap SD = %.3f, rank_gap_25 SD = %.3f\n",
    "Mean rank_gap = %.3f, mean rank_gap_25 = %.3f\n"
  ),
  rank_gap_sd,
  rank_gap_25_sd,
  mean(panel$rank_gap, na.rm = TRUE),
  mean(panel$rank_gap_25, na.rm = TRUE)
))
