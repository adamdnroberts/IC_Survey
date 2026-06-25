library(estimatr)
library(dplyr)
library(ggplot2)

load("~/IC_Survey/data/survey_panel_dataset.Rdata")

if (!exists("ci_alpha")) ci_alpha <- 0.01

panel$Vote_home_post <- as.integer(
  !is.na(panel$coalition_post) &
    !is.na(panel$home_coalition) &
    panel$home_coalition == panel$coalition_post
)

panel_with_failures <- panel
panel <- filter(panel, Attention_Check == "somewhat_agree")

panel$coalition_pre[is.na(panel$coalition_pre)] <- "Other"

crime_gap_wins_sd <- sd(panel$crime_gap_wins, na.rm = TRUE)
log_crime_gap_sd <- sd(panel$log_crime_gap, na.rm = TRUE)
rank_gap_sd <- sd(panel$rank_gap, na.rm = TRUE)

m_vote <- lm_robust(
  Vote_home_post ~
    crime_gap_wins *
      as.factor(Treatment_Group) +
      rank_gap * as.factor(Treatment_Group) +
      as.factor(coalition_pre),
  alpha = ci_alpha,
  data = panel,
  se_type = "HC2"
)

summary(m_vote)

coef_plot_data_vote <- tidy(m_vote, conf.int = TRUE) %>%
  filter(grepl("Treatment_Group", term) & grepl(":", term)) %>%
  mutate(
    group = case_when(
      grepl("^crime_gap_wins:", term) ~ "CG × Treatment",
      TRUE ~ "RG × Treatment"
    ),
    treatment = sub(".*Treatment_Group\\)", "", term) %>% sub(":.*$", "", .),
    sd = if_else(group == "CG × Treatment", crime_gap_wins_sd, rank_gap_sd),
    across(c(estimate, conf.low, conf.high, std.error), ~ . * sd),
    conf.low95 = estimate - qt(0.975, df) * std.error,
    conf.high95 = estimate + qt(0.975, df) * std.error
  ) %>%
  filter(treatment != "control2") %>%
  select(-sd)

vote_coef_update <- ggplot(
  subset(coef_plot_data_vote),
  aes(y = treatment, x = estimate)
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
  labs(
    y = "Treatment group",
    x = "Standardized coefficient (1 SD increase in predictor)",
    #title = "Incumbent vote post: interaction coefficients",
    caption = paste0("N = ", m_vote$nobs, ", thick bar 95% CI, thin 99% CI")
  ) +
  theme_minimal()

print(vote_coef_update)

ggsave(
  "latex/images/vote_coef_update.pdf",
  plot = vote_coef_update,
  width = 7,
  height = 4.5
)

m_log <- lm_robust(
  Vote_home_post ~
    log_crime_gap *
      as.factor(Treatment_Group) +
      rank_gap * as.factor(Treatment_Group) +
      comp_party_known,
  alpha = ci_alpha,
  data = panel,
  se_type = "HC2"
)

coef_plot_data_log <- tidy(m_log, conf.int = TRUE) %>%
  filter(grepl("Treatment_Group", term) & grepl(":", term)) %>%
  mutate(
    group = case_when(
      grepl("^log_crime_gap:", term) ~ "CG × Treatment",
      TRUE ~ "RG × Treatment"
    ),
    treatment = sub(".*Treatment_Group\\)", "", term) %>% sub(":.*$", "", .),
    sd = if_else(group == "CG × Treatment", log_crime_gap_sd, rank_gap_sd),
    across(c(estimate, conf.low, conf.high, std.error), ~ . * sd),
    conf.low95 = estimate - qt(0.975, df) * std.error,
    conf.high95 = estimate + qt(0.975, df) * std.error
  ) %>%
  filter(treatment != "control2") %>%
  select(-sd)

vote_coef_update_log <- ggplot(
  subset(coef_plot_data_log),
  aes(y = treatment, x = estimate)
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
  labs(
    y = "Treatment group",
    x = "Standardized coefficient (1 SD increase in predictor)",
    #title = "Incumbent vote post: interaction coefficients",
    caption = paste0("N = ", m_log$nobs, ", thick bar 95% CI, thin 99% CI")
  ) +
  theme_minimal()

print(vote_coef_update_log)

ggsave(
  "latex/images/vote_coef_update_log.pdf",
  plot = vote_coef_update_log,
  width = 7,
  height = 4.5
)
