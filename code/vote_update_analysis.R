library(estimatr)
library(dplyr)
library(ggplot2)

load("~/IC_Survey/data/survey_panel_dataset.Rdata")

panel$Vote_home_post <- as.integer(
  !is.na(panel$coalition_post) &
    !is.na(panel$home_coalition) &
    panel$home_coalition == panel$coalition_post
)

panel_with_failures <- panel
panel <- filter(panel, Attention_Check == "somewhat_agree")

panel$coalition_pre[is.na(panel$coalition_pre)] <- "Other"

m_vote <- lm_robust(
  Vote_home_post ~
    crime_gap_wins *
      as.factor(Treatment_Group) +
      rank_gap * as.factor(Treatment_Group) +
      as.factor(coalition_pre),
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
    treatment = sub(".*Treatment_Group\\)", "", term) %>% sub(":.*$", "", .)
  )

vote_coef_update <- ggplot(
  subset(coef_plot_data_vote),
  aes(y = treatment, x = estimate, xmin = conf.low, xmax = conf.high)
) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey50") +
  geom_pointrange() +
  facet_wrap(~group, scales = "free_x") +
  labs(
    y = "Treatment group",
    x = "Coefficient estimate",
    #title = "Incumbent vote post: interaction coefficients",
    caption = paste0("N = ", m_vote$nobs)
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
  #alpha = ci_alpha,
  data = panel,
  se_type = "HC2"
)

coef_plot_data_vote <- tidy(m_log, conf.int = TRUE) %>%
  filter(grepl("Treatment_Group", term) & grepl(":", term)) %>%
  mutate(
    group = case_when(
      grepl("^log_crime_gap:", term) ~ "CG × Treatment",
      TRUE ~ "RG × Treatment"
    ),
    treatment = sub(".*Treatment_Group\\)", "", term) %>% sub(":.*$", "", .)
  )

vote_coef_update <- ggplot(
  subset(coef_plot_data_vote),
  aes(y = treatment, x = estimate, xmin = conf.low, xmax = conf.high)
) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey50") +
  geom_pointrange() +
  facet_wrap(~group, scales = "free_x") +
  labs(
    y = "Treatment group",
    x = "Coefficient estimate",
    #title = "Incumbent vote post: interaction coefficients",
    caption = paste0("N = ", m_vote$nobs)
  ) +
  theme_minimal()

print(vote_coef_update)
