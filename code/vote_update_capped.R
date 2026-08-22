# Alt-spec companion to the main vote-intention figure in vote_update_analysis.R.
# Same outcome, sample and controls; the only change is how extreme perception
# gaps enter the model:
#
#   vote_update_analysis.R (m_log) -> log_crime_gap      -> vote_coef_update_log_cg.pdf
#   this script            (m_cap) -> crime_gap_capped   -> vote_coef_update_capped.pdf
#
# crime_gap_capped is built in create_panel_dataset.R by top-coding the robbery
# estimate at robbery_cap_mult x the maximum observed home rate, so this spec
# keeps respondents with very large gaps but bounds their leverage, where the log
# spec compresses the whole scale. Holding everything else fixed makes the two
# slides differ only in that choice.
#
# Deliberately NOT copied from m_vote in vote_update_analysis.R: that model also
# includes as.factor(actual_rank). Adding it here would make the alt-spec slide
# differ from the main slide in two ways at once.

library(estimatr)
library(dplyr)
library(ggplot2)
library(broom)

load("data/derived/survey_panel_dataset.Rdata")

if (!exists("ci_alpha")) {
  ci_alpha <- 0.01
}

# Colorblind-friendly (Okabe-Ito) palette, matching vote_update_analysis.R
arm_colors <- c(
  control2 = "#999999",
  T1 = "#56B4E9",
  T2 = "#009E73",
  T3 = "#D55E00",
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

crime_gap_capped_sd <- sd(panel$crime_gap_capped, na.rm = TRUE)
rank_gap_sd <- sd(panel$rank_gap, na.rm = TRUE)

m_cap <- lm_robust(
  Vote_home_post ~
    crime_gap_capped *
    as.factor(Treatment_Group) +
    rank_gap * as.factor(Treatment_Group) +
    as.factor(coalition_pre) +
    inc_vote,
  alpha = ci_alpha,
  data = panel,
  se_type = "HC2"
)

summary(m_cap)

coef_plot_data_cap <- tidy(m_cap, conf.int = TRUE) %>%
  filter(grepl("Treatment_Group", term) & grepl(":", term)) %>%
  mutate(
    group = case_when(
      grepl("^crime_gap_capped:", term) ~ "CG × Treatment",
      TRUE ~ "RG × Treatment"
    ),
    treatment = sub(".*Treatment_Group\\)", "", term) %>% sub(":.*$", "", .),
    sd = if_else(group == "CG × Treatment", crime_gap_capped_sd, rank_gap_sd),
    across(c(estimate, conf.low, conf.high, std.error), ~ . * sd),
    conf.low95 = estimate - qt(0.975, df) * std.error,
    conf.high95 = estimate + qt(0.975, df) * std.error
  ) %>%
  filter(treatment != "control2") %>%
  dplyr::select(-sd)

vote_coef_update_capped <- ggplot(
  coef_plot_data_cap,
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
    caption = paste0("N = ", m_cap$nobs, ", bars 95% CI")
  ) +
  theme_minimal()

print(vote_coef_update_capped)

ggsave(
  "latex/images/vote_coef_update_capped.pdf",
  plot = vote_coef_update_capped,
  width = 7,
  height = 4.5
)
