library(estimatr)
library(modelsummary)
library(dplyr)
library(ggplot2)
library(broom)
library(readxl)

load("~/IC_Survey/data/survey_panel_dataset.Rdata")

panel_with_failures <- panel
panel <- filter(panel, Attention_Check == "somewhat_agree")

m_winsorized <- lm_robust(
  Home_Crime_Handling_Change ~
    crime_gap_wins *
      as.factor(Treatment_Group) +
      rank_gap * as.factor(Treatment_Group) +
      comp_party_known,
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
  Home_Crime_Handling_Change ~
    log_crime_gap *
      as.factor(Treatment_Group) +
      rank_gap * as.factor(Treatment_Group) +
      comp_party_known,
  alpha = ci_alpha,
  data = panel_exclude_extreme,
  se_type = "HC2"
)

m_log <- lm_robust(
  Home_Crime_Handling_Change ~
    log_crime_gap *
      as.factor(Treatment_Group) +
      rank_gap * as.factor(Treatment_Group) +
      comp_party_known,
  alpha = ci_alpha,
  data = panel,
  se_type = "HC2"
)

log_crime_gap_sd <- sd(panel$log_crime_gap, na.rm = TRUE)
crime_gap_wins_sd <- sd(panel$crime_gap_wins, na.rm = TRUE)
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
      conf.low95 = estimate - qt(0.975, df) * std.error,
      conf.high95 = estimate + qt(0.975, df) * std.error
    ) %>%
    select(-sd)
}

coef_plot_both <- bind_rows(
  extract_coef_plot(
    m_winsorized,
    "crime_gap_wins",
    "m_winsorized",
    crime_gap_wins_sd,
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
    model == "m_winsorized" &
      treatment != "control2"
  ),
  aes(
    y = treatment,
    x = estimate
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
  labs(
    y = "Treatment group",
    x = "Standardized coefficient (1 SD increase in predictor)",
    caption = paste0(
      "N = ",
      m_winsorized$nobs,
      ", thick bar 95% CI, thin 99% CI"
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
    breaks = c("m_winsorized", "m_log", "m_exclude_extreme"),
    labels = c(
      "m_winsorized" = "Winsorized level gap",
      "m_log" = "Log gap (all)",
      "m_exclude_extreme" = "Log gap (extremes dropped)"
    )
  ) +
  labs(
    y = "Treatment group",
    x = "Standardized coefficient (1 SD increase in predictor)",
    color = "Specification",
    caption = paste0(
      "N = ",
      m_winsorized$nobs,
      ", thick bar 95% CI, thin 99% CI"
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

m_attn_all <- lm_robust(
  Home_Crime_Handling_Change ~
    crime_gap_wins *
      as.factor(Treatment_Group) +
      rank_gap * as.factor(Treatment_Group) +
      comp_party_known,
  alpha = ci_alpha,
  data = panel_with_failures,
  se_type = "HC2"
)

attn_check_compare <- bind_rows(
  extract_coef_plot(
    m_winsorized,
    "crime_gap_wins",
    "Excludes attn-check failures",
    crime_gap_wins_sd,
    rank_gap_sd
  ),
  extract_coef_plot(
    m_attn_all,
    "crime_gap_wins",
    "Includes attn-check failures",
    crime_gap_wins_sd,
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
    aes(xmin = conf.low95, xmax = conf.high95),
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
    y = "Treatment group",
    x = "Standardized coefficient (1 SD increase in predictor)",
    color = "Sample",
    caption = paste0(
      "N = ",
      m_winsorized$nobs,
      " (excl. failures) vs ",
      m_attn_all$nobs,
      " (incl. failures)",
      ", thick bar 95% CI, thin 99% CI"
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
  Home_Crime_Handling_Change ~
    log_crime_gap *
      as.factor(t_pooled_control) +
      rank_gap * as.factor(t_pooled_control) +
      comp_party_known,
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
    x = estimate
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
  labs(
    y = "Treatment group",
    x = "Standardized coefficient (1 SD increase in predictor)",
    caption = paste0(
      "N = ",
      m_log_pooled$nobs,
      ", thick bar 95% CI, thin 99% CI"
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

panel$rank_gap_25 <- panel$rank_prior - panel$actual_rank_25

rank_gap_25_sd <- sd(panel$rank_gap_25, na.rm = TRUE)

m_rg25 <- lm_robust(
  Home_Crime_Handling_Change ~
    crime_gap_wins *
      as.factor(Treatment_Group) +
      rank_gap_25 * as.factor(Treatment_Group) +
      comp_party_known,
  alpha = ci_alpha,
  data = panel,
  se_type = "HC2"
)

coef_plot_rg25 <- extract_coef_plot(
  m_rg25,
  "crime_gap_wins",
  "m_rg25",
  crime_gap_wins_sd,
  rank_gap_25_sd
)

inc_update_coef_plot_rg25 <- ggplot(
  coef_plot_rg25,
  aes(
    y = treatment,
    x = estimate
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
  labs(
    y = "Treatment group",
    x = "Standardized coefficient (1 SD increase in predictor)",
    caption = paste0("N = ", m_rg25$nobs, ", thick bar 95% CI, thin 99% CI")
  ) +
  theme_minimal()

print(inc_update_coef_plot_rg25)

ggsave(
  "latex/images/inc_update_alt_rank_gap_plot.pdf",
  plot = inc_update_coef_plot_rg25,
  width = 7,
  height = 4.5
)

car::linearHypothesis(
  m_rg25,
  "as.factor(Treatment_Group)control2:rank_gap_25 = as.factor(Treatment_Group)T3:rank_gap_25"
)
