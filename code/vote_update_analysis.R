library(estimatr)
library(dplyr)
library(ggplot2)
#TO DO: add robustness check using asinh_crime_gap instead of log_crime_gap
load("data/derived/survey_panel_dataset.Rdata")

if (!exists("ci_alpha")) {
  ci_alpha <- 0.01
}

# Colorblind-friendly (Okabe-Ito) palette, matching crime_rate_accuracy_update.R
arm_colors <- c(
  control2 = "#999999",
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

panel$Vote_home_pre <- as.integer(
  !is.na(panel$coalition_post) &
    !is.na(panel$home_coalition) &
    panel$home_coalition == panel$coalition_pre
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
log_crime_gap_sd <- sd(panel$log_crime_gap, na.rm = TRUE)
rank_gap_sd <- sd(panel$rank_gap, na.rm = TRUE)

m_vote <- lm_robust(
  Vote_home_post ~
    crime_gap_capped *
    as.factor(Treatment_Group) +
    rank_gap * as.factor(Treatment_Group) +
    as.factor(coalition_pre) +
    as.factor(actual_rank) +
    inc_vote,
  alpha = ci_alpha,
  data = panel,
  se_type = "HC2"
)

summary(m_vote)

coef_plot_data_vote <- tidy(m_vote, conf.int = TRUE) %>%
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
  #filter(treatment != "control2") %>%
  dplyr::select(-sd)

vote_coef_update <- ggplot(
  subset(coef_plot_data_vote),
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
    #title = "Incumbent vote post: interaction coefficients",
    caption = paste0("N = ", m_vote$nobs, ", bars 95% CI")
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
    rank_gap *
      as.factor(Treatment_Group) +
    # as.numeric(MORENA_Crime_Rating_Pre) +
    # as.numeric(MC_Crime_Rating_Pre) +
    # as.numeric(Coalition_PAN_PRI_PRD_Crime_Rating_Pre) +
    #as.factor(coalition_pre) +
    #as.factor(home_party_knowledge) +
    inc_vote,
  alpha = ci_alpha,
  #subset(
  #subset(
  data = panel,
  #home_party_knowledge == "Correct"
  #Importance_Crime == "1" | Importance_Crime == "2" | Importance_Crime == "3"
  #),
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
  dplyr::select(-sd)

vote_coef_update_log <- ggplot(
  subset(coef_plot_data_log),
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
    #title = "Incumbent vote post: interaction coefficients",
    caption = paste0("N = ", m_log$nobs, ", bars 95% CI")
  ) +
  theme_minimal()

print(vote_coef_update_log)

# Doubling interpretation: scale CG coefficients by log(2) instead of SD
coef_plot_data_log_doubling <- tidy(m_log, conf.int = TRUE) %>%
  filter(grepl("Treatment_Group", term) & grepl(":", term)) %>%
  mutate(
    group = case_when(
      grepl("^log_crime_gap:", term) ~ "CG × Treatment",
      TRUE ~ "RG × Treatment"
    ),
    treatment = sub(".*Treatment_Group\\)", "", term) %>% sub(":.*$", "", .),
    scale_factor = if_else(group == "CG × Treatment", log(2), rank_gap_sd),
    across(c(estimate, conf.low, conf.high, std.error), ~ . * scale_factor),
    conf.low95 = estimate - qt(0.975, df) * std.error,
    conf.high95 = estimate + qt(0.975, df) * std.error
  ) %>%
  filter(treatment != "control2") %>%
  dplyr::select(-scale_factor)

vote_coef_update_log_doubling <- ggplot(
  coef_plot_data_log_doubling,
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
    x = "Effect of doubling perception gap (CG) / 1 SD increase (RG)",
    caption = paste0("N = ", m_log$nobs, ", bars 95% CI")
  ) +
  theme_minimal()

print(vote_coef_update_log_doubling)

ggsave(
  "latex/images/vote_coef_update_log_doubling.pdf",
  plot = vote_coef_update_log_doubling,
  width = 7,
  height = 4.5
)

# Same coefficients as vote_coef_update_log, but one standalone plot per gap
# measure instead of a two-panel facet.
build_log_gap_plot <- function(group_label, x_label) {
  ggplot(
    subset(coef_plot_data_log, group == group_label),
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
      caption = paste0("N = ", m_log$nobs, ", bars 95% CI")
    ) +
    theme_minimal()
}

vote_coef_update_log_cg <- build_log_gap_plot(
  "CG × Treatment",
  "Standardized coefficient (1 SD increase in crime gap)"
)

vote_coef_update_log_rg <- build_log_gap_plot(
  "RG × Treatment",
  "Standardized coefficient (1 SD increase in rank gap)"
)

print(vote_coef_update_log_cg)
print(vote_coef_update_log_rg)

# Also write to the poster project so the poster picks up the updated figure
# directly (matches the vote_coef_update.pdf save above). Set POSTER_FIG_DIR in
# .Renviron to enable; skipped silently on machines where it is unset.
poster_fig_dir <- Sys.getenv("POSTER_FIG_DIR")
fig_dirs <- c(
  "latex/images",
  if (nzchar(poster_fig_dir) && dir.exists(poster_fig_dir)) poster_fig_dir
)

for (dir in fig_dirs) {
  ggsave(
    file.path(dir, "vote_coef_update_log.pdf"),
    plot = vote_coef_update_log,
    width = 7,
    height = 4.5
  )
  ggsave(
    file.path(dir, "vote_coef_update_log_cg.pdf"),
    plot = vote_coef_update_log_cg,
    width = 4.5,
    height = 4.5
  )
  ggsave(
    file.path(dir, "vote_coef_update_log_rg.pdf"),
    plot = vote_coef_update_log_rg,
    width = 4.5,
    height = 4.5
  )
}

#updating curve
panel$comparison_treat <- ifelse(
  panel$Treatment_Group %in% c("T2", "T3", "T4"),
  1,
  0
)

m_vote_pooled_comparisons <- lm_robust(
  Vote_home_post ~
    crime_gap_capped *
    as.factor(Treatment_Group) +
    rank_gap * comparison_treat +
    as.factor(coalition_pre),
  alpha = ci_alpha,
  data = panel,
  se_type = "HC2"
)
summary(m_vote)

# GAM updating curves moved to code/exploratory/vote_update_gam.R

# ── 25% threshold robustness: refit m_vote with rank_gap_25 and compare ────────
# Alternative rank-gap measure (matches belief_update_analysis.R): a comparison
# municipality counts as "fewer" robberies only if its rate is at least 25%
# below the home rate. rank_gap_25 = actual_rank_25 - rank_prior.
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

m_vote_25 <- lm_robust(
  Vote_home_post ~
    log_crime_gap *
    as.factor(Treatment_Group) +
    rank_gap_25 * as.factor(Treatment_Group) +
    #as.factor(coalition_pre) +
    inc_vote,
  alpha = ci_alpha,
  data = panel,
  se_type = "HC2"
)

summary(m_vote_25)

# Standardized CG×Treatment and RG×Treatment interaction coefficients for one
# model, matching the coef_plot_data_vote logic above (thick bar = 95% CI, thin =
# 99% CI). rg_sd is the SD of the model's rank-gap predictor.
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
      sd = if_else(group == "CG × Treatment", crime_gap_capped_sd, rg_sd),
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
      m_vote$nobs,
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

# ── Subgroup: respondents who correctly named their home governing coalition ──
# Same specification and plot as m_log / vote_coef_update_log, refit on the
# subset with home_party_knowledge == "Correct". Coefficients are scaled by the
# full-sample SDs (log_crime_gap_sd, rank_gap_sd) so the estimates are directly
# comparable to the main figure.
panel_correct <- filter(panel, home_party_knowledge == "Correct")

m_log_correct <- lm_robust(
  Vote_home_post ~
    log_crime_gap *
    as.factor(Treatment_Group) +
    rank_gap * as.factor(Treatment_Group) +
    inc_vote,
  alpha = ci_alpha,
  data = panel_correct,
  se_type = "HC2"
)

summary(m_log_correct)

coef_plot_data_log_correct <- tidy(m_log_correct, conf.int = TRUE) %>%
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
  dplyr::select(-sd)

vote_coef_update_log_correct <- ggplot(
  coef_plot_data_log_correct,
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
    title = "Knows home governing coalition",
    caption = paste0("N = ", m_log_correct$nobs, ", bars 95% CI")
  ) +
  theme_minimal()

print(vote_coef_update_log_correct)

ggsave(
  "latex/images/vote_coef_update_log_correct.pdf",
  plot = vote_coef_update_log_correct,
  width = 7,
  height = 4.5
)

# ── Full sample vs. correct-knowledge subgroup on one plot ────────────────────
# Overlays the m_log and m_log_correct interaction coefficients. Both frames are
# already scaled by the full-sample SDs, so the two sets are comparable.
coef_compare_correct <- bind_rows(
  mutate(coef_plot_data_log, sample = "All respondents"),
  mutate(coef_plot_data_log_correct, sample = "Knows home coalition")
) %>%
  mutate(
    sample = factor(
      sample,
      levels = c("All respondents", "Knows home coalition")
    )
  )

# Treatment arms keep the arm_colors scheme used by vote_coef_update_log; the
# two samples are separated by shape and line type instead of by color.
vote_coef_compare_correct <- ggplot(
  coef_compare_correct,
  aes(y = treatment, x = estimate, color = treatment, shape = sample)
) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey50") +
  geom_errorbar(
    aes(xmin = conf.low95, xmax = conf.high95, linetype = sample),
    orientation = "y",
    width = 0,
    linewidth = 0.5,
    position = position_dodge(width = 0.5)
  ) +
  geom_point(size = 2, position = position_dodge(width = 0.5)) +
  scale_color_manual(values = arm_colors, guide = "none") +
  scale_shape_manual(
    values = c("All respondents" = 16, "Knows home coalition" = 1),
    name = "Sample"
  ) +
  scale_linetype_manual(
    values = c("All respondents" = "solid", "Knows home coalition" = "22"),
    name = "Sample"
  ) +
  guides(
    shape = guide_legend(override.aes = list(color = "black")),
    linetype = guide_legend(override.aes = list(color = "black"))
  ) +
  facet_wrap(~group, scales = "free_x") +
  labs(
    y = "Treatment group",
    x = "Standardized coefficient (1 SD increase in predictor)",
    caption = paste0(
      "N = ",
      m_log$nobs,
      " (all) / ",
      m_log_correct$nobs,
      " (knows coalition)",
      ", bars 95% CI"
    )
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")

print(vote_coef_compare_correct)

ggsave(
  "latex/images/vote_coef_compare_correct.pdf",
  plot = vote_coef_compare_correct,
  width = 7,
  height = 4.5
)
