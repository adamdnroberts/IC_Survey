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

# Thin bar = 99% CI (conf.low/conf.high at ci_alpha), thick bar = 95% CI
ci_bars <- function() {
  list(
    geom_errorbar(
      aes(xmin = conf.low, xmax = conf.high),
      orientation = "y",
      width = 0,
      linewidth = 0.5,
      position = position_dodge(width = 0.5)
    ),
    geom_errorbar(
      aes(xmin = conf.low95, xmax = conf.high95),
      orientation = "y",
      width = 0,
      linewidth = 2,
      alpha = 0.4,
      position = position_dodge(width = 0.5)
    )
  )
}

ci_caption <- "thick bar 95% CI, thin 99% CI"

panel$Vote_home_post <- as.integer(
  !is.na(panel$coalition_post) &
    !is.na(panel$home_coalition) &
    panel$home_coalition == panel$coalition_post
)

panel$coalition_pre[is.na(panel$coalition_pre)] <- "Other"

panel$inc_vote <- as.numeric(panel$coalition_pre == panel$home_coalition)

panel <- filter(
  panel,
  muni_changed == 0 &
    Attention_Check == "somewhat_agree" &
    Treatment_Group != "control2"
)

crime_gap_capped_sd <- sd(panel$crime_gap_capped, na.rm = TRUE)
log_crime_gap_sd <- sd(panel$log_crime_gap, na.rm = TRUE)
rank_gap_sd <- sd(panel$rank_gap, na.rm = TRUE)

# Treatment interaction coefficients from one model, scaled for plotting: the
# crime-gap terms by cg_scale, the rank-gap terms by rank_gap_sd.
extract_coefs <- function(model, cg_term, cg_scale) {
  tidy(model, conf.int = TRUE) %>%
    filter(grepl("Treatment_Group", term) & grepl(":", term)) %>%
    mutate(
      group = if_else(
        startsWith(term, paste0(cg_term, ":")),
        "CG × Treatment",
        "RG × Treatment"
      ),
      treatment = sub(".*Treatment_Group\\)", "", term) %>% sub(":.*$", "", .),
      scale = if_else(group == "CG × Treatment", cg_scale, rank_gap_sd),
      across(c(estimate, conf.low, conf.high, std.error), ~ . * scale),
      conf.low95 = estimate - qt(0.975, df) * std.error,
      conf.high95 = estimate + qt(0.975, df) * std.error
    ) %>%
    dplyr::select(-scale)
}

coef_plot <- function(data, x_label, n, title = NULL, facet = TRUE) {
  ggplot(data, aes(y = treatment, x = estimate, color = treatment)) +
    geom_vline(xintercept = 0, linetype = "dashed", color = "grey50") +
    ci_bars() +
    geom_point(position = position_dodge(width = 0.5)) +
    scale_color_manual(values = arm_colors, guide = "none") +
    (if (facet) facet_wrap(~group, scales = "free_x")) +
    labs(
      y = "Treatment group",
      x = x_label,
      title = title,
      caption = paste0("N = ", n, ", ", ci_caption)
    ) +
    theme_minimal()
}

std_label <- "Standardized coefficient (1 SD increase in predictor)"

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

m_log <- lm_robust(
  Vote_home_post ~
    log_crime_gap *
    as.factor(Treatment_Group) +
    rank_gap * as.factor(Treatment_Group) +
    inc_vote,
  alpha = ci_alpha,
  data = panel,
  se_type = "HC2"
)

summary(m_log)

coef_plot_data_vote <- extract_coefs(
  m_vote,
  "crime_gap_capped",
  crime_gap_capped_sd
)
coef_plot_data_log <- extract_coefs(m_log, "log_crime_gap", log_crime_gap_sd)
# Doubling interpretation: scale CG coefficients by log(2) instead of SD
coef_plot_data_log_doubling <- extract_coefs(m_log, "log_crime_gap", log(2))

vote_coef_update <- coef_plot(coef_plot_data_vote, std_label, m_vote$nobs)
vote_coef_update_log <- coef_plot(coef_plot_data_log, std_label, m_log$nobs)
vote_coef_update_log_doubling <- coef_plot(
  coef_plot_data_log_doubling,
  "Effect of doubling perception gap (CG) / 1 SD increase (RG)",
  m_log$nobs
)

# Same coefficients as vote_coef_update_log, but one standalone plot per gap
# measure instead of a two-panel facet.
vote_coef_update_log_cg <- coef_plot(
  filter(coef_plot_data_log, group == "CG × Treatment"),
  "Standardized coefficient (1 SD increase in crime gap)",
  m_log$nobs,
  title = "CG × Treatment",
  facet = FALSE
)
vote_coef_update_log_rg <- coef_plot(
  filter(coef_plot_data_log, group == "RG × Treatment"),
  "Standardized coefficient (1 SD increase in rank gap)",
  m_log$nobs,
  title = "RG × Treatment",
  facet = FALSE
)

# File name -> plot and size (inches, width x height)
figures <- list(
  vote_coef_update = list(vote_coef_update, c(7, 4.5)),
  vote_coef_update_log = list(vote_coef_update_log, c(7, 4.5)),
  vote_coef_update_log_doubling = list(vote_coef_update_log_doubling, c(7, 4.5)),
  vote_coef_update_log_cg = list(vote_coef_update_log_cg, c(4.5, 4.5)),
  vote_coef_update_log_rg = list(vote_coef_update_log_rg, c(4.5, 4.5))
)

# Also write to the poster project so the poster picks up the updated figures
# directly. Set POSTER_FIG_DIR in .Renviron to enable; skipped silently on
# machines where it is unset.
poster_fig_dir <- Sys.getenv("POSTER_FIG_DIR")
fig_dirs <- c(
  "latex/images",
  if (nzchar(poster_fig_dir) && dir.exists(poster_fig_dir)) poster_fig_dir
)

for (name in names(figures)) {
  print(figures[[name]][[1]])
  for (dir in fig_dirs) {
    ggsave(
      file.path(dir, paste0(name, ".pdf")),
      plot = figures[[name]][[1]],
      width = figures[[name]][[2]][1],
      height = figures[[name]][[2]][2]
    )
  }
}

# GAM updating curves moved to code/exploratory/vote_update_gam.R

# 25% threshold rank-gap robustness moved to code/vote_update_rank_gap_25.R

# Party-knowledge subgroup moved to code/vote_update_knowledge_subgroup.R
