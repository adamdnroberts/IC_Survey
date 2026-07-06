library(estimatr)
library(dplyr)
library(ggplot2)

load("~/IC_Survey/data/survey_panel_dataset.Rdata")

if (!exists("ci_alpha")) {
  ci_alpha <- 0.01
}

# Colorblind-friendly (Okabe-Ito) palette, matching crime_rate_accuracy_update.R
arm_colors <- c(
  control2 = "#999999",
  T1 = "#56B4E9",
  T2 = "#009E73",
  T3 = "#E69F00",
  T4 = "#0072B2"
)

panel$Vote_home_post <- as.integer(
  !is.na(panel$coalition_post) &
    !is.na(panel$home_coalition) &
    panel$home_coalition == panel$coalition_post
)

panel_full <- panel

panel_with_failures <- filter(panel_full, muni_changed == 0)
panel <- filter(panel_with_failures, Attention_Check == "somewhat_agree")

panel$coalition_pre[is.na(panel$coalition_pre)] <- "Other"

panel$inc_vote <- as.numeric(panel$coalition_pre == panel$home_coalition)

crime_gap_wins_sd <- sd(panel$crime_gap_wins, na.rm = TRUE)
log_crime_gap_sd <- sd(panel$log_crime_gap, na.rm = TRUE)
rank_gap_sd <- sd(panel$rank_gap, na.rm = TRUE)

m_vote <- lm_robust(
  Vote_home_post ~
    crime_gap_wins *
    as.factor(Treatment_Group) +
    rank_gap * as.factor(Treatment_Group) +
    as.factor(coalition_pre) +
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
  aes(y = treatment, x = estimate, color = treatment)
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
  scale_color_manual(values = arm_colors, guide = "none") +
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
  "C:/Users/adamd/Dropbox/Apps/Overleaf/PolMeth 2026 Poster/figures/vote_coef_update.pdf",
  plot = vote_coef_update,
  width = 7,
  height = 4.5
)
m_log <- lm_robust(
  Vote_home_post ~
    log_crime_gap *
    as.factor(Treatment_Group) +
    rank_gap * as.factor(Treatment_Group) +
    as.factor(coalition_pre) +
    inc_vote,
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
  aes(y = treatment, x = estimate, color = treatment)
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
  scale_color_manual(values = arm_colors, guide = "none") +
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

# ── Test: comparison-treatment rank_gap interactions > control2's (m_vote) ─────
# One-sided tests that the rank_gap x Treatment_Group interaction coefficient is
# larger for each comparison treatment (T2, T3, T4) than for control2.
# Difference = b_Tx - b_control2; SE from the HC2 variance-covariance matrix.
rank_gap_term <- function(g) {
  paste0("as.factor(Treatment_Group)", g, ":rank_gap")
}

b_vote <- coef(m_vote)
V_vote <- vcov(m_vote)
base_term <- rank_gap_term("control2")

stopifnot(all(
  c(base_term, sapply(c("T2", "T3", "T4"), rank_gap_term)) %in% names(b_vote)
))

rank_gap_contrasts <- bind_rows(lapply(c("T2", "T3", "T4"), function(g) {
  tx <- rank_gap_term(g)
  diff <- b_vote[[tx]] - b_vote[[base_term]]
  se <- sqrt(
    V_vote[tx, tx] + V_vote[base_term, base_term] - 2 * V_vote[tx, base_term]
  )
  tstat <- diff / se
  data.frame(
    treatment = g,
    coef = b_vote[[tx]],
    control2_coef = b_vote[[base_term]],
    diff = diff,
    std.error = se,
    t = tstat,
    p_one_sided = pt(tstat, m_vote$df.residual, lower.tail = TRUE)
  )
}))

cat("\nH0: (Tx : rank_gap) - (control2 : rank_gap) <= 0   vs.  H1: > 0\n")
print(rank_gap_contrasts, row.names = FALSE, digits = 4)

library(car)

# ── Wald tests on the rank_gap x Treatment interactions (m_vote, HC2 vcov) ─────
# Pairwise equality of the rank_gap:Treatment interaction across arms, plus a
# joint test that the three comparison arms' interactions are all zero.
rg <- function(g) paste0("as.factor(Treatment_Group)", g, ":rank_gap")

rank_gap_tests <- list(
  "T2 = T1" = paste(rg("T2"), "=", rg("T1")),
  "T2 = T3" = paste(rg("T2"), "=", rg("T3")),
  "T2 = T4" = paste(rg("T2"), "=", rg("T4")),
  "T3 = T4" = paste(rg("T3"), "=", rg("T4")),
  "T2 = T3 = T4 = 0" = c(
    paste0("0 = ", rg("T4")),
    paste0("0 = ", rg("T3")),
    paste0("0 = ", rg("T2"))
  )
)

# Extract the test statistic (Chisq or F), its df, and p-value from one test.
lh_row <- function(hyp, label) {
  res <- linearHypothesis(m_log, hyp)
  stat_col <- intersect(c("Chisq", "F"), names(res))[1]
  p_col <- grep("^Pr", names(res), value = TRUE)[1]
  i <- nrow(res)
  data.frame(
    test = label,
    df = res[["Df"]][i],
    statistic = res[[stat_col]][i],
    stat_type = stat_col,
    p_value = res[[p_col]][i],
    row.names = NULL
  )
}

rank_gap_test_table <- do.call(
  rbind,
  Map(lh_row, rank_gap_tests, names(rank_gap_tests))
)

cat("\nWald tests: rank_gap x Treatment interaction contrasts (m_vote, HC2)\n")
print(rank_gap_test_table, row.names = FALSE, digits = 4)

#updating curve
panel$comparison_treat <- ifelse(
  panel$Treatment_Group %in% c("T2", "T3", "T4"),
  1,
  0
)

m_vote_pooled_comparisons <- lm_robust(
  Vote_home_post ~
    crime_gap_wins *
    as.factor(Treatment_Group) +
    rank_gap * comparison_treat +
    as.factor(coalition_pre),
  alpha = ci_alpha,
  data = panel,
  se_type = "HC2"
)
summary(m_vote)

# GAM updating curves moved to code/vote_update_gam.R

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
    crime_gap_wins *
    as.factor(Treatment_Group) +
    rank_gap_25 * as.factor(Treatment_Group) +
    as.factor(coalition_pre) +
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
        grepl("^crime_gap_wins:", term),
        "CG × Treatment",
        "RG × Treatment"
      ),
      treatment = sub(".*Treatment_Group\\)", "", term) %>% sub(":.*$", "", .),
      sd = if_else(group == "CG × Treatment", crime_gap_wins_sd, rg_sd),
      across(c(estimate, conf.low, conf.high, std.error), ~ . * sd),
      conf.low95 = estimate - qt(0.975, df) * std.error,
      conf.high95 = estimate + qt(0.975, df) * std.error,
      model = model_label
    ) %>%
    filter(treatment != "control2") %>%
    select(-sd)
}

coef_compare_25 <- bind_rows(
  build_interaction_coefs(m_vote, rank_gap_sd, "Original rank gap"),
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
      ", thick bar 95% CI, thin 99% CI"
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
