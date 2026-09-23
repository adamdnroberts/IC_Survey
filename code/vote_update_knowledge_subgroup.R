# Incumbent-vote updating among respondents who correctly named their home
# municipality's governing coalition. Split out from vote_update_analysis.R.
# Self-contained: builds the same panel, outcome and controls, fits the main
# log-crime-gap model on the full sample and on the correct-knowledge subset,
# and writes two figures.
#
# The subgroup matters because the same-coalition treatment can only work
# through partisan identity for respondents who know which coalition governs
# them. If the effect were an artifact of confusion, it should weaken here.
#
#   vote_coef_update_log_correct.pdf  subgroup on its own
#   vote_coef_compare_correct.pdf     full sample and subgroup overlaid
#
# Both frames are scaled by the full-sample SDs so the two sets of coefficients
# are directly comparable and line up with vote_coef_update_log.pdf.

library(estimatr)
library(dplyr)
library(ggplot2)

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

# Thin bar = 99% CI (conf.low/conf.high at ci_alpha), thick bar = 95% CI.
# by_sample dashes the thin bar by sample for the overlaid comparison plot; the
# thick bar stays solid, since a dashed line that wide reads as blocks.
ci_bars <- function(by_sample = FALSE) {
  thin_aes <- if (by_sample) {
    aes(xmin = conf.low, xmax = conf.high, linetype = sample)
  } else {
    aes(xmin = conf.low, xmax = conf.high)
  }
  list(
    geom_errorbar(
      thin_aes,
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

log_crime_gap_sd <- sd(panel$log_crime_gap, na.rm = TRUE)
rank_gap_sd <- sd(panel$rank_gap, na.rm = TRUE)

# Shared specification: identical to m_log in vote_update_analysis.R.
vote_formula <- Vote_home_post ~
  log_crime_gap * as.factor(Treatment_Group) +
    rank_gap * as.factor(Treatment_Group) +
    inc_vote

m_log <- lm_robust(
  vote_formula,
  alpha = ci_alpha,
  data = panel,
  se_type = "HC2"
)

panel_correct <- filter(panel, home_party_knowledge == "Correct")

m_log_correct <- lm_robust(
  vote_formula,
  alpha = ci_alpha,
  data = panel_correct,
  se_type = "HC2"
)

summary(m_log_correct)

cat(sprintf(
  "Knows home coalition: %d of %d respondents (%.1f%%)\n",
  nrow(panel_correct),
  nrow(panel),
  100 * nrow(panel_correct) / nrow(panel)
))

# Both series are scaled by the full-sample SDs, so the subgroup estimates sit
# on the same axis as the main figure rather than on their own subgroup scale.
extract_coefs <- function(model) {
  tidy(model, conf.int = TRUE) %>%
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
}

coef_plot_data_log <- extract_coefs(m_log)
coef_plot_data_log_correct <- extract_coefs(m_log_correct)

# ── FDR control within the subgroup's interaction family ─────────────────────
# Benjamini-Hochberg q-values on the observed p-values, following the family
# convention in "code/multiple comparisons/fdr_inc_other.R": T1 is excluded, so
# the family is the 3 CG x arm and 3 RG x arm terms for T2-T4. The all-8 family
# (T1 included) is printed alongside because the figure shows all eight, and the
# conclusion should not turn on which of the two a reader has in mind.
fdr_table <- coef_plot_data_log_correct %>%
  dplyr::select(group, treatment, estimate, p.value) %>%
  arrange(group, treatment) %>%
  mutate(q_BH_all8 = p.adjust(p.value, method = "BH"))

fdr_table$q_BH_T2toT4 <- NA_real_
keep <- fdr_table$treatment != "T1"
fdr_table$q_BH_T2toT4[keep] <- p.adjust(fdr_table$p.value[keep], method = "BH")

cat("\nBH q-values, correct-knowledge subgroup:\n")
print(as.data.frame(fdr_table), digits = 3)

# ── Subgroup on its own ───────────────────────────────────────────────────────
vote_coef_update_log_correct <- ggplot(
  coef_plot_data_log_correct,
  aes(y = treatment, x = estimate, color = treatment)
) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey50") +
  ci_bars() +
  geom_point(position = position_dodge(width = 0.5)) +
  scale_color_manual(values = arm_colors, guide = "none") +
  facet_wrap(~group, scales = "free_x") +
  labs(
    y = "Treatment group",
    x = "Standardized coefficient (1 SD increase in predictor)",
    title = "Knows home governing coalition",
    caption = paste0("N = ", m_log_correct$nobs, ", ", ci_caption)
  ) +
  theme_minimal()

print(vote_coef_update_log_correct)

ggsave(
  "latex/images/vote_coef_update_log_correct.pdf",
  plot = vote_coef_update_log_correct,
  width = 7,
  height = 4.5
)
cat("Wrote latex/images/vote_coef_update_log_correct.pdf\n")

# ── Full sample vs. correct-knowledge subgroup on one plot ────────────────────
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
  ci_bars(by_sample = TRUE) +
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
      ", ",
      ci_caption
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
cat("Wrote latex/images/vote_coef_compare_correct.pdf\n")
