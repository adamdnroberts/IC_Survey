# Incumbent-vote updating among respondents who rank crime as highly important.
# Split out from vote_update_analysis.R, where this subset sits commented out in
# the m_log call. Self-contained: builds the same panel, outcome and controls,
# then refits the log-crime-gap model on the restricted sample.
#
# Importance_Crime is the *position* of "Seguridad / Delincuencia" in the wave 1
# issue ranking (app_wave1.R), so 1 = most important. "Highly important" is
# therefore the top three slots, matching the commented filter in
# vote_update_analysis.R.
#
# The full-sample counterpart of this model is m_log in vote_update_analysis.R.

library(estimatr)
library(dplyr)
library(ggplot2)

load("data/derived/survey_panel_dataset.Rdata")

if (!exists("ci_alpha")) {
  ci_alpha <- 0.01
}

# Crime rank counted as "highly important". Edit to widen or narrow the subset.
importance_top_n <- 3

# Colorblind-friendly (Okabe-Ito) palette, matching vote_update_analysis.R
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

# ── Restrict to respondents who rank crime among their top issues ─────────────
panel_high_imp <- filter(
  panel,
  !is.na(Importance_Crime),
  as.integer(Importance_Crime) <= importance_top_n
)

cat(sprintf(
  "Crime ranked in top %d issues: %d of %d respondents (%.1f%%)\n",
  importance_top_n,
  nrow(panel_high_imp),
  nrow(panel),
  100 * nrow(panel_high_imp) / nrow(panel)
))

# SDs are computed on the estimation sample, not the full panel, so the
# standardized coefficients below are in SD units of this subset.
log_crime_gap_sd <- sd(panel_high_imp$log_crime_gap, na.rm = TRUE)
rank_gap_sd <- sd(panel_high_imp$rank_gap, na.rm = TRUE)

m_log_high_imp <- lm_robust(
  Vote_home_post ~
    log_crime_gap *
    as.factor(Treatment_Group) +
    rank_gap * as.factor(Treatment_Group) +
    as.factor(coalition_pre) +
    as.factor(actual_rank) +
    inc_vote,
  alpha = ci_alpha,
  data = panel_high_imp,
  se_type = "HC2"
)

summary(m_log_high_imp)

coef_plot_data <- tidy(m_log_high_imp, conf.int = TRUE) %>%
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

vote_coef_high_imp <- ggplot(
  coef_plot_data,
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
    caption = paste0(
      "Crime ranked in top ",
      importance_top_n,
      " issues. N = ",
      m_log_high_imp$nobs,
      ", bars 95% CI"
    )
  ) +
  theme_minimal()

print(vote_coef_high_imp)

ggsave(
  "latex/images/vote_coef_update_crime_important.pdf",
  plot = vote_coef_high_imp,
  width = 7,
  height = 4.5
)
cat("\nWrote latex/images/vote_coef_update_crime_important.pdf\n")

# ── Appendix table: treatment interactions only ───────────────────────────────
# Reports the eight gap x arm interaction terms in raw (unstandardized) units,
# so a CG row is the change in P(incumbent vote) per one-unit change in
# log_crime_gap and an RG row is per one-rank change. Controls (coalition_pre,
# actual_rank, inc_vote), the arm main effects and the gap main effects are all
# in the fitted model but suppressed from the table; the note says so.

int_terms <- tidy(m_log_high_imp, conf.int = TRUE) %>%
  filter(grepl("Treatment_Group", term) & grepl(":", term)) %>%
  mutate(
    gap = factor(
      if_else(
        grepl("^log_crime_gap:", term),
        "Crime-rate gap (log)",
        "Rank gap"
      ),
      levels = c("Crime-rate gap (log)", "Rank gap")
    ),
    arm = sub(".*Treatment_Group\\)", "", term) %>% sub(":.*$", "", .)
  ) %>%
  filter(arm != "control2") %>%
  arrange(gap, arm)

stars <- function(p) {
  dplyr::case_when(
    p < 0.001 ~ "^{***}",
    p < 0.01 ~ "^{**}",
    p < 0.05 ~ "^{*}",
    TRUE ~ ""
  )
}

# ci_alpha is 0.01 by default in this project, but the table reports 95% CIs, so
# these are recomputed from the SEs rather than taken from tidy()'s columns.
int_terms <- int_terms %>%
  mutate(
    ci_lo = estimate - qt(0.975, df) * std.error,
    ci_hi = estimate + qt(0.975, df) * std.error
  )

int_rows <- sprintf(
  "%s & $%.4f%s$ & $(%.4f)$ & $[%.4f,\\ %.4f]$ \\\\",
  int_terms$arm,
  int_terms$estimate,
  stars(int_terms$p.value),
  int_terms$std.error,
  int_terms$ci_lo,
  int_terms$ci_hi
)

gap_breaks <- which(!duplicated(int_terms$gap))
body <- character(0)
for (i in seq_along(int_rows)) {
  if (i %in% gap_breaks) {
    if (i > 1) {
      body <- c(body, "\\midrule")
    }
    body <- c(
      body,
      sprintf(
        "\\multicolumn{4}{l}{\\textit{%s $\\times$ arm}} \\\\",
        as.character(int_terms$gap[i])
      )
    )
  }
  body <- c(body, int_rows[i])
}

table_tex <- paste0(
  "\\begin{table}[htbp]\n",
  "\\centering\n",
  "\\small\n",
  "\\caption{Treatment interactions among respondents who rank crime in their ",
  "top ",
  importance_top_n,
  " issues. Entries are linear-probability ",
  "coefficients for post-treatment incumbent vote intention, with HC2 robust ",
  "standard errors in parentheses and 95\\% confidence intervals in brackets. ",
  "The model also includes each arm's main effect, both gap main effects, ",
  "pre-treatment coalition, actual robbery rank, and prior incumbent vote; ",
  "these are omitted here. Control is the reference arm. ",
  "$N = ",
  format(m_log_high_imp$nobs, big.mark = "{,}"),
  "$, ",
  "$R^2 = ",
  sprintf("%.3f", m_log_high_imp$r.squared),
  "$. ",
  "$^{*}p<0.05$, $^{**}p<0.01$, $^{***}p<0.001$.}\n",
  "\\begin{tabular}{lrrr}\n",
  "\\toprule\n",
  "\\textbf{Arm} & \\textbf{Estimate} & \\textbf{SE} & \\textbf{95\\% CI} \\\\\n",
  "\\midrule\n",
  paste(body, collapse = "\n"),
  "\n",
  "\\bottomrule\n",
  "\\end{tabular}\n",
  "\\label{tab:vote_crime_important}\n",
  "\\end{table}\n"
)

writeLines(table_tex, "latex/tables/vote_crime_important.tex")
cat("Wrote latex/tables/vote_crime_important.tex\n")
