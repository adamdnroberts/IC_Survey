# vote_update_weighted.R
#
# Robustness check for vote_coef_update_log.pdf: does the interaction pattern
# survive reweighting the sample to the population it is meant to speak for?
#
# The experiment is randomized, so the unweighted estimates in
# code/vote_update_analysis.R are already internally valid. Weighting does not
# correct bias -- it changes the estimand from a sample-average to a
# population-average effect, i.e. it is about generalizability. Expect wider
# intervals: these are interactions with a continuous moderator, and weighting
# always costs effective sample size.
#
# Weights are raked (iterative proportional fitting) to four marginal
# distributions -- sex, age bracket, SEL and state -- using the same population
# benchmarks code/census_comparison.R reports against
# (code/census_benchmarks.R). Raking on margins rather than post-stratifying on
# the full 2 x 6 x 6 x 28 cell cross leaves nothing empty at this N.
#
# Inputs:  data/derived/survey_panel_dataset.Rdata
#          data/raw/INEGI_censo_sexo_estado.xlsx (via census_benchmarks.R)
# Outputs: latex/images/vote_coef_weighted.pdf
#          data/derived/panel_weights_wave2.rds (PID -> weight)

library(estimatr)
library(dplyr)
library(ggplot2)
library(broom)

load("data/derived/survey_panel_dataset.Rdata")
source("code/census_benchmarks.R")

if (!exists("ci_alpha")) {
  ci_alpha <- 0.01
}

# Raked weights are trimmed to this range (after normalizing to mean 1) so a
# handful of respondents in thin cells cannot dominate the fit. Trimming is
# applied inside the IPF loop, so the final margins are approximate, not exact;
# the printed margin check below shows how far off they end up.
weight_trim <- c(0.25, 4)
rake_max_iter <- 100
rake_tol <- 1e-6

# Colorblind-friendly (Okabe-Ito) palette, matching vote_update_analysis.R
arm_colors <- c(
  control2 = "#999999",
  T1 = "#56B4E9",
  T2 = "#009E73",
  T3 = "#D55E00",
  T4 = "#0072B2"
)

# -- Sample and outcome: identical to m_log in vote_update_analysis.R ----------
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

# -- Raking variables ---------------------------------------------------------
# Panel columns are suffixed by wave; use the wave 2 profile, as
# census_comparison.R does. SEL code 7 folds into 6 to match the wave 2
# collection-time recode and POP_SEL_W2.
panel <- panel %>%
  mutate(
    rake_sex = as.character(NQ_Sex_w2),
    rake_age = as.character(age_bracket(NQ_Age_w2)),
    rake_sel = {
      # NQ_SEL is character and can be blank; blanks become NA and are dropped
      # by the completeness check below.
      s <- suppressWarnings(as.integer(NQ_SEL_w2))
      s[!is.na(s) & s == 7L] <- 6L
      as.character(s)
    },
    rake_region = as.character(NQ_Region_w2)
  )

rake_vars <- c("rake_sex", "rake_age", "rake_sel", "rake_region")

# Targets as shares summing to 1, keyed by the same codes the panel carries.
as_share <- function(x) x / sum(x)
rake_targets <- list(
  rake_sex = as_share(census_sex),
  rake_age = as_share(POP_AGE),
  rake_sel = as_share(POP_SEL_W2),
  rake_region = as_share(CENSUS_REGION_SHARE)
)

# A respondent missing any raking variable, or sitting in a cell with no
# population target, cannot be weighted; drop them and say so rather than
# silently carrying a weight of 1.
complete_rake <- Reduce(
  function(a, b) a & b,
  lapply(rake_vars, function(v) {
    !is.na(panel[[v]]) & panel[[v]] %in% names(rake_targets[[v]])
  })
)
cat(sprintf(
  "Raking sample: %d of %d respondents (%d dropped for missing or off-frame sex, age, SEL or region)\n",
  sum(complete_rake),
  nrow(panel),
  sum(!complete_rake)
))
panel <- panel[complete_rake, ]

# -- Iterative proportional fitting -------------------------------------------
# One pass per margin per iteration: scale each cell's weights so the weighted
# share of that cell matches its target, then renormalize and trim. Cells the
# sample never realizes are simply absent -- their target mass is redistributed
# across the cells that do appear, which is the usual raking behaviour.
rake_weights <- function(data, vars, targets, trim, max_iter, tol) {
  w <- rep(1, nrow(data))
  for (iter in seq_len(max_iter)) {
    w_prev <- w
    for (v in vars) {
      cells <- data[[v]]
      observed <- tapply(w, cells, sum)
      target <- targets[[v]][names(observed)]
      # Renormalize the target over realized cells only.
      target <- target / sum(target)
      scale_factor <- (target * sum(w)) / observed
      w <- w * as.numeric(scale_factor[cells])
    }
    w <- w / mean(w)
    w <- pmin(pmax(w, trim[1]), trim[2])
    w <- w / mean(w)
    if (max(abs(w - w_prev)) < tol) {
      cat(sprintf("Raking converged in %d iterations\n", iter))
      break
    }
    if (iter == max_iter) {
      cat(sprintf(
        "Raking hit the %d-iteration cap; max weight change %.4g\n",
        max_iter,
        max(abs(w - w_prev))
      ))
    }
  }
  w
}

panel$rake_weight <- rake_weights(
  panel,
  rake_vars,
  rake_targets,
  weight_trim,
  rake_max_iter,
  rake_tol
)

# Kish's effective sample size: the unweighted N that would give the same
# variance. The gap between this and nrow(panel) is the cost of weighting.
n_eff <- sum(panel$rake_weight)^2 / sum(panel$rake_weight^2)
cat(sprintf(
  "Weights: mean %.3f, range [%.2f, %.2f], Kish n_eff %.1f of %d (%.0f%%)\n",
  mean(panel$rake_weight),
  min(panel$rake_weight),
  max(panel$rake_weight),
  n_eff,
  nrow(panel),
  100 * n_eff / nrow(panel)
))

# Margin check: how close the trimmed weights actually land to the targets.
for (v in rake_vars) {
  achieved <- tapply(panel$rake_weight, panel[[v]], sum) /
    sum(panel$rake_weight)
  target <- rake_targets[[v]][names(achieved)]
  target <- target / sum(target)
  cat(sprintf(
    "  %-12s max deviation from target: %.1f pp\n",
    v,
    100 * max(abs(achieved - target))
  ))
}

saveRDS(
  data.frame(
    Netquest_PID = panel$Netquest_PID,
    rake_weight = panel$rake_weight,
    stringsAsFactors = FALSE
  ),
  "data/derived/panel_weights_wave2.rds"
)

# -- Unweighted vs weighted fit -----------------------------------------------
# m_log is refit here rather than reused from vote_update_analysis.R because the
# unweightable respondents dropped above are also out of the weighted fit; both
# series must run on the same rows for the comparison to mean anything. Its N
# can therefore be slightly below the N on vote_coef_update_log.pdf.
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

m_log_weighted <- lm_robust(
  vote_formula,
  alpha = ci_alpha,
  data = panel,
  weights = rake_weight,
  se_type = "HC2"
)

summary(m_log_weighted)

# Both series are scaled by the same unweighted SDs so the two sets of
# coefficients sit on one comparable axis.
log_crime_gap_sd <- sd(panel$log_crime_gap, na.rm = TRUE)
rank_gap_sd <- sd(panel$rank_gap, na.rm = TRUE)

# Same extraction as coef_plot_data_log in vote_update_analysis.R.
extract_coefs <- function(model, model_label) {
  tidy(model, conf.int = TRUE) %>%
    filter(grepl("Treatment_Group", term) & grepl(":", term)) %>%
    mutate(
      model = model_label,
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

coef_compare_weighted <- bind_rows(
  extract_coefs(m_log, "Unweighted"),
  extract_coefs(m_log_weighted, "Census-weighted")
) %>%
  mutate(model = factor(model, levels = c("Unweighted", "Census-weighted")))

# Treatment arms keep the arm_colors scheme used by vote_coef_update_log; the
# two estimators are separated by shape and line type, as in
# vote_coef_compare_correct.pdf.
vote_coef_weighted <- ggplot(
  coef_compare_weighted,
  aes(y = treatment, x = estimate, color = treatment, shape = model)
) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey50") +
  geom_errorbar(
    aes(xmin = conf.low95, xmax = conf.high95, linetype = model),
    orientation = "y",
    width = 0,
    linewidth = 0.5,
    position = position_dodge(width = 0.5)
  ) +
  geom_point(size = 2, position = position_dodge(width = 0.5)) +
  scale_color_manual(values = arm_colors, guide = "none") +
  scale_shape_manual(
    values = c("Unweighted" = 16, "Census-weighted" = 1),
    name = "Estimator"
  ) +
  scale_linetype_manual(
    values = c("Unweighted" = "solid", "Census-weighted" = "22"),
    name = "Estimator"
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
      ", Kish effective N = ",
      sprintf("%.0f", n_eff),
      " when weighted, bars 95% CI"
    )
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")

print(vote_coef_weighted)

ggsave(
  "latex/images/vote_coef_weighted.pdf",
  plot = vote_coef_weighted,
  width = 7,
  height = 4.5
)
