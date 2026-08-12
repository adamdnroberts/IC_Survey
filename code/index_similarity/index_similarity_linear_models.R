# ── Arm contrasts in the importance-modulation of the bad-news slope ──────────
# ── Respondent-level index: MEAN over the comparisons shown ───────────────────
#
# Linear probability model asking whether one arm differs from another in how
# comparison importance bends the crime_gap slope. Two contrasts are reported:
#     T4 vs control  — does the same-coalition comparison differ from placebo?
#     T4 vs T2       — does the same-coalition comparison differ from the
#                      non-partisan one, i.e. is partisanship doing the work?
#
# The reported quantity is the cross-partial
#     d^2 P(inc vote) / d log_crime_gap d comparison_importance_lp
# in each arm, and the difference between them (the triple interaction).
#
# The index averages the benchmark-model salience of the (up to four)
# comparisons a respondent was shown: "how salient was their comparison set as
# a whole". index_similarity_linear_models_max.R runs the identical analysis
# with the max instead — see that script's header for when the two differ.
#
# Standalone: run from the project root with
#   Rscript code/index_similarity/index_similarity_linear_models.R

# Loads `panel` with comp_importance_lp_1..4, Vote_home_post, and the
# create_panel_dataset.R columns (rank_gap, log_crime_gap, home_coalition,
# coalition_pre, muni_changed, Attention_Check).
source("code/index_similarity/build_importance_index.R")
# Sample construction, the LPM, and the contrast definitions. Shared with the
# max variant so the aggregator below is the only difference between them.
source("code/index_similarity/lpm_arm_contrasts.R")

# Coverage for the printed contrast tables. Set unconditionally:
# create_panel_dataset.R defines ci_alpha but only saves `panel`, so the value
# never survives the process boundary and an inherited session value would
# silently change the intervals without changing the code.
ci_alpha <- 0.05

lm_panel <- build_lm_panel(panel, agg_fun = agg_mean, agg_label = "mean")
res_mean <- run_arm_contrasts(lm_panel, ci_alpha = ci_alpha)
