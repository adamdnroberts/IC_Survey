# ── Arm contrasts in the importance-modulation of the bad-news slope ──────────
# ── Respondent-level index: MAX over the comparisons shown ────────────────────
#
# Identical to index_similarity_linear_models.R in sample, model and reported
# contrasts. The single difference is how the four per-comparison salience
# scores are collapsed to one number per respondent:
#
#   mean  "how salient was their comparison set as a whole"
#   max   "how salient was the single most salient comparison they saw"
#
# The max is the right summary if what moves a respondent is one compelling
# benchmark rather than the average quality of the set — a respondent shown one
# highly comparable municipality and three irrelevant ones has a low mean but a
# high max. The two indices differ most for respondents with a heterogeneous
# comparison set, so a result that holds under one and not the other is
# informative about which comparison the respondent actually attended to.
#
# Note the scales are NOT comparable across the two scripts: the max of four
# log-odds is mechanically higher and less dispersed than their mean, so the
# cross-partial coefficients (which are per unit of the index) cannot be read
# side by side as effect sizes. Compare signs, significance, and each arm's
# ordering — not the raw magnitudes.
#
# Standalone: run from the project root with
#   Rscript code/index_similarity/index_similarity_linear_models_max.R

# Loads `panel` with comp_importance_lp_1..4, Vote_home_post, and the
# create_panel_dataset.R columns (rank_gap, log_crime_gap, home_coalition,
# coalition_pre, muni_changed, Attention_Check).
source("code/index_similarity/build_importance_index.R")
# Sample construction, the LPM, and the contrast definitions. Shared with the
# mean variant so the aggregator below is the only difference between them.
source("code/index_similarity/lpm_arm_contrasts.R")

# Coverage for the printed contrast tables. Set unconditionally:
# create_panel_dataset.R defines ci_alpha but only saves `panel`, so the value
# never survives the process boundary and an inherited session value would
# silently change the intervals without changing the code.
ci_alpha <- 0.05

lm_panel <- build_lm_panel(panel, agg_fun = agg_max, agg_label = "max")
res_max <- run_arm_contrasts(lm_panel, ci_alpha = ci_alpha)
