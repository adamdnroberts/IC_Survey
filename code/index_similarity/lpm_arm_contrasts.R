# ── Shared LPM arm-contrast machinery for the index_similarity analyses ───────
# Defines functions only — sourcing this runs no analysis and touches no data.
# Callers source build_importance_index.R first (for `panel` and the
# per-comparison comp_importance_lp_1..4 columns), then drive it with the
# respondent-level aggregator they want:
#
#   index_similarity_linear_models.R      mean over the comparisons shown
#   index_similarity_linear_models_max.R  max over the comparisons shown
#
# The aggregator is the ONLY thing that differs between those two scripts. Keep
# it that way: if the model, the sample filters or the contrast definitions ever
# need to change, change them here once rather than in each driver.
library(dplyr)
library(estimatr)

# ── Respondent-level aggregators ──────────────────────────────────────────────
# Both take the matrix of per-comparison scores and return one number per row,
# with all-NA rows mapped to NA rather than to the degenerate value the
# underlying R function returns (NaN for mean, -Inf for max).

# Mean salience of the comparisons a respondent was shown.
agg_mean <- function(m) {
  out <- rowMeans(m, na.rm = TRUE)
  out[is.nan(out)] <- NA
  out
}

# Salience of the single most salient comparison a respondent was shown.
agg_max <- function(m) {
  out <- suppressWarnings(apply(m, 1, max, na.rm = TRUE))
  # max(na.rm = TRUE) on an all-NA row returns -Inf with a warning, which would
  # otherwise sail into the regression as an extreme value rather than a
  # missing one. is.finite also catches the +Inf that a degenerate score would
  # produce.
  out[!is.finite(out)] <- NA
  out
}

# ── Analysis sample ───────────────────────────────────────────────────────────
# Drop muni movers + failed attention check, set comparison_importance_lp with
# the supplied aggregator, then require complete cases on everything the model
# uses. All filters are arm-independent, so they are applied once here and each
# contrast subsets to its own two arms.
# NOT the same sample as gam_data in index_comparisons.R: that one pools T2-T4,
# aggregates by mean only, and does not require non-NA log_crime_gap or
# inc_vote. Any N quoted from here describes this sample only.
build_lm_panel <- function(panel, agg_fun, agg_label) {
  lp_cols <- paste0("comp_importance_lp_", 1:4)
  stopifnot(all(lp_cols %in% names(panel)))

  out <- panel %>%
    filter(muni_changed == 0, Attention_Check == "somewhat_agree")

  # Overwrite whatever build_importance_index.R left in this column, so the
  # driver's choice of aggregator is what the analysis actually uses regardless
  # of that script's default.
  out$comparison_importance_lp <- agg_fun(as.matrix(out[lp_cols]))

  out$coalition_pre[is.na(out$coalition_pre)] <- "Other"
  out$inc_vote <- as.numeric(out$coalition_pre == out$home_coalition)

  out <- out %>%
    filter(
      !is.na(rank_gap),
      !is.na(comparison_importance_lp),
      !is.na(log_crime_gap)
    )

  # inc_vote is NA wherever home_coalition is NA (home municipality absent from
  # magar2024, or an l01 string matching none of the three coalition patterns).
  # lm_robust would drop these rows via na.action without saying so, leaving the
  # reported N describing a larger sample than the one actually fitted. Drop
  # them here and report the count.
  n_pre_incvote <- nrow(out)
  out <- out %>% filter(!is.na(inc_vote))

  cat(sprintf("\n=== respondent-level index: %s ===\n", agg_label))
  cat(sprintf(
    "dropped %d of %d rows for NA inc_vote (unmatched home_coalition)\n",
    n_pre_incvote - nrow(out),
    n_pre_incvote
  ))
  cat("comparison_importance_lp:\n")
  print(summary(out$comparison_importance_lp))
  cat("\nrespondents by arm (complete cases, before the two-arm subsets):\n")
  print(table(out$Treatment_Group))

  # Tag AFTER the filters: dplyr does not promise to carry custom attributes
  # through filter(), and a dropped label would print as an empty index name
  # rather than failing, leaving two runs indistinguishable in the output.
  attr(out, "agg_label") <- agg_label
  out
}

# ── Contrast machinery ────────────────────────────────────────────────────────
# Locate a coefficient by the tokens in its (order-independent) name. Errors if
# the tokens do not identify exactly one coefficient, which is also what catches
# a term being dropped from or renamed in the formula.
find_coef <- function(cn, tokens, exclude = character(0)) {
  hit <- vapply(
    cn,
    function(n) {
      all(vapply(tokens, grepl, logical(1), x = n, fixed = TRUE)) &&
        !any(vapply(exclude, grepl, logical(1), x = n, fixed = TRUE))
    },
    logical(1)
  )
  out <- cn[hit]
  if (length(out) != 1L) {
    stop(sprintf(
      "expected 1 coef for tokens {%s}, found %d: %s",
      paste(tokens, collapse = ", "),
      length(out),
      paste(out, collapse = " | ")
    ))
  }
  out
}

# Linear contrast L on b, given per-coefficient weights. Weights (rather than a
# list of names) are what let the difference row put 0 on the baseline term.
contrast <- function(b, V, weights, ci_alpha) {
  L <- setNames(numeric(length(b)), names(b))
  L[names(weights)] <- weights
  est <- as.numeric(L %*% b)
  se <- sqrt(as.numeric(t(L) %*% V %*% L))
  z <- est / se
  crit <- qnorm(1 - ci_alpha / 2)
  data.frame(
    estimate = est,
    std.error = se,
    statistic = z,
    p.value = 2 * pnorm(-abs(z)),
    conf.low = est - crit * se,
    conf.high = est + crit * se
  )
}

# Fit the LPM on {ref_arm, focal_arm} and return the cross-partial in each arm
# plus their difference. ref_arm is the factor reference level, so every
# arm_group coefficient is a contrast against it. Set explicitly:
# Treatment_Group is a character column, and leaving lm_robust to factor it puts
# the reference level at the mercy of locale collation.
#
# Each contrast is fit on ITS OWN two arms only, rather than one model over all
# arms: nothing is assumed common across arms a given comparison does not
# involve. Different contrasts therefore have different samples and different
# Ns, and their coefficients are not entries in a single table.
#
# The cross-partial is
#   beta[importance:crime]                                  in ref_arm
#   beta[importance:crime] + beta[focal:importance:crime]    in focal_arm
# so the difference IS the triple coefficient. The formula has no
# rank_gap:log_crime_gap term, so the cross-partial does not depend on rank_gap
# or inc_vote — one number per arm.
arm_contrast <- function(lm_panel, ref_arm, focal_arm, ci_alpha = 0.05) {
  # Which aggregator produced this panel must reach the printed header. A NULL
  # here would make sprintf() return character(0) and cat() print nothing,
  # leaving the mean and max runs indistinguishable in a saved log.
  agg_label <- attr(lm_panel, "agg_label")
  stopifnot(is.character(agg_label), length(agg_label) == 1L)

  levs <- c(ref_arm, focal_arm)
  dat <- lm_panel %>%
    filter(Treatment_Group %in% levs) %>%
    mutate(arm_group = factor(Treatment_Group, levels = levs))

  fit <- lm_robust(
    Vote_home_post ~
      arm_group * rank_gap * comparison_importance_lp +
      arm_group * log_crime_gap * comparison_importance_lp +
      inc_vote,
    alpha = ci_alpha,
    data = dat,
    se_type = "HC2"
  )
  # lm_robust drops NA rows silently; make sure the sample we describe is the
  # sample that was fitted (see the inc_vote NA filter above).
  stopifnot(fit$nobs == nrow(dat))

  b <- coef(fit)
  V <- vcov(fit)
  cn <- names(b)

  base_nm <- find_coef(
    cn,
    c("log_crime_gap", "comparison_importance_lp"),
    exclude = c("arm_group", "rank_gap")
  )
  focal_nm <- find_coef(
    cn,
    c(
      paste0("arm_group", focal_arm),
      "log_crime_gap",
      "comparison_importance_lp"
    ),
    exclude = "rank_gap"
  )

  tab <- bind_rows(
    cbind(
      quantity = sprintf("%s (baseline)", ref_arm),
      contrast(b, V, setNames(1, base_nm), ci_alpha)
    ),
    cbind(
      quantity = sprintf("%s (net)", focal_arm),
      contrast(b, V, setNames(c(1, 1), c(base_nm, focal_nm)), ci_alpha)
    ),
    cbind(
      quantity = sprintf("%s - %s (difference)", focal_arm, ref_arm),
      contrast(b, V, setNames(1, focal_nm), ci_alpha)
    )
  )

  list(
    fit = fit,
    data = dat,
    table = tab,
    focal_nm = focal_nm,
    ref_arm = ref_arm,
    focal_arm = focal_arm,
    agg_label = agg_label
  )
}

# Takes only the result, so the labels printed are necessarily the arms fitted
# and the aggregator actually used.
report_contrast <- function(res) {
  ref_arm <- res$ref_arm
  focal_arm <- res$focal_arm
  counts <- table(res$data$arm_group)
  cat(sprintf(
    "\n\n=== %s vs %s [index: %s] (N = %d; %s) ===\n",
    focal_arm,
    ref_arm,
    res$agg_label,
    res$fit$nobs,
    paste(
      sprintf("%s = %d", names(counts), as.integer(counts)),
      collapse = ", "
    )
  ))
  print(summary(res$fit))
  cat(
    "\nImportance-modulation of the crime_gap slope",
    " [d^2 P(inc vote) / d log_crime_gap d importance]:\n",
    sep = ""
  )
  print(res$table, row.names = FALSE)
  cat(sprintf(
    "\nThe '%s - %s' row IS the %s coefficient; it is the test of whether the\n",
    focal_arm,
    ref_arm,
    res$focal_nm
  ))
  cat("two arms differ in this modulation. The first two rows are the levels\n")
  cat("being compared, and are not themselves that test.\n")
  cat(
    "A negative value = bad news reduces incumbent vote MORE when the\n",
    "comparison is more expected/representative; positive = it reduces it less.\n",
    sep = ""
  )
  invisible(res)
}

# Run the standard pair of contrasts (T4 vs control, T4 vs T2) on one panel.
# T2 vs T4: both arms show a comparison bar chart; T4's comparisons share the
# home municipality's governing coalition and are labelled with party names,
# T2's are drawn without regard to party. The difference isolates the partisan
# content of the comparison, holding the presence of a comparison fixed — which
# control does not.
run_arm_contrasts <- function(lm_panel, ci_alpha = 0.05) {
  res <- list(
    t4_control = arm_contrast(lm_panel, "control", "T4", ci_alpha),
    t4_t2 = arm_contrast(lm_panel, "T2", "T4", ci_alpha)
  )
  invisible(lapply(res, report_contrast))
  invisible(res)
}
