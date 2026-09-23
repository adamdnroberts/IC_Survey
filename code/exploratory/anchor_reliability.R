# Test-retest diagnostic for the "shared within-wave anchor" story.
#
# Question: is the incumbent-vs-other-coalitions difference measure more precise
# than the incumbent rating alone because respondents anchor on the other
# coalitions they rate in the SAME wave, rather than on what they said a week
# earlier in wave 1?
#
# Pre ratings come from wave 1, post from wave 2 (see create_panel_dataset.R),
# so any respondent-specific scale-use shift between sessions is transient noise
# in the incumbent rating but cancels out of a within-wave difference.
#
# Run on UNTREATED respondents only, so pre -> post movement is measurement, not
# treatment.
#
# READ THIS BEFORE INTERPRETING THE OUTPUT. An earlier version of this script
# claimed two tests were diagnostic that are not. Both caveats below are load
# bearing; the persistence ratio added as test 3 is the only statistic here that
# actually discriminates.
#
#   1. Test-retest reliability. NOT DIAGNOSTIC, reported for description only.
#      The earlier claim was that differencing normally lowers reliability, so
#      r_diff > r_inc could only mean shared noise. That is a rule of thumb, not
#      a theorem: with NEGATIVELY correlated true scores, differencing amplifies
#      true variance and raises reliability with purely independent error
#      (e.g. V(T)=1 each, C_T=-1, V(e)=1 each gives r_inc=0.50, r_diff=0.67).
#      Conversely, when true scores are POSITIVELY correlated -- which c_cross
#      > 0 establishes here -- differencing destroys true variance, so
#      r_diff < r_inc is expected WHETHER OR NOT an anchor exists. The observed
#      sign therefore carries no information either way.
#
#   2. Within-wave vs cross-wave covariance. WEAKLY IDENTIFIED. Cov(inc, opp)
#      measured inside a single wave contains the true association plus any
#      shared transient anchor; measured ACROSS waves it was assumed to contain
#      only the true association. That assumption requires the shared true score
#      to be PERFECTLY STABLE over the week. It is not -- r_inc and r_opp show
#      substantial instability in these same data. Any decay of the shared
#      factor depresses c_cross and inflates anchor_var by exactly that amount.
#      A two-wave design cannot separate method variance from ordinary drift;
#      that needs a third wave or a repeated measure within a wave. Treat
#      anchor_var as an UPPER BOUND on the anchor, not an estimate of it.
#
#   3. Persistence ratio. THE INFORMATIVE TEST. Under a common-factor model
#      inc = a*F + u_i, opp = b*F + u_o with F autocorrelated at rho_F and no
#      method factor, c_cross / c_within = rho_F exactly. Compare that ratio to
#      sqrt(r_inc * r_opp), the typical persistence of the individual measures.
#      A transient within-wave anchor inflates c_within, pushing the ratio BELOW
#      the measures' own persistence. So:
#        ratio_gap < 0  -> consistent with a within-wave anchor
#        ratio_gap > 0  -> the shared component is MORE persistent than the
#                          measures themselves, i.e. a stable shared belief
#                          factor with faster-decaying idiosyncratic parts, and
#                          no method factor is needed to explain the data.
#
# Usage:
#   "/c/Program Files/R/R-4.5.1/bin/Rscript.exe" --vanilla \
#     code/exploratory/anchor_reliability.R

library(dplyr)

set.seed(20260831)

n_boot <- 2000

load("data/derived/survey_panel_dataset.Rdata")

num <- function(x) suppressWarnings(as.numeric(x))

# ── Build the incumbent and other-coalition ratings in both waves ─────────────
# Same construction as code/plots/t4_belief_updates_comparison.R: the "other
# coalitions" rating is the mean of the two coalition ratings that are NOT the
# coalition governing the respondent's home municipality.
build_measures <- function(d) {
  post_rating <- cbind(
    "MORENA/PVEM/PT" = num(d$MORENA_Crime_Rating_Post),
    "PAN/PRI/PRD" = num(d$Coalition_PAN_PRI_PRD_Crime_Rating_Post),
    "MC" = num(d$MC_Crime_Rating_Post)
  )

  pre_rating <- cbind(
    "MORENA/PVEM/PT" = num(d$MORENA_Crime_Rating_Pre),
    "PAN/PRI/PRD" = num(d$Coalition_PAN_PRI_PRD_Crime_Rating_Pre),
    "MC" = num(d$MC_Crime_Rating_Pre)
  )

  opp <- t(vapply(
    seq_len(nrow(d)),
    function(i) {
      hc <- d$home_coalition[i]
      if (is.na(hc)) {
        return(c(pre = NA_real_, post = NA_real_))
      }
      keep <- colnames(pre_rating) != hc
      pre <- pre_rating[i, keep]
      post <- post_rating[i, keep]
      c(
        pre = if (all(is.na(pre))) NA_real_ else mean(pre, na.rm = TRUE),
        post = if (all(is.na(post))) NA_real_ else mean(post, na.rm = TRUE)
      )
    },
    numeric(2)
  ))

  tibble(
    inc_pre = num(d$Home_Crime_Handling_Pre),
    inc_post = num(d$Home_Crime_Handling_Post),
    opp_pre = opp[, "pre"],
    opp_post = opp[, "post"]
  ) %>%
    mutate(
      diff_pre = inc_pre - opp_pre,
      diff_post = inc_post - opp_post
    )
}

# ── Statistics computed on one sample ────────────────────────────────────────
# Returned as a flat named vector so the bootstrap can rbind them.
anchor_stats <- function(m) {
  r_inc <- cor(m$inc_pre, m$inc_post)
  r_opp <- cor(m$opp_pre, m$opp_post)
  r_diff <- cor(m$diff_pre, m$diff_post)

  # Within-wave covariance of the two ratings, averaged over the two waves.
  c_within <- mean(c(
    cov(m$inc_pre, m$opp_pre),
    cov(m$inc_post, m$opp_post)
  ))

  # Cross-wave covariance of the two ratings, averaged over both pairings. A
  # transient anchor cannot contribute across a week, but this estimates the
  # true-score covariance ONLY if the shared true score is perfectly stable
  # (see header test 2) -- otherwise it is depressed by decay.
  c_cross <- mean(c(
    cov(m$inc_pre, m$opp_post),
    cov(m$inc_post, m$opp_pre)
  ))

  v_inc <- mean(c(var(m$inc_pre), var(m$inc_post)))
  v_opp <- mean(c(var(m$opp_pre), var(m$opp_post)))
  v_diff <- mean(c(var(m$diff_pre), var(m$diff_post)))

  # Reliability the difference WOULD have if the two ratings shared no transient
  # component, i.e. if the within-wave covariance equalled the cross-wave one.
  # Only the denominator changes: Cov(diff_pre, diff_post) is built entirely
  # from cross-wave terms and so is unaffected by shared within-wave noise.
  cov_diff <- cov(m$diff_pre, m$diff_post)
  r_diff_no_anchor <- cov_diff / (v_inc + v_opp - 2 * c_cross)

  # r_diff above is a correlation (denominator sd(diff_pre)*sd(diff_post));
  # r_diff_no_anchor is a covariance over an AVERAGED variance. Subtracting one
  # from the other mixes two different denominators and is only valid when
  # var(diff_pre) == var(diff_post). r_diff_var is the exact analogue of
  # r_diff_no_anchor, so the anchor gap must be computed from that pair.
  r_diff_var <- cov_diff / (v_inc + v_opp - 2 * c_within)

  # See header test 3. Positive ratio_gap argues AGAINST a within-wave anchor.
  persistence_shared <- c_cross / c_within
  persistence_measures <- sqrt(r_inc * r_opp)

  c(
    r_inc = r_inc,
    r_opp = r_opp,
    r_diff = r_diff,
    r_diff_minus_r_inc = r_diff - r_inc,
    c_within = c_within,
    c_cross = c_cross,
    anchor_var = c_within - c_cross,
    persistence_shared = persistence_shared,
    persistence_measures = persistence_measures,
    ratio_gap = persistence_shared - persistence_measures,
    v_inc = v_inc,
    v_opp = v_opp,
    v_diff = v_diff,
    r_diff_var = r_diff_var,
    r_diff_no_anchor = r_diff_no_anchor,
    anchor_gain = r_diff_var - r_diff_no_anchor
  )
}

boot_ci <- function(m, stat_fn, n_boot) {
  n <- nrow(m)
  draws <- vapply(
    seq_len(n_boot),
    function(b) stat_fn(m[sample.int(n, n, replace = TRUE), ]),
    numeric(length(stat_fn(m)))
  )
  # quantile(na.rm = TRUE) would silently drop failed draws and degrade coverage
  # without warning, so count them explicitly instead.
  n_bad <- sum(apply(draws, 2, function(col) any(!is.finite(col))))
  if (n_bad > 0) {
    warning(n_bad, " of ", n_boot, " bootstrap draws were non-finite")
  }
  t(apply(draws, 1, quantile, probs = c(0.025, 0.975), na.rm = TRUE))
}

report <- function(label, d) {
  m <- build_measures(d) %>%
    filter(if_all(everything(), ~ !is.na(.)))

  cat("\n", strrep("=", 68), "\n", sep = "")
  cat(label, " (n = ", nrow(m), ")\n", sep = "")
  cat(strrep("=", 68), "\n", sep = "")

  if (nrow(m) < 30) {
    cat("Too few complete cases for a stable estimate; skipping.\n")
    return(invisible(NULL))
  }

  est <- anchor_stats(m)
  ci <- boot_ci(m, anchor_stats, n_boot)

  out <- data.frame(
    estimate = round(est, 3),
    ci_low = round(ci[, 1], 3),
    ci_high = round(ci[, 2], 3)
  )

  cat("\n-- Test-retest reliability (DESCRIPTIVE ONLY, not diagnostic) --\n")
  print(out[c("r_inc", "r_opp", "r_diff", "r_diff_minus_r_inc"), ])
  cat(
    "\nDo NOT read the sign of r_diff_minus_r_inc as evidence about anchoring.\n",
    "With positively correlated true scores (c_cross > 0 below), differencing\n",
    "destroys true variance, so r_diff < r_inc is expected either way. Note\n",
    "also that low reliability does NOT imply large standard errors:\n",
    "reliability is a noise-to-total RATIO, while SEs track absolute residual\n",
    "variance. Compare v_diff with v_inc below before concluding anything\n",
    "about precision.\n",
    sep = ""
  )

  cat("\n-- Within-wave vs cross-wave covariance of inc and opp --\n")
  print(out[c("c_within", "c_cross", "anchor_var"), ])
  cat(
    "\nanchor_var is an UPPER BOUND on the shared transient component, not an\n",
    "estimate: any decay of the shared true score over the week inflates it.\n",
    "A CI excluding zero is therefore NOT by itself evidence of anchoring.\n",
    sep = ""
  )

  cat("\n-- Persistence ratio (the test that discriminates) --\n")
  print(out[c("persistence_shared", "persistence_measures", "ratio_gap"), ])
  cat(
    "\nratio_gap < 0: the shared component decays FASTER than the measures\n",
    "themselves, which is what a within-wave anchor would produce.\n",
    "ratio_gap > 0: the shared component is MORE persistent than the measures,\n",
    "which points to a stable shared belief factor and needs no anchor.\n",
    sep = ""
  )

  cat("\n-- Variances, and the anchor's reliability gain --\n")
  print(out[c("v_inc", "v_opp", "v_diff"), ])
  cat(
    "\nIf v_diff < v_inc the difference outcome has a smaller absolute spread,\n",
    "which lowers its coefficient SE with no gain in measurement quality.\n",
    sep = ""
  )
  print(out[c("r_diff_var", "r_diff_no_anchor", "anchor_gain"), ])
  cat(
    "\nanchor_gain uses r_diff_var, not the r_diff correlation above, so both\n",
    "terms share the same denominator form and the subtraction is valid.\n",
    sep = ""
  )

  invisible(out)
}

# ── Untreated respondents only ───────────────────────────────────────────────
# Both control arms are weather placebos, so neither should move crime beliefs.
# Reported separately in case control2's comparison bar chart matters on its own.
base <- filter(panel, muni_changed == 0, Attention_Check == "somewhat_agree")

report("Control only (weather placebo, home municipality only)",
  filter(base, Treatment_Group == "control"))

report("Control + Control2 (both weather placebos)",
  filter(base, Treatment_Group %in% c("control", "control2")))

cat("\n")