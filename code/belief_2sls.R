# belief_2sls.R
# ──────────────────────────────────────────────────────────────────────────────
# EXPLORATORY (NOT pre-registered). 2SLS estimate of the causal effect of a
# treatment-induced revision in comparative crime-ranking beliefs on downstream
# outcomes (home-municipality crime-handling approval; incumbent vote intention).
#
# Why 2SLS here:
#   rank_gap (= rank_prior - actual_rank) is a PRE-treatment respondent trait that
#   is correlated with the outcomes in every arm, including the no-information
#   placebos (see belief_update_analysis.R / the rank_gap_25 results). So the OLS
#   rank_gap x Treatment interactions conflate treatment-induced updating with a
#   pre-existing association. 2SLS isolates only the random, treatment-induced
#   slice of belief revision.
#
# Structural equation:
#   Y = a + b*rank_update + c*rank_gap + d*comparison + e
#     - rank_update = rank_post - rank_prior   (ENDOGENOUS: revision of perceived
#       comparative ranking; correlated with unobserved respondent traits)
#     - rank_gap, comparison enter as EXOGENOUS controls (their main effects are
#       absorbed, so rank_gap's direct pre-treatment association with Y does not
#       leak into b)
#
# Excluded instrument:  comparison x rank_gap
#   Random assignment of `comparison` makes the interaction excludable GIVEN the
#   rank_gap and comparison main effects: among respondents with the same
#   rank_gap, comparison is assigned at random and shifts how much they revise
#   their ranking (the "learning-rate" first stage). The identifying (exclusion)
#   assumption is that this interaction moves Y ONLY through rank_update.
#
# CAVEAT: exclusion is most defensible for the non-partisan comparison (T2). The
# partisan arms (T3, T4) additionally carry a party-cue channel that may touch Y
# directly; treat the arm-specific / pooled estimates accordingly.
# ──────────────────────────────────────────────────────────────────────────────

library(dplyr)
library(estimatr)

load("~/IC_Survey/data/survey_panel_dataset.Rdata")

if (!exists("ci_alpha")) ci_alpha <- 0.01

# ── 1. Sample: attention-check passers, home municipality unchanged ───────────
panel <- filter(panel, Attention_Check == "somewhat_agree", muni_changed == 0)

# ── 2. Post-treatment comparative ranking (mirrors rank_prior construction in
#       create_panel_dataset.R / update_plots.R) and the belief revision ────────
rank_cols_post <- c(
  "Crime_Rank_Comp_1_Post",
  "Crime_Rank_Comp_2_Post",
  "Crime_Rank_Comp_3_Post",
  "Crime_Rank_Comp_4_Post"
)

panel$rank_post <- 1 +
  rowSums(
    sapply(panel[rank_cols_post], function(x) x %in% c("fewer")),
    na.rm = TRUE
  )

panel$rank_update <- panel$rank_post - panel$rank_prior

# ── 3. Treatment indicators and outcomes ──────────────────────────────────────
# `comparison` = arms that deliver comparative ranking information.
panel$comparison <- as.integer(panel$Treatment_Group %in% c("T2", "T3", "T4"))
panel$T2 <- as.integer(panel$Treatment_Group == "T2")
panel$T3 <- as.integer(panel$Treatment_Group == "T3")
panel$T4 <- as.integer(panel$Treatment_Group == "T4")

panel$coalition_pre[is.na(panel$coalition_pre)] <- "Other"
panel$Vote_home_post <- as.integer(
  !is.na(panel$coalition_post) &
    !is.na(panel$home_coalition) &
    panel$home_coalition == panel$coalition_post
)

# Keep complete cases on the variables the 2SLS uses. crime_gap_wins (winsorized
# level-gap) and comp_party_known mirror the controls in belief_update_analysis.R.
model_vars <- c(
  "rank_update", "rank_gap", "comparison",
  "crime_gap_wins", "comp_party_known", "coalition_pre",
  "Home_Crime_Handling_Change", "Vote_home_post", "T2", "T3", "T4"
)
panel <- panel[stats::complete.cases(panel[model_vars]), ]

# ── 4. First stage (the "learning-rate" regression) ───────────────────────────
# rank_update on rank_gap, comparison, and their interaction. The interaction
# coefficient is the excluded instrument's first-stage effect: treated
# (comparison) respondents revise their ranking toward the truth in proportion to
# their pre-treatment gap, so it should be strongly negative (rank_gap > 0 means
# the prior over-ranked home's crime, which the chart pushes back down).
first_stage <- lm_robust(
  rank_update ~ rank_gap * comparison,
  data = panel, se_type = "HC2"
)

# ── 5. 2SLS — pooled comparison instrument ────────────────────────────────────
# Endogenous: rank_update. Exogenous controls: rank_gap, comparison, the
# crime-level-gap channel (crime_gap_wins and its comparison interaction), and
# comp_party_known. Excluded instrument: rank_gap:comparison. (estimatr:
# regressors | instruments, where the exogenous controls instrument themselves.)
iv_handling <- iv_robust(
  Home_Crime_Handling_Change ~ rank_update + rank_gap + comparison +
    crime_gap_wins + crime_gap_wins:comparison + comp_party_known +
    as.factor(coalition_pre) |
    rank_gap + comparison + crime_gap_wins + crime_gap_wins:comparison +
      comp_party_known + as.factor(coalition_pre) + rank_gap:comparison,
  data = panel, se_type = "HC2", diagnostics = TRUE
)

iv_vote <- iv_robust(
  Vote_home_post ~ rank_update + rank_gap + comparison +
    crime_gap_wins + crime_gap_wins:comparison + comp_party_known +
    as.factor(coalition_pre) |
    rank_gap + comparison + crime_gap_wins + crime_gap_wins:comparison +
      comp_party_known + as.factor(coalition_pre) + rank_gap:comparison,
  data = panel, se_type = "HC2", diagnostics = TRUE
)

# ── 6. Naive OLS analogue (treats rank_update as exogenous) for contrast ──────
ols_handling <- lm_robust(
  Home_Crime_Handling_Change ~ rank_update + rank_gap + comparison +
    crime_gap_wins + crime_gap_wins:comparison + comp_party_known +
    as.factor(coalition_pre),
  data = panel, se_type = "HC2"
)
ols_vote <- lm_robust(
  Vote_home_post ~ rank_update + rank_gap + comparison +
    crime_gap_wins + crime_gap_wins:comparison + comp_party_known +
    as.factor(coalition_pre),
  data = panel, se_type = "HC2"
)

# ── 7. Over-identified spec: arm-specific instruments (T2/T3/T4 x rank_gap) ────
# Three excluded instruments for one endogenous regressor -> a Sargan over-ID
# test. NB: over-ID rejection here can reflect heterogeneous arm effects or an
# exclusion violation (e.g., party cue in T3/T4), not only invalid instruments.
# crime_gap_wins is interacted per arm to mirror belief_update_analysis.R.
iv_handling_arms <- iv_robust(
  Home_Crime_Handling_Change ~ rank_update + rank_gap + T2 + T3 + T4 +
    crime_gap_wins + crime_gap_wins:T2 + crime_gap_wins:T3 +
    crime_gap_wins:T4 + comp_party_known + as.factor(coalition_pre) |
    rank_gap + T2 + T3 + T4 + crime_gap_wins + crime_gap_wins:T2 +
      crime_gap_wins:T3 + crime_gap_wins:T4 + comp_party_known +
      as.factor(coalition_pre) + rank_gap:T2 + rank_gap:T3 + rank_gap:T4,
  data = panel, se_type = "HC2", diagnostics = TRUE
)
iv_vote_arms <- iv_robust(
  Vote_home_post ~ rank_update + rank_gap + T2 + T3 + T4 +
    crime_gap_wins + crime_gap_wins:T2 + crime_gap_wins:T3 +
    crime_gap_wins:T4 + comp_party_known + as.factor(coalition_pre) |
    rank_gap + T2 + T3 + T4 + crime_gap_wins + crime_gap_wins:T2 +
      crime_gap_wins:T3 + crime_gap_wins:T4 + comp_party_known +
      as.factor(coalition_pre) + rank_gap:T2 + rank_gap:T3 + rank_gap:T4,
  data = panel, se_type = "HC2", diagnostics = TRUE
)

# ── 8. Report ─────────────────────────────────────────────────────────────────
cat(strrep("=", 72), "\n")
cat("2SLS: effect of treatment-induced ranking revision on outcomes\n")
cat(sprintf("N = %d   (comparison arms: %d, non-comparison: %d)\n",
            nrow(panel), sum(panel$comparison), sum(panel$comparison == 0)))
cat(strrep("=", 72), "\n")

cat("\n── First stage: rank_update ~ rank_gap * comparison ──\n")
print(tidy(first_stage)[, c("term", "estimate", "std.error", "p.value")],
      row.names = FALSE, digits = 4)

iv_row <- function(m, label) {
  t <- tidy(m)
  r <- t[t$term == "rank_update", ]
  data.frame(model = label, estimate = r$estimate, std.error = r$std.error,
             p.value = r$p.value, ci.low = r$conf.low, ci.high = r$conf.high)
}

cat("\n── Coefficient on rank_update (revision -> outcome) ──\n")
print(bind_rows(
  iv_row(ols_handling, "Handling: OLS (naive)"),
  iv_row(iv_handling,  "Handling: 2SLS pooled"),
  iv_row(iv_handling_arms, "Handling: 2SLS arms"),
  iv_row(ols_vote, "Vote: OLS (naive)"),
  iv_row(iv_vote,  "Vote: 2SLS pooled"),
  iv_row(iv_vote_arms, "Vote: 2SLS arms")
), row.names = FALSE, digits = 4)

diag_line <- function(m, label) {
  d <- m$diagnostic_first_stage_fstatistic
  wu <- m$diagnostic_endogeneity_test
  oi <- m$diagnostic_overid_test
  cat(sprintf("\n%s\n", label))
  if (!is.null(d)) cat(sprintf("  weak-instrument F = %.1f (p = %.4g)\n",
                               d["value"], d["p.value"]))
  if (!is.null(wu)) cat(sprintf("  Wu-Hausman endogeneity p = %.4g\n", wu["p.value"]))
  if (!is.null(oi) && !is.na(oi["p.value"]))
    cat(sprintf("  Sargan over-ID p = %.4g\n", oi["p.value"]))
}

cat("\n── Diagnostics ──")
diag_line(iv_handling, "Handling, pooled instrument:")
diag_line(iv_handling_arms, "Handling, arm-specific instruments:")
diag_line(iv_vote, "Vote, pooled instrument:")
diag_line(iv_vote_arms, "Vote, arm-specific instruments:")

cat("\nInterpretation: a strong first stage + Wu-Hausman rejection justify 2SLS\n")
cat("over OLS. The exclusion restriction is an assumption, not a test; the\n")
cat("Sargan p only screens instrument-set consistency. Lead with reduced-form\n")
cat("ITT results; report this as a secondary 'effect of beliefs' estimate.\n")
