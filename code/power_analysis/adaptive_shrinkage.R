# Adaptive shrinkage (ash) across outcomes — the multiple-comparisons procedure
# in the PAP (Inference Criteria): Equation 2 is estimated for each outcome,
# yielding a family of estimates per research question; ash is applied to the
# vector of per-outcome estimates and SEs for each RQ simultaneously, returning
# posterior means shrunk toward zero and a local false sign rate (lfsr).
#
# This replaces the earlier SIMULATED demonstration with the REAL fitted models.
#
# Equation 2 (PAP):
#   dY_i = a + a1 CG_i + a2 RG_i + sum_k d_k T_ik
#            + sum_k b_k (CG_i x T_ik) + sum_k g_k (RG_i x T_ik) + e_i
# estimated by OLS per outcome; HC2 robust SEs. CG = crime_gap_wins (winsorized
# level gap), RG = rank_gap. No additional controls are included, matching the
# registered equation (note: the home-outcome script m_winsorized additionally
# adds comp_party_known; here we follow Eq. 2 as written for cross-outcome
# comparability).
#
# RQ -> test (PAP, Primary Analysis):
#   RQ1: b_1 = 0            (T1 vs pure control; CG x T1 interaction)
#   RQ2: g_2 - g_1 = 0      (T2 vs T1)
#   RQ3: g_3 - g_2 = 0  and g_4 - g_2 = 0   (party labels vs non-partisan)
#   RQ4: g_4 - g_3 = 0      (opposite- vs same-coalition)
#
# Outcomes (all dY = post - pre, except vote = post-level incumbent indicator):
#   home  Home crime handling          (all arms)
#   mor   MORENA coalition rating       (comparison arms only; T1 excluded)
#   non   PAN/PRI/PRD coalition rating  (comparison arms only)
#   mc    MC coalition rating           (comparison arms only)
#   rank  Relative robbery ranking      (all arms)
#   vote  Incumbent vote, post-level    (all arms)
# Coalition outcomes are restricted to comparison arms (control, control2, T2,
# T3, T4) per the PAP, so they have no T1 term and contribute only to RQ3/RQ4.

library(dplyr)
library(sandwich)
library(ashr)
library(mashr)

load("~/IC_Survey/data/survey_panel_dataset.Rdata")

# ── Sample: home municipality unchanged across waves, attention-check pass ────
panel <- panel %>%
  filter(muni_changed == 0, Attention_Check == "somewhat_agree")

# ── Build outcomes ────────────────────────────────────────────────────────────
num <- function(x) suppressWarnings(as.numeric(x))

panel$mor_chg <- num(panel$MORENA_Crime_Rating_Post) -
  num(panel$MORENA_Crime_Rating_Pre)
panel$non_chg <- num(panel$Coalition_PAN_PRI_PRD_Crime_Rating_Post) -
  num(panel$Coalition_PAN_PRI_PRD_Crime_Rating_Pre)
panel$mc_chg <- num(panel$MC_Crime_Rating_Post) -
  num(panel$MC_Crime_Rating_Pre)

# Relative robbery ranking: rank_post built like rank_prior (count of "fewer")
rank_cols_post <- paste0("Crime_Rank_Comp_", 1:4, "_Post")
panel$rank_post <- 1 +
  rowSums(sapply(panel[rank_cols_post], function(x) x %in% "fewer"), na.rm = TRUE)
panel$rank_chg <- panel$rank_post - panel$rank_prior

# Vote: post-level incumbent indicator (matches vote_update_analysis.R)
panel$vote_home_post <- as.integer(
  !is.na(panel$coalition_post) &
    !is.na(panel$home_coalition) &
    panel$home_coalition == panel$coalition_post
)

outcomes <- list(
  home = list(y = "Home_Crime_Handling_Change", arms = "all",  label = "Home crime handling"),
  mor  = list(y = "mor_chg",                    arms = "comp", label = "MORENA rating"),
  non  = list(y = "non_chg",                    arms = "comp", label = "PAN/PRI/PRD rating"),
  mc   = list(y = "mc_chg",                     arms = "comp", label = "MC rating"),
  rank = list(y = "rank_chg",                   arms = "all",  label = "Relative robbery rank"),
  vote = list(y = "vote_home_post",             arms = "all",  label = "Incumbent vote (post)")
)

# ── Fit Equation 2 per outcome ────────────────────────────────────────────────
fit_eq2 <- function(spec) {
  d <- panel
  if (spec$arms == "comp") {
    d <- d[d$Treatment_Group != "T1", ] # coalition outcomes: drop T1 arm
  }
  d$Treatment_Group <- relevel(
    droplevels(factor(d$Treatment_Group)),
    ref = "control"
  )
  # Standardize the outcome to unit SD so the per-outcome interaction estimates
  # are on a common (outcome-SD) scale before ash pools them. ash assumes the
  # estimates are exchangeable draws from one prior; pooling raw coefficients
  # across 0-100 sliders, a 1-5 rank, and a 0/1 vote violates that and collapses
  # the prior to a point mass at 0. (Not specified in the PAP; required to make
  # the cross-outcome pooling coherent. Per-estimate z-stats are unaffected.)
  d[[spec$y]] <- d[[spec$y]] / sd(d[[spec$y]], na.rm = TRUE)
  fml <- as.formula(paste0(
    spec$y,
    " ~ crime_gap_wins * Treatment_Group + rank_gap * Treatment_Group"
  ))
  lm(fml, data = d)
}

models <- lapply(outcomes, fit_eq2)

# ── Linear-combination helper (HC2 robust) ────────────────────────────────────
# Find the interaction coefficient for `var` (crime_gap_wins / rank_gap) x arm,
# regardless of which side of the ":" R put it on.
inter <- function(m, var, arm) {
  cn <- names(coef(m))
  cands <- c(
    paste0(var, ":Treatment_Group", arm),
    paste0("Treatment_Group", arm, ":", var)
  )
  hit <- cands[cands %in% cn]
  stopifnot(length(hit) == 1)
  hit
}

# Estimate and HC2 SE of a linear combination sum(pos) - sum(neg) of coefficients.
lincom <- function(m, pos, neg = character(0)) {
  b <- coef(m)
  V <- vcovHC(m, type = "HC2")
  L <- setNames(numeric(length(b)), names(b))
  L[pos] <- L[pos] + 1
  if (length(neg) > 0) L[neg] <- L[neg] - 1
  est <- sum(L * b)
  se <- sqrt(drop(t(L) %*% V %*% L))
  c(est = est, se = se)
}

# ── RQ statistic per outcome ──────────────────────────────────────────────────
rq_stat <- function(m, rq) {
  switch(rq,
    RQ1  = lincom(m, inter(m, "crime_gap_wins", "T1")),
    RQ2  = lincom(m, inter(m, "rank_gap", "T2"), inter(m, "rank_gap", "T1")),
    RQ3a = lincom(m, inter(m, "rank_gap", "T3"), inter(m, "rank_gap", "T2")),
    RQ3b = lincom(m, inter(m, "rank_gap", "T4"), inter(m, "rank_gap", "T2")),
    RQ4  = lincom(m, inter(m, "rank_gap", "T4"), inter(m, "rank_gap", "T3"))
  )
}

# Which outcomes contribute to each RQ (coalition outcomes lack a T1 term, so
# they enter only the RQ3/RQ4 families).
all_out <- names(outcomes)
t1_out <- names(Filter(function(s) s$arms == "all", outcomes)) # home, rank, vote
rq_outcomes <- list(
  RQ1  = t1_out,
  RQ2  = t1_out,
  RQ3a = all_out,
  RQ3b = all_out,
  RQ4  = all_out
)

# ── ash per RQ (pooled across outcomes) ───────────────────────────────────────
ash_by_rq <- lapply(names(rq_outcomes), function(rq) {
  outs <- rq_outcomes[[rq]]
  stats <- t(vapply(outs, function(o) rq_stat(models[[o]], rq), numeric(2)))
  fit <- ash(stats[, "est"], stats[, "se"])
  data.frame(
    RQ = rq,
    outcome = vapply(outs, function(o) outcomes[[o]]$label, character(1)),
    betahat = stats[, "est"],
    se = stats[, "se"],
    post_mean = get_pm(fit),
    lfsr = get_lfsr(fit),
    svalue = fit$result$svalue,
    row.names = NULL
  )
})
ash_table <- do.call(rbind, ash_by_rq)

cat("== Adaptive shrinkage (ash) across outcomes, per research question ==\n")
cat("   betahat   = OLS estimate, outcome standardized to unit SD (HC2 SE)\n")
cat("   post_mean = ash posterior mean (shrunk toward 0)\n")
cat("   lfsr      = local false sign rate (post. prob. the sign is wrong)\n")
cat("   NOTE: families are small (3-6 estimates); empirical-Bayes ash is very\n")
cat("   conservative at this size and tends to return the global null.\n\n")
print(ash_table, row.names = FALSE, digits = 4)

# ── mashr: joint model across outcomes (balanced RQ3/RQ4 contrasts only) ──────
# EXPLORATORY extra (not required by the PAP). mashr borrows strength across
# outcomes that move together; restricted to the RQ contrasts estimable for all
# six outcomes so the matrix is complete.
balanced <- c("RQ3a", "RQ3b", "RQ4")
B_mat <- t(sapply(balanced, function(rq) {
  vapply(all_out, function(o) rq_stat(models[[o]], rq)["est"], numeric(1))
}))
S_mat <- t(sapply(balanced, function(rq) {
  vapply(all_out, function(o) rq_stat(models[[o]], rq)["se"], numeric(1))
}))
rownames(B_mat) <- rownames(S_mat) <- balanced
colnames(B_mat) <- colnames(S_mat) <- vapply(all_out, function(o) outcomes[[o]]$label, character(1))

mash_fit <- tryCatch({
  md <- mash_set_data(B_mat, S_mat)
  mash(md, cov_canonical(md), verbose = FALSE)
}, error = function(e) {
  message("mashr step skipped: ", conditionMessage(e))
  NULL
})

if (!is.null(mash_fit)) {
  cat("\n== mashr posterior means (rows = RQ contrast, cols = outcome) ==\n")
  print(round(get_pm(mash_fit), 3))
  cat("\n== mashr lfsr ==\n")
  print(round(get_lfsr(mash_fit), 3))
}
