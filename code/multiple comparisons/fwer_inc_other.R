# Westfall-Young free step-down FWER adjustment for the treatment-interaction
# coefficients of a SINGLE model, m_inc_other (incumbent minus average of the
# other coalitions, post levels, controlling for both pre levels). The family is
# all 8 interaction coefficients jointly: the 4 CG × Treatment terms
# (log_crime_gap:T1…T4) and the 4 RG × Treatment terms (T1…T4:rank_gap), where
# T1–T4 are the treatment arms relative to the control baseline. FWER is
# controlled across all 8 together (one family), not split by CG/RG.
#
# Only Treatment_Group is permuted; the outcome/control columns
# (inc_minus_opp_avg_post, inc_pre, opp_avg_pre, coalition_pre, log_crime_gap,
# rank_gap) do not depend on treatment and are built in
# home_party_update_analysis.R, so they are invariant under permutation.

library(estimatr)

# Build panel + the inc_minus_opp_avg_post / inc_pre / opp_avg_pre columns if not
# already loaded (source is skipped when panel already has the expected rows).
if (!exists("panel") || nrow(panel) != 1735) {
  source(
    "C:/Users/adamd/Documents/IC_Survey/code/home_party_update_analysis.R",
    echo = FALSE
  )
}

if (!exists("ci_alpha")) {
  ci_alpha <- 0.01
}

# Fit m_inc_other on an explicit data frame (so the permuted data is used).
fit_inc_other <- function(dat) {
  lm_robust(
    inc_minus_opp_avg_post ~
      inc_pre + opp_avg_pre +
      log_crime_gap * as.factor(Treatment_Group) +
      rank_gap * as.factor(Treatment_Group) +
      coalition_pre,
    alpha = ci_alpha,
    data = dat,
    se_type = "HC2"
  )
}

# Treatment arms (everything except the control baseline). Permutation only
# reshuffles these labels, so the coefficient names below stay fixed.
arms <- setdiff(sort(unique(as.character(panel$Treatment_Group))), "control")
cg_terms <- paste0("log_crime_gap:as.factor(Treatment_Group)", arms)
rg_terms <- paste0("as.factor(Treatment_Group)", arms, ":rank_gap")
terms8 <- c(cg_terms, rg_terms) # fixed order: 4 CG, then 4 RG

# Return the 8 interaction p-values in the fixed `terms8` order.
compute_H <- function(dat) {
  fit_inc_other(dat)$p.value[terms8]
}

H <- compute_H(panel) # observed p-values, fixed order
k <- length(H) # 8

# Westfall-Young free step-down (minP), one family. Order hypotheses from MOST
# to LEAST significant by observed p; each permutation's p-values are reordered
# the SAME way (never sorted independently), reduced to successive minima from
# least- to most-significant, counted against the observed p at each rank, then
# made monotone. adj_p[o] scatters the sorted adjusted p back to `terms8` order.
wy_stepdown <- function(p_obs, Pperm) {
  m <- length(p_obs)
  o <- order(p_obs) # most -> least significant
  p_sorted <- p_obs[o]
  count <- numeric(m)
  for (b in seq_len(ncol(Pperm))) {
    q <- Pperm[o, b]
    for (i in (m - 1):1) {
      q[i] <- min(q[i], q[i + 1])
    }
    count <- count + (q <= p_sorted)
  }
  p_adj_sorted <- count / ncol(Pperm)
  for (i in 2:m) {
    p_adj_sorted[i] <- max(p_adj_sorted[i], p_adj_sorted[i - 1])
  }
  adj <- numeric(m)
  adj[o] <- p_adj_sorted
  adj
}

n <- 1000

# Permuted p-values: rows = terms8 order, cols = permutations.
Pperm <- matrix(NA_real_, nrow = k, ncol = n)
for (b in 1:n) {
  if (b %% 100 == 0) {
    print(b / n)
  }
  perm <- panel
  perm$Treatment_Group <- sample(panel$Treatment_Group) # permute treatment labels
  Pperm[, b] <- compute_H(perm)
}

adj_p <- wy_stepdown(H, Pperm)

# ── Results table ────────────────────────────────────────────────────────────
coef_labels <- c(
  paste0("CG x ", arms, " (log_crime_gap)"),
  paste0("RG x ", arms, " (rank_gap)")
)

fwer_table <- data.frame(
  coefficient = coef_labels,
  raw_p = round(H, 4),
  fwer_p = round(adj_p, 4),
  row.names = NULL
)

# Most-significant first
fwer_table <- fwer_table[order(fwer_table$fwer_p, fwer_table$raw_p), ]

cat(
  "\nm_inc_other: FWER-adjusted p-values across all 8 interaction",
  " coefficients (", n, " permutations)\n",
  sep = ""
)
print(fwer_table, row.names = FALSE)
