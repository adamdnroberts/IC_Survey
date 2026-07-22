# Only (re)build the models/panel if they aren't already loaded. Sourcing is
# skipped when `panel` exists and already has the expected number of rows.
if (!exists("panel") || nrow(panel) != 1735) {
  source(
    "C:/Users/adamd/Documents/IC_Survey/code/home_party_update_analysis.R",
    echo = FALSE
  )
}

# Wrappers that take an explicit data frame, so the permuted data is actually
# used (fit_ancova() in the sourced script hardcodes data = panel). The outcome
# columns (party_minus_opp_avg_post, opp_avg_pre, party_minus_inc_change, ...)
# do not depend on treatment, so they are already built on `panel` and are
# invariant under permutation — only Treatment_Group is permuted below.
fit_change <- function(outcome, dat) {
  lm_robust(
    as.formula(paste0(
      outcome,
      " ~ log_crime_gap * as.factor(Treatment_Group) +",
      " rank_gap * as.factor(Treatment_Group) + coalition_pre"
    )),
    alpha = ci_alpha,
    data = dat,
    se_type = "HC2"
  )
}

fit_ancova_d <- function(outcome, ref_pre, opp_pre, dat) {
  lm_robust(
    as.formula(paste0(
      outcome,
      " ~ ",
      ref_pre,
      " + ",
      opp_pre,
      " +",
      " log_crime_gap * as.factor(Treatment_Group) +",
      " rank_gap * as.factor(Treatment_Group) + coalition_pre"
    )),
    alpha = ci_alpha,
    data = dat,
    se_type = "HC2"
  )
}

cg <- "log_crime_gap:as.factor(Treatment_Group)T4"
rg <- "as.factor(Treatment_Group)T4:rank_gap"

# Fit the two models on `dat`, return the 4 T4-interaction p-values in a FIXED
# order (2 CG × Treatment terms, then 2 RG × Treatment terms).
compute_H <- function(dat) {
  m_diff <- fit_change("party_minus_inc_change", dat)
  m_inc_other <- fit_ancova_d(
    "inc_minus_opp_avg_post",
    "inc_pre",
    "opp_avg_pre",
    dat
  )

  c(
    m_diff$p.value[cg],
    m_inc_other$p.value[cg],
    m_diff$p.value[rg],
    m_inc_other$p.value[rg]
  )
}

H <- compute_H(panel) # observed p-values, fixed hypothesis order
k <- length(H) # 4

# The two coefficient families are tested SEPARATELY: FWER is controlled within
# the 2 CG × Treatment hypotheses and within the 2 RG × Treatment hypotheses,
# not jointly across all 4. Both families reuse the same permutations.
cg_idx <- 1:2 # positions of the CG terms in compute_H()'s output
rg_idx <- 3:4 # positions of the RG terms

# Westfall-Young free step-down (minP) for ONE family. Order that family's
# hypotheses from MOST to LEAST significant by observed p-value; each column of
# `Pperm_fam` (a permutation's p-values for this family) is reordered the SAME
# way — never sorted independently — then reduced to successive minima from
# least- to most-significant. Adjusted p = fraction of permutations whose
# successive-minimum is <= the observed p at that rank, made monotone at the end.
wy_stepdown <- function(p_obs_fam, Pperm_fam) {
  m <- length(p_obs_fam)
  o <- order(p_obs_fam) # most -> least significant
  p_sorted <- p_obs_fam[o]
  count <- numeric(m)
  for (b in seq_len(ncol(Pperm_fam))) {
    q <- Pperm_fam[o, b]
    for (i in (m - 1):1) {
      q[i] <- min(q[i], q[i + 1])
    }
    count <- count + (q <= p_sorted)
  }
  p_adj_sorted <- count / ncol(Pperm_fam)
  # enforce monotonicity: adjusted p must be non-decreasing down the ranks
  for (i in 2:m) {
    p_adj_sorted[i] <- max(p_adj_sorted[i], p_adj_sorted[i - 1])
  }
  adj <- numeric(m)
  adj[o] <- p_adj_sorted # scatter back to this family's original order
  adj
}

# q <- 0.05
#
# q_prime <- q / (1 + q)
#
# c <- sum(H < (q_prime / length(H)))
#
# m_hat_0 <- length(H) - c

n <- 1000

# Collect each permutation's 12 p-values (rows = compute_H order, cols = perms),
# then apply the step-down within each family. Both families share these perms.
Pperm <- matrix(NA_real_, nrow = k, ncol = n)
for (b in 1:n) {
  if (b %% 100 == 0) {
    print(b / n)
  }
  perm <- panel
  perm$Treatment_Group <- sample(panel$Treatment_Group) # permute treatment labels
  Pperm[, b] <- compute_H(perm)
}

adj_cg <- wy_stepdown(H[cg_idx], Pperm[cg_idx, , drop = FALSE])
adj_rg <- wy_stepdown(H[rg_idx], Pperm[rg_idx, , drop = FALSE])

# Reassemble in compute_H order (6 CG terms, then 6 RG terms)
adj_p <- numeric(k)
adj_p[cg_idx] <- adj_cg
adj_p[rg_idx] <- adj_rg

# ── Results tables ───────────────────────────────────────────────────────────
# Labels in the SAME fixed order as compute_H(): 2 CG × Treatment terms, then 2
# RG × Treatment terms. adj_p is already aligned to that order (family-adjusted).
model_labels <- c(
  "m_diff",
  "m_inc_other"
)

fwer_table <- data.frame(
  family = rep(c("CG x T4 (log_crime_gap)", "RG x T4 (rank_gap)"), each = 2),
  model = rep(model_labels, 2),
  raw_p = round(H, 4),
  fwer_p = round(adj_p, 4),
  row.names = NULL
)

# Print each family separately, most-significant first within the family.
for (fam in unique(fwer_table$family)) {
  tb <- fwer_table[fwer_table$family == fam, c("model", "raw_p", "fwer_p")]
  tb <- tb[order(tb$fwer_p, tb$raw_p), ]
  cat(
    "\nFWER-adjusted p-values within family: ",
    fam,
    " (",
    n,
    " permutations)\n",
    sep = ""
  )
  print(tb, row.names = FALSE)
}
