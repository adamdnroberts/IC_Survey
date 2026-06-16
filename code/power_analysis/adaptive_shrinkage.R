library(ashr)
library(sandwich)
library(mashr)

set.seed(42)

# ── Parameters ────────────────────────────────────────────────────────────────
N <- 2000
outcome_names <- c("home", "mor", "non", "mc", "rank", "turn", "vote")
n_outcomes <- length(outcome_names)

# True β_k (W × treatment interaction) for each outcome
# Rows = β_1 (T1), β_2 (T2), β_3 (T3), β_4 (T4)
# Cols = home, mor, non, mc, rank, turn, vote
# Interpretation: positive β_k means W > 0 (good news) increases approval,
#   W < 0 (bad news) decreases approval — i.e., treatment amplifies updating
true_betas <- matrix(
  c(
    # β_1 (plain info)             — modest effect on most outcomes
    3.0,
    1.0,
    1.0,
    0.5,
    2.0,
    1.0,
    0.5,
    # β_2 (non-partisan comparison) — amplifies plain info
    2.0,
    1.0,
    1.0,
    0.5,
    2.0,
    1.0,
    0.5,
    # β_3 (opposite-coalition)      — larger shift in coalition approvals
    2.0,
    4.0,
    -3.0,
    0.5,
    2.0,
    1.0,
    1.0,
    # β_4 (same-coalition)          — mirror of β_3 for coalition approvals
    2.0,
    -3.0,
    4.0,
    0.5,
    2.0,
    1.0,
    0.5
  ),
  nrow = 4,
  byrow = TRUE,
  dimnames = list(
    c("beta1", "beta2", "beta3", "beta4"),
    outcome_names
  )
)

# ── Simulate respondents ──────────────────────────────────────────────────────
arm <- sample(0:4, N, replace = TRUE) # 0 = control, 1 = T1 ... 4 = T4
T1 <- as.integer(arm == 1)
T2 <- as.integer(arm == 2)
T3 <- as.integer(arm == 3)
T4 <- as.integer(arm == 4)
W <- rnorm(N) # standardized wrongness

# Explicit interaction terms (avoids ambiguous coef names from lm)
WT1 <- W * T1
WT2 <- W * T2
WT3 <- W * T3
WT4 <- W * T4

# Small main effects (δ_k) and wrongness main effect (γ) — not of primary interest
gamma <- 2
deltas <- c(control = 0, T1 = 2, T2 = 2, T3 = 3, T4 = 2)

# Generate each outcome as a post–pre difference
sim_outcome <- function(betas, sigma = 15) {
  mu <- deltas["T1"] *
    T1 +
    deltas["T2"] * T2 +
    deltas["T3"] * T3 +
    deltas["T4"] * T4 +
    gamma * W +
    betas[1] * WT1 +
    betas[2] * WT2 +
    betas[3] * WT3 +
    betas[4] * WT4
  rnorm(N, mu, sigma)
}

d <- data.frame(T1, T2, T3, T4, W, WT1, WT2, WT3, WT4)
for (nm in outcome_names) {
  d[[paste0("delta_", nm)]] <- sim_outcome(true_betas[, nm])
}

# ── Fit OLS models (Equation 2 from PAP) ─────────────────────────────────────
# ΔY_i = α + Σ δ_k T_ik + γ W_i + Σ β_k (W_i × T_ik) + ε_i
formula_rhs <- "T1 + T2 + T3 + T4 + W + WT1 + WT2 + WT3 + WT4"

models <- lapply(paste0("delta_", outcome_names), function(y) {
  lm(as.formula(paste(y, "~", formula_rhs)), data = d)
})
names(models) <- outcome_names

# ── Extract estimates and HC2 SEs for each RQ ────────────────────────────────
# RQ1: β_1 = 0           → coef "WT1"
# RQ2: β_2 − β_1 = 0     → WT2 − WT1
# RQ3: β_3 − β_2 = 0     → WT3 − WT2
# RQ4: β_4 − β_3 = 0     → WT4 − WT3

extract_coef <- function(models, coef_name) {
  betas <- sapply(models, \(m) coef(m)[[coef_name]])
  ses <- sapply(models, function(m) {
    V <- vcovHC(m, type = "HC2")
    sqrt(V[coef_name, coef_name])
  })
  list(betas = betas, ses = ses)
}

extract_contrast <- function(models, c1, c2) {
  betas <- sapply(models, \(m) coef(m)[[c1]] - coef(m)[[c2]])
  ses <- sapply(models, function(m) {
    V <- vcovHC(m, type = "HC2")
    sqrt(V[c1, c1] + V[c2, c2] - 2 * V[c1, c2])
  })
  list(betas = betas, ses = ses)
}

rq_data <- list(
  RQ1 = extract_coef(models, "WT1"),
  RQ2 = extract_contrast(models, "WT2", "WT1"),
  RQ3 = extract_contrast(models, "WT3", "WT2"),
  RQ4 = extract_contrast(models, "WT4", "WT3")
)

# ── Apply ash per RQ ──────────────────────────────────────────────────────────
# ash pools information across the 7 outcomes for each RQ, fits a common
# unimodal prior on effect sizes, and returns posterior means + lfsr/svalue

ash_results <- lapply(rq_data, function(rq) {
  ash(rq$betas, rq$ses)
})

cat(
  "── ash results ──────────────────────────────────────────────────────────\n"
)
cat("   betahat     = raw OLS estimate\n")
cat("   post_mean   = shrunk posterior mean\n")
cat("   lfsr        = local false sign rate (per-observation)\n")
cat("   svalue      = cumulative FSR if thresholding at this observation\n\n")

for (rq in names(ash_results)) {
  fit <- ash_results[[rq]]
  cat(sprintf(
    "%s (true β: %s):\n",
    rq,
    paste(true_betas[which(names(rq_data) == rq), ], collapse = ", ")
  ))
  print(
    data.frame(
      outcome   = outcome_names,
      true_beta = true_betas[which(names(rq_data) == rq), ],
      betahat   = round(rq_data[[rq]]$betas, 3),
      post_mean = round(get_pm(fit), 3),
      lfsr      = round(get_lfsr(fit), 3),
      svalue    = round(fit$result$svalue, 3)
    ),
    row.names = FALSE
  )
  cat("\n")
}

# ── mashr: joint model across all RQs and outcomes ────────────────────────────
# mashr additionally learns which outcomes tend to move together, giving tighter
# posteriors when outcomes are correlated (same respondents answer all 7)

# Organize into matrices: rows = RQs, cols = outcomes
B_mat <- do.call(rbind, lapply(rq_data, \(rq) rq$betas))
S_mat <- do.call(rbind, lapply(rq_data, \(rq) rq$ses))
rownames(B_mat) <- rownames(S_mat) <- names(rq_data)
colnames(B_mat) <- colnames(S_mat) <- outcome_names

mash_data <- mash_set_data(B_mat, S_mat)

# Canonical covariances (identity + rank-1 structures)
# For real data, also add data-driven covariances via cov_pca() + cov_ed()
U_canon <- cov_canonical(mash_data)
mash_fit <- mash(mash_data, U_canon)

cat(
  "── mashr posterior means ────────────────────────────────────────────────\n"
)
print(round(get_pm(mash_fit), 3))

cat(
  "\n── mashr lfsr ───────────────────────────────────────────────────────────\n"
)
print(round(get_lfsr(mash_fit), 3))
