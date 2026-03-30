# =============================================================================
# Benchmarking Models: Simulation and Comparison
# Based on: Arel-Bundock, Blais & Dassonneville (2021)
# "Do Voters Benchmark Economic Performance?" BJPS 51: 437-449
# =============================================================================
#
# This script demonstrates the algebraic equivalence of Models 3 and 5 from
# the paper, and shows how misinterpreting Model 3 leads to false conclusions
# about benchmarking vs. conventional economic voting.
#
# The two models:
#   Model 3 (KP spec):  V = θ(Gy-Gi) + θi*Gi + controls + ε
#   Model 5 (ABD spec): V = δy*Gy   + δi*Gi + controls + ε
#
# Key algebraic identities (proven in the paper):
#   δy  ≡ θ(y-i)          [local growth coef = marginal effect of domestic growth]
#   δi  ≡ θi - θ(y-i)     [marginal effect of benchmark]
# =============================================================================

library(car) # for linearHypothesis (Wald test)

set.seed(42)

# =============================================================================
# SECTION 1: DATA-GENERATING PROCESSES
# =============================================================================
# We simulate two worlds:
#   (A) Conventional voting only: V responds to Gy but NOT to Gi
#   (B) Benchmarking:             V responds to Gy AND negatively to Gi
# =============================================================================

n <- 500 # number of country-elections

simulate_data <- function(delta_y, delta_i, n = 500, seed = 42) {
  set.seed(seed)

  # Gi: international/benchmark growth (e.g., OECD average)
  Gi <- rnorm(n, mean = 2.5, sd = 1.5)

  # Gy: domestic growth, correlated with Gi (integrated economies)
  # correlation ~0.6, mimicking real data
  Gy <- 0.6 * Gi + rnorm(n, mean = 1, sd = 1.5)

  # Control variable: coalition size (as in KP)
  coalition <- rnorm(n, mean = 0, sd = 1)

  # Incumbent vote share
  V <- 40 +
    delta_y * Gy +
    delta_i * Gi +
    (-2) * coalition +
    rnorm(n, mean = 0, sd = 5)

  data.frame(V, Gy, Gi, coalition, gap = Gy - Gi) # composite term used in Model 3
}

# World A: only conventional voting (delta_i = 0)
data_conventional <- simulate_data(delta_y = 1.5, delta_i = 0)

# World B: true benchmarking (delta_i < 0)
data_benchmarking <- simulate_data(delta_y = 1.5, delta_i = -1.2)


# =============================================================================
# SECTION 2: FIT MODELS 3 AND 5 AND DEMONSTRATE EQUIVALENCE
# =============================================================================

fit_and_compare <- function(data, world_label) {
  cat(
    "─────────────────────────────────────────────────────────────────────────\n"
  )
  cat("WORLD:", world_label, "\n")
  cat(
    "─────────────────────────────────────────────────────────────────────────\n\n"
  )

  # --- Model 1: Conventional (no benchmark) ---
  m1 <- lm(V ~ Gy + coalition, data = data)

  # --- Model 2: Powell & Whitten composite (cannot distinguish theories) ---
  m2 <- lm(V ~ gap + coalition, data = data)

  # --- Model 3: KP specification (composite + standalone benchmark) ---
  m3 <- lm(V ~ gap + Gi + coalition, data = data)

  # --- Model 5: ABD simpler specification ---
  m5 <- lm(V ~ Gy + Gi + coalition, data = data)

  # ── Model 1 results ──────────────────────────────────────────────────────
  cat("MODEL 1 — Conventional (no benchmark control)\n")
  cat("  V = βy*Gy + controls\n\n")
  s1 <- summary(m1)$coefficients
  cat(sprintf("  %-25s %8s %8s %8s\n", "Term", "Estimate", "SE", "p-value"))
  cat(sprintf(
    "  %-25s %8.3f %8.3f %8.3f\n",
    "Gy (domestic growth)",
    s1["Gy", "Estimate"],
    s1["Gy", "Std. Error"],
    s1["Gy", "Pr(>|t|)"]
  ))
  cat("\n")

  # ── Model 2 results ──────────────────────────────────────────────────────
  cat("MODEL 2 — Powell & Whitten composite (gap only)\n")
  cat("  V = λ*(Gy-Gi) + controls\n")
  cat("  Problem: cannot distinguish benchmarking from conventional voting\n\n")
  s2 <- summary(m2)$coefficients
  cat(sprintf("  %-25s %8s %8s %8s\n", "Term", "Estimate", "SE", "p-value"))
  cat(sprintf(
    "  %-25s %8.3f %8.3f %8.3f\n",
    "(Gy - Gi) composite",
    s2["gap", "Estimate"],
    s2["gap", "Std. Error"],
    s2["gap", "Pr(>|t|)"]
  ))
  cat("\n")

  # ── Model 3 results ──────────────────────────────────────────────────────
  cat("MODEL 3 — KP specification (composite + standalone Gi)\n")
  cat("  V = θ(y-i)*(Gy-Gi) + θi*Gi + controls\n\n")
  s3 <- summary(m3)$coefficients
  cat(sprintf("  %-25s %8s %8s %8s\n", "Term", "Estimate", "SE", "p-value"))
  cat(sprintf(
    "  %-25s %8.3f %8.3f %8.3f\n",
    "θ(y-i) [gap = Gy-Gi]",
    s3["gap", "Estimate"],
    s3["gap", "Std. Error"],
    s3["gap", "Pr(>|t|)"]
  ))
  cat(sprintf(
    "  %-25s %8.3f %8.3f %8.3f\n",
    "θi [standalone Gi]",
    s3["Gi", "Estimate"],
    s3["Gi", "Std. Error"],
    s3["Gi", "Pr(>|t|)"]
  ))
  cat("\n")

  cat("  *** COMMON MISINTERPRETATION ***\n")
  cat("  Many authors treat θ(y-i) as 'the effect of relative performance'.\n")
  cat(
    "  But ∂V/∂Gy = θ(y-i) — it is just the marginal effect of DOMESTIC growth!\n"
  )
  cat("  A positive θ(y-i) supports CONVENTIONAL voting, not benchmarking.\n\n")

  cat("  Correct test of benchmarking from Model 3:\n")
  cat("  H0: ∂V/∂Gi = 0, i.e. θi - θ(y-i) = 0\n")
  theta_yi <- coef(m3)["gap"]
  theta_i <- coef(m3)["Gi"]
  marginal_gi_m3 <- theta_i - theta_yi
  wald <- linearHypothesis(m3, "Gi - gap = 0")
  cat(sprintf(
    "  ∂V/∂Gi = θi - θ(y-i) = %.3f - %.3f = %.3f\n",
    theta_i,
    theta_yi,
    marginal_gi_m3
  ))
  cat(sprintf("  Wald test p-value: %.4f\n\n", wald$`Pr(>F)`[2]))

  # ── Model 5 results ──────────────────────────────────────────────────────
  cat("MODEL 5 — ABD simpler specification\n")
  cat("  V = δy*Gy + δi*Gi + controls\n\n")
  s5 <- summary(m5)$coefficients
  cat(sprintf("  %-25s %8s %8s %8s\n", "Term", "Estimate", "SE", "p-value"))
  cat(sprintf(
    "  %-25s %8.3f %8.3f %8.3f\n",
    "δy [domestic Gy]",
    s5["Gy", "Estimate"],
    s5["Gy", "Std. Error"],
    s5["Gy", "Pr(>|t|)"]
  ))
  cat(sprintf(
    "  %-25s %8.3f %8.3f %8.3f\n",
    "δi [benchmark Gi]",
    s5["Gi", "Estimate"],
    s5["Gi", "Std. Error"],
    s5["Gi", "Pr(>|t|)"]
  ))
  cat("\n")

  cat("  Test of benchmarking: H0: δi = 0\n")
  cat(sprintf(
    "  δi p-value: %.4f  → %s\n\n",
    s5["Gi", "Pr(>|t|)"],
    ifelse(
      s5["Gi", "Pr(>|t|)"] < 0.05,
      "REJECT null (benchmarking supported)",
      "FAIL TO REJECT null (no benchmarking)"
    )
  ))

  # ── Equivalence check ────────────────────────────────────────────────────
  cat("EQUIVALENCE CHECK: Models 3 and 5 are algebraically identical\n\n")

  delta_y_m5 <- coef(m5)["Gy"]
  delta_i_m5 <- coef(m5)["Gi"]

  cat(sprintf("  δy (Model 5)          = %8.6f\n", delta_y_m5))
  cat(sprintf("  θ(y-i) (Model 3)      = %8.6f\n", theta_yi))
  cat(sprintf(
    "  Difference            = %8.2e  ← should be ~0\n\n",
    delta_y_m5 - theta_yi
  ))

  cat(sprintf("  δi (Model 5)          = %8.6f\n", delta_i_m5))
  cat(sprintf("  θi-θ(y-i) (Model 3)   = %8.6f\n", marginal_gi_m3))
  cat(sprintf(
    "  Difference            = %8.2e  ← should be ~0\n\n",
    delta_i_m5 - marginal_gi_m3
  ))

  cat(sprintf("  R² Model 3            = %.6f\n", summary(m3)$r.squared))
  cat(sprintf("  R² Model 5            = %.6f\n", summary(m5)$r.squared))
  cat(sprintf(
    "  Residual sum identical: %s\n\n",
    isTRUE(all.equal(sum(resid(m3)^2), sum(resid(m5)^2)))
  ))

  invisible(list(m1 = m1, m2 = m2, m3 = m3, m5 = m5))
}

results_conv <- fit_and_compare(
  data_conventional,
  "CONVENTIONAL VOTING ONLY (δi = 0)"
)
results_bench <- fit_and_compare(
  data_benchmarking,
  "TRUE BENCHMARKING (δi = -1.2)"
)


# =============================================================================
# SECTION 3: MONTE CARLO — FALSE POSITIVE RATE OF THE MISINTERPRETATION
# =============================================================================
# How often does the INCORRECT interpretation of Model 3 (treating θ(y-i) as
# evidence of benchmarking) falsely support benchmarking in a world where only
# conventional voting exists?
# =============================================================================

cat(
  "=============================================================================\n"
)
cat("SECTION 3: MONTE CARLO SIMULATION (1000 runs)\n")
cat("World: conventional voting only (true δi = 0, true δy = 1.5)\n")
cat(
  "Question: how often does each approach correctly identify no benchmarking?\n"
)
cat(
  "=============================================================================\n\n"
)

n_sims <- 1000
results_mc <- data.frame(
  # Model 3 misinterpretation: p-value of θ(y-i) — WRONG test
  p_wrong = numeric(n_sims),
  # Model 3 correct Wald test: p-value of θi - θ(y-i) = 0 — CORRECT
  p_wald = numeric(n_sims),
  # Model 5: p-value of δi — CORRECT (and simpler)
  p_model5 = numeric(n_sims)
)

for (i in seq_len(n_sims)) {
  d <- simulate_data(delta_y = 1.5, delta_i = 0, n = 200, seed = i)
  m3 <- lm(V ~ gap + Gi + coalition, data = d)
  m5 <- lm(V ~ Gy + Gi + coalition, data = d)

  s3 <- summary(m3)$coefficients
  s5 <- summary(m5)$coefficients

  # WRONG: treating θ(y-i) as the benchmarking test
  results_mc$p_wrong[i] <- s3["gap", "Pr(>|t|)"]

  # CORRECT via Wald test on Model 3
  results_mc$p_wald[i] <- linearHypothesis(m3, "Gi - gap = 0")$`Pr(>F)`[2]

  # CORRECT via Model 5 (should be identical to Wald above)
  results_mc$p_model5[i] <- s5["Gi", "Pr(>|t|)"]
}

alpha <- 0.05
cat(sprintf("  False positive rate (α = %.2f):\n\n", alpha))
cat(sprintf(
  "  WRONG  — p(θ(y-i)) < α, misreads domestic growth as benchmarking:\n"
))
cat(sprintf(
  "           %.1f%%  (should be ~0%% in conventional-only world)\n\n",
  mean(results_mc$p_wrong < alpha) * 100
))
cat(sprintf("  CORRECT — Wald test [θi - θ(y-i) = 0] on Model 3:\n"))
cat(sprintf(
  "           %.1f%%  (should be ~%.0f%% = nominal α)\n\n",
  mean(results_mc$p_wald < alpha) * 100,
  alpha * 100
))
cat(sprintf("  CORRECT — δi in Model 5:\n"))
cat(sprintf(
  "           %.1f%%  (should be ~%.0f%% = nominal α)\n\n",
  mean(results_mc$p_model5 < alpha) * 100,
  alpha * 100
))


# =============================================================================
# SECTION 4: CONDITIONAL BENCHMARKING (Model 6 from the paper)
# =============================================================================
# Extension: benchmarking strength varies with a moderator M (e.g. trade openness)
#
# V = δy*Gy + δi*Gi + δym*(Gy×M) + δim*(Gi×M) + δm*M + controls + ε
# =============================================================================

cat(
  "=============================================================================\n"
)
cat("SECTION 4: CONDITIONAL BENCHMARKING (Model 6)\n")
cat("V = δy*Gy + δi*Gi + δym*(Gy×M) + δim*(Gi×M) + δm*M + controls + ε\n")
cat(
  "=============================================================================\n\n"
)

set.seed(123)
n <- 600

Gi <- rnorm(n, 2.5, 1.5)
Gy <- 0.6 * Gi + rnorm(n, 1, 1.5)
M <- runif(n, 0, 1) # moderator: e.g. trade openness [0,1]
coalition <- rnorm(n, 0, 1)

# True DGP: benchmarking only exists at high M
# δim = -2 means: at M=1, marginal effect of Gi = δi + δim*1 = 0 + (-2) = -2
V6 <- 40 +
  1.5 * Gy + # δy = 1.5
  0 * Gi + # δi = 0 (no benchmarking at M=0)
  0.5 * (Gy * M) + # δym = 0.5
  (-2) * (Gi * M) + # δim = -2 (benchmarking increases with M)
  1.0 * M + # δm
  (-2) * coalition +
  rnorm(n, 0, 5)

data6 <- data.frame(V6, Gy, Gi, M, coalition)

m6 <- lm(V6 ~ Gy + Gi + I(Gy * M) + I(Gi * M) + M + coalition, data = data6)
s6 <- summary(m6)$coefficients

cat("Estimated coefficients:\n\n")
cat(sprintf("  %-25s %8s %8s %8s\n", "Term", "Estimate", "SE", "p-value"))
rows <- c("Gy", "Gi", "I(Gy * M)", "I(Gi * M)", "M", "coalition", "(Intercept)")
labels <- c(
  "δy  [Gy]",
  "δi  [Gi]",
  "δym [Gy×M]",
  "δim [Gi×M]",
  "δm  [M]",
  "Coalition",
  "Intercept"
)
for (j in seq_along(rows)) {
  cat(sprintf(
    "  %-25s %8.3f %8.3f %8.3f\n",
    labels[j],
    s6[rows[j], "Estimate"],
    s6[rows[j], "Std. Error"],
    s6[rows[j], "Pr(>|t|)"]
  ))
}

delta_i_hat <- coef(m6)["Gi"]
delta_im_hat <- coef(m6)["I(Gi * M)"]

cat("\nMarginal effect of Gi at selected values of M:\n")
cat(sprintf("  %-10s %10s\n", "M value", "∂V/∂Gi"))
for (m_val in c(0, 0.25, 0.5, 0.75, 1.0)) {
  me <- delta_i_hat + delta_im_hat * m_val
  cat(sprintf("  %-10.2f %10.3f\n", m_val, me))
}
cat("\n  (True values: 0 at M=0, -2 at M=1)\n\n")
