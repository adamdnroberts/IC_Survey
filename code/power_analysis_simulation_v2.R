library(dplyr)
library(purrr)
library(ggplot2)
library(ggrepel)

set.seed(42)

n_reps           <- 1000
completion_rate  <- 0.75   # expected survey completion rate; adjust for your attrition estimate
n_primary_tests  <- 4      # number of interaction hypotheses tested (Bonferroni denominator)
# NOTE: sd_error = 20 is a key sensitivity assumption. If true residual SD is higher
# (e.g., 25–30 on a 0–100 scale), required N increases substantially.

# Power analysis for the estimating equation:
#
#   y_i = α0 + α1 CW_i + α2 RW_i
#           + Σ δ_k T_ik
#           + Σ β_k (T_ik × CW_i)
#           + Σ γ_k (T_ik × RW_i)
#           + ε_i
#
# CW_i ~ N(0, sd_cw): crime wrongness
# RW_i ~ rounded N(0, sd_rw), clamped to integers in [-4, 4]: relative ranking wrongness
# Treatment arms: control, control2, T1, T2, T3, T4 (6 groups)
# Power computed for a single coefficient of interest (default: T2 x RW)

simulate_power <- function(
  alpha0 = 50,
  alpha1 = 0,
  alpha2 = 0,
  delta = c(0, 0, 0, 0),
  beta = c(0, 0, 0, 0),
  gamma = c(0, 0, 0, 0),
  sd_cw = 1.5,
  sd_rw = 1.5,
  sd_error = 20,
  sample_n = 4000,
  n_groups = 6,
  alpha_level = 0.05,
  coef_of_interest = "treatment_factor2:RW"
) {
  treatment <- rep(0:(n_groups - 1), floor(sample_n / n_groups))
  treatment_factor <- factor(treatment)
  n_obs <- length(treatment)

  p_values   <- numeric(n_reps)
  r2_values  <- numeric(n_reps)
  sd_outcomes <- numeric(n_reps)
  sd_RW_vals <- numeric(n_reps)

  for (i in 1:n_reps) {
    CW <- rnorm(n_obs, mean = 0, sd = sd_cw)
    RW <- pmin(4L, pmax(-4L, round(rnorm(n_obs, mean = 0, sd = sd_rw))))

    X <- model.matrix(~ treatment_factor * CW + treatment_factor * RW)

    y_raw <- alpha0 +
      alpha1 * CW +
      alpha2 * RW +
      delta[1] * (treatment_factor == 1) +
      delta[2] * (treatment_factor == 2) +
      delta[3] * (treatment_factor == 3) +
      delta[4] * (treatment_factor == 4) +
      beta[1] * (treatment_factor == 1) * CW +
      beta[2] * (treatment_factor == 2) * CW +
      beta[3] * (treatment_factor == 3) * CW +
      beta[4] * (treatment_factor == 4) * CW +
      gamma[1] * (treatment_factor == 1) * RW +
      gamma[2] * (treatment_factor == 2) * RW +
      gamma[3] * (treatment_factor == 3) * RW +
      gamma[4] * (treatment_factor == 4) * RW +
      rnorm(n_obs, mean = 0, sd = sd_error)

    y <- pmin(100, pmax(0, round(y_raw)))

    sd_outcomes[i] <- sd(y)
    sd_RW_vals[i]  <- sd(RW)   # Fixed: original incorrectly used sd(CW) here

    fit <- lm.fit(X, y)
    residuals <- fit$residuals
    rss <- sum(residuals^2)
    tss <- sum((y - mean(y))^2)
    r2_values[i] <- 1 - rss / tss

    df_resid <- length(y) - fit$rank
    sigma2   <- rss / df_resid
    XtX_inv  <- chol2inv(fit$qr$qr[1:fit$rank, 1:fit$rank, drop = FALSE])
    se       <- sqrt(diag(XtX_inv) * sigma2)

    idx    <- which(colnames(X) == coef_of_interest)
    t_stat <- fit$coefficients[idx] / se[idx]
    p_values[i] <- 2 * pt(abs(t_stat), df_resid, lower.tail = FALSE)
  }

  tibble(
    gamma_t2_rw     = gamma[2],
    sd_error        = sd_error,
    alpha_level     = alpha_level,
    sample_n        = sample_n,
    power           = mean(p_values < alpha_level),
    mean_r2         = mean(r2_values),
    mean_sd_outcome = mean(sd_outcomes),
    sd_RW           = mean(sd_RW_vals)
  )
}

# ── Simulation grid ───────────────────────────────────────────────────────────
# Vary effect size, sample size, and alpha level (uncorrected vs Bonferroni).
# Upper bound on sample_n extended to 6000 so Bonferroni curves reach 80%.

bonferroni_alpha <- round(0.05 / n_primary_tests, 4)

params <- expand.grid(
  gamma_t2    = c(2, 3, 4),
  sample_n    = seq(500, 6000, by = 100),
  alpha_level = c(0.05, bonferroni_alpha)
)

n_sims     <- nrow(params)
total_reps <- n_sims * n_reps
done_reps  <- 0

power_list <- vector("list", n_sims)
for (j in seq_len(n_sims)) {
  power_list[[j]] <- simulate_power(
    gamma       = c(0, params$gamma_t2[j], 0, 0),
    sample_n    = params$sample_n[j],
    alpha_level = params$alpha_level[j],
    coef_of_interest = "treatment_factor2:RW"
  )
  done_reps <- done_reps + n_reps
  cat(sprintf("\r  %d%% complete  (%d / %d reps)",
              round(100 * done_reps / total_reps),
              done_reps, total_reps))
}
cat("\n")
power_results <- bind_rows(power_list)

# Cohen's d approximation for the T2 x RW interaction (using corrected sd_RW)
power_results <- power_results %>%
  mutate(
    cohens_d_raw = gamma_t2_rw * sd_RW / mean_sd_outcome,
    cohens_d     = round(cohens_d_raw * 20) / 20,
    alpha_label  = ifelse(
      alpha_level == 0.05,
      "α = 0.05 (uncorrected)",
      sprintf("α = %.4f (Bonferroni, %d tests)", alpha_level, n_primary_tests)
    )
  )

# ── 80% power crossings ───────────────────────────────────────────────────────

crossings <- power_results %>%
  group_by(gamma_t2_rw, alpha_level, alpha_label) %>%
  arrange(sample_n) %>%
  mutate(
    next_power = lead(power),
    next_n     = lead(sample_n)
  ) %>%
  filter(
    (power < 0.8 & next_power >= 0.8) |
    (power > 0.8 & next_power <= 0.8)
  ) %>%
  mutate(
    n_at_80       = sample_n + (0.8 - power) * (next_n - sample_n) / (next_power - power),
    recruitment_n = ceiling(n_at_80 / completion_rate)
  ) %>%
  slice(1) %>%
  ungroup() %>%
  mutate(power_80 = 0.8)

# ── Plot ──────────────────────────────────────────────────────────────────────

power_line_graph <- ggplot(
  power_results,
  aes(x = sample_n, y = power, color = as.factor(cohens_d))
) +
  geom_line() +
  geom_hline(yintercept = 0.8, linetype = "dashed", color = "red") +
  geom_point(
    data = crossings,
    aes(x = n_at_80, y = power_80),
    size = 3, shape = 21, fill = "white"
  ) +
  geom_text_repel(
    data = crossings,
    aes(
      x = n_at_80, y = power_80,
      label = sprintf("N = %.0f\n(recruit %.0f)", n_at_80, recruitment_n)
    ),
    nudge_y = 0.06, size = 2.8, show.legend = FALSE
  ) +
  facet_wrap(~ alpha_label) +
  labs(
    color   = "Cohen's d (T2 × RW)",
    x       = "Completed Responses (Analysis N)",
    y       = "Power",
    title   = "Power analysis: T2 × RW interaction (gamma_2)",
    caption = sprintf(
      "Recruitment N assumes %.0f%% completion rate. Bonferroni for %d interaction tests.",
      completion_rate * 100, n_primary_tests
    )
  ) +
  theme_classic() +
  theme(strip.background = element_blank(), strip.text = element_text(size = 9))

print(power_line_graph)

ggsave(
  "latex/images/power_graph_v2.pdf",
  plot   = power_line_graph,
  width  = 10,
  height = 4.5
)
