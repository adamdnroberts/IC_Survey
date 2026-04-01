library(dplyr)
library(purrr)
library(ggplot2)
library(ggrepel)

set.seed(42)

n_reps <- 1000

# Power analysis for the estimating equation:
#
#   y_i = α0 + α1 CW_i + α2 RW_i
#           + Σ δ_k T_ik
#           + Σ β_k (T_ik × CW_i)
#           + Σ γ_k (T_ik × RW_i)
#           + ε_i
#
# CW_i ~ U(-4, 4): crime wrongness (actual minus prior crime rank, range -4 to 4)
# RW_i ~ U(-4, 4): relative ranking wrongness
# Treatment arms: control (baseline), T1, T2, T3, T4, control2
# Power is computed for a single coefficient of interest (default: T1 x CW)

simulate_power <- function(
  alpha0 = 50, # intercept
  alpha1 = 0, # main effect of CW (control group)
  alpha2 = 0, # main effect of RW (control group)
  delta = c(0, 0, 0, 0), # main effects of T1–T4
  beta = c(0, 0, 0, 0), # T_k x CW interaction coefficients
  gamma = c(0, 0, 0, 0), # T_k x RW interaction coefficients
  sd_cw = 1.5, # SD of CW ~ N(0, sd_cw); ~1–2 ranks off on a 1–5 scale
  sd_rw = 1.5, # SD of RW ~ rounded N(0, sd_rw), clamped to integers in [-4, 4]
  sd_error = 20,
  sample_n = 4000,
  n_groups = 6,
  coef_of_interest = "treatment_factor1:CW"
) {
  treatment <- rep(0:(n_groups - 1), floor(sample_n / n_groups))
  treatment_factor <- factor(treatment)
  n_obs <- length(treatment)

  p_values <- numeric(n_reps)
  r2_values <- numeric(n_reps)
  sd_outcomes <- numeric(n_reps)
  sd_RW <- numeric(n_reps)

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
    sd_RW[i] <- sd(CW)

    fit <- lm.fit(X, y)
    residuals <- fit$residuals
    rss <- sum(residuals^2)
    tss <- sum((y - mean(y))^2)
    r2_values[i] <- 1 - rss / tss

    df_resid <- length(y) - fit$rank
    sigma2 <- rss / df_resid
    XtX_inv <- chol2inv(fit$qr$qr[1:fit$rank, 1:fit$rank, drop = FALSE])
    se <- sqrt(diag(XtX_inv) * sigma2)

    idx <- which(colnames(X) == coef_of_interest)
    t_stat <- fit$coefficients[idx] / se[idx]
    p_values[i] <- 2 * pt(abs(t_stat), df_resid, lower.tail = FALSE)
  }

  tibble(
    gamma_t2_rw = gamma[2],
    sd_error = sd_error,
    sample_n = sample_n,
    power = mean(p_values < 0.05),
    mean_r2 = mean(r2_values),
    mean_sd_outcome = mean(sd_outcomes),
    sd_RW = mean(sd_RW)
  )
}

# ── Simulation grid ───────────────────────────────────────────────────────────
# Vary T2 x RW effect size and sample size; all other interactions set to zero

params <- expand.grid(
  gamma_t2 = c(2, 3, 4),
  sample_n = seq(500, 4500, by = 100)
)

n_sims     <- nrow(params)
total_reps <- n_sims * n_reps
done_reps  <- 0

power_list <- vector("list", n_sims)
for (j in seq_len(n_sims)) {
  power_list[[j]] <- simulate_power(
    gamma    = c(0, params$gamma_t2[j], 0, 0),
    sample_n = params$sample_n[j],
    coef_of_interest = "treatment_factor2:RW"
  )
  done_reps <- done_reps + n_reps
  cat(sprintf("\r  %d%% complete  (%d / %d reps)",
              round(100 * done_reps / total_reps),
              done_reps, total_reps))
}
cat("\n")
power_results <- bind_rows(power_list)

# Cohen's d approximation for the T2 x RW interaction
power_results <- power_results %>%
  mutate(
    cohens_d_raw = gamma_t2_rw * sd_RW / mean_sd_outcome,
    cohens_d = round(cohens_d_raw * 20) / 20
  )

# ── 80% power crossings ───────────────────────────────────────────────────────

crossings <- power_results %>%
  group_by(gamma_t2_rw) %>%
  arrange(sample_n) %>%
  mutate(
    next_power = lead(power),
    next_n = lead(sample_n)
  ) %>%
  filter(
    (power < 0.8 & next_power >= 0.8) |
      (power > 0.8 & next_power <= 0.8)
  ) %>%
  mutate(
    n_at_80 = sample_n +
      (0.8 - power) * (next_n - sample_n) / (next_power - power)
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
    size = 3,
    shape = 21,
    fill = "white"
  ) +
  geom_text_repel(
    data = crossings,
    aes(x = n_at_80, y = power_80, label = sprintf("N = %.0f", n_at_80)),
    nudge_y = 0.05,
    size = 3,
    show.legend = FALSE
  ) +
  labs(
    color = "Cohen's d (T2 x RW)",
    x = "Total Sample Size",
    y = "Power",
    title = "Power analysis: T2 x RW interaction (gamma_2)"
  ) +
  theme_classic()

print(power_line_graph)

ggsave(
  "latex/images/power_graph.pdf",
  plot = power_line_graph,
  width = 6,
  height = 4
)
