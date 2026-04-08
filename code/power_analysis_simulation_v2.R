library(dplyr)
library(purrr)
library(ggplot2)
library(ggrepel)

set.seed(42)

n_reps <- 1000
completion_rate <- 0.91 # expected survey completion rate; adjust for your attrition estimate
n_primary_tests <- 5 # number of interaction hypotheses tested (Bonferroni denominator)
# NOTE: sd_error = 20 is a key sensitivity assumption. If true residual SD is higher
# (e.g., 25–30 on a 0–100 scale), required N increases substantially.

# Power analysis for the estimating equation:
#
#   y_i = α0 + α1 CG_i + α2 RG_i
#           + Σ δ_k T_ik
#           + Σ β_k (T_ik × CG_i)
#           + Σ γ_k (T_ik × RG_i)
#           + ε_i
#
# CG_i ~ N(0, sd_cw): crime wrongness
# RG_i ~ rounded N(0, sd_rw), clamped to integers in [-4, 4]: relative ranking wrongness
# Treatment arms: control, control2, T1, T2, T3, T4 (6 groups)
# Power computed for a single coefficient of interest (default: T2 x RG)

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
  coef_of_interest = "treatment_factor2:RG",
  param_value = 0
) {
  treatment <- rep(
    0:(n_groups - 1),
    times = c(
      floor(sample_n / 5),
      floor(sample_n / 10),
      floor(sample_n / 10),
      floor(sample_n / 5),
      floor(sample_n / 5),
      floor(sample_n / 5)
    )
  )
  treatment_factor <- factor(treatment)
  n_obs <- length(treatment)

  p_values <- numeric(n_reps)
  r2_values <- numeric(n_reps)
  sd_outcomes <- numeric(n_reps)
  sd_RG_vals <- numeric(n_reps)
  sd_CG_vals <- numeric(n_reps)

  for (i in 1:n_reps) {
    CG <- rnorm(n_obs, mean = 0, sd = sd_cw)
    RG <- pmin(4L, pmax(-4L, round(rnorm(n_obs, mean = 0, sd = sd_rw))))

    X <- model.matrix(~ treatment_factor * CG + treatment_factor * RG)

    y_raw <- alpha0 +
      alpha1 * CG +
      alpha2 * RG +
      delta[1] * (treatment_factor == 1) +
      delta[2] * (treatment_factor == 2) +
      delta[3] * (treatment_factor == 3) +
      delta[4] * (treatment_factor == 4) +
      beta[1] * (treatment_factor == 1) * CG +
      beta[2] * (treatment_factor == 2) * CG +
      beta[3] * (treatment_factor == 3) * CG +
      beta[4] * (treatment_factor == 4) * CG +
      gamma[1] * (treatment_factor == 1) * RG +
      gamma[2] * (treatment_factor == 2) * RG +
      gamma[3] * (treatment_factor == 3) * RG +
      gamma[4] * (treatment_factor == 4) * RG +
      rnorm(n_obs, mean = 0, sd = sd_error)

    y <- pmin(100, pmax(0, round(y_raw)))

    sd_outcomes[i] <- sd(y)
    sd_RG_vals[i] <- sd(RG) # Fixed: original incorrectly used sd(CG) here
    sd_CG_vals[i] <- sd(CG)

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
    effect_param = param_value,
    sd_error = sd_error,
    alpha_level = alpha_level,
    sample_n = sample_n,
    power = mean(p_values < alpha_level),
    mean_r2 = mean(r2_values),
    mean_sd_outcome = mean(sd_outcomes),
    mean_sd_rw = mean(sd_RG_vals),
    mean_sd_cw = mean(sd_CG_vals)
  )
}

# ── Simulation grid ───────────────────────────────────────────────────────────
# Fixed sample size (Wave 2 retained); vary effect size and alpha level.

fixed_n <- 2000
bonferroni_alpha <- round(0.05 / n_primary_tests, 4)

# ── T1 (Plain Info) × CG simulation ──────────────────────────────────────────

params_t1cw <- expand.grid(
  beta_t1 = seq(0.5, 7, by = 0.2),
  alpha_level = c(0.05, bonferroni_alpha)
)

n_sims_t1cw <- nrow(params_t1cw)
total_reps_t1cw <- n_sims_t1cw * n_reps
done_reps <- 0

power_list_t1cw <- vector("list", n_sims_t1cw)
for (j in seq_len(n_sims_t1cw)) {
  power_list_t1cw[[j]] <- simulate_power(
    beta = c(0, params_t1cw$beta_t1[j], 0, 0),
    sample_n = fixed_n,
    alpha_level = params_t1cw$alpha_level[j],
    coef_of_interest = "treatment_factor2:CG",
    param_value = params_t1cw$beta_t1[j]
  )
  done_reps <- done_reps + n_reps
  cat(sprintf(
    "\r  T1×CG: %d%% complete  (%d / %d reps)",
    round(100 * done_reps / total_reps_t1cw),
    done_reps,
    total_reps_t1cw
  ))
}
cat("\n")
power_results_t1cw <- bind_rows(power_list_t1cw)

power_results_t1cw <- power_results_t1cw %>%
  mutate(
    cohens_d = effect_param * mean_sd_cw / mean_sd_outcome,
    alpha_label = ifelse(
      alpha_level == 0.05,
      "α = 0.05 (uncorrected)",
      sprintf("α = %.4f (Bonferroni, %d tests)", alpha_level, n_primary_tests)
    )
  )

mde_t1cw <- power_results_t1cw %>%
  group_by(alpha_level, alpha_label) %>%
  arrange(cohens_d) %>%
  mutate(
    next_power = lead(power),
    next_d = lead(cohens_d)
  ) %>%
  filter(power < 0.8 & next_power >= 0.8) %>%
  mutate(
    mde_d = cohens_d +
      (0.8 - power) * (next_d - cohens_d) / (next_power - power)
  ) %>%
  slice(1) %>%
  ungroup() %>%
  mutate(power_80 = 0.8)

power_line_graph_t1cw <- ggplot(
  power_results_t1cw,
  aes(x = cohens_d, y = power, color = alpha_label, linetype = alpha_label)
) +
  geom_line(linewidth = 0.8) +
  geom_hline(yintercept = 0.8, linetype = "dashed", color = "grey40") +
  geom_point(
    data = mde_t1cw,
    aes(x = mde_d, y = power_80),
    size = 3,
    shape = 21,
    fill = "white",
    show.legend = FALSE
  ) +
  geom_text_repel(
    data = mde_t1cw,
    aes(x = mde_d, y = power_80, label = sprintf("MDE = %.2f", mde_d)),
    nudge_y = 0.06,
    size = 3,
    show.legend = FALSE
  ) +
  scale_x_continuous(breaks = seq(0, 0.5, by = 0.05)) +
  scale_y_continuous(breaks = seq(0, 1, by = 0.1), limits = c(0, 1)) +
  labs(
    color = NULL,
    linetype = NULL,
    x = "Effect Size (Cohen's d)",
    y = "Power",
    title = sprintf(
      "Power analysis: T1 (Plain Info) × CG interaction (N = %s Wave 2 completions)",
      format(fixed_n, big.mark = ",")
    ),
    caption = sprintf(
      "N = %s corresponds to %.0f%% recontact rate from %s Wave 1 respondents. Bonferroni for %d interaction tests.",
      format(fixed_n, big.mark = ","),
      100 * fixed_n / 2180,
      format(2180, big.mark = ","),
      n_primary_tests
    )
  ) +
  theme_classic() +
  theme(legend.position = "bottom")

print(power_line_graph_t1cw)

ggsave(
  "latex/images/power_graph_t1_cw.pdf",
  plot = power_line_graph_t1cw,
  width = 10,
  height = 4.5
)

# ── T2 (Non-partisan) × RG simulation ────────────────────────────────────────

params_t2rw <- expand.grid(
  gamma_t2np = seq(0.5, 7, by = 0.2),
  alpha_level = c(0.05, bonferroni_alpha)
)

n_sims_t2rw <- nrow(params_t2rw)
total_reps_t2rw <- n_sims_t2rw * n_reps
done_reps <- 0

power_list_t2rw <- vector("list", n_sims_t2rw)
for (j in seq_len(n_sims_t2rw)) {
  power_list_t2rw[[j]] <- simulate_power(
    gamma = c(0, 0, params_t2rw$gamma_t2np[j], 0),
    sample_n = fixed_n,
    alpha_level = params_t2rw$alpha_level[j],
    coef_of_interest = "treatment_factor3:RG",
    param_value = params_t2rw$gamma_t2np[j]
  )
  done_reps <- done_reps + n_reps
  cat(sprintf(
    "\r  T2×RG: %d%% complete  (%d / %d reps)",
    round(100 * done_reps / total_reps_t2rw),
    done_reps,
    total_reps_t2rw
  ))
}
cat("\n")
power_results_t2rw <- bind_rows(power_list_t2rw)

power_results_t2rw <- power_results_t2rw %>%
  mutate(
    cohens_d = effect_param * mean_sd_rw / mean_sd_outcome,
    alpha_label = ifelse(
      alpha_level == 0.05,
      "α = 0.05 (uncorrected)",
      sprintf("α = %.4f (Bonferroni, %d tests)", alpha_level, n_primary_tests)
    )
  )

mde_t2rw <- power_results_t2rw %>%
  group_by(alpha_level, alpha_label) %>%
  arrange(cohens_d) %>%
  mutate(
    next_power = lead(power),
    next_d = lead(cohens_d)
  ) %>%
  filter(power < 0.8 & next_power >= 0.8) %>%
  mutate(
    mde_d = cohens_d +
      (0.8 - power) * (next_d - cohens_d) / (next_power - power)
  ) %>%
  slice(1) %>%
  ungroup() %>%
  mutate(power_80 = 0.8)

power_line_graph_t2rw <- ggplot(
  power_results_t2rw,
  aes(x = cohens_d, y = power, color = alpha_label, linetype = alpha_label)
) +
  geom_line(linewidth = 0.8) +
  geom_hline(yintercept = 0.8, linetype = "dashed", color = "grey40") +
  geom_point(
    data = mde_t2rw,
    aes(x = mde_d, y = power_80),
    size = 3,
    shape = 21,
    fill = "white",
    show.legend = FALSE
  ) +
  geom_text_repel(
    data = mde_t2rw,
    aes(x = mde_d, y = power_80, label = sprintf("MDE = %.2f", mde_d)),
    nudge_y = 0.06,
    size = 3,
    show.legend = FALSE
  ) +
  scale_x_continuous(breaks = seq(0, 0.5, by = 0.05)) +
  scale_y_continuous(breaks = seq(0, 1, by = 0.1), limits = c(0, 1)) +
  labs(
    color = NULL,
    linetype = NULL,
    x = "Effect Size (Cohen's d)",
    y = "Power",
    title = sprintf(
      "Power analysis: T2 (Non-partisan) × RG interaction (N = %s Wave 2 completions)",
      format(fixed_n, big.mark = ",")
    ),
    caption = sprintf(
      "N = %s corresponds to %.0f%% recontact rate from %s Wave 1 respondents. Bonferroni for %d interaction tests.",
      format(fixed_n, big.mark = ","),
      100 * fixed_n / 2180,
      format(2180, big.mark = ","),
      n_primary_tests
    )
  ) +
  theme_classic() +
  theme(legend.position = "bottom")

print(power_line_graph_t2rw)

ggsave(
  "latex/images/power_graph_t2_rw.pdf",
  plot = power_line_graph_t2rw,
  width = 10,
  height = 4.5
)
