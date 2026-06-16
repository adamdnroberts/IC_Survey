library(dplyr)
library(purrr)
library(ggplot2)
library(ggrepel)
library(MASS)

set.seed(42)

n_reps <- 1000

# Power analysis for the crime importance heterogeneity analysis.
# Model: y ~ T + W + T*W + C + T*C
# where W is a pre-treatment covariate and C is standardized crime importance
# (derived from respondents' ranking of crime among n_issues political issues).
# Coefficient of interest: phi_k (T_k * C interaction).
simulate_power <- function(
  beta0 = 50,
  delta1 = 0,
  delta2 = 0,
  delta3 = 0,
  delta4 = 0, # treatment main effects
  gamma = 0, # W main effect
  beta_w1 = 0,
  beta_w2 = 0,
  beta_w3 = 0,
  beta_w4 = 0, # W x T
  theta = 0, # C main effect
  phi1 = 0,
  phi2 = 0,
  phi3 = 0,
  phi4 = 0, # C x T (of interest)
  wc_cor = 0.5, # correlation between W and C
  n_issues = 5,
  sd_error = 20,
  sample_n = 2000,
  n_groups = 5,
  coef_of_interest = "treatment_factor1:C_i"
) {
  treatment <- rep(0:(n_groups - 1), floor(sample_n / n_groups))
  treatment_factor <- factor(treatment)
  n_obs <- length(treatment)

  p_values <- numeric(n_reps)
  r2_values <- numeric(n_reps)
  sd_outcomes <- numeric(n_reps)
  sd_C_vals <- numeric(n_reps)

  sigma <- matrix(c(1, wc_cor, wc_cor, 1), nrow = 2)

  for (i in 1:n_reps) {
    # Draw W and C from a bivariate normal with moderate correlation
    draws <- mvrnorm(n_obs, mu = c(0, 0), Sigma = sigma)
    W_i <- pmin(4, pmax(-4, draws[, 1]))

    # Discretize C latent variable into n_issues rank categories, then standardize
    C_raw <- cut(
      draws[, 2],
      breaks = quantile(
        draws[, 2],
        probs = seq(0, 1, length.out = n_issues + 1)
      ),
      labels = 1:n_issues,
      include.lowest = TRUE
    )
    C_i <- c(scale(as.numeric(C_raw)))

    # Model matrix: intercept, T1-T4, W, C, T1:W-T4:W, T1:C-T4:C (15 columns)
    X <- model.matrix(~ treatment_factor * W_i + treatment_factor * C_i)

    true_coefs <- c(
      beta0,
      delta1,
      delta2,
      delta3,
      delta4,
      gamma,
      theta,
      beta_w1,
      beta_w2,
      beta_w3,
      beta_w4,
      phi1,
      phi2,
      phi3,
      phi4
    )

    y_raw <- X %*% true_coefs + rnorm(n_obs, mean = 0, sd = sd_error)
    y <- pmin(100, pmax(0, round(y_raw)))

    sd_outcomes[i] <- sd(y)
    sd_C_vals[i] <- sd(C_i)

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
    phi1 = phi1,
    sd_error = sd_error,
    sample_n = sample_n,
    power = mean(p_values < 0.05),
    mean_r2 = mean(r2_values),
    mean_sd_outcome = mean(sd_outcomes),
    sd_C = mean(sd_C_vals)
  )
}

params <- expand.grid(
  phi1 = c(4, 5, 6),
  sample_n = seq(500, 3000, by = 100)
)

power_results <- pmap_dfr(
  params,
  ~ simulate_power(
    phi1 = ..1,
    sample_n = ..2,
    coef_of_interest = "treatment_factor1:C_i"
  )
)

power_results$cohens_d_raw <- power_results$phi1 *
  power_results$sd_C /
  power_results$mean_sd_outcome
power_results$cohens_d <- round(power_results$cohens_d_raw * 20) / 20

crossings_precise <- power_results %>%
  group_by(phi1) %>%
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

power_line_graph <- ggplot(
  power_results,
  aes(x = sample_n, y = power, color = as.factor(cohens_d))
) +
  geom_line() +
  geom_hline(yintercept = 0.8, linetype = "dashed", color = "red") +
  geom_point(
    data = crossings_precise,
    aes(x = n_at_80, y = power_80),
    size = 3,
    shape = 21,
    fill = "white"
  ) +
  geom_text_repel(
    data = crossings_precise,
    aes(x = n_at_80, y = power_80, label = sprintf("N = %.0f", n_at_80)),
    nudge_y = 0.05,
    size = 3,
    show.legend = FALSE
  ) +
  labs(
    color = "Interaction Effect Size\n(Cohen's d)",
    x = "Total Sample Size",
    y = "Power"
  ) +
  theme_classic()

print(power_line_graph)

ggsave(
  "C:/Users/adamd/Documents/IC_Survey/latex/68f2c388f14090ff511a63c6/images/power_graph_crime_importance.pdf",
  plot = power_line_graph,
  width = 6,
  height = 4
)
