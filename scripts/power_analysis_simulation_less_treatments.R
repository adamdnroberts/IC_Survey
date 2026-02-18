library(dplyr)
library(purrr)
library(ggplot2)

set.seed(42)

n_reps <- 1000

simulate_power <- function(
  beta0 = 50,
  beta1 = 0,
  beta2 = 0,
  beta3 = 0,
  beta4 = 0,
  interact = 0,
  interact1 = 0,
  interact2 = 0,
  interact3 = 0,
  interact4 = 0,
  sd_error = 20,
  sample_n = 4000,
  n_groups = 5,
  coef_of_interest = "treatment_factor1",
  comparison_coef = "treatment_factor2"
) {
  # Move design matrix creation inside the function
  treatment <- rep(0:(n_groups - 1), floor(sample_n / n_groups))
  treatment_factor <- factor(treatment)
  n_obs <- length(treatment)

  p_values <- numeric(n_reps)
  r2_values <- numeric(n_reps)
  # coef_diffs <- numeric(n_reps)
  # p_values_diff <- numeric(n_reps)
  sd_outcomes <- numeric(n_reps)
  sd_interact <- numeric(n_reps)

  for (i in 1:n_reps) {
    interact_var <- pmin(
      4,
      pmax(-4, rnorm(n_obs, mean = 0, sd = 1))
    )
    X <- model.matrix(~ treatment_factor * interact_var)

    y_raw <- beta0 +
      beta1 * X[, "treatment_factor1"] +
      beta2 * X[, "treatment_factor2"] +
      beta3 * X[, "treatment_factor3"] +
      beta4 * X[, "treatment_factor4"] +
      interact * X[, "interact_var"] +
      interact1 * X[, "treatment_factor1:interact_var"] +
      interact2 * X[, "treatment_factor2:interact_var"] +
      interact3 * X[, "treatment_factor3:interact_var"] +
      interact4 * X[, "treatment_factor4:interact_var"] +
      rnorm(n_obs, mean = 0, sd = sd_error)

    y <- pmin(100, pmax(0, round(y_raw)))

    sd_outcomes[i] <- sd(y)
    sd_interact[i] <- sd(interact_var)

    # linear model
    fit <- lm.fit(X, y)
    residuals <- fit$residuals
    rss <- sum(residuals^2)
    tss <- sum((y - mean(y))^2)
    r2_values[i] <- 1 - rss / tss

    df_resid <- length(y) - fit$rank
    sigma2 <- rss / df_resid
    XtX_inv <- chol2inv(fit$qr$qr[1:fit$rank, 1:fit$rank, drop = FALSE])
    se <- sqrt(diag(XtX_inv) * sigma2)

    idx1 <- which(colnames(X) == coef_of_interest)
    t_stat <- fit$coefficients[idx1] / se[idx1]
    p_values[i] <- 2 * pt(abs(t_stat), df_resid, lower.tail = FALSE)

    # idx2 <- which(colnames(X) == comparison_coef)
    #
    # coef_diff <- coef_diffs[i] <- fit$coefficients[idx1] -
    #   fit$coefficients[idx2]
    # se_diff <- sqrt(
    #   XtX_inv[idx1, idx1] *
    #     sigma2 +
    #     XtX_inv[idx2, idx2] * sigma2 -
    #     2 * XtX_inv[idx1, idx2] * sigma2
    # )
    # t_stat_diff <- coef_diff / se_diff
    # p_values_diff[i] <- 2 * pt(abs(t_stat_diff), df_resid, lower.tail = FALSE)
  }

  result <- tibble(
    interact1 = interact1,
    sd_error = sd_error,
    sample_n = sample_n,
    power = mean(p_values < 0.05),
    mean_r2 = mean(r2_values),
    # mean_coef_diff = mean(coef_diffs),
    # coef_diff_power = mean(p_values_diff < 0.05),
    mean_sd_outcome = mean(sd_outcomes),
    sd_interact_var = mean(sd_interact)
  )
}

# Update simulation grid
params <- expand.grid(
  interact1 = c(4, 5, 6),
  sample_n = seq(500, 4500, by = 100) # Add sample sizes
)

power_results <- pmap_dfr(
  params,
  ~ simulate_power(
    interact1 = ..1,
    sample_n = ..2,
    coef_of_interest = "treatment_factor1:interact_var"
  )
)

power_results$cohens_d_raw <- power_results$interact1 *
  power_results$sd_interact_var /
  power_results$mean_sd_outcome
power_results$cohens_d <- round(power_results$cohens_d_raw * 20) / 20

crossings_precise <- power_results %>%
  group_by(interact1) %>%
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
    # Linear interpolation
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
  geom_text(
    data = crossings_precise,
    aes(x = n_at_80, y = power_80, label = sprintf("%.2f", n_at_80)),
    nudge_y = 0.05,
    size = 3,
    show.legend = FALSE
  ) +
  #facet_wrap(~cohens_d) +
  labs(
    color = "Interaction Effect Size",
    x = "Total Sample Size",
    y = "Power"
  ) +
  theme_classic()

print(power_line_graph)

ggsave(
  "docs/power_graph.pdf",
  plot = power_line_graph,
  width = 6,
  height = 4
)
