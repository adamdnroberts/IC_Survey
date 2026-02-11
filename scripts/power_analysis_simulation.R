library(dplyr)
library(purrr)
library(ggplot2)

set.seed(42)

# simulation parameters
n_reps <- 200
sample_n <- 6000
n_groups <- 7
treatment <- rep(0:(n_groups - 1), floor(sample_n / n_groups))
# treatment_group <- factor(ifelse(
#   treatment <= 2,
#   "Woman",
#   ifelse(treatment <= 4, "Black", "Mormon")
# ))
# treatment_group <- relevel(treatment_group, ref = "Woman")
# loss <- ifelse(treatment %% 2 == 0, 1, 0)
interact <- rep(0:1, each = 7, length.out = length(treatment))

treatment_factor <- factor(treatment)

X <- model.matrix(~ treatment_factor * interact)

simulate_power <- function(
  beta0 = 50,
  beta1 = 0,
  beta2 = 0,
  beta3 = 1,
  beta4 = 1,
  beta5 = 1,
  beta6 = 1,
  interact = -1,
  sd_error = 20,
  coef_of_interest = "treatment_factor1",
  comparison_coef = "treatment_factor2"
) {
  p_values <- numeric(n_reps)
  r2_values <- numeric(n_reps)
  coef_diffs <- numeric(n_reps)
  p_values_diff <- numeric(n_reps)

  for (i in 1:n_reps) {
    if (i %% 200 == 0) {
      cat(
        "effect:",
        beta1,
        ", sd:",
        sd_error,
        ", rep:",
        i,
        "\n"
      )
    }

    # --- simulate data inline (replaces create_outcome)
    y_raw <- beta0 +
      beta1 * X[, "treatment_factor1"] +
      beta2 * X[, "treatment_factor2"] +
      beta3 * X[, "treatment_factor3"] +
      beta4 * X[, "treatment_factor4"] +
      beta5 * X[, "treatment_factor5"] +
      beta6 * X[, "treatment_factor6"] +
      interact * X[, "interact"] +
      beta1 * interact * X[, "treatment_factor1:interact"] +
      beta2 * interact * X[, "treatment_factor2:interact"] +
      beta3 * interact * X[, "treatment_factor3:interact"] +
      beta4 * interact * X[, "treatment_factor4:interact"] +
      beta5 * interact * X[, "treatment_factor5:interact"] +
      beta6 * interact * X[, "treatment_factor6:interact"] +
      rnorm(length(treatment), mean = 0, sd = sd_error)
    #bio_raw <- signal + error
    # transform to bounded outcome
    #y <- bio_raw #1 / (1 + exp(-bio_raw))
    y <- pmin(100, pmax(0, round(y_raw)))
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

    idx2 <- which(colnames(X) == comparison_coef)

    coef_diff <- coef_diffs[i] <- fit$coefficients[idx1] -
      fit$coefficients[idx2]
    se_diff <- sqrt(
      XtX_inv[idx1, idx1] *
        sigma2 +
        XtX_inv[idx2, idx2] * sigma2 -
        2 * XtX_inv[idx1, idx2] * sigma2
    )
    t_stat_diff <- coef_diff / se_diff
    p_values_diff[i] <- 2 * pt(abs(t_stat_diff), df_resid, lower.tail = FALSE)
  }

  result <- tibble(
    beta1 = beta1,
    sd_error = sd_error,
    power = mean(p_values < 0.05),
    mean_r2 = mean(r2_values),
    mean_coef_diff = mean(coef_diffs),
    coef_diff_power = mean(p_values_diff < 0.05)
  )

  # # NEW: Add mean difference if calculated
  # if (!is.null(comparison_coef)) {
  #   result$mean_coef_diff <- mean(coef_diffs)
  #   result$sd_coef_diff <- sd(coef_diffs)
  # }
}

# --- simulation grid ---
params <- expand.grid(
  beta1 = seq(0, 20, by = 1) #,
  #sd_error = seq(0.1, 0.5, by = 0.1)
)


start_time <- Sys.time()
power_results <- pmap_dfr(
  params,
  ~ simulate_power(
    beta1 = ..1,
    sd_error = 20, #..2,
    coef_of_interest = "treatment_factor1"
  )
)

end_time <- Sys.time()
run_time <- end_time - start_time
print(run_time)

power_results$cohens_d <- power_results$beta1 /
  power_results$sd_error

# --- plots ---
# Interpolate exact crossing point
crossings_precise <- power_results %>%
  group_by(sd_error) %>%
  arrange(cohens_d) %>%
  mutate(
    next_power = lead(power),
    next_effect = lead(cohens_d)
  ) %>%
  filter(
    (power < 0.8 & next_power >= 0.8) |
      (power > 0.8 & next_power <= 0.8)
  ) %>%
  mutate(
    # Linear interpolation
    effect_at_80 = cohens_d +
      (0.8 - power) * (next_effect - cohens_d) / (next_power - power)
  ) %>%
  slice(1) %>%
  ungroup() %>%
  mutate(power_80 = 0.8) # Exact y-value

power_line_graph <- ggplot(
  power_results,
  aes(x = cohens_d, y = power)
) +
  geom_line() +
  geom_hline(yintercept = 0.8, linetype = "dashed", color = "red") +
  geom_point(
    data = crossings_precise,
    aes(x = effect_at_80, y = power_80),
    size = 3,
    shape = 21,
    fill = "white"
  ) +
  geom_text(
    data = crossings_precise,
    aes(x = effect_at_80, y = power_80, label = sprintf("%.2f", effect_at_80)),
    nudge_y = 0.05,
    size = 3,
    show.legend = FALSE
  ) +
  labs(
    #title = "Estimated Power vs True Interaction Effect",
    #subtitle = "Interaction loss effect compared to woman winning effect",
    x = "True Effect (Cohen's d)",
    y = "Power (fraction of p < 0.05)"
  ) +
  theme_classic()

power_line_graph

crossings_precise <- power_results %>%
  group_by(sd_error) %>%
  arrange(cohens_d) %>%
  mutate(
    next_power = lead(coef_diff_power),
    next_effect = lead(cohens_d)
  ) %>%
  filter(
    (coef_diff_power < 0.8 & next_power >= 0.8) |
      (coef_diff_power > 0.8 & next_power <= 0.8)
  ) %>%
  mutate(
    # Linear interpolation
    effect_at_80 = cohens_d +
      (0.8 - coef_diff_power) *
        (next_effect - cohens_d) /
        (next_power - coef_diff_power)
  ) %>%
  slice(1) %>%
  ungroup() %>%
  mutate(power_80 = 0.8) # Exact y-value

coef_diff_power_line_graph <- ggplot(
  power_results,
  aes(x = cohens_d, y = coef_diff_power)
) +
  geom_line() +
  geom_hline(yintercept = 0.8, linetype = "dashed", color = "red") +
  geom_point(
    data = crossings_precise,
    aes(x = effect_at_80, y = power_80),
    size = 3,
    shape = 21,
    fill = "white"
  ) +
  geom_text(
    data = crossings_precise,
    aes(x = effect_at_80, y = power_80, label = sprintf("%.2f", effect_at_80)),
    nudge_y = 0.05,
    size = 3,
    show.legend = FALSE
  ) +
  labs(
    #title = "Estimated Power vs True Interaction Effect",
    #subtitle = "Interaction loss effect compared to woman winning effect",
    x = "True Effect (Cohen's d)",
    y = "Power (fraction of p < 0.05)"
  ) +
  theme_classic()

#coef_diff_power_line_graph

ggsave(
  "C:/Users/adamd/Documents/incumbent_comparisons/power_graph.pdf",
  plot = power_line_graph,
  width = 6,
  height = 4
)
