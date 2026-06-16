library(dplyr)
library(ggplot2)

set.seed(42)

# ── Simulation parameters ─────────────────────────────────────────────────────

n         <- 2000
n_per_arm <- n / 5   # equal allocation across 5 arms

# Extreme-news cutoff: E_i = 1 if |W_i| > e_cutoff
e_cutoff <- 1

# True DGP coefficients matching the estimating equation:
#   y_i = α + Σ δ_k T_ik + γ W_i + Σ β_k (W_i × T_ik)
#           + λ E_i + Σ γ_k (E_i × T_ik) + ε_i

alpha   <- 50         # intercept

delta   <- c(         # main effects of T1–T4 (control is baseline)
  T1 = 0,
  T2 = 0,
  T3 = 0,
  T4 = 0
)

gamma   <- 0          # main effect of W (control: no wrongness effect)

beta    <- c(         # W_i x T_k coefficients
  T1 = 1,            # T1: modest linear W effect; extremes captured by gamma_e
  T2 = 2,
  T3 = 2,
  T4 = 2
)

lambda  <- 0          # main effect of E_i (control: no extreme-news effect)

gamma_e <- c(         # E_i x T_k coefficients
  T1 = 3,            # T1: large additional effect when news is extreme
  T2 = 0,
  T3 = 0,
  T4 = 0
)

sigma   <- 20         # residual SD

# ── Simulate data ─────────────────────────────────────────────────────────────

treatment <- factor(
  rep(c("control", "T1", "T2", "T3", "T4"), each = n_per_arm),
  levels = c("control", "T1", "T2", "T3", "T4")
)

# Wrongness: standardized difference between actual and prior crime rank.
# In the real survey this is discrete (rank 1–5), but here continuous N(0,1).
W <- rnorm(n, mean = 0, sd = 1)

# Extreme-news indicator
E <- as.numeric(abs(W) > e_cutoff)

mu <- alpha +
  (treatment == "T1") * delta["T1"] +
  (treatment == "T2") * delta["T2"] +
  (treatment == "T3") * delta["T3"] +
  (treatment == "T4") * delta["T4"] +
  gamma * W +
  (treatment == "T1") * beta["T1"] * W +
  (treatment == "T2") * beta["T2"] * W +
  (treatment == "T3") * beta["T3"] * W +
  (treatment == "T4") * beta["T4"] * W +
  lambda * E +
  (treatment == "T1") * gamma_e["T1"] * E +
  (treatment == "T2") * gamma_e["T2"] * E +
  (treatment == "T3") * gamma_e["T3"] * E +
  (treatment == "T4") * gamma_e["T4"] * E

y <- mu + rnorm(n, sd = sigma)

sim_data <- data.frame(treatment, W, E, y)

# ── OLS model ─────────────────────────────────────────────────────────────────

# y ~ α + Σ δ_k T_ik + γ W + Σ β_k (W × T_ik) + λ E + Σ γ_k (E × T_ik)
fit_ols <- lm(
  y ~ treatment + W + W:treatment + E + E:treatment,
  data = sim_data
)

cat("=== OLS model ===\n")
print(summary(fit_ols))

# ── Predictions plot ──────────────────────────────────────────────────────────

w_seq <- seq(min(W), max(W), length.out = 200)

pred_data <- expand.grid(
  W         = w_seq,
  treatment = levels(treatment)
) %>%
  mutate(
    treatment = factor(treatment, levels = levels(treatment)),
    E         = as.numeric(abs(W) > e_cutoff)
  )

pred_data$fit_ols <- predict(fit_ols, newdata = pred_data)

pred_data <- pred_data %>%
  mutate(
    true_mu = alpha +
      (treatment == "T1") * delta["T1"] +
      (treatment == "T2") * delta["T2"] +
      (treatment == "T3") * delta["T3"] +
      (treatment == "T4") * delta["T4"] +
      gamma * W +
      (treatment == "T1") * beta["T1"] * W +
      (treatment == "T2") * beta["T2"] * W +
      (treatment == "T3") * beta["T3"] * W +
      (treatment == "T4") * beta["T4"] * W +
      lambda * E +
      (treatment == "T1") * gamma_e["T1"] * E +
      (treatment == "T2") * gamma_e["T2"] * E +
      (treatment == "T3") * gamma_e["T3"] * E +
      (treatment == "T4") * gamma_e["T4"] * E
  )

p <- ggplot(pred_data, aes(x = W)) +
  geom_vline(
    xintercept = c(-e_cutoff, e_cutoff),
    linetype = "dotted", color = "grey60"
  ) +
  geom_line(aes(y = true_mu, color = "True DGP"),  linewidth = 0.8, linetype = "dashed") +
  geom_line(aes(y = fit_ols, color = "OLS"),        linewidth = 0.8) +
  facet_wrap(~ treatment, nrow = 1) +
  scale_color_manual(
    values = c("True DGP" = "black", "OLS" = "#0072B2")
  ) +
  labs(
    x     = "Wrongness (W)",
    y     = "Predicted outcome (y)",
    color = NULL,
    title = "OLS with extreme-news indicator by treatment arm",
    caption = paste0("Dotted lines mark |W| = ", e_cutoff, " (E_i threshold)")
  ) +
  theme_bw() +
  theme(legend.position = "bottom")

print(p)

# ── Coefficient plot: extreme-news terms ──────────────────────────────────────

# Extract E and E:treatment coefficients with 95% CIs
coef_df <- as.data.frame(confint(fit_ols)) %>%
  setNames(c("ci_lo", "ci_hi"))
coef_df$estimate <- coef(fit_ols)
coef_df$term     <- rownames(coef_df)

extreme_coefs <- coef_df %>%
  filter(grepl("^E($|:)", term)) %>%
  mutate(
    arm = case_when(
      term == "E"                  ~ "Control",
      term == "E:treatmentT1"      ~ "T1",
      term == "E:treatmentT2"      ~ "T2",
      term == "E:treatmentT3"      ~ "T3",
      term == "E:treatmentT4"      ~ "T4"
    ),
    arm = factor(arm, levels = c("Control", "T1", "T2", "T3", "T4"))
  )

p_coef <- ggplot(extreme_coefs, aes(x = arm, y = estimate)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +
  geom_errorbar(aes(ymin = ci_lo, ymax = ci_hi), width = 0.15, color = "grey40") +
  geom_point(size = 3, color = "#0072B2") +
  labs(
    x     = NULL,
    y     = "Coefficient estimate (95% CI)",
    title = "Extreme-news coefficients by arm",
    subtitle = paste0(
      "Control = \u03bb (main E effect); T1\u2013T4 = \u03b3_k (E \u00d7 T_k interaction)\n",
      "E_i = 1 if |W_i| > ", e_cutoff
    )
  ) +
  theme_bw()

print(p_coef)
