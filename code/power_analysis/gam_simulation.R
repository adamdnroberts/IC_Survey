library(mgcv)
library(dplyr)
library(ggplot2)

set.seed(42)

# ── Simulation parameters ─────────────────────────────────────────────────────

n <- 2000
n_per_arm <- n / 5 # equal allocation across 5 arms

# True DGP coefficients
alpha <- 50 # intercept (e.g. mid-point of a 0–100 slider)
delta <- c(
  # main effects of T1–T4 (control is baseline)
  T1 = 0,
  T2 = 0,
  T3 = 0,
  T4 = 0
)
gamma <- 0 # main effect of W for control (near zero by design)
beta <- c(
  # W_i x T_k interaction coefficients (linear); all positive
  # T1 linear term handled separately by beta_t1_lin
  T2 = 2,
  T3 = 2,
  T4 = 2
)
beta_t1_lin <- 0.6 # T1: linear component — chosen so T1 extremes match other arms at W=±2
beta_t1_sq <- 0.5 # T1: quadratic component
beta_t1_cub <- 0.1 # T1: cubic component — small to keep extremes in range
sigma <- 20 # residual SD

# ── Simulate data ─────────────────────────────────────────────────────────────

treatment <- factor(
  rep(c("control", "T1", "T2", "T3", "T4"), each = n_per_arm),
  levels = c("control", "T1", "T2", "T3", "T4")
)

# Wrongness: standardized difference between actual and prior crime rank.
# In the real survey this is discrete (rank 1–5), but here continuous N(0,1).
W <- rnorm(n, mean = 0, sd = 1)

# Build linear predictor from true DGP.
# T2 uses a quadratic W term (U-shaped): effect is large when |W| is large,
# small when W is near zero.
mu <- alpha +
  (treatment == "T1") * delta["T1"] +
  (treatment == "T2") * delta["T2"] +
  (treatment == "T3") * delta["T3"] +
  (treatment == "T4") * delta["T4"] +
  gamma * W +
  (treatment == "T1") * beta_t1_lin * W +
  (treatment == "T1") * beta_t1_sq * W^2 +
  (treatment == "T1") * beta_t1_cub * W^3 +
  (treatment == "T2") * beta["T2"] * W +
  (treatment == "T3") * beta["T3"] * W +
  (treatment == "T4") * beta["T4"] * W

y <- mu + rnorm(n, sd = sigma)

sim_data <- data.frame(treatment, W, y)

# ── Models ────────────────────────────────────────────────────────────────────

# Parametric baseline: linear W and W:treatment interaction (OLS via gam)
fit_lm <- gam(
  y ~ treatment + W + W:treatment,
  data = sim_data
)

# GAM: treatment-specific smooths of W.
# s(W, by = treatment) fits a separate smooth for each arm, subsuming both
# the main effect of W and the W x treatment interaction.
# k = 5 limits flexibility; increase for more complex non-linearity.
fit_gam <- gam(
  y ~ treatment + s(W, by = treatment, k = 5),
  data = sim_data,
  method = "REML"
)

# ── Summaries ─────────────────────────────────────────────────────────────────

cat("=== Parametric model ===\n")
print(summary(fit_lm))

cat("\n=== GAM ===\n")
print(summary(fit_gam))

# ── Predictions plot ──────────────────────────────────────────────────────────

w_seq <- seq(min(W), max(W), length.out = 100)

pred_data <- expand.grid(
  W = w_seq,
  treatment = levels(treatment)
) %>%
  mutate(treatment = factor(treatment, levels = levels(treatment)))

pred_data$fit_lm <- predict(fit_lm, newdata = pred_data)
pred_data$fit_gam <- predict(fit_gam, newdata = pred_data)

# True conditional mean for comparison
pred_data <- pred_data %>%
  mutate(
    true_mu = alpha +
      (treatment == "T1") * delta["T1"] +
      (treatment == "T2") * delta["T2"] +
      (treatment == "T3") * delta["T3"] +
      (treatment == "T4") * delta["T4"] +
      gamma * W +
      (treatment == "T1") * beta["T1"] * W +
      (treatment == "T1") * beta_t1_quad * W^3 +
      (treatment == "T2") * beta["T2"] * W +
      (treatment == "T3") * beta["T3"] * W +
      (treatment == "T4") * beta["T4"] * W
  )

p <- ggplot(pred_data, aes(x = W)) +
  geom_line(
    aes(y = true_mu, color = "True DGP"),
    linewidth = 0.8,
    linetype = "dashed"
  ) +
  geom_line(aes(y = fit_lm, color = "Linear"), linewidth = 0.8) +
  geom_line(aes(y = fit_gam, color = "GAM"), linewidth = 0.8) +
  facet_wrap(~treatment, nrow = 1) +
  scale_color_manual(
    values = c("True DGP" = "black", "Linear" = "#0072B2", "GAM" = "#E69F00")
  ) +
  labs(
    x = "Wrongness (W)",
    y = "Predicted outcome (y)",
    color = NULL,
    title = "GAM vs. linear fit by treatment arm"
  ) +
  theme_bw() +
  theme(legend.position = "bottom")

print(p)
