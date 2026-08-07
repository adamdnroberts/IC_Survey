# Bayesian analysis of benchmark selection with crime × importance interaction
# Input:  data/benchmark_panel.rds (from build_benchmark_panel.R)
# Output: latex/images/comparison_crime_coef_plot.pdf

library(dplyr)
library(tidyr)
library(brms)
library(ggplot2)

long_df <- readRDS("data/derived/benchmark_panel.rds")
n_respondents <- n_distinct(long_df$Respondent_ID)

priors <- c(
  prior(student_t(2, 0, 2.5), class = b),
  prior(student_t(2, 0, 2.5), class = Intercept),
  prior(exponential(1), class = sd)
)

fit_benchmark_crime <- brm(
  bf(
    Selected ~
      log_dist_km +
      log_pop_ratio +
      log_dist_km * log_pop_ratio +
      same_state +
      same_coalition +
      vote_coalition_match +
      cand_coalition +
      home_coalition +
      log_home_pop +
      pool +
      crime_diff +
      CI_f +
      crime_diff * CI_f +
      (1 | Respondent_ID),
    decomp = "QR"
  ),
  data = long_df,
  family = bernoulli(link = "logit"),
  prior = priors,
  chains = 4,
  cores = 4,
  iter = 2000,
  warmup = 1000,
  seed = 42,
  file = "data/derived/fit_benchmark_crime",
  # See benchmark_analysis.R: brms defaults to file_refit = "never", which
  # silently reloads a stale fit after the model or its data changes.
  file_refit = "on_change"
)

summary(fit_benchmark_crime)

coef_labels_crime <- c(
  "crime_diff:CI_f1" = "Crime difference × crime importance1",
  "crime_diff:CI_f2" = "Crime difference × crime importance2",
  "crime_diff:CI_f4" = "Crime difference × crime importance4",
  "crime_diff:CI_f5" = "Crime difference × crime importance5"
)

fe_draws_crime <- as.data.frame(fixef(fit_benchmark_crime, summary = FALSE))

cat("Crime model coefficient names:\n")
print(names(fe_draws_crime))

draws_crime <- fe_draws_crime %>%
  pivot_longer(everything(), names_to = "term", values_to = "draw") %>%
  filter(term %in% names(coef_labels_crime)) %>%
  mutate(pp_change = (plogis(draw) - 0.5) * 100)

plot_df_crime <- draws_crime %>%
  group_by(term) %>%
  summarise(
    mean = mean(pp_change),
    lo95 = quantile(pp_change, 0.025),
    hi95 = quantile(pp_change, 0.975),
    lo50 = quantile(pp_change, 0.25),
    hi50 = quantile(pp_change, 0.75),
    .groups = "drop"
  ) %>%
  mutate(
    label = factor(coef_labels_crime[term], levels = rev(coef_labels_crime))
  )

benchmark_crime_coef_plot <- ggplot(plot_df_crime, aes(x = mean, y = label)) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey50") +
  geom_linerange(aes(xmin = lo95, xmax = hi95), linewidth = 0.6) +
  geom_linerange(aes(xmin = lo50, xmax = hi50), linewidth = 1.6) +
  geom_point(size = 2.5, shape = 21, fill = "white", stroke = 1) +
  labs(
    x = "Posterior mean percentage-point change (from 50% baseline)",
    y = NULL,
    title = "Crime ratio as a predictor of municipality selection",
    caption = sprintf(
      "N = %d respondents. Thick lines: 50%% CI. Thin lines: 95%% CI.",
      n_respondents
    )
  ) +
  theme_classic() +
  theme(axis.text.y = element_text(size = 10))

print(benchmark_crime_coef_plot)

ggsave(
  "latex/images/comparison_crime_coef_plot.pdf",
  plot = benchmark_crime_coef_plot,
  width = 7,
  height = 5
)
