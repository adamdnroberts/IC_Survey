# Bayesian analysis of benchmark municipality selection (base model)
# Input:  data/benchmark_panel.rds (from build_benchmark_panel.R)
# Output: latex/images/comparison_coef_plot.pdf

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

fit_benchmark <- brm(
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
  file = "data/derived/fit_benchmark",
  # Without this brms defaults to file_refit = "never": edit the formula,
  # priors or upstream panel and it silently reloads the stale fit, so
  # benchmark_model.tex would report a model that no longer exists in the code.
  file_refit = "on_change"
)

summary(fit_benchmark)

fe_draws <- as.data.frame(fixef(fit_benchmark, summary = FALSE))

coalition_contrast <- fe_draws %>%
  mutate(diff = `cand_coalitionMORENADPVEMDPT` - `cand_coalitionPANDPRIDPRD`)

cat(sprintf(
  "MORENA − PAN/PRI/PRD log-odds difference:\n  mean = %.3f, 95%% CI [%.3f, %.3f]\n  P(MORENA > PAN/PRI/PRD) = %.3f\n",
  mean(coalition_contrast$diff),
  quantile(coalition_contrast$diff, 0.025),
  quantile(coalition_contrast$diff, 0.975),
  mean(coalition_contrast$diff > 0)
))

coef_labels <- c(
  "log_dist_km" = "Log distance (km)",
  "log_pop_ratio" = "Log pop. ratio (cand/home)",
  "log_dist_km:log_pop_ratio" = "Log distance × log pop. ratio",
  "same_state" = "Same state",
  "same_coalition" = "Same coalition",
  "vote_coalition_match" = "Vote coalition match",
  "cand_coalitionMORENADPVEMDPT" = "Cand. coalition: MORENA/PVEM/PT",
  "cand_coalitionPANDPRIDPRD" = "Cand. coalition: PAN/PRI/PRD"
)

draws <- fe_draws %>%
  select(
    log_dist_km,
    log_pop_ratio,
    `log_dist_km:log_pop_ratio`,
    same_state,
    same_coalition,
    vote_coalition_match,
    cand_coalitionMORENADPVEMDPT,
    cand_coalitionPANDPRIDPRD
  ) %>%
  pivot_longer(everything(), names_to = "term", values_to = "draw") %>%
  mutate(pp_change = (plogis(draw) - 0.5) * 100)

plot_df <- draws %>%
  group_by(term) %>%
  summarise(
    mean = mean(pp_change),
    lo95 = quantile(pp_change, 0.025),
    hi95 = quantile(pp_change, 0.975),
    .groups = "drop"
  ) %>%
  mutate(
    label = factor(coef_labels[term], levels = rev(coef_labels))
  )

benchmark_coef_plot <- ggplot(
  filter(
    plot_df,
    label != "Cand. coalition: MORENA/PVEM/PT" &
      label != "Cand. coalition: PAN/PRI/PRD"
  ),
  aes(x = mean, y = label)
) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey50") +
  geom_linerange(aes(xmin = lo95, xmax = hi95), linewidth = 0.9) +
  geom_point(size = 2.5, shape = 21, fill = "white", stroke = 1) +
  labs(
    x = "Posterior mean percentage-point change (from 50% baseline)",
    y = NULL,
    title = "Predictors of comparison municipality selection",
    caption = sprintf(
      "N = %d respondents. Lines: 95%% credible intervals.",
      n_respondents
    )
  ) +
  theme_classic() +
  theme(axis.text.y = element_text(size = 10))

print(benchmark_coef_plot)

ggsave(
  "latex/images/comparison_coef_plot.pdf",
  plot = benchmark_coef_plot,
  width = 7,
  height = 4.5
)

# ── LaTeX table ───────────────────────────────────────────────────────────────

n_obs <- nrow(long_df)

fe_summary <- fixef(fit_benchmark)
model_summary <- summary(fit_benchmark)
re_sd <- model_summary$random$Respondent_ID["sd(Intercept)", ]

param_labels <- c(
  "Intercept" = "Intercept",
  "log_dist_km" = "Log distance (km)",
  "log_pop_ratio" = "Log pop.\\ ratio (cand./home)",
  "log_dist_km:log_pop_ratio" = "Log distance $\\times$ log pop.\\ ratio",
  "same_state" = "Same state",
  "same_coalition" = "Same coalition (governing)",
  "vote_coalition_match" = "Vote--coalition match",
  "cand_coalitionMORENADPVEMDPT" = "Cand.\\ coalition: MORENA/PVEM/PT",
  "cand_coalitionPANDPRIDPRD" = "Cand.\\ coalition: PAN/PRI/PRD",
  "cand_coalitionOther" = "Cand.\\ coalition: Other",
  "home_coalitionMORENADPVEMDPT" = "Home coalition: MORENA/PVEM/PT",
  "home_coalitionPANDPRIDPRD" = "Home coalition: PAN/PRI/PRD",
  "log_home_pop" = "Log home pop.\\",
  "poolnearest" = "Pool: nearest",
  "poollargest" = "Pool: largest"
)

fmt_num <- function(x) sprintf("$%.2f$", x)

fe_rows <- sapply(rownames(fe_summary), function(p) {
  label <- if (p %in% names(param_labels)) param_labels[p] else p
  paste0(
    label, " & ",
    fmt_num(fe_summary[p, "Estimate"]), " & ",
    fmt_num(fe_summary[p, "Est.Error"]), " & ",
    fmt_num(fe_summary[p, "Q2.5"]), " & ",
    fmt_num(fe_summary[p, "Q97.5"]), " & 1.00 \\\\"
  )
})

re_row <- paste0(
  "$\\sigma_\\alpha$ (respondent) & ",
  fmt_num(re_sd["Estimate"]), " & ",
  fmt_num(re_sd["Est.Error"]), " & ",
  fmt_num(re_sd["l-95% CI"]), " & ",
  fmt_num(re_sd["u-95% CI"]), " & 1.00 \\\\"
)

n_obs_fmt <- format(n_obs, big.mark = "{,}")
n_resp_fmt <- format(n_respondents, big.mark = "{,}")

table_tex <- paste0(
  "\\begin{table}[htpb]\n",
  "\\centering\n",
  "\\small\n",
  "\\rowcolors{2}{gray!15}{white}\n",
  "\\caption{Bayesian multilevel logistic regression: predictors of benchmark municipality selection.\n",
  "Estimates are posterior means; Est.\\ SD is the posterior standard deviation.\n",
  "Reference categories: MC (cand.\\ and home coalition), random (pool).\n",
  "$N = ", n_obs_fmt, "$ observations, ", n_resp_fmt, " respondents.}\n",
  "\\begin{tabular}{lrrrrc}\n",
  "\\toprule\n",
  "\\textbf{Parameter} & \\textbf{Estimate} & \\textbf{Est.\\ SD} & \\textbf{l-95\\% CI} & \\textbf{u-95\\% CI} & $\\hat{R}$ \\\\\n",
  "\\midrule\n",
  "\\multicolumn{6}{l}{\\textit{Regression coefficients}} \\\\\n",
  paste(fe_rows, collapse = "\n"), "\n",
  "\\midrule\n",
  "\\multicolumn{6}{l}{\\textit{Random effects}} \\\\\n",
  re_row, "\n",
  "\\bottomrule\n",
  "\\end{tabular}\n",
  "\\label{tab:benchmark_model}\n",
  "\\end{table}\n"
)

writeLines(table_tex, "latex/tables/benchmark_model.tex")
cat("Wrote latex/tables/benchmark_model.tex\n")
