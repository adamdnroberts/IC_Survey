# Equivalence-based balance tests across the six experimental treatment arms.
#
# Instead of a difference test (where failing to reject "means are equal" is
# weak evidence of balance), we run an equivalence test: the null is that the
# between-arm variation is LARGE, so rejecting it lets us positively conclude
# the arms are practically balanced. TOSTER::equ_anova() does this as a single
# omnibus test per covariate across all six arms at once (no pairwise blow-up).
#
# install.packages("TOSTER")

library(TOSTER)
library(dplyr)

load("data/survey_panel_dataset.Rdata") # -> `panel`

# Restrict to the same estimation sample as the update analyses
# (belief_update_analysis.R / vote_update_analysis.R): respondents whose home
# municipality is identical across waves and who passed the attention check.
panel <- panel %>%
  filter(muni_changed == 0, Attention_Check == "somewhat_agree")

alpha <- 0.05

# ── Equivalence bound ─────────────────────────────────────────────────────────
#
# equ_anova() expects the bound in PARTIAL ETA-SQUARED, but we want to reason in
# SDs. A "small" standardized mean difference of d = 0.2 maps to Cohen's
# f = d / 2 = 0.1 (the two-group case), and eta^2 = f^2 / (1 + f^2). This gives a
# scale-free bound that applies to every covariate below, including 0/1 dummies.
d_bound <- 0.2
f_bound <- d_bound / 2
eta2_bound <- f_bound^2 / (1 + f_bound^2) # ~= 0.0099

# ── Covariates to test ────────────────────────────────────────────────────────
#
# Pre-treatment only. Numeric covariates go straight into equ_anova; categorical
# covariates are expanded into one 0/1 indicator per level and each indicator is
# tested separately (an equivalence test of that category's share across arms).
numeric_covs <- c(
  age = "NQ_Age_w2",
  crime_prior_rank = "rank_prior",
  home_robbery_rate = "home_rate",
  crime_gap = "crime_gap",
  crime_handling_pre = "Home_Crime_Handling_Pre",
  crime_importance = "Importance_Crime",
  comp_party_known = "comp_party_known"
)

categorical_covs <- c("NQ_Sex_w2", "NQ_Region_w2", "NQ_SEL_w2", "coalition_pre")

# ── Helpers ───────────────────────────────────────────────────────────────────

# Run a one-way equivalence ANOVA of `y` across treatment arm `g`. Returns the
# native equ_anova() row (effect, df1, df2, F.value, p.null, pes, eqbound, MET,
# p.equ) prefixed with the covariate label, or NULL if `y` has no usable
# variation.
run_equ <- function(y, g, label, type) {
  if (length(y) != length(g)) {
    warning(sprintf("Skipping '%s': column missing or wrong length.", label))
    return(NULL)
  }
  df <- data.frame(y = as.numeric(y), g = factor(g))
  df <- df[stats::complete.cases(df), ]
  df$g <- droplevels(df$g)
  if (length(unique(df$y)) < 2 || nlevels(df$g) < 2) {
    return(NULL)
  }
  fit <- aov(y ~ g, data = df)
  res <- equ_anova(fit, eqbound = eta2_bound, alpha = alpha)
  cbind(covariate = label, type = type, n = nrow(df), res, row.names = NULL)
}

# Expand a categorical vector into a named list of 0/1 indicators (NA-preserving).
make_dummies <- function(x) {
  x <- as.character(x)
  x[is.na(x) | x == ""] <- NA
  levels_x <- sort(unique(na.omit(x)))
  setNames(lapply(levels_x, function(l) as.numeric(x == l)), levels_x)
}

# ── Run the tests ─────────────────────────────────────────────────────────────

results <- list()

for (nm in names(numeric_covs)) {
  results[[length(results) + 1]] <- run_equ(
    y = panel[[numeric_covs[[nm]]]],
    g = panel$Treatment_Group,
    label = nm,
    type = "numeric"
  )
}

for (cov in categorical_covs) {
  dummies <- make_dummies(panel[[cov]])
  for (lv in names(dummies)) {
    results[[length(results) + 1]] <- run_equ(
      y = dummies[[lv]],
      g = panel$Treatment_Group,
      label = paste0(cov, ":", lv),
      type = "categorical"
    )
  }
}

balance_equ <- do.call(rbind, results)

# p.equ < alpha => reject "large difference" => arms are practically equivalent
# on this covariate at the 0.2 SD bound.
balance_equ$equivalent <- balance_equ$p.equ < alpha

# ── Report ────────────────────────────────────────────────────────────────────

cat(sprintf(
  "Equivalence balance tests across %d arms | bound: d = %.2f SD (eta^2 = %.4f), alpha = %.2f\n\n",
  nlevels(factor(panel$Treatment_Group)),
  d_bound,
  eta2_bound,
  alpha
))

print(
  balance_equ %>%
    mutate(across(where(is.numeric), ~ round(.x, 4))) %>%
    select(covariate, type, n, pes, eqbound, F.value, p.equ, equivalent)
)

cat(sprintf(
  "\nEquivalent on %d of %d covariates.\n",
  sum(balance_equ$equivalent, na.rm = TRUE),
  nrow(balance_equ)
))

write.csv(
  balance_equ,
  "data/balance_equivalence_tests.csv",
  row.names = FALSE
)

# ── LaTeX table ───────────────────────────────────────────────────────────────
#
# Collapse each categorical covariate to its single WORST-balanced level (max
# partial eta^2 / min p.equ): if the least-balanced category clears the bound,
# every category does. Numeric covariates keep their single row. Result is a
# compact ~11-row booktabs table. Requires \usepackage{booktabs} in the preamble.

nice_label <- c(
  age = "Age",
  crime_prior_rank = "Crime rank (prior)",
  home_robbery_rate = "Home robbery rate",
  crime_gap = "Crime gap",
  crime_handling_pre = "Crime handling (pre)",
  crime_importance = "Crime importance",
  comp_party_known = "Comparison party knowledge",
  NQ_Sex_w2 = "Sex",
  NQ_Region_w2 = "Region",
  NQ_SEL_w2 = "Socioeconomic level",
  coalition_pre = "Vote intention coalition (pre)"
)

tab <- balance_equ %>%
  mutate(base = ifelse(type == "categorical", sub(":.*$", "", covariate), covariate)) %>%
  group_by(base, type) %>%
  summarise(
    # Report the worst-balanced level (max pes). Index into the original pes
    # vector BEFORE reassigning pes, so F/p come from that same level.
    n_levels = n(),
    n = max(n),
    F.value = F.value[which.max(pes)],
    p.equ = p.equ[which.max(pes)],
    equivalent = all(equivalent),
    pes = max(pes),
    .groups = "drop"
  ) %>%
  mutate(
    Covariate = ifelse(
      type == "categorical",
      sprintf("%s (%d cat.)", nice_label[base], n_levels),
      nice_label[base]
    ),
    Equivalent = ifelse(equivalent, "Yes", "No")
  ) %>%
  arrange(type, base)

# Build the booktabs LaTeX by hand (avoids a knitr/kable dependency).
body_rows <- sprintf(
  "%s & %d & %.4f & %.3f & %.4f & %s \\\\",
  tab$Covariate,
  tab$n,
  tab$pes,
  tab$F.value,
  tab$p.equ,
  tab$Equivalent
)

balance_latex <- c(
  "\\begin{table}[htbp]",
  "\\centering",
  sprintf(
    "\\caption{Equivalence balance tests across the six treatment arms (equivalence bound $d = %.2f$ SD, $\\eta^2 = %.4f$; $\\alpha = %.2f$). Categorical covariates are collapsed to their least-balanced category.}",
    d_bound,
    eta2_bound,
    alpha
  ),
  "\\label{tab:balance_equivalence}",
  "\\begin{tabular}{lrrrrc}",
  "\\toprule",
  "Covariate & $N$ & $\\eta^2_p$ & $F$ & $p_{\\text{equ}}$ & Equivalent \\\\",
  "\\midrule",
  body_rows,
  "\\bottomrule",
  "\\end{tabular}",
  "\\end{table}"
)

dir.create("latex/tables", showWarnings = FALSE, recursive = TRUE)
writeLines(balance_latex, "latex/tables/balance_equivalence.tex")
