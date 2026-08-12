# ── Appendix table: does comparison salience moderate the T4 effect? ──────────
# Runs both contrasts (T4 vs control, T4 vs T2) under both respondent-level
# aggregations of the benchmark-salience index (mean and max) and writes
# latex/tables/salience_moderation.tex.
#
# Reported quantity is the cross-partial
#     d^2 P(inc vote) / d log_crime_gap d salience
# scaled to ONE SD of the index within each contrast's own estimation sample and
# expressed in percentage points, so the numbers are comparable across the two
# aggregations (whose raw scales differ) and against the main-text T4 effect.
# The SD is sample-specific, so the two contrasts within an index row block are
# scaled by slightly different SDs — this is intentional (each is standardised
# on the sample it was fitted on) and the SD is printed in the table.
#
# Run from the project root:
#   Rscript code/index_similarity/salience_moderation_table.R
source("code/index_similarity/build_importance_index.R")
source("code/index_similarity/lpm_arm_contrasts.R")

ci_alpha <- 0.05
aggregators <- list(mean = agg_mean, max = agg_max)
contrasts_wanted <- list(c("control", "T4"), c("T2", "T4"))

rows <- list()
for (agg_label in names(aggregators)) {
  lm_panel <- build_lm_panel(
    panel,
    agg_fun = aggregators[[agg_label]],
    agg_label = agg_label
  )
  for (pair in contrasts_wanted) {
    ref_arm <- pair[1]
    focal_arm <- pair[2]
    res <- arm_contrast(lm_panel, ref_arm, focal_arm, ci_alpha)

    # SD of the index on the sample this contrast was actually fitted on.
    idx_sd <- sd(res$data$comparison_importance_lp)

    # table rows are ordered: ref baseline, focal net, difference.
    tab <- res$table
    stopifnot(nrow(tab) == 3L)

    rows[[length(rows) + 1L]] <- data.frame(
      index = agg_label,
      ref_arm = ref_arm,
      focal_arm = focal_arm,
      idx_sd = idx_sd,
      ref_slope = 100 * tab$estimate[1] * idx_sd,
      focal_slope = 100 * tab$estimate[2] * idx_sd,
      diff = 100 * tab$estimate[3] * idx_sd,
      diff_lo = 100 * tab$conf.low[3] * idx_sd,
      diff_hi = 100 * tab$conf.high[3] * idx_sd,
      p = tab$p.value[3],
      n = res$fit$nobs,
      stringsAsFactors = FALSE
    )
  }
}
rows <- do.call(rbind, rows)

# ── Emit LaTeX ────────────────────────────────────────────────────────────────
index_label <- c(mean = "Mean", max = "Max")

body <- character(0)
for (agg_label in names(aggregators)) {
  blk <- rows[rows$index == agg_label, ]
  body <- c(body, sprintf(
    "\\multicolumn{7}{l}{\\emph{Panel %s: %s salience index}} \\\\",
    if (agg_label == "mean") "A" else "B",
    index_label[[agg_label]]
  ))
  for (i in seq_len(nrow(blk))) {
    body <- c(body, sprintf(
      "\\quad %s vs.\\ %s & %.2f & %.2f & %.2f & [%.2f, %.2f] & %.2f & %d \\\\",
      blk$focal_arm[i],
      blk$ref_arm[i],
      blk$ref_slope[i],
      blk$focal_slope[i],
      blk$diff[i],
      blk$diff_lo[i],
      blk$diff_hi[i],
      blk$p[i],
      blk$n[i]
    ))
  }
  if (agg_label == "mean") body <- c(body, "\\addlinespace")
}

latex <- c(
  "\\begin{table}[htbp]",
  "\\centering",
  paste0(
    "\\caption{Salience moderation of the crime-gap slope. Each cell is the ",
    "cross-partial $\\partial^2 \\Pr(\\text{incumbent vote}) / \\partial CG \\, ",
    "\\partial \\text{Salience}$, in percentage points per one standard ",
    "deviation of the salience index. Each row is a separate linear ",
    "probability model fit on its own two arms, with HC2 standard errors. The ",
    "``Difference'' column is the triple-interaction coefficient and is the ",
    "test of whether the two arms differ in how salience moderates the ",
    "response to crime information.}"
  ),
  "\\label{tab:salience_moderation}",
  "\\begin{tabular}{lrrrcrr}",
  "\\toprule",
  paste(
    "Comparison & Ref.\\ arm & T4 & Difference & 95\\% CI &",
    "$p$ & $N$ \\\\"
  ),
  "\\midrule",
  body,
  "\\bottomrule",
  "\\end{tabular}",
  "\\end{table}"
)

dir.create("latex/tables", showWarnings = FALSE, recursive = TRUE)
writeLines(latex, "latex/tables/salience_moderation.tex")
cat("\nwrote latex/tables/salience_moderation.tex\n")
print(rows, row.names = FALSE, digits = 3)
