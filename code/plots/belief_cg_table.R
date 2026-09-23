# Writes latex/tables/belief_cg_all_models.tex: the CG x arm interactions for
# all three belief outcomes in one table.
#
# Replaces two things in the belief section of main.tex -- the two-outcome
# figure (t4_belief_updates_comparison_cg.pdf) and the single-outcome table
# (tables/inc_vs_other_coalitions.tex). Putting all three outcomes and all four
# arms in one place makes the decomposition visible to the reader: the third
# column is exactly the first minus the second, so anyone can see how much of
# the composite effect comes from the incumbent rating and how much from the
# opposition rating, rather than having to take the composite on faith.
#
# Sourced at the end of all_arms_belief_updates_comparison.R, which fits the
# models; run standalone it will source that script first.

if (!exists("cp_inc") || !exists("cp_opp") || !exists("cp_inc_other") ||
  !exists("log_crime_gap_sd")) {
  source("code/plots/all_arms_belief_updates_comparison.R")
}

cg <- dplyr::bind_rows(cp_inc, cp_opp, cp_inc_other)
cg <- cg[cg$group == "CG × Treatment", ]

model_order <- c(
  "Incumbent update",
  "Other-coalitions update",
  "Incumbent vs. other coalitions"
)

fits <- list(m_inc, m_opp, m_inc_other)

cell <- function(m, arm_id, what) {
  r <- cg[cg$model == m & cg$treatment == arm_id, ]
  switch(
    what,
    est = sprintf("%.2f", r$estimate),
    # Exact p-values rather than significance stars. The argument in the text
    # is that the informative comparisons are ACROSS ARMS, not each cell
    # against zero; stars mark only the latter, and would flatten the
    # difference between a p of 0.19 and one of 0.49 -- which is exactly the
    # contrast the decomposition turns on.
    p = sprintf("[%.2f]", r$p.value)
  )
}

# Two lines per arm: estimate, p-value.
rows <- unlist(lapply(c("T1", "T2", "T3", "T4"), function(a) {
  vals <- lapply(c("est", "p"), function(w) {
    vapply(model_order, cell, "", arm_id = a, what = w)
  })
  c(
    paste0(
      "CG $\\times$ ", a, " & ", paste(vals[[1]], collapse = " & "), " \\\\"
    ),
    paste0(" & ", paste(vals[[2]], collapse = " & "), " \\\\[2pt]")
  )
}))

# The specification and the sample are described in the text immediately above
# the table, and the column headers already name the three outcomes, so the
# caption carries only what a reader needs to read the numbers themselves.
caption <- paste0(
  "\\caption{Crime-level perception gap ($CG$) $\\times$ arm interactions for ",
  "three belief outcomes measured on 0--100 sliders. The third column is the ",
  "first minus the second. Coefficients are standardized to a 1 SD increase ",
  "in $CG$; $p$-values from HC2 robust standard errors in brackets. The ",
  "reference category for treatment is Control.}"
)

tex <- c(
  "\\begin{table}[htpb]",
  "\\centering",
  "\\small",
  caption,
  "\\begin{tabular}{lrrr}",
  "\\toprule",
  paste0(
    "\\textbf{Term} & \\textbf{Incumbent} & \\textbf{Other coalitions} & ",
    "\\textbf{Incumbent $-$ other} \\\\"
  ),
  "\\midrule",
  rows,
  "\\midrule",
  paste0(
    "$N$ & ",
    paste(vapply(fits, function(f) format(f$nobs, big.mark = ","), ""),
      collapse = " & "
    ),
    " \\\\"
  ),
  paste0(
    "$R^2$ & ",
    paste(vapply(fits, function(f) sprintf("%.2f", f$r.squared), ""),
      collapse = " & "
    ),
    " \\\\"
  ),
  "\\bottomrule",
  "\\end{tabular}",
  "\\label{tab:belief_cg}",
  "\\end{table}"
)

writeLines(tex, "latex/tables/belief_cg_all_models.tex")
cat("\nWrote latex/tables/belief_cg_all_models.tex\n")

# ── Are the arms different FROM EACH OTHER? ──────────────────────────────────
# Each cell of the table tests one arm against control. The text's claim is
# about a comparison the table does not run: that T4 differs from the other
# treatments. Concluding that from "T4 is significant and T3 is not" is the
# Gelman-Stern error -- a difference in significance is not a significant
# difference -- so the contrast has to be estimated directly.
#
# Every CG x arm coefficient is already a difference from the same control
# baseline, so the arm-vs-arm contrast is just the difference of two
# interaction coefficients, with
#
#     Var(b_a - b_b) = V_aa + V_bb - 2 V_ab
#
# taken from the model's HC2 covariance matrix. The covariance term matters:
# the arms share a control group, so the two estimates are correlated and
# treating the SEs as independent would overstate the contrast's precision.
#
# Standardization by log_crime_gap_sd is a common positive scalar, so it moves
# the estimate and its SE together and leaves t and p unchanged; it is applied
# only to keep the contrast on the same scale as the table.
cg_term <- function(a) paste0("log_crime_gap:as.factor(Treatment_Group)", a)

arm_contrast <- function(fit, a, b) {
  ta <- cg_term(a)
  tb <- cg_term(b)
  v <- vcov(fit)
  est <- unname(coef(fit)[ta] - coef(fit)[tb])
  se <- sqrt(v[ta, ta] + v[tb, tb] - 2 * v[ta, tb])
  t <- est / se
  data.frame(
    contrast = paste(a, "-", b),
    estimate = est * log_crime_gap_sd,
    std.error = se * log_crime_gap_sd,
    t = t,
    p.value = 2 * pt(-abs(t), df = fit$df.residual)
  )
}

pairs <- utils::combn(c("T1", "T2", "T3", "T4"), 2, simplify = FALSE)

contrasts <- do.call(rbind, lapply(seq_along(fits), function(i) {
  out <- do.call(rbind, lapply(pairs, function(p) {
    arm_contrast(fits[[i]], p[1], p[2])
  }))
  out$model <- model_order[i]
  # Holm across the six pairs WITHIN a model. Reported alongside the raw p
  # rather than instead of it: these contrasts are secondary to the arm-vs-
  # control tests in the table, and none of them was pre-specified.
  out$p_holm <- p.adjust(out$p.value, method = "holm")
  out[, c("model", "contrast", "estimate", "std.error", "t", "p.value",
    "p_holm")]
}))

old_width <- getOption("width")
options(width = 120)
cat("\nArm-vs-arm contrasts of the CG interactions, within model.\n")
cat("(Each row: does arm a's CG slope differ from arm b's? Standardized to",
  "1 SD of CG.)\n")
print(contrasts, digits = 3, row.names = FALSE)
options(width = old_width)
