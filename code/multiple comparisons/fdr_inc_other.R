# FDR control for the treatment-interaction coefficients of m_inc_other
# (incumbent minus average of the other coalitions, post levels, controlling for
# both pre levels). FDR is controlled with standard Benjamini-Hochberg (1995)
# q-values applied directly to the observed (analytic) p-values. BH is used
# instead of the Anderson (2008) / BKY (2006) sharpened two-stage procedure
# because with such a small family the sharpened m0 estimate is unstable and can
# push q-values below their raw p-values; plain BH guarantees q >= p and is the
# more defensible choice at this family size.
#
# The family excludes T1: it is the 3 CG × Treatment terms (log_crime_gap:T2…T4)
# and the 3 RG × Treatment terms (T2…T4:rank_gap) = 6 coefficients, with T2–T4
# the treatment arms relative to the control baseline.

library(estimatr)

# Build panel + the inc_minus_opp_avg_post / inc_pre / opp_avg_pre columns if not
# already loaded (source is skipped when panel already has the expected rows).
if (!exists("panel") || nrow(panel) != 1735) {
  source(
    "C:/Users/adamd/Documents/IC_Survey/code/home_party_update_analysis.R",
    echo = FALSE
  )
}

if (!exists("ci_alpha")) {
  ci_alpha <- 0.01
}

m_inc_other <- lm_robust(
  inc_minus_opp_avg_post ~
    inc_pre + opp_avg_pre +
    log_crime_gap * as.factor(Treatment_Group) +
    rank_gap * as.factor(Treatment_Group) +
    coalition_pre,
  alpha = ci_alpha,
  data = panel,
  se_type = "HC2"
)

# The interaction p-values (CG terms, then RG terms). Arms exclude the control
# baseline and T1, so the FDR family is T2–T4 × {CG, RG} = 6 coefficients.
arms <- setdiff(sort(unique(as.character(panel$Treatment_Group))), c("control", "T1"))
cg_terms <- paste0("log_crime_gap:as.factor(Treatment_Group)", arms)
rg_terms <- paste0("as.factor(Treatment_Group)", arms, ":rank_gap")
int_terms <- c(cg_terms, rg_terms)

H <- m_inc_other$p.value[int_terms]

# Benjamini-Hochberg FDR q-values (guarantees q >= p; stable for small families).
fdr_q <- p.adjust(H, method = "BH")

# ── Results table ────────────────────────────────────────────────────────────
coef_labels <- c(
  paste0("CG x ", arms, " (log_crime_gap)"),
  paste0("RG x ", arms, " (rank_gap)")
)

fdr_table <- data.frame(
  coefficient = coef_labels,
  raw_p = round(H, 4),
  fdr_q = round(fdr_q, 4),
  row.names = NULL
)

# Most-significant first
fdr_table <- fdr_table[order(fdr_table$fdr_q, fdr_table$raw_p), ]

cat(
  "\nm_inc_other: Benjamini-Hochberg FDR q-values across ",
  length(H),
  " interaction coefficients (T1 excluded)\n",
  sep = ""
)
print(fdr_table, row.names = FALSE)
