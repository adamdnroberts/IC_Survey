# ── Preamble: reproduce vote_update_analysis.R's sample and models ────────────
# Rebuilds m_vote and m_log so this script runs standalone. Same estimation
# sample as vote_update_analysis.R: home municipality unchanged across waves,
# passed the attention check, and EXCLUDING control2. With control2 dropped the
# reference arm is control, so interaction terms exist for T1–T4 (T1 = plain
# information serves as the baseline for the comparison-treatment contrasts).
library(estimatr)
library(dplyr)

load("~/IC_Survey/data/survey_panel_dataset.Rdata")

if (!exists("ci_alpha")) {
  ci_alpha <- 0.01
}

panel$Vote_home_post <- as.integer(
  !is.na(panel$coalition_post) &
    !is.na(panel$home_coalition) &
    panel$home_coalition == panel$coalition_post
)

panel_full <- panel
panel_full$coalition_pre[is.na(panel_full$coalition_pre)] <- "Other"
panel_full$inc_vote <- as.numeric(
  panel_full$coalition_pre == panel_full$home_coalition
)

panel_with_failures <- filter(panel_full, muni_changed == 0)
panel <- filter(
  panel_with_failures,
  Attention_Check == "somewhat_agree" & Treatment_Group != "control2"
)

m_vote <- lm_robust(
  Vote_home_post ~
    crime_gap_wins *
    as.factor(Treatment_Group) +
    rank_gap * as.factor(Treatment_Group) +
    as.factor(coalition_pre) +
    inc_vote,
  alpha = ci_alpha,
  data = panel,
  se_type = "HC2"
)

m_log <- lm_robust(
  Vote_home_post ~
    log_crime_gap *
    as.factor(Treatment_Group) +
    rank_gap * as.factor(Treatment_Group) +
    inc_vote,
  alpha = ci_alpha,
  data = panel,
  se_type = "HC2"
)

# ── Test: comparison-treatment rank_gap interactions vs T1's (m_vote) ─────────
# Two-sided tests of whether the rank_gap x Treatment_Group interaction
# coefficient differs between each comparison treatment (T2, T3, T4) and T1
# (plain info). Difference = b_Tx - b_T1; SE from the HC2 variance-covariance
# matrix.
rank_gap_term <- function(g) {
  paste0("as.factor(Treatment_Group)", g, ":rank_gap")
}

b_vote <- coef(m_vote)
V_vote <- vcov(m_vote)
base_term <- rank_gap_term("T1")

stopifnot(all(
  c(base_term, sapply(c("T2", "T3", "T4"), rank_gap_term)) %in% names(b_vote)
))

rank_gap_contrasts <- bind_rows(lapply(c("T2", "T3", "T4"), function(g) {
  tx <- rank_gap_term(g)
  diff <- b_vote[[tx]] - b_vote[[base_term]]
  se <- sqrt(
    V_vote[tx, tx] + V_vote[base_term, base_term] - 2 * V_vote[tx, base_term]
  )
  tstat <- diff / se
  data.frame(
    treatment = g,
    coef = b_vote[[tx]],
    t1_coef = b_vote[[base_term]],
    diff = diff,
    std.error = se,
    t = tstat,
    p_two_sided = 2 * pt(-abs(tstat), m_vote$df.residual)
  )
}))

cat("\nH0: (Tx : rank_gap) - (T1 : rank_gap) = 0   vs.  H1: != 0\n")
print(rank_gap_contrasts, row.names = FALSE, digits = 4)

library(car)

# ── Wald tests on the rank_gap x Treatment interactions (m_vote, HC2 vcov) ─────
# Pairwise equality of the rank_gap:Treatment interaction across arms, plus a
# joint test that the three comparison arms' interactions are all zero.
rg <- function(g) paste0("as.factor(Treatment_Group)", g, ":rank_gap")

rank_gap_tests <- list(
  "T2 = T1" = paste(rg("T2"), "=", rg("T1")),
  "T2 = T3" = paste(rg("T2"), "=", rg("T3")),
  "T2 = T4" = paste(rg("T2"), "=", rg("T4")),
  "T3 = T4" = paste(rg("T3"), "=", rg("T4")),
  "T2 = T3 = T4 = 0" = c(
    #paste0("0 = ", rg("T4")),
    paste0("0 = ", rg("T3")),
    paste0("0 = ", rg("T2"))
  )
)

# Extract the test statistic (Chisq or F), its df, and p-value from one test.
lh_row <- function(hyp, label) {
  res <- linearHypothesis(m_log, hyp)
  stat_col <- intersect(c("Chisq", "F"), names(res))[1]
  p_col <- grep("^Pr", names(res), value = TRUE)[1]
  i <- nrow(res)
  data.frame(
    test = label,
    df = res[["Df"]][i],
    statistic = res[[stat_col]][i],
    stat_type = stat_col,
    p_value = res[[p_col]][i],
    row.names = NULL
  )
}

rank_gap_test_table <- do.call(
  rbind,
  Map(lh_row, rank_gap_tests, names(rank_gap_tests))
)

cat("\nWald tests: rank_gap x Treatment interaction contrasts (m_vote, HC2)\n")
print(rank_gap_test_table, row.names = FALSE, digits = 4)

# ── Wald tests on the crime_gap (CG) x Treatment interactions (m_log, HC2) ─────
# How the log_crime_gap:Treatment interaction for T4 differs from T1, T2, and T3.
cg <- function(g) paste0("log_crime_gap:as.factor(Treatment_Group)", g)

crime_gap_tests <- list(
  "T4 = T1" = paste(cg("T4"), "=", cg("T1")),
  "T4 = T2" = paste(cg("T4"), "=", cg("T2")),
  "T4 = T3" = paste(cg("T4"), "=", cg("T3"))
)

crime_gap_test_table <- do.call(
  rbind,
  Map(lh_row, crime_gap_tests, names(crime_gap_tests))
)

cat(
  "\nWald tests: crime_gap (CG) x Treatment interaction contrasts (m_log, HC2)\n"
)
print(crime_gap_test_table, row.names = FALSE, digits = 4)