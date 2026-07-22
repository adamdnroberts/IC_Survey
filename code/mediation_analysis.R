library(estimatr)
library(modelsummary)
library(dplyr)
library(ggplot2)
library(broom)
library(mediation)

load("~/IC_Survey/data/survey_panel_dataset.Rdata")

panel_full <- panel

if (!exists("ci_alpha")) {
  ci_alpha <- 0.01
}

panel_with_failures <- filter(panel_full, muni_changed == 0)
panel <- filter(panel_with_failures, Attention_Check == "somewhat_agree")

# ── Mediator: incumbent minus opposition-average crime rating (post) ──────────
# inc_minus_opp_avg_post = Home_Crime_Handling_Post - (average post-treatment
# crime rating across the coalitions that do NOT govern the home municipality).
# Derivation mirrors home_party_update_analysis.R.
num <- function(x) suppressWarnings(as.numeric(x))

coalition_post_rating <- cbind(
  "MORENA/PVEM/PT" = num(panel$MORENA_Crime_Rating_Post),
  "PAN/PRI/PRD" = num(panel$Coalition_PAN_PRI_PRD_Crime_Rating_Post),
  "MC" = num(panel$MC_Crime_Rating_Post)
)

coalition_pre_rating <- cbind(
  "MORENA/PVEM/PT" = num(panel$MORENA_Crime_Rating_Pre),
  "PAN/PRI/PRD" = num(panel$Coalition_PAN_PRI_PRD_Crime_Rating_Pre),
  "MC" = num(panel$MC_Crime_Rating_Pre)
)

# Per-respondent opposition benchmark: average across the non-home coalitions,
# computed for both post and pre ratings. NA home_coalition or all-NA other
# ratings -> NA.
opp_benchmarks <- t(vapply(
  seq_len(nrow(panel)),
  function(i) {
    hc <- panel$home_coalition[i]
    if (is.na(hc)) {
      return(c(avg_post = NA_real_, avg_pre = NA_real_))
    }
    keep <- colnames(coalition_post_rating) != hc
    post <- coalition_post_rating[i, keep]
    pre <- coalition_pre_rating[i, keep]
    c(
      avg_post = if (all(is.na(post))) NA_real_ else mean(post, na.rm = TRUE),
      avg_pre = if (all(is.na(pre))) NA_real_ else mean(pre, na.rm = TRUE)
    )
  },
  numeric(2)
))

panel$opp_avg_post <- opp_benchmarks[, "avg_post"]
panel$opp_avg_pre <- opp_benchmarks[, "avg_pre"]
panel$inc_post <- num(panel$Home_Crime_Handling_Post)
panel$inc_pre <- num(panel$Home_Crime_Handling_Pre)
panel$inc_minus_opp_avg_post <- panel$inc_post - panel$opp_avg_post

panel$Vote_home_post <- as.integer(
  !is.na(panel$coalition_post) &
    !is.na(panel$home_coalition) &
    panel$home_coalition == panel$coalition_post
)

# ── Pooled mediator & outcome models (all arms, control2 excluded) ────────────
# The mediator model matches m_inc_other in home_party_update_analysis.R: the
# a-path (treatment -> inc_minus_opp_avg_post) is specified identically -- same
# log_crime_gap and rank_gap interactions with arm, same pre-level controls
# (inc_pre + opp_avg_pre) and coalition_pre, all arms pooled with control as the
# reference. The outcome model adds the mediator to predict incumbent vote. A
# single pooled fit is used for every arm; mediate() selects the control vs.
# treat.value contrast, so the control baseline and covariate distribution match
# across arms (unlike the earlier pairwise fits).
panel_med <- filter(panel, Treatment_Group != "control2")
panel_med$Treatment_Group <- relevel(
  droplevels(factor(panel_med$Treatment_Group)),
  ref = "control"
)

m_mediator <- lm(
  inc_minus_opp_avg_post ~
    inc_pre +
    opp_avg_pre +
    log_crime_gap * Treatment_Group +
    rank_gap * Treatment_Group +
    coalition_pre,
  data = panel_med
)

m_outcome <- lm(
  Vote_home_post ~
    inc_minus_opp_avg_post +
    inc_pre +
    opp_avg_pre +
    log_crime_gap * Treatment_Group +
    rank_gap * Treatment_Group +
    coalition_pre,
  data = panel_med
)

# ACME (d0) + p-value for each treatment arm vs. control.
med_out <- list()
for (arm in c("T1", "T2", "T3", "T4")) {
  med_out[[arm]] <- mediate(
    m_mediator,
    m_outcome,
    treat = "Treatment_Group",
    mediator = "inc_minus_opp_avg_post",
    control.value = "control",
    treat.value = arm,
    robustSE = TRUE,
    sims = 1000
  )
  print(paste0(
    arm,
    ": ACME= ",
    med_out[[arm]]$d0,
    ", p-value= ",
    med_out[[arm]]$d0.p
  ))
}
med.outT4 <- med_out[["T4"]]

# ── T4: crime-gap vs rank-gap channel decomposition ──────────────────────────
# CG and RG are pre-treatment moderators, not mediators, so this is moderated
# mediation, not a multiple-mediator ACME. The mediator model is linear, so the
# T4-vs-control shift in the mediator decomposes additively:
#   dM = delta_T4 + b_cg * log_crime_gap + b_rg * rank_gap.
# Multiplying each averaged piece by the mediator->outcome coefficient (gamma)
# splits the average mediated effect into a dummy, crime-gap, and rank-gap
# channel (approximate; exact for this linear, no-M*T-interaction spec). NOTE:
# the dummy-vs-channel split depends on where log_crime_gap / rank_gap are zeroed
# (they are not centered), so treat only the total and the interaction slopes as
# structural.
cf_med <- coef(m_mediator)
pick <- function(cf, a, b) {
  cf[grep(paste0("(^|:)", a, ".*:.*", b, "|(^|:)", b, ".*:.*", a), names(cf))]
}
gamma <- m_outcome$coefficients["inc_minus_opp_avg_post"]
delta_T4 <- cf_med["Treatment_GroupT4"]
b_cg_T4 <- pick(cf_med, "log_crime_gap", "Treatment_GroupT4")
b_rg_T4 <- pick(cf_med, "rank_gap", "Treatment_GroupT4")
mean_cg <- mean(panel_med$log_crime_gap, na.rm = TRUE)
mean_rg <- mean(panel_med$rank_gap, na.rm = TRUE)

chan_dummy <- gamma * delta_T4
chan_cg <- gamma * b_cg_T4 * mean_cg
chan_rg <- gamma * b_rg_T4 * mean_rg

cat("\nT4 mediated-effect channel decomposition (gamma * dM):\n")
cat(sprintf("  dummy channel : % .6f\n", chan_dummy))
cat(sprintf("  crime-gap ch. : % .6f\n", chan_cg))
cat(sprintf("  rank-gap ch.  : % .6f\n", chan_rg))
cat(sprintf("  total (approx): % .6f\n", chan_dummy + chan_cg + chan_rg))
cat(sprintf("  ACME (d0)     : % .6f\n", med.outT4$d0))

# Inferential moderated mediation: ACME with each gap neutralized (set to 0), so
# only the other channel's interaction contributes to the mediator shift.
med.outT4_cg <- mediate( # rank channel off -> crime-gap channel active
  m_mediator,
  m_outcome,
  treat = "Treatment_Group",
  mediator = "inc_minus_opp_avg_post",
  control.value = "control",
  treat.value = "T4",
  covariates = list(rank_gap = 0),
  robustSE = TRUE,
  sims = 1000
)
med.outT4_rg <- mediate( # crime-gap channel off -> rank channel active
  m_mediator,
  m_outcome,
  treat = "Treatment_Group",
  mediator = "inc_minus_opp_avg_post",
  control.value = "control",
  treat.value = "T4",
  covariates = list(log_crime_gap = 0),
  robustSE = TRUE,
  sims = 1000
)
cat("\nT4 ACME with rank_gap = 0 (crime-gap channel):\n")
print(summary(med.outT4_cg))
cat("\nT4 ACME with log_crime_gap = 0 (rank-gap channel):\n")
print(summary(med.outT4_rg))

# ── Robustness: magnitude-of-surprise (|gap|) moderators ──────────────────────
# The signed-gap spec above forces the treatment's effect on the mediator to be
# largest at gap = 0 and to move monotonically with the (signed) gap -- it cannot
# represent the updating prediction that LARGER |surprise| (in either direction)
# drives more updating. Here we refit the reconciled models with ABSOLUTE-value
# gaps so the moderator is the magnitude of prior surprise, then re-check whether
# mediation appears where the theory predicts it (high |gap|).
#
# Result (see below when run): it does not. The rating -> vote (b) path stays
# strong, but T4's effect on the rating is concentrated among LOW-surprise
# respondents (the T4 x abs_rank_gap slope is negative and significant), the
# average ACME is null, and every representative point (mean / median / p90 of
# |gap|) is null. The moderated-mediation test is marginal and runs opposite to
# theory (larger ACME at low |gap|). We therefore report null mediation.
panel_med$abs_log_crime_gap <- abs(panel_med$log_crime_gap)
panel_med$abs_rank_gap <- abs(panel_med$rank_gap)

m_mediator_abs <- lm(
  inc_minus_opp_avg_post ~
    inc_pre +
    opp_avg_pre +
    abs_log_crime_gap * Treatment_Group +
    abs_rank_gap * Treatment_Group +
    coalition_pre,
  data = panel_med
)

m_outcome_abs <- lm(
  Vote_home_post ~
    inc_minus_opp_avg_post +
    inc_pre +
    opp_avg_pre +
    abs_log_crime_gap * Treatment_Group +
    abs_rank_gap * Treatment_Group +
    coalition_pre,
  data = panel_med
)

cat("\n[|gap| robustness] a-path interaction coefs (mediator, |gap| x T4):\n")
cf_abs <- coef(summary(m_mediator_abs))
print(cf_abs[grep("Treatment_GroupT4", rownames(cf_abs)), , drop = FALSE])
cat("[|gap| robustness] b-path (mediator -> vote):\n")
print(coef(summary(m_outcome_abs))["inc_minus_opp_avg_post", , drop = FALSE])

# T4 ACME evaluated unconditionally and at representative |gap| levels.
mediate_T4_abs <- function(cov = NULL) {
  mediate(
    m_mediator_abs,
    m_outcome_abs,
    treat = "Treatment_Group",
    mediator = "inc_minus_opp_avg_post",
    control.value = "control",
    treat.value = "T4",
    covariates = cov,
    robustSE = TRUE,
    sims = 1000
  )
}
acg <- panel_med$abs_log_crime_gap
arg <- panel_med$abs_rank_gap
p <- function(x, prob) as.numeric(quantile(x, prob, na.rm = TRUE))

med_abs_T4 <- mediate_T4_abs()
cat("\n[|gap| robustness] T4 ACME, unconditional average:\n")
print(summary(med_abs_T4))

for (lbl in c("median", "mean", "p90")) {
  cg <- switch(lbl,
    median = median(acg, na.rm = TRUE),
    mean = mean(acg, na.rm = TRUE),
    p90 = p(acg, 0.90)
  )
  rg <- switch(lbl,
    median = median(arg, na.rm = TRUE),
    mean = mean(arg, na.rm = TRUE),
    p90 = p(arg, 0.90)
  )
  cat(sprintf("\n[|gap| robustness] T4 ACME at %s |gaps|:\n", lbl))
  print(summary(mediate_T4_abs(
    list(abs_log_crime_gap = cg, abs_rank_gap = rg)
  )))
}

# Formal moderated mediation: does the ACME differ between low (p10) and high
# (p90) magnitude-of-surprise respondents?
cat("\n[|gap| robustness] test.modmed: LOW |gaps| (p10) vs HIGH |gaps| (p90):\n")
print(test.modmed(
  med_abs_T4,
  covariates.1 = list(abs_log_crime_gap = p(acg, 0.10), abs_rank_gap = p(arg, 0.10)),
  covariates.2 = list(abs_log_crime_gap = p(acg, 0.90), abs_rank_gap = p(arg, 0.90)),
  sims = 1000
))
