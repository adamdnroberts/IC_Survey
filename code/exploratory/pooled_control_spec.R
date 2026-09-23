# Main log specification for BOTH vote intention and the belief outcomes, with
# control2 POOLED into the control baseline instead of dropped.
#
# The published analysis drops control2 entirely (vote_update_analysis.R,
# t4_belief_updates_comparison.R) and a companion script uses control2 as the
# baseline INSTEAD of control (vote_update_control2_baseline.R). This script is
# the third option: both placebo arms treated as one baseline, which buys back
# ~n control2 respondents and makes the baseline "saw placebo content" rather
# than "saw placebo content with no chart".
#
# WHAT POOLING COSTS. control and control2 are not the same placebo. control
# shows home precipitation only; control2 adds a comparison bar chart against
# same-state municipalities. Pooling therefore assumes the presence of a chart
# does not itself move the outcome. That assumption is testable and is checked
# at the end of this script (see "Pooling diagnostic"): if the control-vs-control2
# contrast is non-null, the pooled baseline is a blend of two different things
# and the treatment coefficients are biased toward whichever it resembles less.
#
# THE rank_gap DECISION. rank_gap = actual_rank - rank_prior. For arms that show
# a crime comparison (T2-T4) actual_rank is the home municipality's robbery rank
# among the municipalities displayed. For plain control nobody is shown a
# comparison, so rank_gap is a counterfactual: the rank they WOULD have been
# shown. control2 respondents are shown a chart, but of PRECIPITATION, so two
# different quantities could fill this column for them:
#
#   crime-based (default here): the counterfactual robbery rank, identical in
#     construction to plain control. Keeps rank_gap the same variable for every
#     row, which is what a pooled baseline requires -- a single regressor cannot
#     mean robbery rank for some baseline rows and rainfall rank for others.
#   precipitation-based: the rank they actually saw, as
#     vote_update_control2_baseline.R builds it. Coherent when control2 is its
#     OWN baseline, incoherent when pooled into one.
#
# Default is crime-based. Set use_precip_rank_for_control2 <- TRUE to run the
# other variant; both are reported so the choice is visible rather than buried.
#
# Usage:
#   "/c/Program Files/R/R-4.5.1/bin/Rscript.exe" --vanilla \
#     code/exploratory/pooled_control_spec.R

library(estimatr)
library(dplyr)
library(broom)

if (!exists("use_precip_rank_for_control2")) {
  use_precip_rank_for_control2 <- FALSE
}

ci_alpha <- 0.05

load("data/derived/survey_panel_dataset.Rdata")

num <- function(x) suppressWarnings(as.numeric(x))

# ── Outcomes and controls, built exactly as in the source scripts ─────────────
panel$Vote_home_post <- as.integer(
  !is.na(panel$coalition_post) &
    !is.na(panel$home_coalition) &
    panel$home_coalition == panel$coalition_post
)

panel$coalition_pre[is.na(panel$coalition_pre)] <- "Other"
panel$inc_vote <- as.numeric(panel$coalition_pre == panel$home_coalition)

# Belief outcomes: incumbent rating, the average of the coalitions that do NOT
# govern the home municipality, and their difference (same construction as
# code/plots/t4_belief_updates_comparison.R).
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

opp <- t(vapply(
  seq_len(nrow(panel)),
  function(i) {
    hc <- panel$home_coalition[i]
    if (is.na(hc)) {
      return(c(post = NA_real_, pre = NA_real_))
    }
    keep <- colnames(coalition_pre_rating) != hc
    po <- coalition_post_rating[i, keep]
    pr <- coalition_pre_rating[i, keep]
    c(
      post = if (all(is.na(po))) NA_real_ else mean(po, na.rm = TRUE),
      pre = if (all(is.na(pr))) NA_real_ else mean(pr, na.rm = TRUE)
    )
  },
  numeric(2)
))

panel$opp_avg_post <- opp[, "post"]
panel$opp_avg_pre <- opp[, "pre"]
panel$inc_post <- num(panel$Home_Crime_Handling_Post)
panel$inc_pre <- num(panel$Home_Crime_Handling_Pre)
panel$inc_minus_opp_avg_post <- panel$inc_post - panel$opp_avg_post

# ── Optional: rebuild control2's rank_gap from precipitation ─────────────────
# Mirrors vote_update_control2_baseline.R. Off by default; see header.
if (use_precip_rank_for_control2) {
  precip <- readRDS("data/precip_data.rds")
  precip_of <- function(ids) precip$precip_mm[match(ids, precip$muni_id)]
  home_precip <- precip_of(panel$Found_Municipality_ID)
  comp_precip <- sapply(1:4, function(i) {
    precip_of(panel[[paste0("Comparison_Muni_", i, "_ID")]])
  })
  is_c2 <- panel$Treatment_Group == "control2"
  panel$actual_rank[is_c2] <- (1 + rowSums(
    comp_precip < home_precip,
    na.rm = TRUE
  ))[is_c2]
  panel$rank_gap <- panel$actual_rank - panel$rank_prior
}

# ── Samples ──────────────────────────────────────────────────────────────────
base <- filter(panel, muni_changed == 0, Attention_Check == "somewhat_agree")

# Published baseline: control2 dropped, control is the reference.
d_drop <- base %>%
  filter(Treatment_Group != "control2") %>%
  mutate(arm = relevel(droplevels(factor(Treatment_Group)), ref = "control"))

# Pooled baseline: control2 folded into control.
d_pool <- base %>%
  mutate(
    arm = factor(
      if_else(Treatment_Group == "control2", "control", Treatment_Group)
    ),
    arm = relevel(arm, ref = "control")
  )

# ── Model fitting on a given sample ──────────────────────────────────────────
# Vote uses the m_log form from vote_update_analysis.R; the belief outcomes use
# the harmonized form from t4_belief_updates_comparison.R, in which every belief
# model shares one right-hand side so their coefficients decompose exactly.
vote_rhs <- "log_crime_gap * arm + rank_gap * arm + inc_vote"
belief_rhs <- paste(
  "inc_pre",
  "opp_avg_pre",
  "log_crime_gap * arm",
  "rank_gap * arm",
  "coalition_pre",
  sep = " + "
)

fit <- function(outcome, rhs, d) {
  lm_robust(
    as.formula(paste(outcome, "~", rhs)),
    alpha = ci_alpha,
    data = d,
    se_type = "HC2"
  )
}

# Pull the gap x arm interactions and put them on a "1 SD of the gap" scale,
# using SDs computed on the sample actually being fit.
grab <- function(model, d, label) {
  cg_sd <- sd(d$log_crime_gap, na.rm = TRUE)
  rg_sd <- sd(d$rank_gap, na.rm = TRUE)
  tidy(model) %>%
    filter(grepl(":", term), grepl("arm", term)) %>%
    mutate(
      gap = if_else(grepl("log_crime_gap", term), "CG", "RG"),
      arm = gsub(".*arm([A-Za-z0-9]+).*", "\\1", term),
      sd = if_else(gap == "CG", cg_sd, rg_sd),
      estimate = estimate * sd,
      std.error = std.error * sd,
      t = estimate / std.error,
      outcome = label
    ) %>%
    dplyr::select(outcome, gap, arm, estimate, std.error, t, p.value)
}

run_all <- function(d, tag) {
  bind_rows(
    grab(fit("Vote_home_post", vote_rhs, d), d, "vote"),
    grab(fit("inc_post", belief_rhs, d), d, "inc"),
    grab(fit("opp_avg_post", belief_rhs, d), d, "opp"),
    grab(fit("inc_minus_opp_avg_post", belief_rhs, d), d, "inc_minus_opp")
  ) %>%
    mutate(spec = tag)
}

res <- bind_rows(run_all(d_drop, "drop_c2"), run_all(d_pool, "pool_c2"))

# ── Side-by-side comparison ──────────────────────────────────────────────────
cmp <- res %>%
  tidyr::pivot_wider(
    id_cols = c(outcome, gap, arm),
    names_from = spec,
    values_from = c(estimate, std.error, t)
  ) %>%
  transmute(
    outcome,
    gap,
    arm,
    est_drop = estimate_drop_c2,
    est_pool = estimate_pool_c2,
    se_drop = std.error_drop_c2,
    se_pool = std.error_pool_c2,
    t_drop = t_drop_c2,
    t_pool = t_pool_c2,
    # How far the estimate moved, in standard errors of the published estimate.
    # Anything much above ~0.3 means pooling is not a cosmetic change.
    shift_in_se = (est_pool - est_drop) / se_drop
  ) %>%
  arrange(outcome, gap, arm)

cat("\nControl2 rank_gap built from: ",
  if (use_precip_rank_for_control2) "PRECIPITATION" else "crime (default)",
  "\n", sep = "")
cat("N dropped-c2 = ", nrow(d_drop), ", N pooled = ", nrow(d_pool),
  " (+", nrow(d_pool) - nrow(d_drop), ")\n", sep = "")
cat("\nInteraction coefficients, standardized to 1 SD of the gap:\n")
print(as.data.frame(cmp), digits = 3, row.names = FALSE)

# ── Pooling diagnostic ───────────────────────────────────────────────────────
# Pooling is only legitimate if control and control2 behave alike. Refit on the
# two placebo arms alone, with control2 as a "treatment", and test its main
# effect and both gap interactions. Non-null here invalidates the pooling.
d_plac <- base %>%
  filter(Treatment_Group %in% c("control", "control2")) %>%
  mutate(arm = relevel(droplevels(factor(Treatment_Group)), ref = "control"))

cat("\n\nPooling diagnostic: control2 vs control, placebo arms only\n")
cat("(A non-null coefficient means the two placebos differ and should not be",
  "pooled.)\n")
for (o in c("Vote_home_post", "inc_post", "inc_minus_opp_avg_post")) {
  rhs <- if (o == "Vote_home_post") vote_rhs else belief_rhs
  m <- fit(o, rhs, d_plac)
  tt <- tidy(m) %>%
    filter(grepl("arm", term)) %>%
    dplyr::select(term, estimate, std.error, p.value)
  cat("\n--", o, "--\n")
  print(as.data.frame(tt), digits = 3, row.names = FALSE)
}
