# Belief outcomes with CONTROL2 as the omitted baseline instead of control.
#
# This is the belief-outcome companion to code/vote_update_control2_baseline.R,
# which does the same swap for vote intention only.
#
# WHY. The ranking-gap (RG) interactions in the belief models are estimated
# against plain control, which shows no comparison at all. RG is by construction
# a comparison quantity -- it is the gap between the rank the respondent is shown
# and the rank they expected -- so an RG interaction measured against plain
# control blends two things: the effect of the comparison's CONTENT, and the
# effect of being shown a comparison at all. Control2 shows a comparison bar
# chart with placebo (precipitation) content, so benchmarking against it nets out
# the second and leaves the first.
#
# That this matters is not hypothetical. Restricted to the two placebo arms,
# control2 has its own significant rank_gap slope on both belief outcomes
# (see code/exploratory/pooled_control_spec.R), which is exactly the "saw a
# chart" effect this script removes.
#
# THE rank_gap SWAP. As in vote_update_control2_baseline.R, control2 rows get
# actual_rank rebuilt from precipitation rather than robbery rates, because the
# rainfall chart is the ranking those respondents were actually shown. Every
# other arm keeps its crime-based actual_rank. rank_prior is left alone: it is
# the pre-treatment CRIME ranking, which the rainfall chart, as a placebo, fails
# to speak to.
#
# Models use the harmonized belief specification from
# code/plots/t4_belief_updates_comparison.R: one sample, one right-hand side, so
# the composite coefficient is exactly the difference of the two component
# coefficients.
#
# Usage:
#   "/c/Program Files/R/R-4.5.1/bin/Rscript.exe" --vanilla \
#     code/exploratory/belief_control2_baseline.R

library(estimatr)
library(dplyr)
library(broom)

ci_alpha <- 0.05

load("data/derived/survey_panel_dataset.Rdata")

num <- function(x) suppressWarnings(as.numeric(x))

panel$coalition_pre[is.na(panel$coalition_pre)] <- "Other"

# ── Belief outcomes ──────────────────────────────────────────────────────────
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

# ── control2: rank the comparison municipalities on RAINFALL, not crime ──────
# Identical construction to vote_update_control2_baseline.R.
precip <- readRDS("data/precip_data.rds")
precip_of <- function(ids) precip$precip_mm[match(ids, precip$muni_id)]

panel$home_precip <- precip_of(panel$Found_Municipality_ID)
comp_precip <- sapply(1:4, function(i) {
  precip_of(panel[[paste0("Comparison_Muni_", i, "_ID")]])
})
panel$actual_rank_precip <- 1 + rowSums(
  comp_precip < panel$home_precip,
  na.rm = TRUE
)

is_c2 <- panel$Treatment_Group == "control2"
panel$actual_rank[is_c2] <- panel$actual_rank_precip[is_c2]
panel$rank_gap <- panel$actual_rank - panel$rank_prior

# ── Samples: same filters, differing only in which placebo arm is dropped ────
base <- filter(panel, muni_changed == 0, Attention_Check == "somewhat_agree")

d_c2 <- base %>%
  filter(Treatment_Group != "control") %>%
  mutate(arm = relevel(droplevels(factor(Treatment_Group)), ref = "control2"))

d_c1 <- base %>%
  filter(Treatment_Group != "control2") %>%
  mutate(arm = relevel(droplevels(factor(Treatment_Group)), ref = "control"))

belief_rhs <- paste(
  "inc_pre",
  "opp_avg_pre",
  "log_crime_gap * arm",
  "rank_gap * arm",
  "coalition_pre",
  sep = " + "
)

fit <- function(outcome, d) {
  lm_robust(
    as.formula(paste(outcome, "~", belief_rhs)),
    alpha = ci_alpha,
    data = d,
    se_type = "HC2"
  )
}

grab <- function(model, d, label) {
  cg_sd <- sd(d$log_crime_gap, na.rm = TRUE)
  rg_sd <- sd(d$rank_gap, na.rm = TRUE)
  tidy(model) %>%
    filter(grepl(":", term), grepl("arm", term)) %>%
    mutate(
      gap = if_else(grepl("log_crime_gap", term), "CG", "RG"),
      arm = gsub(".*arm([A-Za-z0-9]+).*", "\\1", term),
      estimate = estimate * if_else(gap == "CG", cg_sd, rg_sd),
      std.error = std.error * if_else(gap == "CG", cg_sd, rg_sd),
      t = estimate / std.error,
      outcome = label
    ) %>%
    dplyr::select(outcome, gap, arm, estimate, std.error, t, p.value)
}

run_all <- function(d, tag) {
  bind_rows(
    grab(fit("inc_post", d), d, "inc"),
    grab(fit("opp_avg_post", d), d, "opp"),
    grab(fit("inc_minus_opp_avg_post", d), d, "inc_minus_opp")
  ) %>%
    mutate(spec = tag)
}

res <- bind_rows(run_all(d_c1, "vs_control"), run_all(d_c2, "vs_control2"))

cmp <- res %>%
  filter(arm %in% c("T1", "T2", "T3", "T4")) %>%
  tidyr::pivot_wider(
    id_cols = c(outcome, gap, arm),
    names_from = spec,
    values_from = c(estimate, t)
  ) %>%
  transmute(
    outcome,
    gap,
    arm,
    est_vs_c1 = estimate_vs_control,
    t_vs_c1 = t_vs_control,
    est_vs_c2 = estimate_vs_control2,
    t_vs_c2 = t_vs_control2,
    # What is left once the "saw a chart" component is netted out. A large drop
    # on RG means the effect was about seeing a comparison, not its content.
    retained = est_vs_c2 / est_vs_c1
  ) %>%
  arrange(outcome, gap, arm)

cat("\nN vs control = ", nrow(d_c1), ", N vs control2 = ", nrow(d_c2), "\n",
  sep = ""
)
cat("\nBelief interactions, standardized to 1 SD of the gap.\n")
cat("vs_c1 = published baseline (plain control); vs_c2 = placebo-chart",
  "baseline.\n")
print(as.data.frame(cmp), digits = 3, row.names = FALSE)
