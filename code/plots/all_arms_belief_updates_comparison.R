# All-arms companion to code/plots/t4_belief_updates_comparison.R.
#
# Identical common sample, models and standardization as that script; the only
# difference is that the plots keep all four treatment arms (T1-T4) instead of
# filtering to T4, with one plot per gap measure:
#
#   1. Incumbent update:
#        outcome inc_post (Home_Crime_Handling_Post).
#   2. Other-coalitions update:
#        outcome opp_avg_post (mean rating of the coalitions NOT governing the
#        respondent's municipality). Not plotted; fit so the decomposition below
#        can be checked.
#   3. Incumbent vs. other coalitions:
#        post difference outcome inc_minus_opp_avg_post.
#   4. Home-party update:
#        outcome party_post (Home_Party_Crime_Handling_Post). Uses the common
#        RHS plus its own pre level, so it is comparable in controls but is not
#        part of the decomposition.
#
# Why the right-hand sides must match. Because outcome 3 is outcome 1 minus
# outcome 2, the coefficients obey the exact algebraic identity
#
#     beta(inc vs. other) = beta(incumbent) - beta(other coalitions)
#
# but ONLY when all three models share the same regressors and sample. Earlier
# versions controlled for the corresponding pre level(s) only, so m_inc used
# inc_pre, m_opp used opp_avg_pre, and m_inc_other used both: the models
# differed in form, not just in outcome, and the identity held only
# approximately. With the RHS harmonized, any gap between the incumbent and the
# difference estimate is EXACTLY the treatment's effect on the opposition
# rating, and whatever remains is a difference in precision alone. The check at
# the end of this script is a regression guard against future edits, not
# evidence: once the RHS and sample match, the identity holds by construction.
#
# Harmonized spec (differs from the source scripts, which each use their own
# sample/controls and change-score outcomes):
#   - Sample: muni_changed == 0, attention-check passers, control2 EXCLUDED.
#   - Form: post-treatment outcome regressed on BOTH pre levels (inc_pre +
#     opp_avg_pre) plus coalition_pre and the full gap x arm interactions --
#     identical across models 1-3.
#   - Standardization: predictor SDs computed once on the common sample, so all
#     bars are on a common "1 SD increase in predictor" scale.

library(estimatr)
library(dplyr)
library(ggplot2)
library(broom)

load("data/derived/survey_panel_dataset.Rdata")

panel_full <- panel

if (!exists("ci_alpha")) {
  ci_alpha <- 0.01
}

# Colorblind-friendly (Okabe-Ito) palette, matching vote_update_analysis.R
arm_colors <- c(
  control2 = "#999999",
  T1 = "#56B4E9",
  T2 = "#009E73",
  T3 = "#D55E00",
  T4 = "#0072B2"
)

# Shared coefficient extractor (identical to the source scripts): pulls the
# CG × Treatment and RG × Treatment interaction rows, standardizes by the given
# predictor SDs, and adds 95% CIs alongside the model's (99%) CIs.
extract_coef_plot <- function(model, cg_pattern, model_label, cg_sd, rg_sd) {
  tidy(model, conf.int = TRUE) %>%
    filter(grepl(
      paste0(
        cg_pattern,
        ":as\\.factor|as\\.factor.*:rank_gap(?!:)|(?<=:)rank_gap:as\\.factor"
      ),
      term,
      perl = TRUE
    )) %>%
    filter(!grepl(paste0(cg_pattern, ":rank_gap:as\\.factor"), term)) %>%
    mutate(
      model = model_label,
      group = case_when(
        grepl(paste0("^", cg_pattern, ":as\\.factor"), term) ~ "CG × Treatment",
        TRUE ~ "RG × Treatment"
      ),
      treatment = sub(".*as\\.factor\\([^)]*\\)", "", term) %>%
        sub(":.*$", "", .)
    ) %>%
    mutate(
      sd = if_else(group == "CG × Treatment", cg_sd, rg_sd),
      across(c(estimate, conf.low, conf.high, std.error), ~ . * sd),
      conf.low95 = estimate - qt(0.975, df) * std.error,
      conf.high95 = estimate + qt(0.975, df) * std.error
    ) %>%
    dplyr::select(-sd)
}

num <- function(x) suppressWarnings(as.numeric(x))

# ── Common sample: control2 excluded, attention-check passers, muni unchanged ──
panel <- filter(
  panel_full,
  muni_changed == 0 &
    Attention_Check == "somewhat_agree" &
    Treatment_Group != "control2"
)

# ── Build the incumbent-vs-opposition post-on-post outcome (same construction
#    as home_party_update_analysis.R) on the common sample ─────────────────────
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

opp_benchmarks <- t(vapply(
  seq_len(nrow(panel)),
  function(i) {
    hc <- panel$home_coalition[i]
    if (is.na(hc)) {
      return(c(avg_post = NA_real_, avg_pre = NA_real_))
    }
    keep <- colnames(coalition_pre_rating) != hc
    pre <- coalition_pre_rating[i, keep]
    post <- coalition_post_rating[i, keep]
    avg_post <- if (all(is.na(post))) NA_real_ else mean(post, na.rm = TRUE)
    avg_pre <- if (all(is.na(pre))) NA_real_ else mean(pre, na.rm = TRUE)
    c(avg_post = avg_post, avg_pre = avg_pre)
  },
  numeric(2)
))

panel$opp_avg_post <- opp_benchmarks[, "avg_post"]
panel$opp_avg_pre <- opp_benchmarks[, "avg_pre"]
panel$inc_post <- num(panel$Home_Crime_Handling_Post)
panel$inc_pre <- num(panel$Home_Crime_Handling_Pre)
panel$party_post <- num(panel$Home_Party_Crime_Handling_Post)
panel$party_pre <- num(panel$Home_Party_Crime_Handling_Pre)
panel$inc_minus_opp_avg_post <- panel$inc_post - panel$opp_avg_post

# ── Force a single estimation sample ─────────────────────────────────────────
# Listwise deletion is per-model, so models with different outcomes can end up
# on different rows even with an identical RHS (e.g. a row with opp_avg_post
# missing survives in m_inc but not in m_inc_other). The decomposition identity
# requires the SAME rows, so drop incomplete cases up front rather than leaving
# it to each lm_robust call.
# Only the variables the DECOMPOSITION models use. party_pre/party_post are
# deliberately excluded: m_party is not part of the identity, so gating the
# other three models on its missingness would drop rows for no reason -- and
# because log_crime_gap_sd is computed after this filter, that would shift every
# standardized coefficient in figures the paper quotes numerically.
# inc_minus_opp_avg_post is omitted as redundant: it is NA iff a component is.
analysis_vars <- c(
  "inc_post",
  "opp_avg_post",
  "inc_pre",
  "opp_avg_pre",
  "log_crime_gap",
  "rank_gap",
  "Treatment_Group",
  "coalition_pre"
)

n_before <- nrow(panel)
panel <- panel[complete.cases(panel[, analysis_vars]), ]
cat(
  "Common estimation sample: ", nrow(panel), " of ", n_before,
  " rows (", n_before - nrow(panel), " dropped for missingness)\n",
  sep = ""
)

# ── Standardization SDs computed once on the common sample ────────────────────
log_crime_gap_sd <- sd(panel$log_crime_gap, na.rm = TRUE)
rank_gap_sd <- sd(panel$rank_gap, na.rm = TRUE)

# ── Models: common sample, IDENTICAL right-hand side ─────────────────────────
# Every outcome is a POST-treatment level (or a difference of two) regressed on
# BOTH pre levels plus coalition_pre and the full gap x arm interactions. The
# RHS is built once and reused so the three decomposition models cannot drift
# apart; see the identity described in the header.
common_rhs <- paste(
  "inc_pre",
  "opp_avg_pre",
  "log_crime_gap * as.factor(Treatment_Group)",
  "rank_gap * as.factor(Treatment_Group)",
  "coalition_pre",
  sep = " + "
)

fit_common <- function(outcome, extra = NULL) {
  rhs <- paste(c(common_rhs, extra), collapse = " + ")
  lm_robust(
    as.formula(paste(outcome, "~", rhs)),
    alpha = ci_alpha,
    data = panel,
    se_type = "HC2"
  )
}

m_inc <- fit_common("inc_post")
m_opp <- fit_common("opp_avg_post")
m_inc_other <- fit_common("inc_minus_opp_avg_post")

# Home-party update is not part of the decomposition, so it adds its own pre
# level on top of the common control set.
m_party <- fit_common("party_post", extra = "party_pre")

cp_inc <- extract_coef_plot(
  m_inc,
  "log_crime_gap",
  "Incumbent update",
  log_crime_gap_sd,
  rank_gap_sd
)
cp_party <- extract_coef_plot(
  m_party,
  "log_crime_gap",
  "Home-party update",
  log_crime_gap_sd,
  rank_gap_sd
)
cp_inc_other <- extract_coef_plot(
  m_inc_other,
  "log_crime_gap",
  "Incumbent vs. other coalitions",
  log_crime_gap_sd,
  rank_gap_sd
)
cp_opp <- extract_coef_plot(
  m_opp,
  "log_crime_gap",
  "Other-coalitions update",
  log_crime_gap_sd,
  rank_gap_sd
)

# ── Combine all four arms and plot ───────────────────────────────────────────
outcome_levels <- c(
  "Incumbent update",
  "Home-party update",
  "Incumbent vs. other coalitions"
)

arm_levels <- c("T1", "T2", "T3", "T4")

all_arm_coefs <- bind_rows(cp_inc, cp_party, cp_inc_other) %>%
  filter(treatment %in% arm_levels) %>%
  mutate(
    model = factor(model, levels = rev(outcome_levels)),
    # Reversed so T1 sits at the top of each outcome's dodged cluster, which
    # matches the top-to-bottom reading order of the legend.
    treatment = factor(treatment, levels = rev(arm_levels))
  )

# One plot per gap measure: outcomes on the y axis, the four arms dodged within
# each outcome.
build_arm_gap_plot <- function(
  group_label,
  x_label,
  outcomes = outcome_levels,
  title = group_label
) {
  ggplot(
    subset(all_arm_coefs, group == group_label & model %in% outcomes),
    aes(y = model, x = estimate, color = treatment)
  ) +
    geom_vline(xintercept = 0, linetype = "dashed", color = "grey50") +
    geom_errorbar(
      aes(xmin = conf.low95, xmax = conf.high95),
      orientation = "y",
      width = 0,
      linewidth = 0.5,
      position = position_dodge(width = 0.6)
    ) +
    geom_point(position = position_dodge(width = 0.6)) +
    scale_color_manual(
      values = arm_colors,
      breaks = arm_levels,
      name = "Arm"
    ) +
    labs(
      y = NULL,
      x = x_label,
      title = title,
      caption = "Bars show 95% CIs"
    ) +
    theme_minimal()
}

all_arms_updates_comparison_cg <- build_arm_gap_plot(
  "CG × Treatment",
  "Standardized coefficient (1 SD increase in crime gap)",
  # Same outcome restriction as the T4 CG plot.
  outcomes = c("Incumbent update", "Incumbent vs. other coalitions"),
  title = "CG × Treatment"
)

all_arms_updates_comparison_rg <- build_arm_gap_plot(
  "RG × Treatment",
  "Standardized coefficient (1 SD increase in rank gap)"
)

print(all_arms_updates_comparison_cg)
print(all_arms_updates_comparison_rg)

ggsave(
  "latex/images/all_arms_belief_updates_comparison_cg.pdf",
  plot = all_arms_updates_comparison_cg,
  width = 6.5,
  height = 4
)

ggsave(
  "latex/images/all_arms_belief_updates_comparison_rg.pdf",
  plot = all_arms_updates_comparison_rg,
  width = 6.5,
  height = 4.5
)

# ── Verify the decomposition ─────────────────────────────────────────────────
# NOTE: this is a REGRESSION GUARD, not a finding. OLS is linear in the
# outcome, so with an identical RHS and identical rows the identity
# beta3 = (X'X)^-1 X'(y1 - y2) = beta1 - beta2 holds by construction. The
# residual can only be non-zero if a future edit breaks the shared RHS or the
# common sample. Running it confirms nothing about the data.
#
# Reading the table: `opp` is how much of each arm's effect on the difference
# comes from the OPPOSITION rating moving rather than the incumbent rating.
# Near zero means the difference measure is just a less noisy way of measuring
# the same incumbent update -- in which case compare `se_inc` with
# `se_inc_other` to see whether the difference buys precision instead.
decomposition <- bind_rows(cp_inc, cp_opp, cp_inc_other) %>%
  # Both gap measures: the RG interactions decompose exactly as the CG ones do.
  dplyr::select(group, model, treatment, estimate, std.error) %>%
  tidyr::pivot_wider(
    names_from = model,
    values_from = c(estimate, std.error)
  ) %>%
  transmute(
    gap = sub(" .*", "", group),
    arm = treatment,
    inc = `estimate_Incumbent update`,
    opp = `estimate_Other-coalitions update`,
    inc_other = `estimate_Incumbent vs. other coalitions`,
    residual = inc_other - (inc - opp),
    se_inc = `std.error_Incumbent update`,
    se_opp = `std.error_Other-coalitions update`,
    se_inc_other = `std.error_Incumbent vs. other coalitions`,
    t_inc = inc / se_inc,
    t_opp = opp / se_opp,
    t_inc_other = inc_other / se_inc_other,
    se_ratio = se_inc_other / se_inc
  ) %>%
  arrange(gap, arm)

cat("
Decomposition (standardized, 1 SD of the relevant gap):
")
print(as.data.frame(decomposition), digits = 3, row.names = FALSE)

stopifnot(max(abs(decomposition$residual)) < 1e-8)
