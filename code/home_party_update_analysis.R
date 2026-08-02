# Mirrors the main analysis in belief_update_analysis.R, but the outcome is the
# crime-handling rating of the coalition GOVERNING the respondent's home
# municipality (Home_Party_Crime_Handling_Change), rather than the
# home-municipality ("incumbent") crime-handling rating (Home_Crime_Handling_Change).
#
# Home_Party_Crime_Handling_Change is built in create_panel_dataset.R: for each
# respondent it selects the coalition-specific crime-handling rating matching
# home_coalition (MORENA/PVEM/PT -> MORENA_*, PAN/PRI/PRD ->
# Coalition_PAN_PRI_PRD_*, MC -> MC_*), differenced wave 1 (pre) -> wave 2 (post).
#
# Same main spec (m_log), sample, standardization, and plot styling as
# belief_update_analysis.R — robustness checks omitted.

library(estimatr)
library(dplyr)
library(ggplot2)
library(broom)

load("~/IC_Survey/data/survey_panel_dataset.Rdata")

panel_full <- panel

if (!exists("ci_alpha")) {
  ci_alpha <- 0.01
}

# Colorblind-friendly (Okabe-Ito) palette, matching vote_update_analysis.R
arm_colors <- c(
  control2 = "#999999",
  T1 = "#56B4E9",
  T2 = "#009E73",
  T3 = "#E69F00",
  T4 = "#0072B2"
)

panel_with_failures <- filter(panel_full, muni_changed == 0)
panel <- filter(
  panel_with_failures,
  Attention_Check == "somewhat_agree" & Treatment_Group != "control2"
)

m_log <- lm_robust(
  Home_Party_Crime_Handling_Change ~
    log_crime_gap *
    as.factor(Treatment_Group) +
    rank_gap * as.factor(Treatment_Group) +
    coalition_pre,
  alpha = ci_alpha,
  data = panel,
  se_type = "HC2"
)

log_crime_gap_sd <- sd(panel$log_crime_gap, na.rm = TRUE)
rank_gap_sd <- sd(panel$rank_gap, na.rm = TRUE)

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

coef_plot_data <- extract_coef_plot(
  m_log,
  "log_crime_gap",
  "m_log",
  log_crime_gap_sd,
  rank_gap_sd
)

party_update_coef_plot <- ggplot(
  subset(coef_plot_data, treatment != "control2"),
  aes(
    y = treatment,
    x = estimate,
    color = treatment
  )
) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey50") +
  geom_errorbar(
    aes(xmin = conf.low, xmax = conf.high),
    orientation = "y",
    width = 0,
    linewidth = 0.5,
    position = position_dodge(width = 0.5)
  ) +
  geom_errorbar(
    aes(xmin = conf.low95, xmax = conf.high95),
    orientation = "y",
    width = 0,
    linewidth = 2,
    alpha = 0.4,
    position = position_dodge(width = 0.5)
  ) +
  geom_point(position = position_dodge(width = 0.5)) +
  scale_color_manual(values = arm_colors, guide = "none") +
  facet_wrap(~group, scales = "free_x") +
  labs(
    title = "Home-party (governing coalition) crime-handling update",
    y = "Treatment group",
    x = "Standardized coefficient (1 SD increase in predictor)",
    caption = paste0(
      "N = ",
      m_log$nobs,
      ", thick bar 95% CI, thin 99% CI"
    )
  ) +
  theme_minimal()

print(party_update_coef_plot)

ggsave(
  "latex/images/home_party_update_coef_plot.pdf",
  plot = party_update_coef_plot,
  width = 7,
  height = 4.5
)

# ── Home-party minus incumbent update ────────────────────────────────────────
# Outcome = home-party (governing coalition) crime-handling change MINUS the
# incumbent (home-government) crime-handling change. Sign convention:
#   positive => party rating moved more than the incumbent rating
#   negative => party rating moved less than the incumbent rating
# (i.e. negative means the change for the coalition governing the home
# municipality is smaller than the change for the home municipality itself.)
panel$party_minus_inc_change <- panel$Home_Party_Crime_Handling_Change -
  panel$Home_Crime_Handling_Change

m_diff <- lm_robust(
  party_minus_inc_change ~
    log_crime_gap *
    as.factor(Treatment_Group) +
    rank_gap * as.factor(Treatment_Group) +
    coalition_pre,
  alpha = ci_alpha,
  data = panel,
  se_type = "HC2"
)

coef_plot_diff <- extract_coef_plot(
  m_diff,
  "log_crime_gap",
  "m_diff",
  log_crime_gap_sd,
  rank_gap_sd
)

diff_update_coef_plot <- ggplot(
  subset(coef_plot_diff, treatment != "control2"),
  aes(
    y = treatment,
    x = estimate,
    color = treatment
  )
) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey50") +
  geom_errorbar(
    aes(xmin = conf.low, xmax = conf.high),
    orientation = "y",
    width = 0,
    linewidth = 0.5,
    position = position_dodge(width = 0.5)
  ) +
  geom_errorbar(
    aes(xmin = conf.low95, xmax = conf.high95),
    orientation = "y",
    width = 0,
    linewidth = 2,
    alpha = 0.4,
    position = position_dodge(width = 0.5)
  ) +
  geom_point(position = position_dodge(width = 0.5)) +
  scale_color_manual(values = arm_colors, guide = "none") +
  facet_wrap(~group, scales = "free_x") +
  labs(
    title = "Home-party minus incumbent crime-handling update",
    y = "Treatment group",
    x = "Standardized coefficient (1 SD increase in predictor)",
    caption = paste0(
      "Outcome: home-party minus incumbent crime-handling change. N = ",
      m_diff$nobs,
      ", thick bar 95% CI, thin 99% CI"
    )
  ) +
  theme_minimal()

print(diff_update_coef_plot)

ggsave(
  "latex/images/inc_minus_home_party_update_coef_plot.pdf",
  plot = diff_update_coef_plot,
  width = 7,
  height = 4.5
)

# ── Post-on-post opposition comparisons (pre-adjusted) ───────────────────────
# The opposition comparisons are modeled as the POST-treatment difference between
# a reference rating and an OPPOSITION benchmark, controlling for BOTH
# pre-treatment levels separately (reference pre + opposition pre). Reference =
# incumbent (home government, Home_Crime_Handling_*) or home party (governing
# coalition, Home_Party_Crime_Handling_*). "Opposition" = the coalitions that do
# NOT govern the home municipality, summarized two ways:
#   (a) average of the other coalitions' ratings, and
#   (b) the single other coalition the respondent rated HIGHEST at baseline (pre).
# Sign of the outcome: positive => reference rated above the opposition benchmark
# post-treatment; the pre controls net out the corresponding baseline gap. All
# ratings are 0-100 sliders (pre from wave 1, post from wave 2).
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

# Per-respondent opposition benchmarks (post & pre): average across the non-home
# coalitions, and the non-home coalition with the highest baseline (pre) rating
# (ties -> first). NA home_coalition or all-NA other priors -> NA.
opp_benchmarks <- t(vapply(
  seq_len(nrow(panel)),
  function(i) {
    hc <- panel$home_coalition[i]
    if (is.na(hc)) {
      return(c(
        avg_post = NA_real_,
        avg_pre = NA_real_,
        top_post = NA_real_,
        top_pre = NA_real_
      ))
    }
    keep <- colnames(coalition_pre_rating) != hc
    pre <- coalition_pre_rating[i, keep]
    post <- coalition_post_rating[i, keep]
    avg_post <- if (all(is.na(post))) NA_real_ else mean(post, na.rm = TRUE)
    avg_pre <- if (all(is.na(pre))) NA_real_ else mean(pre, na.rm = TRUE)
    if (all(is.na(pre))) {
      top_post <- NA_real_
      top_pre <- NA_real_
    } else {
      j <- which.max(pre)
      top_post <- post[[j]]
      top_pre <- pre[[j]]
    }
    c(
      avg_post = avg_post,
      avg_pre = avg_pre,
      top_post = top_post,
      top_pre = top_pre
    )
  },
  numeric(4)
))

panel$opp_avg_post <- opp_benchmarks[, "avg_post"]
panel$opp_avg_pre <- opp_benchmarks[, "avg_pre"]
panel$opp_top_post <- opp_benchmarks[, "top_post"]
panel$opp_top_pre <- opp_benchmarks[, "top_pre"]

# Reference POST/PRE levels
panel$inc_post <- num(panel$Home_Crime_Handling_Post)
panel$inc_pre <- num(panel$Home_Crime_Handling_Pre)
panel$party_post <- num(panel$Home_Party_Crime_Handling_Post)
panel$party_pre <- num(panel$Home_Party_Crime_Handling_Pre)

# Post-on-post difference outcomes
panel$inc_minus_opp_avg_post <- panel$inc_post - panel$opp_avg_post
panel$inc_minus_opp_top_post <- panel$inc_post - panel$opp_top_post
panel$party_minus_opp_avg_post <- panel$party_post - panel$opp_avg_post
panel$party_minus_opp_top_post <- panel$party_post - panel$opp_top_post

# Fit the m_log spec on a post-on-post difference outcome, controlling for both
# pre-treatment levels separately (reference pre + opposition pre).
fit_ancova <- function(outcome, ref_pre, opp_pre, data = panel) {
  fml <- as.formula(paste0(
    outcome,
    " ~ ",
    ref_pre,
    " + ",
    opp_pre,
    " +",
    " log_crime_gap * as.factor(Treatment_Group) +",
    " rank_gap * as.factor(Treatment_Group) + coalition_pre"
  ))
  lm_robust(fml, alpha = ci_alpha, data = data, se_type = "HC2")
}

# Build, print, and save the standardized interaction coefficient plot.
save_coef_plot <- function(model, model_label, title, caption, outfile) {
  cp <- extract_coef_plot(
    model,
    "log_crime_gap",
    model_label,
    log_crime_gap_sd,
    rank_gap_sd
  )
  p <- ggplot(
    subset(cp, treatment != "control2"),
    aes(y = treatment, x = estimate, color = treatment)
  ) +
    geom_vline(xintercept = 0, linetype = "dashed", color = "grey50") +
    geom_errorbar(
      aes(xmin = conf.low, xmax = conf.high),
      orientation = "y",
      width = 0,
      linewidth = 0.5,
      position = position_dodge(width = 0.5)
    ) +
    geom_errorbar(
      aes(xmin = conf.low95, xmax = conf.high95),
      orientation = "y",
      width = 0,
      linewidth = 2,
      alpha = 0.4,
      position = position_dodge(width = 0.5)
    ) +
    geom_point(position = position_dodge(width = 0.5)) +
    scale_color_manual(values = arm_colors, guide = "none") +
    facet_wrap(~group, scales = "free_x") +
    labs(
      title = title,
      y = "Treatment group",
      x = "Standardized coefficient (1 SD increase in predictor)",
      caption = caption
    ) +
    theme_minimal()
  print(p)
  ggsave(outfile, plot = p, width = 7, height = 4.5)
  invisible(p)
}

# ── Home-party vs opposition (post, pre-adjusted) ────────────────────────────
m_other <- fit_ancova("party_minus_opp_avg_post", "party_pre", "opp_avg_pre")
save_coef_plot(
  m_other,
  "m_other",
  "Home-party minus average of other coalitions (post, pre-adjusted)",
  paste0(
    "Outcome: home-party minus mean(other coalitions), post levels;",
    " controls for both pre levels. N = ",
    m_other$nobs,
    ", thick bar 95% CI, thin 99% CI"
  ),
  "latex/images/home_party_minus_other_coalitions_update_coef_plot.pdf"
)

m_highest <- fit_ancova("party_minus_opp_top_post", "party_pre", "opp_top_pre")
save_coef_plot(
  m_highest,
  "m_highest",
  "Home-party minus highest-rated other coalition (post, pre-adjusted)",
  paste0(
    "Outcome: home-party minus top baseline-rated other coalition, post levels;",
    " controls for both pre levels. N = ",
    m_highest$nobs,
    ", thick bar 95% CI, thin 99% CI"
  ),
  "latex/images/home_party_minus_highest_other_coalition_update_coef_plot.pdf"
)

# ── Incumbent vs opposition (post, pre-adjusted) ─────────────────────────────
m_inc_other <- fit_ancova("inc_minus_opp_avg_post", "inc_pre", "opp_avg_pre")
save_coef_plot(
  m_inc_other,
  "m_inc_other",
  "Incumbent minus average of other coalitions (post, pre-adjusted)",
  paste0(
    "Outcome: incumbent minus mean(other coalitions), post levels;",
    " controls for both pre levels. N = ",
    m_inc_other$nobs,
    ", thick bar 95% CI, thin 99% CI"
  ),
  "latex/images/incumbent_minus_other_coalitions_update_coef_plot.pdf"
)

# ── Comparison-arm refit: same model, T1 dropped from the estimation sample ───
# T1 delivers no cross-municipality comparison, so it carries no information
# about the other coalitions this outcome is built from. Restricting estimation
# to the comparison arms follows the specification used for the other
# coalition-level outcomes; Control remains the omitted arm.
panel_comparison <- filter(panel, Treatment_Group != "T1")

m_inc_other_comp <- fit_ancova(
  "inc_minus_opp_avg_post",
  "inc_pre",
  "opp_avg_pre",
  data = panel_comparison
)

# ── LaTeX table: CG x treatment interactions, comparison arms ────────────────
# Reports the crime-level-gap x arm interactions (the treatment effects
# conditional on the content of the information). The RG interactions,
# pre-treatment levels, arm main effects, and coalition_pre controls are
# estimated but not shown.

fmt_num <- function(x) {
  ifelse(is.na(x), "---", sprintf("%.2f", x))
}

fmt_p <- function(p) {
  ifelse(is.na(p), "---", ifelse(p < 0.001, "$<$0.001", sprintf("%.3f", p)))
}

# lm_robust term names -> readable labels, e.g.
# "log_crime_gap:as.factor(Treatment_Group)T4" -> "CG $\times$ T4".
clean_term <- function(x) {
  x <- gsub("as\\.factor\\(Treatment_Group\\)", "", x)
  x <- gsub("log_crime_gap", "CG", x)
  x <- gsub("rank_gap", "RG", x)
  x <- gsub(":", " $\\\\times$ ", x)
  x <- gsub("_", "\\\\_", x)
  x
}

# Keep the CG x arm interactions, in model order (T2 to T4). Rescale to a 1 SD
# increase in the crime-level gap; scaling the estimate and its standard error
# by the same constant leaves the t statistic and p-value unchanged.
td_inc_other <- tidy(m_inc_other_comp) %>%
  filter(grepl(
    "^log_crime_gap:as\\.factor\\(Treatment_Group\\)T[234]$",
    term
  )) %>%
  mutate(
    estimate = estimate * log_crime_gap_sd,
    std.error = std.error * log_crime_gap_sd
  )

if (nrow(td_inc_other) != 3) {
  stop(
    "Expected 3 CG x comparison-arm interactions, got ",
    nrow(td_inc_other),
    " — check the Treatment_Group levels in the estimation sample."
  )
}

coef_rows <- paste0(
  clean_term(td_inc_other$term),
  " & ",
  fmt_num(td_inc_other$estimate),
  " & ",
  fmt_num(td_inc_other$std.error),
  " & ",
  fmt_p(td_inc_other$p.value),
  " \\\\"
)

table_tex <- paste0(
  "\\begin{table}[htpb]\n",
  "\\centering\n",
  "\\small\n",
  "\\caption{Incumbent rating minus the mean rating of the other coalitions\n",
  "(post-treatment levels), regressed on both pre-treatment levels, the\n",
  "perception gaps interacted with treatment arm, and pre-treatment coalition\n",
  "preference; only the crime-level gap ($CG$) $\\times$ arm interactions are\n",
  "shown. Coefficients are standardized to a 1 SD increase in $CG$. HC2 robust\n",
  "standard errors. Sample: home municipality unchanged, attention-check\n",
  "passers, Control2 and T1 excluded (T1 delivers no cross-municipality\n",
  "comparison); Control is the omitted arm.}\n",
  "\\begin{tabular}{lrrr}\n",
  "\\toprule\n",
  "\\textbf{Term} & \\textbf{Estimate} & \\textbf{Std.\\ Error} & ",
  "\\textbf{$p$} \\\\\n",
  "\\midrule\n",
  paste(coef_rows, collapse = "\n"),
  "\n",
  "\\midrule\n",
  "$N$ & \\multicolumn{3}{l}{",
  m_inc_other_comp$nobs,
  "} \\\\\n",
  "$R^2$ & \\multicolumn{3}{l}{",
  fmt_num(m_inc_other_comp$r.squared),
  "} \\\\\n",
  "\\bottomrule\n",
  "\\end{tabular}\n",
  "\\label{tab:inc_vs_other_coalitions}\n",
  "\\end{table}\n"
)

dir.create("latex/tables", showWarnings = FALSE, recursive = TRUE)
writeLines(table_tex, "latex/tables/inc_vs_other_coalitions.tex")
cat("Wrote latex/tables/inc_vs_other_coalitions.tex\n")

# ── Test: is the CG x T4 interaction different from the other arms? ──────────
# The table shows T4 alone separated from zero, which is not the same as T4
# differing from the other arms. These are two-sided tests of that stronger
# claim: difference = b_T4 - b_Tx, SE from the HC2 variance-covariance matrix
# (same construction as difference_tests_vote_analysis.R). Reported on the
# standardized 1 SD CG scale used in the table; scaling the difference and its
# SE by the same constant leaves t and p unchanged.

cg_term <- function(g) paste0("log_crime_gap:as.factor(Treatment_Group)", g)

cg_contrasts <- function(model, others, base = "T4") {
  b <- coef(model)
  V <- vcov(model)
  base_term <- cg_term(base)
  stopifnot(all(c(base_term, sapply(others, cg_term)) %in% names(b)))
  bind_rows(lapply(others, function(g) {
    tx <- cg_term(g)
    diff <- b[[base_term]] - b[[tx]]
    se <- sqrt(V[base_term, base_term] + V[tx, tx] - 2 * V[base_term, tx])
    tstat <- diff / se
    data.frame(
      comparison = paste0(base, " - ", g),
      base_coef = b[[base_term]] * log_crime_gap_sd,
      other_coef = b[[tx]] * log_crime_gap_sd,
      diff = diff * log_crime_gap_sd,
      std.error = se * log_crime_gap_sd,
      t = tstat,
      p_two_sided = 2 * pt(-abs(tstat), model$df.residual)
    )
  }))
}

cat("\nH0: (CG x T4) - (CG x Tx) = 0   vs.  H1: != 0\n")
cat("Comparison-arm model (matches the table); per 1 SD increase in CG\n")
print(
  cg_contrasts(m_inc_other_comp, c("T2", "T3")),
  row.names = FALSE,
  digits = 4
)

# The T4-vs-T1 contrast needs T1 in the sample, so it comes from the
# full-sample fit rather than the restricted one behind the table.
cat("\nSame test against T1, from the full-sample model\n")
print(
  cg_contrasts(m_inc_other, c("T1", "T2", "T3")),
  row.names = FALSE,
  digits = 4
)

# Joint test that the comparison arms' CG interactions are all equal, i.e. that
# the content of the information matters no differently across them.
cg_joint <- car::linearHypothesis(
  m_inc_other_comp,
  c(
    paste(cg_term("T4"), "=", cg_term("T2")),
    paste(cg_term("T4"), "=", cg_term("T3"))
  )
)

cat("\nJoint test (comparison arms): CG x T2 = CG x T3 = CG x T4\n")
print(cg_joint, digits = 4)

m_inc_highest <- fit_ancova("inc_minus_opp_top_post", "inc_pre", "opp_top_pre")
save_coef_plot(
  m_inc_highest,
  "m_inc_highest",
  "Incumbent minus highest-rated other coalition (post, pre-adjusted)",
  paste0(
    "Outcome: incumbent minus top baseline-rated other coalition, post levels;",
    " controls for both pre levels. N = ",
    m_inc_highest$nobs,
    ", thick bar 95% CI, thin 99% CI"
  ),
  "latex/images/incumbent_minus_highest_other_coalition_update_coef_plot.pdf"
)
