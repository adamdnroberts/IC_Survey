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
  Attention_Check == "somewhat_agree" #& Treatment_Group != "control2"
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
    select(-sd)
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
fit_ancova <- function(outcome, ref_pre, opp_pre) {
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
  lm_robust(fml, alpha = ci_alpha, data = panel, se_type = "HC2")
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
