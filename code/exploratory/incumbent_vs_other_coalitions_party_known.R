# Subgroup version of the m_inc_other model in home_party_update_analysis.R:
# the same "incumbent minus average of the other coalitions" post-on-post
# ANCOVA, refit on ONLY the respondents who correctly named the coalition
# governing their home municipality (home_party_known == 1, built in
# create_panel_dataset.R from Home_Governing_Party_Belief_w2 vs home_coalition).
#
# Rationale: the outcome contrasts the home incumbent against the coalitions
# that do not govern at home, so it is only well defined as a partisan contrast
# for respondents who know who governs at home. Respondents who guessed wrong or
# said "don't know" are dropped; so are respondents with NA home_coalition (home
# municipality absent from magar2024), who are already NA on the outcome.
#
# Everything else — sample filters, spec, standardization, plot styling — matches
# home_party_update_analysis.R.

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
  T3 = "#E69F00",
  T4 = "#0072B2"
)

panel_with_failures <- filter(panel_full, muni_changed == 0)
panel <- filter(
  panel_with_failures,
  Attention_Check == "somewhat_agree" & Treatment_Group != "control2"
)

# The subgroup restriction. NA home_party_known (NA home_coalition) is excluded
# along with the incorrect guessers — filter() drops NA rows.
n_before <- nrow(panel)
panel <- filter(panel, home_party_known == 1)
cat(
  "Home-party-known subgroup: ",
  nrow(panel),
  " of ",
  n_before,
  " respondents (",
  sprintf("%.1f", 100 * nrow(panel) / n_before),
  "%)\n",
  sep = ""
)

if (nrow(panel) == 0) {
  stop("No respondents with home_party_known == 1 after the sample filters.")
}

# SDs used to standardize the interaction coefficients. Computed within the
# subgroup, so the "1 SD" on the x axis is a 1 SD move for these respondents.
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

# ── Opposition benchmarks (post & pre) ───────────────────────────────────────
# Identical construction to home_party_update_analysis.R: for each respondent,
# average the crime-handling ratings of the coalitions that do NOT govern the
# home municipality. All ratings are 0-100 sliders (pre wave 1, post wave 2).
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
    c(
      avg_post = if (all(is.na(post))) NA_real_ else mean(post, na.rm = TRUE),
      avg_pre = if (all(is.na(pre))) NA_real_ else mean(pre, na.rm = TRUE)
    )
  },
  numeric(2)
))

panel$opp_avg_post <- opp_benchmarks[, "avg_post"]
panel$opp_avg_pre <- opp_benchmarks[, "avg_pre"]

# Reference (incumbent = home government) POST/PRE levels
panel$inc_post <- num(panel$Home_Crime_Handling_Post)
panel$inc_pre <- num(panel$Home_Crime_Handling_Pre)

# Post-on-post difference outcome. Positive => incumbent rated above the
# opposition average post-treatment; the pre controls net out the baseline gap.
panel$inc_minus_opp_avg_post <- panel$inc_post - panel$opp_avg_post

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

# ── Incumbent vs opposition (post, pre-adjusted), party-known subgroup ───────
m_inc_other_known <- fit_ancova(
  "inc_minus_opp_avg_post",
  "inc_pre",
  "opp_avg_pre"
)

print(summary(m_inc_other_known))

save_coef_plot(
  m_inc_other_known,
  "m_inc_other_known",
  paste0(
    "Incumbent minus average of other coalitions (post, pre-adjusted),\n",
    "respondents who named the home governing party correctly"
  ),
  paste0(
    "Outcome: incumbent minus mean(other coalitions), post levels;",
    " controls for both pre levels.\nSample restricted to correct",
    " home-governing-party guessers. N = ",
    m_inc_other_known$nobs,
    ", thick bar 95% CI, thin 99% CI"
  ),
  paste0(
    "latex/images/",
    "incumbent_minus_other_coalitions_party_known_coef_plot.pdf"
  )
)
