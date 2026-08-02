# Compares the T4 interaction coefficients across three belief-update outcomes,
# fit on a SINGLE common sample in a common ANCOVA form (POST outcome controlling
# for PRE) so the three T4 estimates are directly comparable (apples-to-apples):
#
#   1. Incumbent update:
#        outcome inc_post (Home_Crime_Handling_Post), control inc_pre.
#   2. Home-party update:
#        outcome party_post (Home_Party_Crime_Handling_Post), control party_pre.
#   3. Incumbent vs. other coalitions:
#        post difference outcome inc_minus_opp_avg_post, controls inc_pre +
#        opp_avg_pre.
#
# Harmonized spec (differs from the source scripts, which each use their own
# sample/controls and change-score outcomes):
#   - Sample: muni_changed == 0, attention-check passers, control2 EXCLUDED.
#   - Form: post-treatment outcome regressed on the corresponding pre level(s);
#     coalition_pre included in every model as a shared control.
#   - Standardization: predictor SDs computed once on the common sample, so all
#     bars are on a common "1 SD increase in predictor" scale.

library(estimatr)
library(dplyr)
library(ggplot2)
library(broom)

load("~/IC_Survey/data/survey_panel_dataset.Rdata")

panel_full <- panel

if (!exists("ci_alpha")) {
  ci_alpha <- 0.01
}

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

# ── Standardization SDs computed once on the common sample ────────────────────
log_crime_gap_sd <- sd(panel$log_crime_gap, na.rm = TRUE)
rank_gap_sd <- sd(panel$rank_gap, na.rm = TRUE)

# ── Three models: common sample, common ANCOVA form ──────────────────────────
# All outcomes are POST-treatment levels, controlling for the corresponding
# PRE-treatment level(s); coalition_pre is included in every model so the shared
# control set is common.
m_inc <- lm_robust(
  inc_post ~
    inc_pre +
    log_crime_gap * as.factor(Treatment_Group) +
    rank_gap * as.factor(Treatment_Group) +
    coalition_pre,
  alpha = ci_alpha,
  data = panel,
  se_type = "HC2"
)

m_party <- lm_robust(
  party_post ~
    party_pre +
    log_crime_gap * as.factor(Treatment_Group) +
    rank_gap * as.factor(Treatment_Group) +
    coalition_pre,
  alpha = ci_alpha,
  data = panel,
  se_type = "HC2"
)

# The incumbent-vs-opposition outcome is itself a post difference and controls
# for both pre levels (inc_pre + opp_avg_pre).
m_inc_other <- lm_robust(
  inc_minus_opp_avg_post ~
    inc_pre +
    opp_avg_pre +
    log_crime_gap * as.factor(Treatment_Group) +
    rank_gap * as.factor(Treatment_Group) +
    coalition_pre,
  alpha = ci_alpha,
  data = panel,
  se_type = "HC2"
)

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

# ── Combine T4 rows and plot side by side ────────────────────────────────────
outcome_levels <- c(
  "Incumbent update",
  "Home-party update",
  "Incumbent vs. other coalitions"
)

t4_coefs <- bind_rows(cp_inc, cp_party, cp_inc_other) %>%
  filter(treatment == "T4") %>%
  mutate(model = factor(model, levels = rev(outcome_levels)))

t4_updates_comparison_plot <- ggplot(
  t4_coefs,
  aes(y = model, x = estimate)
) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey50") +
  geom_errorbar(
    aes(xmin = conf.low95, xmax = conf.high95),
    orientation = "y",
    width = 0,
    linewidth = 0.5,
    color = "#0072B2"
  ) +
  geom_point(color = "#0072B2") +
  facet_wrap(~group, scales = "free_x") +
  labs(
    #title = "T4 (same-coalition comparison) belief-update coefficients",
    y = NULL,
    x = "Standardized coefficient (1 SD increase in predictor)",
    caption = paste0(
      "Bars show 95% CIs"
    )
  ) +
  theme_minimal()

print(t4_updates_comparison_plot)

ggsave(
  "latex/images/t4_belief_updates_comparison.pdf",
  plot = t4_updates_comparison_plot,
  width = 8,
  height = 4
)

# Same coefficients, but one standalone plot per gap measure instead of a
# two-panel facet.
build_t4_gap_plot <- function(group_label, x_label) {
  ggplot(
    subset(t4_coefs, group == group_label),
    aes(y = model, x = estimate)
  ) +
    geom_vline(xintercept = 0, linetype = "dashed", color = "grey50") +
    geom_errorbar(
      aes(xmin = conf.low95, xmax = conf.high95),
      orientation = "y",
      width = 0,
      linewidth = 0.5,
      color = "#0072B2"
    ) +
    geom_point(color = "#0072B2") +
    labs(
      y = NULL,
      x = x_label,
      title = group_label,
      caption = "Bars show 95% CIs"
    ) +
    theme_minimal()
}

t4_updates_comparison_cg <- build_t4_gap_plot(
  "CG × Treatment",
  "Standardized coefficient (1 SD increase in crime gap)"
)

t4_updates_comparison_rg <- build_t4_gap_plot(
  "RG × Treatment",
  "Standardized coefficient (1 SD increase in rank gap)"
)

print(t4_updates_comparison_cg)
print(t4_updates_comparison_rg)

ggsave(
  "latex/images/t4_belief_updates_comparison_cg.pdf",
  plot = t4_updates_comparison_cg,
  width = 5.5,
  height = 4
)

ggsave(
  "latex/images/t4_belief_updates_comparison_rg.pdf",
  plot = t4_updates_comparison_rg,
  width = 5.5,
  height = 4
)
