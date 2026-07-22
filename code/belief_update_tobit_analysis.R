# Two-limit Tobit version of the m_winsorized coefficient plot in
# belief_update_analysis.R. Outcome: Home_Crime_Handling_Post (0-100 slider,
# censored at both ends), with the pre-treatment level as a covariate. Plots the
# standardized CG x Treatment and RG x Treatment interaction coefficients with
# robust (Huber-White sandwich) SEs — the Tobit analog of lm_robust's HC2.

library(AER) # tobit() — wraps survival::survreg
library(sandwich)
library(lmtest)
library(dplyr)
library(ggplot2)

load("~/IC_Survey/data/survey_panel_dataset.Rdata")

if (!exists("ci_alpha")) {
  ci_alpha <- 0.01
}

# Colorblind-friendly (Okabe-Ito) palette, matching belief_update_analysis.R
arm_colors <- c(
  control2 = "#999999",
  T1 = "#56B4E9",
  T2 = "#009E73",
  T3 = "#E69F00",
  T4 = "#0072B2"
)

panel$Attention_Check_Fail <- ifelse(
  panel$Attention_Check == "somewhat_agree",
  1,
  0
)

# Same sample as m_log (control2 excluded)
panel <- panel %>%
  filter(
    muni_changed == 0,
    Attention_Check == "somewhat_agree",
    Treatment_Group != "control2"
  )

panel$Home_Crime_Handling_Post <- as.numeric(panel$Home_Crime_Handling_Post)
panel$Home_Crime_Handling_Pre <- as.numeric(panel$Home_Crime_Handling_Pre)

m_tobit <- tobit(
  Home_Crime_Handling_Post ~
    Home_Crime_Handling_Pre +
    log_crime_gap * as.factor(Treatment_Group) +
    rank_gap * as.factor(Treatment_Group) +
    coalition_pre,
  left = 0,
  right = 100,
  data = panel
)

# Robust (sandwich) coefficient table — the Tobit counterpart to HC-robust SEs.
ct <- coeftest(m_tobit, vcov. = sandwich(m_tobit))

# SDs of the standardizing predictors (as in belief_update_analysis.R)
log_crime_gap_sd <- sd(panel$log_crime_gap, na.rm = TRUE)
rank_gap_sd <- sd(panel$rank_gap, na.rm = TRUE)

# Pull the CG x Treatment and RG x Treatment interaction terms and standardize,
# mirroring extract_coef_plot() in belief_update_analysis.R. Tobit inference is
# z-based (MLE), so CIs use normal quantiles rather than the t.
cg_pattern <- "log_crime_gap"

coef_plot_tobit <- data.frame(
  term = rownames(ct),
  estimate = ct[, "Estimate"],
  std.error = ct[, "Std. Error"],
  row.names = NULL
) %>%
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
    group = case_when(
      grepl(paste0("^", cg_pattern, ":as\\.factor"), term) ~ "CG × Treatment",
      TRUE ~ "RG × Treatment"
    ),
    treatment = sub(".*as\\.factor\\([^)]*\\)", "", term) %>%
      sub(":.*$", "", .),
    sd = if_else(group == "CG × Treatment", log_crime_gap_sd, rank_gap_sd),
    across(c(estimate, std.error), ~ . * sd),
    conf.low = estimate - qnorm(0.995) * std.error, # 99% (thin bar)
    conf.high = estimate + qnorm(0.995) * std.error,
    conf.low95 = estimate - qnorm(0.975) * std.error, # 95% (thick bar)
    conf.high95 = estimate + qnorm(0.975) * std.error
  ) %>%
  select(-sd)

inc_update_coef_plot_tobit <- ggplot(
  subset(coef_plot_tobit, treatment != "control2"),
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
    title = "Incumbent (home government) crime-handling update",
    y = "Treatment group",
    x = "Standardized coefficient (1 SD increase in predictor)",
    caption = paste0(
      "Two-limit Tobit (0-100), robust SEs, N = ",
      length(m_tobit$linear.predictors),
      ", thick bar 95% CI, thin 99% CI"
    )
  ) +
  theme_minimal()

print(inc_update_coef_plot_tobit)

ggsave(
  "latex/images/inc_update_tobit_coef_plot.pdf",
  plot = inc_update_coef_plot_tobit,
  width = 7,
  height = 4.5
)

# control2 is now excluded from the sample (see filter above), so this control2
# vs. T2 contrast is no longer estimable.
# library(car)
# V <- sandwich::sandwich(m_tobit) # robust vcov; omit for model-based SEs
#
# t1 <- "as.factor(Treatment_Group)control2:rank_gap"
# t2 <- "as.factor(Treatment_Group)T2:rank_gap"
#
# # Option A — car (matches what belief_update_analysis.R uses)
# linearHypothesis(m_tobit, paste(t1, "=", t2), vcov. = V)
# linearHypothesis(m_tobit, paste(t1, "=", t2))

# ── Home-party (governing coalition) version ─────────────────────────────────
# Same two-limit Tobit, but the outcome is the crime-handling rating of the
# coalition GOVERNING the respondent's home municipality (Home_Party_*), with
# its pre-treatment level as a covariate — the Tobit analog of the home-party
# spec in home_party_update_analysis.R.
panel$Home_Party_Crime_Handling_Post <- as.numeric(
  panel$Home_Party_Crime_Handling_Post
)
panel$Home_Party_Crime_Handling_Pre <- as.numeric(
  panel$Home_Party_Crime_Handling_Pre
)

m_tobit_party <- tobit(
  Home_Party_Crime_Handling_Post ~
    Home_Party_Crime_Handling_Pre +
    log_crime_gap * as.factor(Treatment_Group) +
    rank_gap * as.factor(Treatment_Group),
  left = 0,
  right = 100,
  data = panel
)

ct_party <- coeftest(m_tobit_party, vcov. = sandwich(m_tobit_party))

coef_plot_tobit_party <- data.frame(
  term = rownames(ct_party),
  estimate = ct_party[, "Estimate"],
  std.error = ct_party[, "Std. Error"],
  row.names = NULL
) %>%
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
    group = case_when(
      grepl(paste0("^", cg_pattern, ":as\\.factor"), term) ~ "CG × Treatment",
      TRUE ~ "RG × Treatment"
    ),
    treatment = sub(".*as\\.factor\\([^)]*\\)", "", term) %>%
      sub(":.*$", "", .),
    sd = if_else(group == "CG × Treatment", log_crime_gap_sd, rank_gap_sd),
    across(c(estimate, std.error), ~ . * sd),
    conf.low = estimate - qnorm(0.995) * std.error, # 99% (thin bar)
    conf.high = estimate + qnorm(0.995) * std.error,
    conf.low95 = estimate - qnorm(0.975) * std.error, # 95% (thick bar)
    conf.high95 = estimate + qnorm(0.975) * std.error
  ) %>%
  select(-sd)

party_update_coef_plot_tobit <- ggplot(
  subset(coef_plot_tobit_party, treatment != "control2"),
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
    title = "Home-party (governing coalition) crime-handling update",
    y = "Treatment group",
    x = "Standardized coefficient (1 SD increase in predictor)",
    caption = paste0(
      "Two-limit Tobit (0-100), robust SEs, N = ",
      length(m_tobit_party$linear.predictors),
      ", thick bar 95% CI, thin 99% CI"
    )
  ) +
  theme_minimal()

print(party_update_coef_plot_tobit)

ggsave(
  "latex/images/home_party_update_tobit_coef_plot.pdf",
  plot = party_update_coef_plot_tobit,
  width = 7,
  height = 4.5
)
