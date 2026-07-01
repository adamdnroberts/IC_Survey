# Replicates the first analysis (m_winsorized) in belief_update_analysis.R —
# Home_Crime_Handling_Change ~ crime_gap_wins * Treatment + rank_gap * Treatment
# + comp_party_known, HC2 robust, sample = home muni unchanged & attention pass —
# but with three post-treatment COALITION crime-rating levels as the outcomes:
#   Coalition_PAN_PRI_PRD_Crime_Rating_Post, MORENA_Crime_Rating_Post,
#   MC_Crime_Rating_Post  (0-100 sliders, stored as character -> coerced numeric).

library(estimatr)
library(dplyr)
library(ggplot2)
library(broom)

load("~/IC_Survey/data/survey_panel_dataset.Rdata")

if (!exists("ci_alpha")) {
  ci_alpha <- 0.01
}

# Same sample as m_winsorized
panel <- panel %>%
  filter(muni_changed == 0, Attention_Check == "somewhat_agree")

# Outcomes (column name -> display label)
outcomes <- c(
  "Coalition_PAN_PRI_PRD_Crime_Rating_Post" = "PAN/PRI/PRD",
  "MORENA_Crime_Rating_Post" = "MORENA",
  "MC_Crime_Rating_Post" = "MC"
)

# Coerce the (character) rating sliders to numeric; report any lost values.
for (v in names(outcomes)) {
  raw_n <- sum(!is.na(panel[[v]]))
  panel[[v]] <- suppressWarnings(as.numeric(panel[[v]]))
  lost <- raw_n - sum(!is.na(panel[[v]]))
  if (lost > 0) {
    message(sprintf("  %s: %d non-numeric values coerced to NA", v, lost))
  }
}

# SDs of the standardizing predictors (identical across outcomes — they share
# the same right-hand side as m_winsorized)
crime_gap_wins_sd <- sd(panel$crime_gap_wins, na.rm = TRUE)
rank_gap_sd <- sd(panel$rank_gap, na.rm = TRUE)

# Coefficient extractor — copied verbatim from belief_update_analysis.R so the
# standardized CG x Treatment / RG x Treatment interaction coefficients are
# pulled and scaled exactly as in the source analysis.
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

# Fit the m_winsorized spec for one outcome
fit_one <- function(v) {
  fml <- as.formula(paste0(
    v,
    " ~ crime_gap_wins * as.factor(Treatment_Group) +",
    " rank_gap * as.factor(Treatment_Group) + comp_party_known"
  ))
  lm_robust(fml, alpha = ci_alpha, data = panel, se_type = "HC2")
}

models <- lapply(names(outcomes), fit_one)
names(models) <- names(outcomes)

for (v in names(outcomes)) {
  cat("\n================ ", outcomes[[v]], " (", v, ") ================\n", sep = "")
  print(summary(models[[v]]))
}

# Standardized interaction coefficients for every outcome, labelled with N
coef_all <- bind_rows(lapply(names(outcomes), function(v) {
  extract_coef_plot(
    models[[v]],
    "crime_gap_wins",
    sprintf("%s (N=%d)", outcomes[[v]], models[[v]]$nobs),
    crime_gap_wins_sd,
    rank_gap_sd
  )
}))

# Coefficient plot — one row per outcome, CG vs RG in columns (control2 dropped,
# matching inc_update_coef_plot in belief_update_analysis.R)
coalition_rating_coef_plot <- ggplot(
  subset(coef_all, treatment != "control2"),
  aes(y = treatment, x = estimate)
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
  facet_grid(model ~ group, scales = "free_x") +
  labs(
    y = "Treatment group",
    x = "Standardized coefficient (1 SD increase in predictor)",
    caption = "Thick bar 95% CI, thin 99% CI"
  ) +
  theme_minimal()

print(coalition_rating_coef_plot)

ggsave(
  "latex/images/coalition_rating_post_coef_plot.pdf",
  plot = coalition_rating_coef_plot,
  width = 7,
  height = 6
)
