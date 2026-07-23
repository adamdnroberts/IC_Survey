library(dplyr)
library(data.table)
library(sf)

load("~/IC_Survey/data/survey_panel_dataset.Rdata")

# ── Helper ────────────────────────────────────────────────────────────────────
haversine_km <- function(lon1, lat1, lon2, lat2) {
  R <- 6371
  dlat <- (lat2 - lat1) * pi / 180
  dlon <- (lon2 - lon1) * pi / 180
  a <- sin(dlat / 2)^2 +
    cos(lat1 * pi / 180) * cos(lat2 * pi / 180) * sin(dlon / 2)^2
  2 * R * atan2(sqrt(a), sqrt(1 - a))
}

# ── Population lookup (AGEEML) ────────────────────────────────────────────────
ageeml <- data.table::fread(
  "references/AGEEML_202512121054579_utf.csv",
  encoding = "Latin-1"
)
pop_lookup <- setNames(
  as.numeric(ageeml$POB_TOTAL),
  sprintf("%05d", suppressWarnings(as.integer(ageeml$CVEGEO)))
)
pop_lookup <- pop_lookup[!is.na(names(pop_lookup))]

# ── Governing coalition lookup (magar2024) ────────────────────────────────────
load("data/magar2024_coalitions.Rdata")
coalition_lookup <- magar2024 %>%
  mutate(
    muni_id = sprintf("%05d", inegi),
    coalition_label = case_when(
      grepl("morena|pvem|pt", l01) ~ "MORENA/PVEM/PT",
      grepl("pan|pri|prd", l01) ~ "PAN/PRI/PRD",
      grepl("mc", l01) ~ "MC",
      TRUE ~ NA_character_
    )
  )
coalition_vec <- setNames(
  coalition_lookup$coalition_label,
  coalition_lookup$muni_id
)

# ── Centroid lookups (for home <-> comparison distances) ──────────────────────
d_geo <- st_read("data/00mun_simplified.geojson", quiet = TRUE)
coords <- st_coordinates(st_centroid(st_geometry(d_geo)))
muni_ids <- sprintf("%05d", suppressWarnings(as.integer(d_geo$CVEGEO)))
lon_lookup <- setNames(coords[, 1], muni_ids)
lat_lookup <- setNames(coords[, 2], muni_ids)

# ── Attach population, coalition, same-state, and vote-match indicators ────────
# Same-state: first two CVEGEO digits are the state code.
# Vote match: comp governing coalition == respondent's pre-treatment vote
#   coalition (coalition_pre, built in create_panel_dataset.R).
home_id <- sprintf("%05d", as.integer(panel$Found_Municipality_ID))
home_state <- substr(home_id, 1, 2)
home_lon <- lon_lookup[home_id]
home_lat <- lat_lookup[home_id]
home_pop <- pop_lookup[home_id]
home_coalition <- coalition_vec[home_id]

for (i in 1:4) {
  id <- sprintf(
    "%05d",
    as.integer(panel[[paste0("Comparison_Muni_", i, "_ID")]])
  )
  comp_coalition <- coalition_vec[id]

  panel[[paste0("comp_pop_", i)]] <- pop_lookup[id]
  panel[[paste0("comp_coalition_", i)]] <- comp_coalition
  panel[[paste0("comp_same_state_", i)]] <- as.integer(
    substr(id, 1, 2) == home_state
  )
  panel[[paste0("comp_same_coalition_", i)]] <- as.integer(
    !is.na(comp_coalition) &
      !is.na(home_coalition) &
      comp_coalition == home_coalition
  )
  panel[[paste0("comp_vote_match_", i)]] <- as.integer(
    !is.na(comp_coalition) &
      !is.na(panel$coalition_pre) &
      comp_coalition == panel$coalition_pre
  )
  panel[[paste0("comp_dist_km_", i)]] <- haversine_km(
    home_lon,
    home_lat,
    lon_lookup[id],
    lat_lookup[id]
  )
}

# ── Model-importance index for the shown comparisons ──────────────────────────
# Score each comparison with the fitted benchmark-selection model
# (descriptive_analysis.R). The linear predictor is the model's log-odds that a
# respondent *would pick* that municipality as a benchmark, i.e. how salient /
# "important" a comparison it is. We convert to a selection probability and
# average across the comparisons a respondent was actually shown.
benchmark_coef <- c(
  #Intercept = -7.76,
  log_dist_km = -0.23,
  log_pop_ratio = 0.75,
  same_state = 1.02
)

for (i in 1:4) {
  comp_pop <- panel[[paste0("comp_pop_", i)]]
  eta <- benchmark_coef["log_dist_km"] *
    log(panel[[paste0("comp_dist_km_", i)]]) +
    benchmark_coef["log_pop_ratio"] * log((comp_pop + 1) / (home_pop + 1)) +
    benchmark_coef["same_state"] * panel[[paste0("comp_same_state_", i)]]

  panel[[paste0("comp_importance_lp_", i)]] <- as.numeric(eta)
  panel[[paste0("comp_importance_prob_", i)]] <- plogis(as.numeric(eta))
}

# Respondent-level index: mean over the comparisons assigned to the respondent
# (NA-safe). All arms are scored, including control and T1: they were assigned
# comparison municipalities (Control_T1_Comp_Type -> active_comp_munis) and saw
# their names, they just didn't receive crime info on them.
lp_cols <- paste0("comp_importance_lp_", 1:4)
prob_cols <- paste0("comp_importance_prob_", 1:4)
panel$comparison_importance_lp <- rowMeans(panel[lp_cols], na.rm = TRUE)
panel$comparison_importance_prob <- rowMeans(panel[prob_cols], na.rm = TRUE)
# rowMeans of an all-NA row returns NaN (no comparison IDs at all) -> NA
panel$comparison_importance_lp[is.nan(panel$comparison_importance_lp)] <- NA
panel$comparison_importance_prob[is.nan(panel$comparison_importance_prob)] <- NA

# ── GAM: does the RG × treatment effect vary with comparison importance? ───────
# Mirrors vote_update_gam.R (same outcome, sample restrictions, and nuisance
# controls), but replaces the 1-D by-arm smooth s(rank_gap, by = arm_group) with
# a 2-D tensor-product smooth te(rank_gap, comparison_importance_lp, by =
# arm_group). This lets the rank_gap updating slope bend as comparison
# importance changes, and estimates that bending separately by arm — i.e. how
# the RG × treatment effect on incumbent vote moves across comparison_importance_lp.
library(ggplot2)
library(mgcv)

# Outcome + sample restrictions mirror vote_update_gam.R.
panel$Vote_home_post <- as.integer(
  !is.na(panel$coalition_post) &
    !is.na(panel$home_coalition) &
    panel$home_coalition == panel$coalition_post
)

gam_panel <- panel %>%
  filter(muni_changed == 0, Attention_Check == "somewhat_agree")

gam_panel$coalition_pre[is.na(gam_panel$coalition_pre)] <- "Other"
gam_panel$inc_vote <- as.numeric(
  gam_panel$coalition_pre == gam_panel$home_coalition
)

curve_arms <- c("control", "T1", "T2", "T3", "T4")
arm_group_levels <- c("control", "T1", "Comparison")
coalition_pre_mode <- names(which.max(table(gam_panel$coalition_pre)))

# Pool T2–T4 as "Comparison" (as in vote_update_gam.R). Control is the reference
# arm; both it and the Comparison arm now carry comparison_importance_lp.
gam_data <- gam_panel %>%
  filter(
    Treatment_Group %in% curve_arms,
    !is.na(rank_gap),
    !is.na(comparison_importance_lp)
  ) %>%
  mutate(
    arm_group = factor(
      case_when(
        Treatment_Group == "control" ~ "control",
        Treatment_Group == "T1" ~ "T1",
        TRUE ~ "Comparison"
      ),
      levels = arm_group_levels
    ),
    coalition_pre = factor(coalition_pre)
  )

# rank_gap is discrete (integers), so k = 5 on that margin, as in vote_update_gam.R.
m_vote_gam_ci <- gam(
  Vote_home_post ~
    arm_group +
    s(rank_gap, k = 5) +
    te(rank_gap, comparison_importance_lp, by = arm_group, k = c(5, 5)) +
    s(log_crime_gap) +
    te(log_crime_gap, comparison_importance_lp, by = arm_group, k = c(5, 5)) +
    #coalition_pre +
    inc_vote,
  family = binomial(),
  data = gam_data,
  method = "REML"
)

summary(m_vote_gam_ci)


# ── Plot: Comparison − control difference in P(incumbent vote) over comparison ─
# importance, with one line per rank_gap value (accurate prior RG = 0, and
# optimistic / pessimistic priors RG = ±2). Both arms are evaluated at the same
# rank_gap and importance, so the difference isolates the treatment effect and
# how it shifts across comparison importance. crime_gap_wins at its mean,
# coalition_pre at its mode, inc_vote = 1. Probability-scale SEs via delta method
# from the model lpmatrix + covariance (as in vote_update_gam.R's contrast).
crit95 <- qnorm(0.975)
b <- coef(m_vote_gam_ci)
V <- vcov(m_vote_gam_ci)

lp_row <- function(rg, ci, arm, iv = 1) {
  predict(
    m_vote_gam_ci,
    newdata = data.frame(
      rank_gap = rg,
      comparison_importance_lp = ci,
      arm_group = factor(arm, levels = arm_group_levels),
      crime_gap_wins = mean(gam_data$crime_gap_wins, na.rm = TRUE),
      coalition_pre = factor(
        coalition_pre_mode,
        levels = levels(gam_data$coalition_pre)
      ),
      inc_vote = iv
    ),
    type = "lpmatrix"
  )
}

# x-axis: sequence over the comparison-importance range seen in the Comparison arm.
comp_ci <- gam_data$comparison_importance_lp[gam_data$arm_group == "Comparison"]
ci_seq <- seq(
  quantile(comp_ci, 0.05, na.rm = TRUE),
  quantile(comp_ci, 0.95, na.rm = TRUE),
  length.out = 80
)

# lines: a few representative rank_gap values (discrete integers).
rank_levels <- c(-2, 0, 2)

diff_curve <- bind_rows(lapply(rank_levels, function(rg) {
  bind_rows(lapply(ci_seq, function(ci_val) {
    Xc <- lp_row(rg, ci_val, "Comparison")
    X0 <- lp_row(rg, ci_val, "control")
    pc <- plogis(as.vector(Xc %*% b))
    p0 <- plogis(as.vector(X0 %*% b))
    d <- pc - p0
    grad <- (pc * (1 - pc)) * Xc - (p0 * (1 - p0)) * X0
    se <- sqrt(as.vector(grad %*% V %*% t(grad)))
    data.frame(
      comparison_importance_lp = ci_val,
      rank_gap = rg,
      diff = d,
      lwr95 = d - crit95 * se,
      upr95 = d + crit95 * se
    )
  }))
}))

diff_curve$rank_gap <- factor(
  diff_curve$rank_gap,
  levels = rank_levels,
  labels = c(
    "RG = -2 (pessimistic)",
    "RG = 0 (accurate)",
    "RG = +2 (optimistic)"
  )
)

rg_colors <- c(
  "RG = -2 (pessimistic)" = "#0072B2",
  "RG = 0 (accurate)" = "#999999",
  "RG = +2 (optimistic)" = "#E69F00"
)

vote_update_curve_importance <- ggplot(
  diff_curve,
  aes(
    x = comparison_importance_lp,
    y = diff,
    color = rank_gap,
    fill = rank_gap
  )
) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +
  geom_ribbon(aes(ymin = lwr95, ymax = upr95), alpha = 0.15, color = NA) +
  geom_line(linewidth = 0.8) +
  scale_color_manual(values = rg_colors) +
  scale_fill_manual(values = rg_colors) +
  labs(
    y = "P(incumbent vote): Comparison − control",
    x = "Comparison importance (LP)",
    color = "Rank gap (RG)",
    fill = "Rank gap (RG)",
    caption = "ribbon 95% CI; importance shown over 5th–95th percentile range"
  ) +
  facet_wrap(~rank_gap) +
  theme_minimal()

print(vote_update_curve_importance)

# ggsave(
#   "latex/images/vote_update_curve_importance.pdf",
#   plot = vote_update_curve_importance,
#   width = 7,
#   height = 4.5
# )

# ── Robustness: keep T2 / T3 / T4 separate (not pooled) ───────────────────────
# Same specification as the headline, but arm_group is the full Treatment_Group
# level, so each comparison arm gets its own tensor surface. control and T1 stay
# in the fit for the reference/nuisance terms but are not plotted. Cells are
# thinner here (and hence CIs wider); this is a robustness check on the pooled
# "Comparison" result above.
arm_levels_sep <- c("control", "T1", "T2", "T3", "T4")
sep_groups <- c("T2", "T3", "T4")

gam_data_sep <- gam_panel %>%
  filter(
    Treatment_Group %in% arm_levels_sep,
    !is.na(rank_gap),
    !is.na(comparison_importance_lp)
  ) %>%
  mutate(
    arm_group = factor(Treatment_Group, levels = arm_levels_sep),
    coalition_pre = factor(coalition_pre)
  )

m_vote_gam_ci_sep <- gam(
  Vote_home_post ~
    arm_group +
    te(rank_gap, comparison_importance_lp, by = arm_group, k = c(5, 5)) +
    s(crime_gap_wins, k = 5) +
    coalition_pre +
    inc_vote,
  family = binomial(),
  data = gam_data_sep,
  method = "REML"
)

summary(m_vote_gam_ci_sep)

b_sep <- coef(m_vote_gam_ci_sep)
V_sep <- vcov(m_vote_gam_ci_sep)

lp_row_sep <- function(rg, ci, arm, iv = 1) {
  predict(
    m_vote_gam_ci_sep,
    newdata = data.frame(
      rank_gap = rg,
      comparison_importance_lp = ci,
      arm_group = factor(arm, levels = arm_levels_sep),
      crime_gap_wins = mean(gam_data_sep$crime_gap_wins, na.rm = TRUE),
      coalition_pre = factor(
        coalition_pre_mode,
        levels = levels(gam_data_sep$coalition_pre)
      ),
      inc_vote = iv
    ),
    type = "lpmatrix"
  )
}

# Each arm gets its own comparison-importance range (5th–95th pctile) so we don't
# extrapolate an arm past the comparisons it actually showed.
diff_curve_sep <- bind_rows(lapply(sep_groups, function(arm) {
  arm_ci <- gam_data_sep$comparison_importance_lp[
    gam_data_sep$arm_group == arm
  ]
  ci_seq_a <- seq(
    quantile(arm_ci, 0.05, na.rm = TRUE),
    quantile(arm_ci, 0.95, na.rm = TRUE),
    length.out = 80
  )
  bind_rows(lapply(rank_levels, function(rg) {
    bind_rows(lapply(ci_seq_a, function(ci_val) {
      Xa <- lp_row_sep(rg, ci_val, arm)
      X0 <- lp_row_sep(rg, ci_val, "control")
      pa <- plogis(as.vector(Xa %*% b_sep))
      p0 <- plogis(as.vector(X0 %*% b_sep))
      d <- pa - p0
      grad <- (pa * (1 - pa)) * Xa - (p0 * (1 - p0)) * X0
      se <- sqrt(as.vector(grad %*% V_sep %*% t(grad)))
      data.frame(
        arm = arm,
        comparison_importance_lp = ci_val,
        rank_gap = rg,
        diff = d,
        lwr95 = d - crit95 * se,
        upr95 = d + crit95 * se
      )
    }))
  }))
}))

diff_curve_sep$rank_gap <- factor(
  diff_curve_sep$rank_gap,
  levels = rank_levels,
  labels = c(
    "RG = -2 (pessimistic)",
    "RG = 0 (accurate)",
    "RG = +2 (optimistic)"
  )
)
diff_curve_sep$arm <- factor(diff_curve_sep$arm, levels = sep_groups)

vote_update_curve_importance_sep <- ggplot(
  diff_curve_sep,
  aes(
    x = comparison_importance_lp,
    y = diff,
    color = rank_gap,
    fill = rank_gap
  )
) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +
  geom_ribbon(aes(ymin = lwr95, ymax = upr95), alpha = 0.12, color = NA) +
  geom_line(linewidth = 0.8) +
  scale_color_manual(values = rg_colors) +
  scale_fill_manual(values = rg_colors) +
  facet_grid(arm ~ rank_gap) +
  labs(
    y = "P(incumbent vote): arm − control",
    x = "Comparison importance (LP)",
    color = "Rank gap (RG)",
    fill = "Rank gap (RG)",
    caption = "ribbon 95% CI; importance over each arm's 5th–95th percentile range"
  ) +
  theme_minimal()

print(vote_update_curve_importance_sep)

# ggsave(
#   "latex/images/vote_update_curve_importance_byarm.pdf",
#   plot = vote_update_curve_importance_sep,
#   width = 9,
#   height = 7
# )
