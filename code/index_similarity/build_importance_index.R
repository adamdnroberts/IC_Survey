# ── Shared prep for the index_similarity scripts ──────────────────────────────
# Loads the wave1-wave2 panel and attaches everything the comparison-importance
# analyses need on top of create_panel_dataset.R's output:
#   comp_pop_i / comp_coalition_i / comp_same_state_i / comp_same_coalition_i /
#   comp_vote_match_i / comp_dist_km_i / comp_importance_lp_i (i = 1..4)
#   comparison_importance_lp, comparison_importance_prob  (respondent-level means)
#   Vote_home_post                                        (outcome)
# Leaves `panel` in the calling environment. Sourced by index_comparisons.R and
# index_similarity_linear_models.R so neither has to run the other first, and so
# the index they analyse is built in exactly one place.
#
# Requires (paths relative to the project root):
#   data/derived/survey_panel_dataset.Rdata
#   references/AGEEML_202512121054579_utf.csv
#   data/magar2024_coalitions.Rdata
#   data/00mun_simplified.geojson
library(dplyr)
library(data.table)
library(sf)

load("data/derived/survey_panel_dataset.Rdata")

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
# (benchmark_analysis.R). The linear predictor is the model's log-odds that a
# respondent *would pick* that municipality as a benchmark, i.e. how salient /
# "important" a comparison it is. We convert to a selection probability and
# average across the comparisons a respondent was actually shown.
# Posterior means from latex/tables/benchmark_model.tex (tab:benchmark_model).
# The candidate- and home-coalition dummies and the pool fixed effects are
# deliberately omitted: pool has no wave 2 analogue, and home coalition is
# constant within respondent.
benchmark_coef <- c(
  #Intercept = -7.76,
  log_dist_km = -0.23,
  log_pop_ratio = 0.75,
  same_state = 1.02,
  same_coalition = 0.13,
  vote_match = 0.07
)

for (i in 1:4) {
  comp_pop <- panel[[paste0("comp_pop_", i)]]
  eta <- benchmark_coef["log_dist_km"] *
    log(panel[[paste0("comp_dist_km_", i)]]) +
    benchmark_coef["log_pop_ratio"] * log((comp_pop + 1) / (home_pop + 1)) +
    benchmark_coef["same_state"] * panel[[paste0("comp_same_state_", i)]] +
    benchmark_coef["same_coalition"] *
      panel[[paste0("comp_same_coalition_", i)]] +
    benchmark_coef["vote_match"] * panel[[paste0("comp_vote_match_", i)]]

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

# ── Outcome ───────────────────────────────────────────────────────────────────
# Incumbent vote after treatment: post-treatment vote coalition matches the
# coalition governing the home municipality. Mirrors vote_update_gam.R.
panel$Vote_home_post <- as.integer(
  !is.na(panel$coalition_post) &
    !is.na(panel$home_coalition) &
    panel$home_coalition == panel$coalition_post
)
