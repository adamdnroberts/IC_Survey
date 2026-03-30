library(dplyr)
library(sf)
library(lme4)

set.seed(42)
n_respondents <- 300

# ── Load municipality data ────────────────────────────────────────────────────

d_geo <- st_read("data/00mun_simplified.geojson", quiet = TRUE) %>%
  mutate(muni_id = CVEGEO)

load("data/magar2024_coalitions.Rdata")
all_parties <- magar2024 %>%
  mutate(
    CVEGEO = sprintf("%05d", inegi),
    coalition_label = case_when(
      grepl("morena|pvem|pt", l01) ~ "MORENA/PVEM/PT",
      grepl("pan|pri|prd", l01) ~ "PAN/PRI/PRD",
      grepl("mc", l01) ~ "MC",
      TRUE ~ NA_character_
    )
  ) %>%
  select(CVEGEO, coalition_label)

d_geo <- d_geo %>%
  left_join(all_parties, by = c("muni_id" = "CVEGEO")) %>%
  mutate(area_km2 = as.numeric(sf::st_area(.)) / 1e6)

muni_centroids <- sf::st_coordinates(sf::st_centroid(d_geo))
d_geo$centroid_lon <- muni_centroids[, 1]
d_geo$centroid_lat <- muni_centroids[, 2]

nearest10 <- readRDS("data/nearest10.rds")

top20_munis <- d_geo %>%
  st_drop_geometry() %>%
  filter(!is.na(POB_TOTAL)) %>%
  arrange(desc(POB_TOTAL)) %>%
  slice_head(n = 20) %>%
  pull(muni_id)

# ── Simulate respondents with home municipalities ─────────────────────────────

eligible_homes <- d_geo %>%
  st_drop_geometry() %>%
  filter(
    !is.na(POB_TOTAL),
    !is.na(coalition_label),
    !is.na(centroid_lon),
    !is.na(centroid_lat)
  )

home_ids <- sample(eligible_homes$muni_id, n_respondents, replace = TRUE)

# ── True data-generating coefficients (used to simulate selections) ───────────
# Closer municipalities and same-state are more likely to be selected;
# same coalition has a modest positive effect.
b_intercept <- -1.5
b_log_dist <- -0.4 # farther = less likely
b_log_pop_ratio <- 0.2 # larger relative to home = more likely
b_same_coal <- 0.5
b_same_state <- 0.8
b_pool_nearest <- 0.3 # nearest pool slightly more likely than random
b_pool_largest <- 0.2
sigma_alpha <- 0.8 # respondent-level SD

# ── Build model data frame from simulated respondents ────────────────────────

model_rows <- lapply(seq_along(home_ids), function(i) {
  home_id <- home_ids[i]
  home_row <- eligible_homes %>% filter(muni_id == home_id) %>% slice(1)

  nn_ids <- nearest10 %>% filter(muni_id == home_id) %>% pull(neighbor_id)

  # Sample candidates: 5 nearest, 5 largest, 5 random (dedup)
  pool1 <- if (length(nn_ids) >= 5) sample(nn_ids, 5) else nn_ids
  top20_cands <- setdiff(top20_munis, c(home_id, pool1))
  pool2 <- if (length(top20_cands) >= 5) sample(top20_cands, 5) else top20_cands
  remaining <- setdiff(eligible_homes$muni_id, c(home_id, pool1, pool2))
  pool3 <- if (length(remaining) >= 5) sample(remaining, 5) else remaining

  candidates <- c(pool1, pool2, pool3)
  cand_rows <- d_geo %>%
    st_drop_geometry() %>%
    filter(muni_id %in% candidates, !is.na(POB_TOTAL))
  if (nrow(cand_rows) == 0) {
    return(NULL)
  }

  home_sf <- sf::st_sfc(
    sf::st_point(c(home_row$centroid_lon, home_row$centroid_lat)),
    crs = 4326
  )
  cand_sf <- sf::st_as_sf(
    cand_rows,
    coords = c("centroid_lon", "centroid_lat"),
    crs = 4326,
    remove = FALSE
  )
  dist_km <- as.numeric(sf::st_distance(home_sf, cand_sf)) / 1000

  log_dist <- log(pmax(dist_km, 0.1))
  log_pop_ratio <- log(pmax(cand_rows$POB_TOTAL, 1)) -
    log(max(home_row$POB_TOTAL, 1))
  same_coal <- as.integer(
    !is.na(cand_rows$coalition_label) &
      !is.na(home_row$coalition_label) &
      cand_rows$coalition_label == home_row$coalition_label
  )
  same_state <- as.integer(cand_rows$NOM_ENT == home_row$NOM_ENT)
  pool <- case_when(
    cand_rows$muni_id %in% pool1 ~ "nearest",
    cand_rows$muni_id %in% pool2 ~ "largest",
    TRUE ~ "random"
  )

  # Simulate selection using true DGP + respondent random effect
  alpha_r <- rnorm(1, 0, sigma_alpha)
  pool_effect <- ifelse(
    pool == "nearest",
    b_pool_nearest,
    ifelse(pool == "largest", b_pool_largest, 0)
  )
  logit_p <- b_intercept +
    b_log_dist * log_dist +
    b_log_pop_ratio * log_pop_ratio +
    b_same_coal * same_coal +
    b_same_state * same_state +
    pool_effect +
    alpha_r
  p <- 1 / (1 + exp(-logit_p))
  selected <- rbinom(length(p), 1, p)

  data.frame(
    respondent_id = paste0("R", sprintf("%04d", i)),
    muni_id = cand_rows$muni_id,
    selected = selected,
    log_dist_km = log_dist,
    log_pop_ratio = log_pop_ratio,
    same_coalition = same_coal,
    same_state = same_state,
    pool = pool,
    stringsAsFactors = FALSE
  )
})

model_df <- do.call(rbind, Filter(Negate(is.null), model_rows))
model_df$pool <- factor(
  model_df$pool,
  levels = c("random", "nearest", "largest")
)

cat(sprintf(
  "Simulated observations: %d (%d respondents × ~15 municipalities)\n",
  nrow(model_df),
  length(unique(model_df$respondent_id))
))
cat(sprintf("Overall selection rate: %.1f%%\n", mean(model_df$selected) * 100))

# ── Fit multilevel logistic regression ───────────────────────────────────────

m <- glmer(
  selected ~
    log_dist_km +
      log_pop_ratio +
      same_coalition +
      same_state +
      pool +
      (1 | respondent_id),
  data = model_df,
  family = binomial(link = "logit"),
  control = glmerControl(optimizer = "bobyqa")
)

cat(
  "\n── Model summary ─────────────────────────────────────────────────────────────\n"
)
print(summary(m))

cat(
  "\n── Odds ratios (exp(coef)) ───────────────────────────────────────────────────\n"
)
print(round(exp(fixef(m)), 3))

cat(
  "\n── True vs. estimated fixed effects ─────────────────────────────────────────\n"
)
true_vals <- c(
  b_intercept,
  b_log_dist,
  b_log_pop_ratio,
  b_same_coal,
  b_same_state,
  b_pool_largest,
  b_pool_nearest
)
names(true_vals) <- c(
  "(Intercept)",
  "log_dist_km",
  "log_pop_ratio",
  "same_coalition",
  "same_state",
  "poollargest",
  "poolnearest"
)
comparison <- data.frame(
  true = true_vals[names(fixef(m))],
  estimated = round(fixef(m), 3)
)
print(comparison)

cat(
  "\n── Selection rate by pool ────────────────────────────────────────────────────\n"
)
print(
  model_df %>%
    group_by(pool) %>%
    summarize(
      n = n(),
      n_selected = sum(selected),
      rate = round(mean(selected) * 100, 1)
    )
)
