library(dplyr)
library(tidyr)
library(sf)
library(data.table)
library(brms)
library(ggplot2)

# If you need to re-run the model
#file.remove("data/fit_benchmark.rds")

# ── Helper ────────────────────────────────────────────────────────────────────

haversine_km <- function(lon1, lat1, lon2, lat2) {
  R <- 6371
  dlat <- (lat2 - lat1) * pi / 180
  dlon <- (lon2 - lon1) * pi / 180
  a <- sin(dlat / 2)^2 +
    cos(lat1 * pi / 180) * cos(lat2 * pi / 180) * sin(dlon / 2)^2
  2 * R * atan2(sqrt(a), sqrt(1 - a))
}

# ── Load reference data ───────────────────────────────────────────────────────

survey_responses_wave1 <- readRDS("data/wave1_responses.rds")

party_to_coalition <- c(
  morena = "MORENA/PVEM/PT",
  pvem = "MORENA/PVEM/PT",
  pt = "MORENA/PVEM/PT",
  pan = "PAN/PRI/PRD",
  pri = "PAN/PRI/PRD",
  prd = "PAN/PRI/PRD",
  mc = "MC"
)

survey_responses_wave1 <- survey_responses_wave1 %>%
  mutate(
    respondent_coalition = sapply(Vote_Intention_Pre, function(x) {
      if (is.na(x) || x == "") {
        return(NA_character_)
      }
      parties <- trimws(strsplit(x, ";")[[1]])
      coalitions <- unique(na.omit(party_to_coalition[parties]))
      if (length(coalitions) == 1) coalitions else NA_character_
    })
  )

excluded_states <- c(
  "Ciudad de México",
  "Durango",
  "Oaxaca",
  "Veracruz de Ignacio de la Llave"
)

d_geo_all <- st_read("data/00mun_simplified.geojson", quiet = TRUE) %>%
  mutate(muni_id = CVEGEO)

d_geo <- d_geo_all %>%
  filter(!NOM_ENT %in% excluded_states)

# Centroids for home municipalities (excluded states filtered out)
coords <- st_coordinates(st_centroid(d_geo))
centroids <- d_geo %>%
  st_drop_geometry() %>%
  mutate(centroid_lon = coords[, 1], centroid_lat = coords[, 2]) %>%
  select(muni_id, NOM_ENT, centroid_lon, centroid_lat)

# Centroids for candidate municipalities (all states, including excluded)
coords_all <- st_coordinates(st_centroid(d_geo_all))
centroids_all <- d_geo_all %>%
  st_drop_geometry() %>%
  mutate(centroid_lon = coords_all[, 1], centroid_lat = coords_all[, 2]) %>%
  select(muni_id, NOM_ENT, centroid_lon, centroid_lat)

# Population from AGEEML
ageeml <- data.table::fread(
  "references/AGEEML_202512121054579_utf.csv",
  encoding = "Latin-1"
)
pop_lookup <- setNames(
  as.numeric(ageeml$POB_TOTAL),
  sprintf("%05d", suppressWarnings(as.integer(ageeml$CVEGEO)))
)
pop_lookup <- pop_lookup[!is.na(names(pop_lookup))]

# Coalition labels
load("data/magar2024_coalitions.Rdata")
all_parties <- magar2024 %>%
  mutate(
    muni_id = sprintf("%05d", inegi),
    coalition_label = case_when(
      grepl("morena|pvem|pt", l01) ~ "MORENA/PVEM/PT",
      grepl("pan|pri|prd", l01) ~ "PAN/PRI/PRD",
      grepl("mc", l01) ~ "MC",
      TRUE ~ NA_character_
    )
  ) %>%
  select(muni_id, coalition_label)

# Top-20 municipalities by population (nationally, within eligible states)
top20_munis <- names(sort(pop_lookup, decreasing = TRUE))[1:20]

# Nearest-10 lookup
nearest10 <- readRDS("data/nearest10.rds")
nearest10_set <- nearest10 %>%
  select(home_id = muni_id, neighbor_id)

# ── Construct municipality-level covariate tables ─────────────────────────────

muni_meta <- centroids %>%
  left_join(all_parties, by = "muni_id") %>%
  mutate(pop = pop_lookup[muni_id])

muni_meta_all <- centroids_all %>%
  left_join(all_parties, by = "muni_id") %>%
  mutate(pop = pop_lookup[muni_id])

# ── Pivot to long: one row per respondent × candidate municipality ─────────────

long_df <- survey_responses_wave1 %>%
  filter(
    !is.na(Benchmark_Candidate_Municipalities),
    Benchmark_Candidate_Municipalities != ""
  ) %>%
  select(
    Respondent_ID,
    home_id = Found_Municipality_ID,
    Benchmark_Candidate_Municipalities,
    Benchmark_Selected_Municipalities,
    respondent_coalition
  ) %>%
  mutate(home_id = sprintf("%05d", as.integer(home_id))) %>%
  separate_longer_delim(Benchmark_Candidate_Municipalities, delim = ";") %>%
  rename(candidate_id = Benchmark_Candidate_Municipalities) %>%
  mutate(candidate_id = trimws(candidate_id)) %>%
  # Outcome: was this candidate selected?
  mutate(
    selected_vec = strsplit(Benchmark_Selected_Municipalities, ";"),
    Selected = mapply(
      function(cid, sel) cid %in% trimws(sel),
      candidate_id,
      selected_vec
    )
  ) %>%
  select(-selected_vec, -Benchmark_Selected_Municipalities)

# ── Pool membership (nearest / largest / random) ──────────────────────────────
# A candidate is "nearest" if it appears in the nearest-10 for that home_id.
# A candidate is "largest" if it is in the top-20 nationally but not nearest.
# Otherwise "random".

long_df <- long_df %>%
  left_join(
    nearest10_set %>% mutate(is_nearest = TRUE),
    by = c("home_id", "candidate_id" = "neighbor_id")
  ) %>%
  mutate(
    is_nearest = coalesce(is_nearest, FALSE),
    is_largest = candidate_id %in% top20_munis & !is_nearest,
    pool = case_when(
      is_nearest ~ "nearest",
      is_largest ~ "largest",
      TRUE ~ "random"
    )
  ) %>%
  select(-is_nearest, -is_largest)

# ── Join home municipality metadata ──────────────────────────────────────────

home_meta <- muni_meta %>%
  select(
    home_id = muni_id,
    home_lon = centroid_lon,
    home_lat = centroid_lat,
    home_state = NOM_ENT,
    home_pop = pop,
    home_coalition = coalition_label
  )

long_df <- long_df %>%
  left_join(home_meta, by = "home_id")

# ── Join candidate municipality metadata ─────────────────────────────────────

cand_meta <- muni_meta_all %>%
  select(
    candidate_id = muni_id,
    cand_lon = centroid_lon,
    cand_lat = centroid_lat,
    cand_state = NOM_ENT,
    cand_pop = pop,
    cand_coalition = coalition_label
  )

long_df <- long_df %>%
  left_join(cand_meta, by = "candidate_id")

# ── Compute model covariates ──────────────────────────────────────────────────

long_df <- long_df %>%
  mutate(
    dist_km = haversine_km(home_lon, home_lat, cand_lon, cand_lat),
    log_dist_km = log(dist_km),
    log_pop_ratio = log((cand_pop + 1) / (home_pop + 1)),
    same_state = as.integer(cand_state == home_state),
    same_coalition = as.integer(
      !is.na(cand_coalition) &
        !is.na(home_coalition) &
        cand_coalition == home_coalition
    ),
    vote_coalition_match = as.integer(
      !is.na(cand_coalition) &
        !is.na(respondent_coalition) &
        cand_coalition == respondent_coalition
    ),
    cand_coalition = factor(
      coalesce(cand_coalition, "Other"),
      levels = c("MC", "MORENA/PVEM/PT", "PAN/PRI/PRD", "Other")
    ),
    pool = factor(pool, levels = c("random", "nearest", "largest"))
  ) %>%
  select(
    Respondent_ID,
    home_id,
    candidate_id,
    Selected,
    pool,
    log_dist_km,
    log_pop_ratio,
    same_state,
    same_coalition,
    vote_coalition_match,
    dist_km,
    home_pop,
    cand_pop,
    home_coalition,
    cand_coalition,
    cand_state
  )

n_respondents <- n_distinct(long_df$Respondent_ID)

cat(sprintf(
  "Long format: %d rows (%d respondents × ~15 candidates)\n",
  nrow(long_df),
  n_respondents
))
cat(sprintf("Selection rate: %.1f%%\n", 100 * mean(long_df$Selected)))
print(table(long_df$pool))

# ── Bayesian multilevel logistic regression ───────────────────────────────────
# Model: Pr(Y_ri = 1) = logit^{-1}(X_ri β + α_r)
# α_r ~ N(0, σ²_α)  — respondent random intercept
# Priors: t(2, 0, 2.5) on QR-decomposed centered covariates (Goodall 1993)

priors <- c(
  prior(student_t(2, 0, 2.5), class = b),
  prior(student_t(2, 0, 2.5), class = Intercept),
  prior(exponential(1), class = sd)
)

fit_benchmark <- brm(
  bf(
    Selected ~
      log_dist_km +
        log_pop_ratio +
        same_state +
        same_coalition +
        vote_coalition_match +
        cand_coalition +
        pool +
        (1 | Respondent_ID),
    decomp = "QR"
  ),
  data = long_df,
  family = bernoulli(link = "logit"),
  prior = priors,
  chains = 4,
  cores = 4,
  iter = 2000,
  warmup = 1000,
  seed = 42,
  file = "data/fit_benchmark" # caches compiled model + draws
)

summary(fit_benchmark)

# ── Contrast: MORENA vs PAN/PRI/PRD ───────────────────────────────────────────
# Posterior distribution of the difference in log-odds between the two
# coalition fixed effects (both relative to MC as the omitted category).

fe_draws <- as.data.frame(fixef(fit_benchmark, summary = FALSE))

coalition_contrast <- fe_draws %>%
  mutate(diff = `cand_coalitionMORENADPVEMDPT` - `cand_coalitionPANDPRIDPRD`)

cat(sprintf(
  "MORENA − PAN/PRI/PRD log-odds difference:\n  mean = %.3f, 95%% CI [%.3f, %.3f]\n  P(MORENA > PAN/PRI/PRD) = %.3f\n",
  mean(coalition_contrast$diff),
  quantile(coalition_contrast$diff, 0.025),
  quantile(coalition_contrast$diff, 0.975),
  mean(coalition_contrast$diff > 0)
))

# ── Posterior marginal effects plot ───────────────────────────────────────────

coef_labels <- c(
  "log_dist_km" = "Distance (km), per doubling",
  "log_pop_ratio" = "Pop. ratio (cand/home), per doubling",
  "same_state" = "Same state",
  "same_coalition" = "Same coalition",
  "vote_coalition_match" = "Vote coalition match",
  "cand_coalitionMORENADPVEMDPT" = "Cand. coalition: MORENA/PVEM/PT",
  "cand_coalitionPANDPRIDPRD" = "Cand. coalition: PAN/PRI/PRD"
)

draws <- fe_draws %>%
  select(
    log_dist_km,
    log_pop_ratio,
    same_state,
    same_coalition,
    vote_coalition_match,
    cand_coalitionMORENADPVEMDPT,
    cand_coalitionPANDPRIDPRD
  ) %>%
  pivot_longer(everything(), names_to = "term", values_to = "draw") %>%
  mutate(
    draw = if_else(
      term %in% c("log_dist_km", "log_pop_ratio"),
      draw * log(2),
      draw
    ),
    pp_change = (plogis(draw) - 0.5) * 100
  )

plot_df <- draws %>%
  group_by(term) %>%
  summarise(
    mean = mean(pp_change),
    lo95 = quantile(pp_change, 0.025),
    hi95 = quantile(pp_change, 0.975),
    lo50 = quantile(pp_change, 0.25),
    hi50 = quantile(pp_change, 0.75),
    .groups = "drop"
  ) %>%
  mutate(
    label = factor(coef_labels[term], levels = rev(coef_labels))
  )

benchmark_coef_plot <- ggplot(plot_df, aes(x = mean, y = label)) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey50") +
  geom_linerange(aes(xmin = lo95, xmax = hi95), linewidth = 0.6) +
  geom_linerange(aes(xmin = lo50, xmax = hi50), linewidth = 1.6) +
  geom_point(size = 2.5, shape = 21, fill = "white", stroke = 1) +
  labs(
    x = "Posterior mean percentage-point change\n(from 50% baseline; doublings for distance/pop ratio)",
    y = NULL,
    title = "Predictors of comparison municipality selection",
    caption = sprintf(
      "N = %d respondents. Thick lines: 50%% CI. Thin lines: 95%% CI.",
      n_respondents
    )
  ) +
  theme_classic() +
  theme(axis.text.y = element_text(size = 10))

print(benchmark_coef_plot)

ggsave(
  "latex/images/comparison_coef_plot.pdf",
  plot = benchmark_coef_plot,
  width = 7,
  height = 4
)

#test <- long_df %>% group_by(Respondent_ID) %>% summarize(selected_num = sum(Selected == TRUE))
# ggplot(test) +
#   geom_histogram(aes(x = selected_num), bins = 30) +
#   theme_bw()
