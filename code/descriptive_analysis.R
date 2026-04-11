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

survey_responses_wave1 <- survey_responses_wave1 %>%
  mutate(
    respondent_coalition = case_when(
      grepl("morena|pvem|pt", Vote_Intention_Pre, ignore.case = TRUE) ~
        "MORENA/PVEM/PT",
      grepl("pan|pri|prd", Vote_Intention_Pre, ignore.case = TRUE) ~
        "PAN/PRI/PRD",
      grepl("mc", Vote_Intention_Pre, ignore.case = TRUE) ~ "MC",
      TRUE ~ NA_character_
    )
  )

excluded_states <- c(
  "Ciudad de México",
  "Durango",
  "Oaxaca",
  "Veracruz de Ignacio de la Llave"
)

d_geo <- st_read("data/00mun_simplified.geojson", quiet = TRUE) %>%
  mutate(muni_id = CVEGEO) %>%
  filter(!NOM_ENT %in% excluded_states)

# Centroids for distance computation
coords <- st_coordinates(st_centroid(d_geo))
centroids <- d_geo %>%
  st_drop_geometry() %>%
  mutate(centroid_lon = coords[, 1], centroid_lat = coords[, 2]) %>%
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

# ── Construct municipality-level covariate table ──────────────────────────────

muni_meta <- centroids %>%
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
      function(cid, sel) cid %in% trimws(sel[[1]]),
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

cand_meta <- muni_meta %>%
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
    log_dist_km = log(dist_km + 1),
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
    cand_coalition
  )

cat(sprintf(
  "Long format: %d rows (%d respondents × ~15 candidates)\n",
  nrow(long_df),
  n_distinct(long_df$Respondent_ID)
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

# ── Posterior marginal effects plot ───────────────────────────────────────────
# For each coefficient β, the percentage-point change in Pr(selected) when
# increasing the covariate by 1 unit from a 50% baseline:
#   ΔPr = (plogis(β) - 0.5) × 100
# because logit(0.5) = 0, so the new log-odds is simply β.

coef_labels <- c(
  "log_dist_km" = "Log distance (km)",
  "log_pop_ratio" = "Log population ratio",
  "same_state" = "Same state",
  "same_coalition" = "Same coalition",
  "vote_coalition_match" = "Vote coalition match",
  "poolnearest" = "Pool: nearest",
  "poollargest" = "Pool: largest",
  "cand_coalitionMORENADPVEMDPT" = "MORENA",
  "cand_coalitionPANDPRIDPRD" = "PAN/PRI/PRD"
)

draws <- as_draws_df(fit_benchmark) %>%
  select(
    log_dist_km = b_log_dist_km,
    log_pop_ratio = b_log_pop_ratio,
    same_state = b_same_state,
    same_coalition = b_same_coalition,
    vote_coalition_match = b_vote_coalition_match,
    cand_coalitionMORENADPVEMDPT = b_cand_coalitionMORENADPVEMDPT,
    cand_coalitionPANDPRIDPRD = b_cand_coalitionPANDPRIDPRD,
    poolnearest = b_poolnearest,
    poollargest = b_poollargest
  ) %>%
  pivot_longer(everything(), names_to = "term", values_to = "draw") %>%
  mutate(pp_change = (plogis(draw) - 0.5) * 100)

plot_df <- draws %>%
  group_by(term) %>%
  summarise(
    mean = mean(pp_change),
    lo90 = quantile(pp_change, 0.05),
    hi90 = quantile(pp_change, 0.95),
    lo50 = quantile(pp_change, 0.25),
    hi50 = quantile(pp_change, 0.75),
    .groups = "drop"
  ) %>%
  mutate(
    label = factor(coef_labels[term], levels = rev(coef_labels))
  )

benchmark_coef_plot <- ggplot(plot_df, aes(x = mean, y = label)) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey50") +
  geom_linerange(aes(xmin = lo90, xmax = hi90), linewidth = 0.6) +
  geom_linerange(aes(xmin = lo50, xmax = hi50), linewidth = 1.6) +
  geom_point(size = 2.5, shape = 21, fill = "white", stroke = 1) +
  labs(
    x = "Posterior mean percentage-point change\n(from 50% baseline, per unit increase)",
    y = NULL,
    title = "Predictors of benchmark municipality selection",
    caption = "Thick lines: 50% CI. Thin lines: 90% CI."
  ) +
  theme_classic() +
  theme(axis.text.y = element_text(size = 10))

print(benchmark_coef_plot)

ggsave(
  "latex/images/benchmark_coef_plot.pdf",
  plot = benchmark_coef_plot,
  width = 7,
  height = 4
)
