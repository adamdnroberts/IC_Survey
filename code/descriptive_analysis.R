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

# Crime rates
robo_data <- readRDS("data/robo_2025.rds")
crime_lookup <- setNames(
  robo_data$rate_per_100k,
  sprintf("%05d", suppressWarnings(as.integer(robo_data$Cve..Municipio)))
)
crime_lookup <- crime_lookup[!is.na(names(crime_lookup))]

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
      is_largest ~ "largest",
      is_nearest ~ "nearest",
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

# ── Crime importance lookup (respondent-level) ───────────────────────────────

ci_lookup <- setNames(
  6 - as.numeric(survey_responses_wave1$Importance_Crime),
  survey_responses_wave1$Respondent_ID
)

# ── Compute model covariates ──────────────────────────────────────────────────

long_df <- long_df %>%
  mutate(
    home_crime_rate = crime_lookup[home_id],
    cand_crime_rate = crime_lookup[candidate_id],
    CI = ci_lookup[as.character(Respondent_ID)],
    dist_km = haversine_km(home_lon, home_lat, cand_lon, cand_lat),
    log_dist_km = log(dist_km),
    log_pop_ratio = log((cand_pop + 1) / (home_pop + 1)),
    crime_diff = cand_crime_rate - home_crime_rate,
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
    home_coalition = factor(
      coalesce(home_coalition, "Other"),
      levels = c("MC", "MORENA/PVEM/PT", "PAN/PRI/PRD", "Other")
    ),
    pool = factor(pool, levels = c("random", "nearest", "largest")),
    log_home_pop = log(home_pop + 1)
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
    log_home_pop,
    home_coalition,
    dist_km,
    home_pop,
    cand_pop,
    cand_coalition,
    cand_state,
    crime_diff,
    CI
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
        log_dist_km * log_pop_ratio +
        same_state +
        same_coalition +
        vote_coalition_match +
        cand_coalition +
        home_coalition +
        log_home_pop +
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
  "log_dist_km" = "Log distance (km)",
  "log_pop_ratio" = "Log pop. ratio (cand/home)",
  "log_dist_km:log_pop_ratio" = "Log distance × log pop. ratio",
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
    `log_dist_km:log_pop_ratio`,
    same_state,
    same_coalition,
    vote_coalition_match,
    cand_coalitionMORENADPVEMDPT,
    cand_coalitionPANDPRIDPRD
  ) %>%
  pivot_longer(everything(), names_to = "term", values_to = "draw") %>%
  mutate(pp_change = (plogis(draw) - 0.5) * 100)

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
    x = "Posterior mean percentage-point change (from 50% baseline)",
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
  height = 4.5
)

# ── Bayesian model with crime rate ratio ──────────────────────────────────────

n_respondents_crime <- n_distinct(long_df$Respondent_ID)

summary(long_df$crime_diff)

long_df$CI_f <- relevel(as.factor(long_df$CI), ref = "3")
test <- lm(crime_diff ~ CI_f, data = long_df)

em <- emmeans(test, ~CI_f)

as.data.frame(em) %>%
  ggplot(aes(y = CI_f, x = emmean, xmin = lower.CL, xmax = upper.CL)) +
  geom_point(size = 3) +
  geom_errorbar(width = 0.2) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray50") +
  labs(
    x = "CI Level",
    y = "Mean Crime Difference",
    title = "Estimated Means by CI Level"
  ) +
  theme_minimal()

#file.remove("data/fit_benchmark_crime.rds")

fit_benchmark_crime <- brm(
  bf(
    Selected ~
      log_dist_km +
        log_pop_ratio +
        log_dist_km * log_pop_ratio +
        same_state +
        same_coalition +
        vote_coalition_match +
        cand_coalition +
        home_coalition +
        log_home_pop +
        pool +
        crime_diff +
        CI_f +
        crime_diff * CI_f +
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
  file = "data/fit_benchmark_crime"
)
#
# summary(fit_benchmark_crime)
#
# ── Coefficient plot for crime model ─────────────────────────────────────────

coef_labels_crime <- c(
  "crime_diff:CI_f1" = "Crime difference × crime importance1",
  "crime_diff:CI_f2" = "Crime difference × crime importance2",
  "crime_diff:CI_f4" = "Crime difference × crime importance4",
  "crime_diff:CI_f5" = "Crime difference × crime importance5"
)

fe_draws_crime <- as.data.frame(fixef(fit_benchmark_crime, summary = FALSE))

cat("Crime model coefficient names:\n")
print(names(fe_draws_crime))

draws_crime <- fe_draws_crime %>%
  pivot_longer(everything(), names_to = "term", values_to = "draw") %>%
  filter(term %in% names(coef_labels_crime)) %>%
  mutate(pp_change = (plogis(draw) - 0.5) * 100)

plot_df_crime <- draws_crime %>%
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
    label = factor(coef_labels_crime[term], levels = rev(coef_labels_crime))
  )

benchmark_crime_coef_plot <- ggplot(plot_df_crime, aes(x = mean, y = label)) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey50") +
  geom_linerange(aes(xmin = lo95, xmax = hi95), linewidth = 0.6) +
  geom_linerange(aes(xmin = lo50, xmax = hi50), linewidth = 1.6) +
  geom_point(size = 2.5, shape = 21, fill = "white", stroke = 1) +
  labs(
    x = "Posterior mean percentage-point change (from 50% baseline)",
    y = NULL,
    title = "Crime ratio as a predictor of municipality selection",
    caption = sprintf(
      "N = %d respondents. Thick lines: 50%% CI. Thin lines: 95%% CI.",
      n_respondents_crime
    )
  ) +
  theme_classic() +
  theme(axis.text.y = element_text(size = 10))

print(benchmark_crime_coef_plot)

ggsave(
  "latex/images/comparison_crime_coef_plot.pdf",
  plot = benchmark_crime_coef_plot,
  width = 7,
  height = 5
)

# ── Interaction plot: marginal effect of crime_diff at each CI level ──────────
# Find the interaction column name as brms may name it differently
# crime_ci_col <- grep("crime_diff.*CI|CI.*crime_diff", names(fe_draws_crime), value = TRUE)[1]
#
# interaction_df <- purrr::map_dfr(1:5, function(ci_val) {
#   marginal <- fe_draws_crime[["crime_diff"]] + fe_draws_crime[[crime_ci_col]] * ci_val
#   dplyr::tibble(
#     CI_val = ci_val,
#     draw = marginal
#   )
# }) %>%
#   group_by(CI_val) %>%
#   summarise(
#     mean  = mean((plogis(draw) - 0.5) * 100),
#     lo95  = quantile((plogis(draw) - 0.5) * 100, 0.025),
#     hi95  = quantile((plogis(draw) - 0.5) * 100, 0.975),
#     lo50  = quantile((plogis(draw) - 0.5) * 100, 0.25),
#     hi50  = quantile((plogis(draw) - 0.5) * 100, 0.75),
#     .groups = "drop"
#   )
#
# interaction_plot <- ggplot(interaction_df, aes(x = CI_val, y = mean)) +
#   geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +
#   geom_ribbon(aes(ymin = lo95, ymax = hi95), alpha = 0.15) +
#   geom_ribbon(aes(ymin = lo50, ymax = hi50), alpha = 0.3) +
#   geom_line(linewidth = 0.8) +
#   geom_point(size = 2.5, shape = 21, fill = "white", stroke = 1) +
#   scale_x_continuous(
#     breaks = 1:5,
#     labels = c("1\n(least)", "2", "3", "4", "5\n(most)")
#   ) +
#   labs(
#     x = "Crime importance (CI)",
#     y = "Marginal effect of crime difference\n(pp change from 50% baseline, per 100k)",
#     title = "Interaction: effect of crime difference by crime importance",
#     caption = sprintf("N = %d respondents. Dark band: 50%% CI. Light band: 95%% CI.", n_respondents_crime)
#   ) +
#   theme_classic()
#
# print(interaction_plot)
#
# ggsave(
#   "latex/images/comparison_crime_interaction_plot.pdf",
#   plot = interaction_plot,
#   width = 6,
#   height = 4
# )

#test <- long_df %>% group_by(Respondent_ID) %>% summarize(selected_num = sum(Selected == TRUE))
# ggplot(test) +
#   geom_histogram(aes(x = selected_num), bins = 30) +
#   theme_bw()
