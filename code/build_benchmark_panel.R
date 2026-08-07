# Build respondent × candidate panel for benchmark municipality analysis
# Input:  wave1_responses.rds, geojson, coalitions, crime, nearest10
# Output: data/benchmark_panel.rds

library(dplyr)
library(tidyr)
library(sf)
library(data.table)

haversine_km <- function(lon1, lat1, lon2, lat2) {
  R <- 6371
  dlat <- (lat2 - lat1) * pi / 180
  dlon <- (lon2 - lon1) * pi / 180
  a <- sin(dlat / 2)^2 +
    cos(lat1 * pi / 180) * cos(lat2 * pi / 180) * sin(dlon / 2)^2
  2 * R * atan2(sqrt(a), sqrt(1 - a))
}

survey_responses_wave1 <- readRDS("data/derived/wave1_responses.rds")

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

coords <- st_coordinates(st_centroid(d_geo))
centroids <- d_geo %>%
  st_drop_geometry() %>%
  mutate(centroid_lon = coords[, 1], centroid_lat = coords[, 2]) %>%
  select(muni_id, NOM_ENT, centroid_lon, centroid_lat)

coords_all <- st_coordinates(st_centroid(d_geo_all))
centroids_all <- d_geo_all %>%
  st_drop_geometry() %>%
  mutate(centroid_lon = coords_all[, 1], centroid_lat = coords_all[, 2]) %>%
  select(muni_id, NOM_ENT, centroid_lon, centroid_lat)

ageeml <- data.table::fread(
  "references/AGEEML_202512121054579_utf.csv",
  encoding = "Latin-1"
)
pop_lookup <- setNames(
  as.numeric(ageeml$POB_TOTAL),
  sprintf("%05d", suppressWarnings(as.integer(ageeml$CVEGEO)))
)
pop_lookup <- pop_lookup[!is.na(names(pop_lookup))]

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

top20_munis <- names(sort(pop_lookup, decreasing = TRUE))[1:20]

robo_data <- readRDS("data/robo_2025.rds")
crime_lookup <- setNames(
  robo_data$rate_per_100k,
  sprintf("%05d", suppressWarnings(as.integer(robo_data$Cve..Municipio)))
)
crime_lookup <- crime_lookup[!is.na(names(crime_lookup))]

nearest10 <- readRDS("data/nearest10.rds")
nearest10_set <- nearest10 %>%
  select(home_id = muni_id, neighbor_id)

muni_meta <- centroids %>%
  left_join(all_parties, by = "muni_id") %>%
  mutate(pop = pop_lookup[muni_id])

muni_meta_all <- centroids_all %>%
  left_join(all_parties, by = "muni_id") %>%
  mutate(pop = pop_lookup[muni_id])

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
  mutate(
    selected_vec = strsplit(Benchmark_Selected_Municipalities, ";"),
    Selected = mapply(
      function(cid, sel) cid %in% trimws(sel),
      candidate_id,
      selected_vec
    )
  ) %>%
  select(-selected_vec, -Benchmark_Selected_Municipalities)

long_df <- long_df %>%
  left_join(
    nearest10_set %>% mutate(in_nearest10 = TRUE),
    by = c("home_id", "candidate_id" = "neighbor_id")
  ) %>%
  mutate(
    in_nearest10 = coalesce(in_nearest10, FALSE),
    in_top20 = candidate_id %in% top20_munis
  )

set.seed(42)
assign_pools <- function(df) {
  n <- nrow(df)
  pool <- rep("random", n)

  nearest_idx <- which(df$in_nearest10)
  if (length(nearest_idx) > 5) {
    nearest_idx <- sample(nearest_idx, 5)
  }
  pool[nearest_idx] <- "nearest"

  largest_idx <- which(df$in_top20 & pool != "nearest")
  if (length(largest_idx) > 5) {
    largest_idx <- sample(largest_idx, 5)
  }
  pool[largest_idx] <- "largest"

  df$pool <- pool
  df
}

long_df <- long_df %>%
  group_by(Respondent_ID) %>%
  group_modify(~ assign_pools(.x)) %>%
  ungroup() %>%
  select(-in_nearest10, -in_top20)

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

ci_lookup <- setNames(
  6 - as.numeric(survey_responses_wave1$Importance_Crime),
  survey_responses_wave1$Respondent_ID
)

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
    log_home_pop = log(home_pop + 1),
    CI_f = relevel(as.factor(CI), ref = "3")
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
    CI,
    CI_f
  )

n_respondents <- n_distinct(long_df$Respondent_ID)

cat(sprintf(
  "Long format: %d rows (%d respondents × ~15 candidates)\n",
  nrow(long_df),
  n_respondents
))
cat(sprintf("Selection rate: %.1f%%\n", 100 * mean(long_df$Selected)))
print(table(long_df$pool))

saveRDS(long_df, "data/derived/benchmark_panel.rds")
cat("Wrote data/benchmark_panel.rds\n")
