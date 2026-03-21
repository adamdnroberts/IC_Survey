library(dplyr)
library(sf)

# Load data (same as app.R startup)
d_geo <- st_read("data/00mun_simplified.geojson", quiet = TRUE)
d_geo$muni_id <- d_geo$CVEGEO

load("data/magar2024_coalitions.Rdata")
all_parties <- magar2024 %>%
  mutate(
    CVEGEO = sprintf("%05d", inegi),
    governing_party = case_when(
      grepl("morena|pvem|pt", l01) ~ "MORENA",
      grepl("pan", l01) ~ "PAN",
      grepl("pri|prd", l01) ~ "PRI",
      grepl("mc", l01) ~ "MC",
      TRUE ~ NA_character_
    ),
    coalition_label = case_when(
      grepl("morena|pvem|pt", l01) ~ "MORENA/PVEM/PT",
      grepl("pan|pri|prd", l01) ~ "PAN/PRI/PRD",
      grepl("mc", l01) ~ "MC",
      TRUE ~ NA_character_
    )
  ) %>%
  select(CVEGEO, governing_party, coalition_label)

d_geo <- d_geo %>%
  left_join(all_parties, by = c("muni_id" = "CVEGEO")) %>%
  mutate(area_km2 = as.numeric(sf::st_area(.)) / 1e6)

muni_centroid_coords <- sf::st_coordinates(sf::st_centroid(d_geo))
d_geo$centroid_lon <- muni_centroid_coords[, 1]
d_geo$centroid_lat <- muni_centroid_coords[, 2]

large_munis <- d_geo %>%
  st_drop_geometry() %>%
  filter(!is.na(POB_TOTAL) & (POB_TOTAL >= 200000 | is_capital)) %>%
  pull(muni_id)

coalition_a <- c("MORENA", "PT", "PVEM")
coalition_b <- c("PAN", "PRI", "PRD", "MC")

get_same_coalition_parties <- function(party) {
  if (party %in% coalition_a) coalition_a else coalition_b
}

get_opposite_parties <- function(party) {
  if (party %in% coalition_a) coalition_b else coalition_a
}

# Mahalanobis matching function (identical to app.R)
mahal_match_munis <- function(home_id, candidates, n = 4) {
  if (nrow(candidates) == 0) {
    return(candidates)
  }
  if (nrow(candidates) <= n) {
    return(candidates)
  }

  home_row <- d_geo %>%
    st_drop_geometry() %>%
    filter(muni_id == home_id)

  if (nrow(home_row) == 0) {
    return(candidates[seq_len(n), ])
  }

  home_sf <- sf::st_sfc(
    sf::st_point(c(home_row$centroid_lon[1], home_row$centroid_lat[1])),
    crs = 4326
  )
  cand_sf <- sf::st_as_sf(
    candidates,
    coords = c("centroid_lon", "centroid_lat"),
    crs = 4326,
    remove = FALSE
  )
  dist_km <- as.numeric(sf::st_distance(home_sf, cand_sf)) / 1000

  X <- cbind(
    log_pop = log(pmax(candidates$POB_TOTAL, 1)),
    log_area = log(pmax(candidates$area_km2, 0.01)),
    log_dist = log(pmax(dist_km, 0.1))
  )
  x0 <- c(
    log_pop = log(max(home_row$POB_TOTAL[1], 1)),
    log_area = log(max(home_row$area_km2[1], 0.01)),
    log_dist = log(0.1)
  )

  cov_X <- cov(X)
  mahal_dist <- tryCatch(
    mahalanobis(X, x0, cov_X),
    error = function(e) rowSums(scale(X)^2)
  )

  candidates[order(mahal_dist)[seq_len(n)], ]
}

# Helper: run all three match types for a given municipality CVEGEO
test_matching <- function(home_id) {
  home_row <- d_geo %>% st_drop_geometry() %>% filter(muni_id == home_id)
  home_party <- home_row$governing_party[1]

  cat(
    "Home:",
    home_row$NOMGEO[1],
    "|",
    home_row$NOM_ENT[1],
    "| Party:",
    home_party,
    "\n\n"
  )

  base_candidates <- d_geo %>%
    st_drop_geometry() %>%
    filter(
      muni_id != home_id,
      muni_id %in% large_munis,
      !is.na(POB_TOTAL),
      !is.na(area_km2)
    ) %>%
    select(
      muni_id,
      NOMGEO,
      NOM_ENT,
      governing_party,
      coalition_label,
      POB_TOTAL,
      area_km2,
      centroid_lon,
      centroid_lat
    )

  # T2: non-partisan
  np <- mahal_match_munis(home_id, base_candidates)
  cat("T2 (Non-partisan):\n")
  print(np %>% select(NOMGEO, NOM_ENT, governing_party, POB_TOTAL))
  cat("\n")

  # T3: opposite coalition
  opp_parties <- get_opposite_parties(home_party)
  opp_cands <- base_candidates %>% filter(governing_party %in% opp_parties)
  if (nrow(opp_cands) == 0) {
    opp_cands <- base_candidates
  }
  opp <- mahal_match_munis(home_id, opp_cands)
  cat("T3 (Opposite coalition):\n")
  print(opp %>% select(NOMGEO, NOM_ENT, governing_party, POB_TOTAL))
  cat("\n")

  # T4: same coalition
  same_parties <- get_same_coalition_parties(home_party)
  same_cands <- base_candidates %>% filter(governing_party %in% same_parties)
  if (nrow(same_cands) == 0) {
    same_cands <- base_candidates
  }
  same <- mahal_match_munis(home_id, same_cands)
  cat("T4 (Same coalition):\n")
  print(same %>% select(NOMGEO, NOM_ENT, governing_party, POB_TOTAL))
  cat("\n")
}

# Example: test with a specific municipality CVEGEO
# Find a CVEGEO to test with:
# d_geo %>% st_drop_geometry() %>% filter(grepl("Monterrey", NOMGEO)) %>% select(muni_id, NOMGEO, NOM_ENT)

test_matching("19039") # Monterrey

# How often does T2 (non-partisan) match the same municipalities as T4 (same-coalition)?
all_munis <- d_geo %>% st_drop_geometry() %>% filter(!is.na(POB_TOTAL), !is.na(area_km2)) %>% pull(muni_id)

overlap_results <- lapply(all_munis, function(home_id) {
  home_party <- d_geo %>%
    st_drop_geometry() %>%
    filter(muni_id == home_id) %>%
    pull(governing_party) %>%
    `[`(1)

  if (is.na(home_party)) return(NULL)

  base_candidates <- d_geo %>%
    st_drop_geometry() %>%
    filter(muni_id != home_id, muni_id %in% large_munis,
           !is.na(POB_TOTAL), !is.na(area_km2)) %>%
    select(muni_id, NOMGEO, NOM_ENT, governing_party, coalition_label,
           POB_TOTAL, area_km2, centroid_lon, centroid_lat)

  np   <- mahal_match_munis(home_id, base_candidates)$muni_id

  same_parties <- get_same_coalition_parties(home_party)
  same_cands   <- base_candidates %>% filter(governing_party %in% same_parties)
  if (nrow(same_cands) == 0) same_cands <- base_candidates
  same <- mahal_match_munis(home_id, same_cands)$muni_id

  n_overlap <- length(intersect(np, same))
  data.frame(home_id = home_id, governing_party = home_party,
             n_overlap = n_overlap, n_total = length(np))
})

overlap_df <- do.call(rbind, Filter(Negate(is.null), overlap_results))

cat("\nOverlap between T2 (non-partisan) and T4 (same-coalition) matches:\n")
cat("Municipalities with 0 overlapping matches:", sum(overlap_df$n_overlap == 0), "\n")
cat("Municipalities with 1 overlapping match: ", sum(overlap_df$n_overlap == 1), "\n")
cat("Municipalities with 2 overlapping matches:", sum(overlap_df$n_overlap == 2), "\n")
cat("Municipalities with 3 overlapping matches:", sum(overlap_df$n_overlap == 3), "\n")
cat("Municipalities with 4 overlapping matches:", sum(overlap_df$n_overlap == 4), "\n")
cat("\nMean overlap:", round(mean(overlap_df$n_overlap), 2), "out of", overlap_df$n_total[1], "\n")
cat("\nBy governing party:\n")
print(overlap_df %>% group_by(governing_party) %>% summarize(mean_overlap = round(mean(n_overlap), 2), n = n()))

# ── Rank of home municipality by robbery level among the 5 shown ─────────────
# Rank 1 = home has the MOST robberies; Rank 5 = home has the FEWEST.

robo_data <- readRDS("data/robo_2025.rds") %>%
  mutate(muni_id = sprintf("%05d", Cve..Municipio))

robos_lookup <- setNames(robo_data$robos, robo_data$muni_id)

rank_states <- c(
  "Aguascalientes", "Baja California", "Baja California Sur", "Campeche",
  "Chiapas", "Chihuahua", "Coahuila de Zaragoza", "Colima", "Guanajuato",
  "Guerrero", "Hidalgo", "Jalisco", "México", "Michoacán de Ocampo",
  "Morelos", "Nayarit", "Nuevo León", "Puebla", "Querétaro", "Quintana Roo",
  "San Luis Potosí", "Sinaloa", "Sonora", "Tlaxcala", "Zacatecas"
)

rank_munis <- d_geo %>%
  st_drop_geometry() %>%
  filter(!is.na(POB_TOTAL), !is.na(area_km2), NOM_ENT %in% rank_states) %>%
  pull(muni_id)

rank_results <- lapply(seq_along(rank_munis), function(i) {
  home_id <- rank_munis[i]
  if (i %% 100 == 0) cat(sprintf("Processing %d / %d...\n", i, length(rank_munis)))
  home_party <- d_geo %>%
    st_drop_geometry() %>%
    filter(muni_id == home_id) %>%
    pull(governing_party) %>%
    `[`(1)

  home_robos <- robos_lookup[home_id]

  if (is.na(home_party) || is.na(home_robos)) return(NULL)

  base_candidates <- d_geo %>%
    st_drop_geometry() %>%
    filter(muni_id != home_id,
           !is.na(POB_TOTAL), !is.na(area_km2)) %>%
    select(muni_id, NOMGEO, NOM_ENT, governing_party, coalition_label,
           POB_TOTAL, area_km2, centroid_lon, centroid_lat)

  compute_rank <- function(matched_ids) {
    comp_robos <- robos_lookup[matched_ids]
    if (any(is.na(comp_robos))) return(NA_real_)
    rank(-c(home_robos, comp_robos), ties.method = "average")[1]
  }

  # T2: non-partisan
  np_ids <- mahal_match_munis(home_id, base_candidates)$muni_id
  rank_t2 <- compute_rank(np_ids)

  # T3: opposite coalition
  opp_parties <- get_opposite_parties(home_party)
  opp_cands <- base_candidates %>% filter(governing_party %in% opp_parties)
  if (nrow(opp_cands) == 0) opp_cands <- base_candidates
  opp_ids <- mahal_match_munis(home_id, opp_cands)$muni_id
  rank_t3 <- compute_rank(opp_ids)

  # T4: same coalition
  same_parties <- get_same_coalition_parties(home_party)
  same_cands <- base_candidates %>% filter(governing_party %in% same_parties)
  if (nrow(same_cands) == 0) same_cands <- base_candidates
  same_ids <- mahal_match_munis(home_id, same_cands)$muni_id
  rank_t4 <- compute_rank(same_ids)

  data.frame(home_id = home_id, governing_party = home_party,
             rank_t2 = rank_t2, rank_t3 = rank_t3, rank_t4 = rank_t4)
})

rank_df <- do.call(rbind, Filter(Negate(is.null), rank_results))

print_rank_dist <- function(ranks, label) {
  n_valid <- sum(!is.na(ranks))
  cat(label, "(n =", n_valid, "):\n")
  for (r in 1:5) {
    cnt <- sum(round(ranks) == r, na.rm = TRUE)
    cat(sprintf("  Rank %d: %4d  (%5.1f%%)\n",
                r, cnt, cnt / n_valid * 100))
  }
  cat(sprintf("  Mean rank: %.2f\n\n", mean(ranks, na.rm = TRUE)))
}

cat("\n── Home municipality robbery rank among the 5 shown ────────────────────────\n")
cat("Rank 1 = home has MOST robberies; Rank 5 = home has FEWEST\n\n")
print_rank_dist(rank_df$rank_t2, "T2 (Non-partisan comparison)")
print_rank_dist(rank_df$rank_t3, "T3 (Opposite-coalition comparison)")
print_rank_dist(rank_df$rank_t4, "T4 (Same-coalition comparison)")

cat("By governing party (mean rank across treatment types):\n")
print(rank_df %>%
  group_by(governing_party) %>%
  summarize(mean_rank_t2 = round(mean(rank_t2, na.rm = TRUE), 2),
            mean_rank_t3 = round(mean(rank_t3, na.rm = TRUE), 2),
            mean_rank_t4 = round(mean(rank_t4, na.rm = TRUE), 2),
            n = n()))
