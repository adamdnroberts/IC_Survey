library(dplyr)
library(sf)
library(ggplot2)
library(tidyr)

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

coalition_a <- c("MORENA", "PT", "PVEM")
coalition_b <- c("PAN", "PRI", "PRD", "MC")

get_same_coalition_parties <- function(party) {
  if (party %in% coalition_a) coalition_a else coalition_b
}

get_opposite_parties <- function(party) {
  if (party %in% coalition_a) coalition_b else coalition_a
}

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

# ── Rank of home municipality by robbery level among the 5 shown ─────────────
# Rank 1 = home has the MOST robberies; Rank 5 = home has the FEWEST.

robo_data <- readRDS("data/robo_2025.rds") %>%
  mutate(muni_id = sprintf("%05d", Cve..Municipio))

robos_lookup <- setNames(robo_data$rate_per_100k, robo_data$muni_id)

excluded_states <- c(
  "Ciudad de México",
  "Durango",
  "Oaxaca",
  "Veracruz de Ignacio de la Llave"
)

rank_munis <- d_geo %>%
  st_drop_geometry() %>%
  filter(!is.na(POB_TOTAL), !is.na(area_km2), !NOM_ENT %in% excluded_states) %>%
  pull(muni_id)

rank_results <- lapply(seq_along(rank_munis), function(i) {
  home_id <- rank_munis[i]
  if (i %% 100 == 0) {
    cat(sprintf("Processing %d / %d...\n", i, length(rank_munis)))
  }
  home_party <- d_geo %>%
    st_drop_geometry() %>%
    filter(muni_id == home_id) %>%
    pull(governing_party) %>%
    `[`(1)

  home_robos <- robos_lookup[home_id]

  if (is.na(home_party) || is.na(home_robos)) {
    return(NULL)
  }

  base_candidates <- d_geo %>%
    st_drop_geometry() %>%
    filter(muni_id != home_id, !is.na(POB_TOTAL), !is.na(area_km2)) %>%
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

  compute_rank <- function(matched_ids) {
    comp_robos <- robos_lookup[matched_ids]
    if (any(is.na(comp_robos))) {
      return(NA_real_)
    }
    rank(-c(home_robos, comp_robos), ties.method = "average")[1]
  }

  # T2: non-partisan
  np_ids <- mahal_match_munis(home_id, base_candidates)$muni_id
  rank_t2 <- compute_rank(np_ids)

  # T3: opposite coalition
  opp_parties <- get_opposite_parties(home_party)
  opp_cands <- base_candidates %>% filter(governing_party %in% opp_parties)
  if (nrow(opp_cands) == 0) {
    opp_cands <- base_candidates
  }
  opp_ids <- mahal_match_munis(home_id, opp_cands)$muni_id
  rank_t3 <- compute_rank(opp_ids)

  # T4: same coalition
  same_parties <- get_same_coalition_parties(home_party)
  same_cands <- base_candidates %>% filter(governing_party %in% same_parties)
  if (nrow(same_cands) == 0) {
    same_cands <- base_candidates
  }
  same_ids <- mahal_match_munis(home_id, same_cands)$muni_id
  rank_t4 <- compute_rank(same_ids)

  data.frame(
    home_id = home_id,
    governing_party = home_party,
    rank_t2 = rank_t2,
    rank_t3 = rank_t3,
    rank_t4 = rank_t4
  )
})

rank_df <- do.call(rbind, Filter(Negate(is.null), rank_results))

print_rank_dist <- function(ranks, label) {
  n_valid <- sum(!is.na(ranks))
  cat(label, "(n =", n_valid, "):\n")
  for (r in 1:5) {
    cnt <- sum(round(ranks) == r, na.rm = TRUE)
    cat(sprintf("  Rank %d: %4d  (%5.1f%%)\n", r, cnt, cnt / n_valid * 100))
  }
  cat(sprintf("  Mean rank: %.2f\n\n", mean(ranks, na.rm = TRUE)))
}

cat(
  "\n── Home municipality robbery rank among the 5 shown ────────────────────────\n"
)
cat("Rank 1 = home has MOST robberies; Rank 5 = home has FEWEST\n\n")
print_rank_dist(rank_df$rank_t2, "T2 (Non-partisan comparison)")
print_rank_dist(rank_df$rank_t3, "T3 (Opposite-coalition comparison)")
print_rank_dist(rank_df$rank_t4, "T4 (Same-coalition comparison)")

cat("By governing party (mean rank across treatment types):\n")
print(
  rank_df %>%
    group_by(governing_party) %>%
    summarize(
      mean_rank_t2 = round(mean(rank_t2, na.rm = TRUE), 2),
      mean_rank_t3 = round(mean(rank_t3, na.rm = TRUE), 2),
      mean_rank_t4 = round(mean(rank_t4, na.rm = TRUE), 2),
      n = n()
    )
)

# ── Plot: rank distribution by treatment ──────────────────────────────────────

plot_data <- rank_df %>%
  pivot_longer(
    cols = c(rank_t2, rank_t3, rank_t4),
    names_to = "treatment",
    values_to = "rank"
  ) %>%
  filter(!is.na(rank)) %>%
  mutate(
    rank = round(rank),
    treatment = recode(
      treatment,
      rank_t2 = "T2: Non-partisan",
      rank_t3 = "T3: Opposite coalition",
      rank_t4 = "T4: Same coalition"
    )
  ) %>%
  group_by(treatment, rank) %>%
  summarize(n = n(), .groups = "drop") %>%
  group_by(treatment) %>%
  mutate(pct = n / sum(n) * 100)

rank_dist_plot <- ggplot(
  plot_data,
  aes(x = factor(rank), y = pct, fill = treatment)
) +
  geom_col(position = "dodge") +
  geom_hline(yintercept = 20, linetype = "dashed", color = "gray50") +
  scale_fill_manual(
    values = c(
      "T2: Non-partisan" = "#999999",
      "T3: Opposite coalition" = "#E69F00",
      "T4: Same coalition" = "#0072B2"
    )
  ) +
  labs(
    x = "Rank of home municipality (1 = most robberies, 5 = fewest)",
    y = "% of municipalities",
    fill = NULL,
    title = "Distribution of home municipality robbery rank among the 5 shown",
    caption = "Dashed line = uniform distribution (20% per rank)"
  ) +
  theme_classic() +
  theme(legend.position = "bottom")

print(rank_dist_plot)
ggsave(
  "latex/images/rank_dist_plot.pdf",
  plot = rank_dist_plot,
  width = 7,
  height = 4.5
)
