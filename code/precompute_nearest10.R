library(dplyr)
library(sf)
library(data.table)

# Load municipality geodata (same as app.R startup)
d_geo <- st_read("data/00mun_simplified.geojson", quiet = TRUE)
d_geo$muni_id <- d_geo$CVEGEO

# Load population data from AGEEML as a named lookup vector
ageeml <- data.table::fread(
  "references/AGEEML_202512121054579_utf.csv",
  encoding = "Latin-1"
)
pop_lookup <- setNames(
  as.numeric(ageeml$POB_TOTAL),
  sprintf("%05d", as.integer(ageeml$CVEGEO))
)

excluded_states <- c(
  "Ciudad de México",
  "Durango",
  "Oaxaca",
  "Veracruz de Ignacio de la Llave"
)
d_geo <- d_geo %>% filter(!NOM_ENT %in% excluded_states)

# Compute centroids
centroids <- st_centroid(d_geo)

# Pairwise distance matrix (meters)
cat("Computing pairwise distances (this may take a moment)...\n")
dist_mat <- st_distance(centroids)

# For each municipality, get the 10 closest (excluding self)
# Candidates restricted to municipalities with at least 5,000 population
cat("Finding 10 closest municipalities for each...\n")
n_neighbors <- 10

muni_pop <- pop_lookup[d_geo$muni_id]
candidate_idx <- which(!is.na(muni_pop) & muni_pop >= 22000)
cat(sprintf(
  "Candidate pool: %d municipalities with population >= 22,000\n",
  length(candidate_idx)
))

closest <- lapply(seq_len(nrow(d_geo)), function(i) {
  d <- as.numeric(dist_mat[i, ])
  cands <- setdiff(candidate_idx, i)
  idx <- cands[order(d[cands])][seq_len(n_neighbors)]
  data.frame(
    muni_id = d_geo$muni_id[i],
    NOMGEO = d_geo$NOMGEO[i],
    NOM_ENT = d_geo$NOM_ENT[i],
    rank = seq_len(n_neighbors),
    neighbor_id = d_geo$muni_id[idx],
    neighbor_name = d_geo$NOMGEO[idx],
    neighbor_state = d_geo$NOM_ENT[idx],
    dist_km = round(d[idx] / 1000, 1)
  )
})

closest_df <- do.call(rbind, closest)

cat(sprintf(
  "\n10 closest municipalities computed for %d municipalities.\n",
  nrow(d_geo)
))
cat("Preview (first municipality):\n")
print(closest_df %>% filter(muni_id == closest_df$muni_id[1]))

saveRDS(closest_df, "data/nearest10.rds")
cat("\nSaved to data/nearest10.rds\n")
