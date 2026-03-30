library(dplyr)
library(sf)

# Load municipality geodata (same as app.R startup)
d_geo <- st_read("data/00mun_simplified.geojson", quiet = TRUE)
d_geo$muni_id <- d_geo$CVEGEO

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
cat("Finding 10 closest municipalities for each...\n")
n_neighbors <- 10

closest <- lapply(seq_len(nrow(d_geo)), function(i) {
  d <- as.numeric(dist_mat[i, ])
  idx <- order(d)[2:(n_neighbors + 1)]  # [1] is self
  data.frame(
    muni_id      = d_geo$muni_id[i],
    NOMGEO       = d_geo$NOMGEO[i],
    NOM_ENT      = d_geo$NOM_ENT[i],
    rank         = seq_len(n_neighbors),
    neighbor_id  = d_geo$muni_id[idx],
    neighbor_name = d_geo$NOMGEO[idx],
    neighbor_state = d_geo$NOM_ENT[idx],
    dist_km      = round(d[idx] / 1000, 1)
  )
})

closest_df <- do.call(rbind, closest)

cat(sprintf("\n10 closest municipalities computed for %d municipalities.\n", nrow(d_geo)))
cat("Preview (first municipality):\n")
print(closest_df %>% filter(muni_id == closest_df$muni_id[1]))

saveRDS(closest_df, "data/nearest10.rds")
cat("\nSaved to data/nearest10.rds\n")
