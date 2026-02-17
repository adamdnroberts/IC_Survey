library(sf)
library(dplyr)
library(tidyr)

# --- 1. Load data ---
d_geo <- st_read("data/00mun_simplified.geojson", quiet = TRUE)
ageeml <- read.csv(
  "references/AGEEML_202512121054579_utf.csv",
  colClasses = c(CVEGEO = "character", CVE_ENT = "character")
)
magar_incumbents <- read.csv("data/magar_incumbents.csv")

# --- 2. Filter to 3 states: EdoMex (15), Morelos (17), Puebla (21) ---
target_states <- c("15", "17", "21")
d_geo <- d_geo %>% filter(CVE_ENT %in% target_states)

# --- 3. Compute geographic area (km²) ---
d_geo <- d_geo %>%
  st_transform(6372) %>%
  mutate(area_km2 = as.numeric(st_area(geometry)) / 1e6) %>%
  st_drop_geometry()

# --- 4. Merge population from AGEEML (fixes CDMX null issue) ---
pop <- ageeml %>%
  filter(CVE_ENT %in% target_states) %>%
  select(CVEGEO, POB_TOTAL) %>%
  mutate(POB_TOTAL = as.numeric(POB_TOTAL))

d <- d_geo %>%
  select(CVEGEO, CVE_ENT, NOM_ENT, NOM_MUN = NOMGEO, area_km2) %>%
  left_join(pop, by = "CVEGEO")

cat("Municipalities with missing population:", sum(is.na(d$POB_TOTAL)), "\n")

# --- 5. Classify coalitions ---
magar_2024 <- magar_incumbents %>%
  mutate(state_abbr = sub("-.*", "", emm)) %>%
  filter(yr == 2024, state_abbr %in% c("mex", "mor", "pue")) %>%
  mutate(
    CVEGEO = sprintf("%05d", inegi),
    coalition_label = case_when(
      grepl("morena|pvem|pt", part) ~ "MORENA/PVEM/PT",
      grepl("pan", part) & grepl("pri|prd", part) ~ "PAN/PRI/PRD",
      grepl("pan", part) ~ "PAN",
      grepl("pri|prd", part) ~ "PRI/PRD",
      grepl("mc", part) ~ "MC",
      TRUE ~ "Other"
    )
  ) %>%
  select(CVEGEO, coalition_label)

all_parties <- magar_2024
d <- d %>% left_join(all_parties, by = "CVEGEO")

cat("\nCoalition counts:\n")
print(table(d$coalition_label, useNA = "ifany"))

# Drop municipalities with no coalition data, missing population, or "Other" coalition
d <- d %>% filter(
  !is.na(coalition_label),
  coalition_label != "Other",
  !is.na(POB_TOTAL),
  POB_TOTAL > 0
)

# --- 6. Compute Mahalanobis distance between cross-coalition pairs ---
log_pop <- log(d$POB_TOTAL)
log_area <- log(d$area_km2)
X <- cbind(log_pop, log_area)
S <- cov(X)
S_inv <- solve(S)

n <- nrow(d)
# Compute pairwise Mahalanobis distances
maha_dist <- matrix(NA, n, n)
for (i in seq_len(n)) {
  diff <- sweep(X, 2, X[i, ])
  maha_dist[i, ] <- sqrt(rowSums((diff %*% S_inv) * diff))
}

# --- 7. Find 4 nearest matches in each other coalition ---
# Source municipalities: CDMX (09), EdoMex (15), Morelos (17) only
# Match candidates: all 4 states (including Puebla)
source_states <- target_states
source_idx <- which(d$CVE_ENT %in% source_states)
coalitions <- unique(d$coalition_label)
k <- 4 # number of matches per coalition
matches <- list()

for (i in source_idx) {
  my_coalition <- d$coalition_label[i]
  other_coalitions <- setdiff(coalitions, my_coalition)

  for (oc in other_coalitions) {
    idx <- which(d$coalition_label == oc)
    dists <- maha_dist[i, idx]
    top_k <- idx[order(dists)[seq_len(min(k, length(idx)))]]
    for (rank in seq_along(top_k)) {
      j <- top_k[rank]
      matches[[length(matches) + 1]] <- data.frame(
        muni_cvegeo = d$CVEGEO[i],
        muni_name = d$NOM_MUN[i],
        muni_state = d$NOM_ENT[i],
        muni_coalition = my_coalition,
        muni_pop = d$POB_TOTAL[i],
        muni_area_km2 = round(d$area_km2[i], 2),
        match_rank = rank,
        match_cvegeo = d$CVEGEO[j],
        match_name = d$NOM_MUN[j],
        match_state = d$NOM_ENT[j],
        match_coalition = oc,
        match_pop = d$POB_TOTAL[j],
        match_area_km2 = round(d$area_km2[j], 2),
        mahalanobis_dist = round(maha_dist[i, j], 4),
        stringsAsFactors = FALSE
      )
    }
  }
}

result <- bind_rows(matches) %>%
  arrange(muni_cvegeo, match_coalition, match_rank)

# --- 8. Output ---
write.csv(result, "output/mahalanobis_matches.csv", row.names = FALSE)
cat(
  "\nSaved",
  nrow(result),
  "matched pairs to output/mahalanobis_matches.csv\n"
)

# Print a few example matches for large municipalities
cat("\nExample matches for large municipalities:\n")
examples <- result %>%
  filter(muni_pop > 500000) %>%
  select(
    muni_name,
    muni_coalition,
    match_name,
    match_coalition,
    mahalanobis_dist
  )
print(as.data.frame(examples))
