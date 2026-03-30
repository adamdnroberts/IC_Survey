library(sf)
library(ggplot2)
library(dplyr)
library(tidyr)

ageeml <- read.csv(
  "references/AGEEML_202512121054579_utf.csv",
  colClasses = c(CVEGEO = "character")
) %>%
  select(CVEGEO, POB_AGEEML = POB_TOTAL) %>%
  mutate(POB_AGEEML = as.numeric(POB_AGEEML))

d_geo <- st_read("data/00mun_simplified.geojson", quiet = TRUE)
d_geo$muni_id <- d_geo$CVEGEO

excluded_states <- c(
  "Ciudad de México",
  "Durango",
  "Oaxaca",
  "Veracruz de Ignacio de la Llave"
)

# Join population — keep all states so excluded states can still appear as neighbors
d <- d_geo %>%
  left_join(ageeml, by = "CVEGEO") %>%
  mutate(pop = coalesce(POB_TOTAL, POB_AGEEML))

# First quartile of population across all non-Oaxaca municipalities
q1_pop <- quantile(d$pop, 0.25, na.rm = TRUE)
cat(sprintf("Q1 population: %s\n", format(q1_pop, big.mark = ",")))

# Project to EPSG:6372 (Mexico ITRF2008, metres) for accurate distances
d_proj <- st_transform(d, 6372)
centroids <- st_centroid(d_proj)

# Full pairwise distance matrix (metres)
dist_mat <- st_distance(centroids, centroids)

# For each municipality, find the 10 closest others and count how many have pop > Q1
n_neighbors <- 5

get_max_dist_km <- function(threshold) {
  idx <- which(!is.na(d_proj$pop) & d_proj$pop > threshold)
  sapply(seq_len(nrow(d_proj)), function(i) {
    candidates <- setdiff(idx, i)
    dists <- as.numeric(dist_mat[i, candidates])
    nearest_dists <- sort(dists)[seq_len(min(n_neighbors, length(candidates)))]
    max(nearest_dists) / 1000
  })
}

plot_df <- data.frame(
  max_dist_km = c(
    get_max_dist_km(quantile(d$pop, 0.25, na.rm = TRUE)),
    get_max_dist_km(quantile(d$pop, 0.50, na.rm = TRUE))
  ),
  threshold = rep(c("Q1", "Median"), each = nrow(d_proj)),
  NOM_ENT = rep(d_proj$NOM_ENT, times = 2)
)

plot_df <- plot_df %>% filter(!NOM_ENT %in% excluded_states)

median_lines <- plot_df %>%
  group_by(threshold) %>%
  summarise(med = median(max_dist_km, na.rm = TRUE), .groups = "drop")

p90_lines <- plot_df %>%
  group_by(threshold) %>%
  summarise(p90 = quantile(max_dist_km, 0.9, na.rm = TRUE), .groups = "drop")

ggplot(plot_df, aes(x = max_dist_km, fill = threshold)) +
  geom_histogram(bins = 50, alpha = 0.5, position = "identity") +
  geom_vline(
    data = median_lines,
    aes(xintercept = med, color = threshold),
    linetype = "dashed",
    linewidth = 0.8
  ) +
  geom_vline(
    data = p90_lines,
    aes(xintercept = p90, color = threshold),
    linetype = "dotted",
    linewidth = 0.8
  ) +
  geom_text(
    data = subset(median_lines, threshold == "Q1"),
    aes(x = med, label = sprintf("%.0f km", med), color = threshold),
    y = Inf,
    vjust = 1.5,
    hjust = -0.1,
    size = 3,
    show.legend = FALSE
  ) +
  geom_text(
    data = subset(median_lines, threshold == "Median"),
    aes(x = med, label = sprintf("%.0f km", med), color = threshold),
    y = Inf,
    vjust = 3.5,
    hjust = -0.1,
    size = 3,
    show.legend = FALSE
  ) +
  geom_text(
    data = subset(p90_lines, threshold == "Q1"),
    aes(x = p90, label = sprintf("%.0f km", p90), color = threshold),
    y = Inf,
    vjust = 5.5,
    hjust = -0.1,
    size = 3,
    show.legend = FALSE
  ) +
  geom_text(
    data = subset(p90_lines, threshold == "Median"),
    aes(x = p90, label = sprintf("%.0f km", p90), color = threshold),
    y = Inf,
    vjust = 7.5,
    hjust = -0.1,
    size = 3,
    show.legend = FALSE
  ) +
  scale_fill_manual(values = c("Q1" = "#0072B2", "Median" = "#E69F00")) +
  scale_color_manual(
    values = c("Q1" = "#0072B2", "Median" = "#E69F00"),
    guide = "none"
  ) +
  labs(
    x = "Distance to 5th nearest neighbor (km)",
    y = "Count",
    fill = "Population threshold",
    title = "Distance to 5th Nearest Neighbor Above Population Threshold"
  ) +
  theme_classic() +
  theme(legend.position = "bottom")

ggsave("output/neighbor_dist_histogram.pdf", width = 8, height = 5)
ggsave("output/neighbor_dist_histogram.png", width = 8, height = 5, dpi = 300)
