library(dplyr)
library(sf)
library(igraph)
library(ggraph)
library(tidyr)
library(fixest)

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
      l01 == "0" ~ NA_character_,
      !is.na(l01) & l01 != "" ~ "Otro",
      TRUE ~ NA_character_
    ),
    coalition_label = case_when(
      grepl("morena|pvem|pt", l01) ~ "MORENA/PVEM/PT",
      grepl("pan|pri|prd", l01) ~ "PAN/PRI/PRD",
      grepl("mc", l01) ~ "MC",
      l01 == "0" ~ NA_character_,
      !is.na(l01) & l01 != "" ~ "Otro",
      TRUE ~ NA_character_
    )
  ) %>%
  select(CVEGEO, governing_party, coalition_label)

excluded_states <- c(
  "Ciudad de México",
  "Durango",
  "Oaxaca",
  "Veracruz de Ignacio de la Llave"
)

d <- d_geo %>%
  mutate(area_km2 = as.numeric(sf::st_area(.)) / 1e6) %>%
  st_drop_geometry() %>%
  left_join(all_parties, by = c("muni_id" = "CVEGEO")) %>%
  filter(!NOM_ENT %in% excluded_states)

# ── Non-main-party municipalities in analysis sample ─────────────────────────

cat(
  "── Non-main-party municipalities (analysis sample) ─────────────────────\n"
)
cat("Total municipalities (excl. states):", nrow(d), "\n")
cat(
  "Governed by main parties:           ",
  sum(!is.na(d$governing_party) & d$governing_party != "Otro"),
  "\n"
)
cat(
  "Not governed by main parties:       ",
  sum(is.na(d$governing_party) | d$governing_party == "Otro"),
  "\n\n"
)
test <- d %>%
  filter(is.na(governing_party) | governing_party == "Otro") %>%
  left_join(
    magar2024 %>%
      mutate(CVEGEO = sprintf("%05d", inegi)) %>%
      select(CVEGEO, l01),
    by = c("muni_id" = "CVEGEO")
  )

# ── Municipalities by coalition ───────────────────────────────────────────────

cat(
  "── Municipalities by coalition ──────────────────────────────────────────\n"
)
muni_counts <- d %>%
  count(coalition_label, name = "n_munis") %>%
  mutate(pct = round(n_munis / sum(n_munis) * 100, 1)) %>%
  arrange(desc(n_munis))
print(muni_counts)

cat(
  "\nMissing coalition_label:",
  sum(is.na(d$coalition_label)),
  "municipalities\n"
)

# ── Population by coalition ───────────────────────────────────────────────────

cat(
  "\n── Population by coalition ──────────────────────────────────────────────\n"
)
pop_by_coalition <- d %>%
  filter(!is.na(POB_TOTAL)) %>%
  group_by(coalition_label) %>%
  summarize(
    total_pop = sum(POB_TOTAL),
    mean_pop = round(mean(POB_TOTAL)),
    median_pop = median(POB_TOTAL),
    n_munis = n()
  ) %>%
  mutate(pct_pop = round(total_pop / sum(total_pop) * 100, 1)) %>%
  arrange(desc(total_pop))
print(pop_by_coalition)

# ── Municipalities by coalition and state (top states per coalition) ───────────

cat(
  "\n── Municipality counts by state and coalition ───────────────────────────\n"
)
state_coalition <- d %>%
  filter(!is.na(coalition_label)) %>%
  count(NOM_ENT, coalition_label) %>%
  tidyr::pivot_wider(
    names_from = coalition_label,
    values_from = n,
    values_fill = 0
  ) %>%
  arrange(NOM_ENT)
print(state_coalition, n = Inf)

# ── Party co-governance network ───────────────────────────────────────────────

known_parties <- c("morena", "pvem", "pt", "pan", "pri", "prd", "mc")

# Detect which parties are present in each municipality's l01 string
party_matrix <- sapply(known_parties, function(p) grepl(p, magar2024$l01))
colnames(party_matrix) <- toupper(known_parties)

# Add "Otro" column: municipalities not matched by any known party and not coded 0
otro_col <- !rowSums(party_matrix) &
  magar2024$l01 != "0" &
  !is.na(magar2024$l01)
party_matrix <- cbind(party_matrix, OTRO = otro_col)

# Build edge list: one row per party pair, weight = # municipalities co-governed
pair_list <- combn(colnames(party_matrix), 2, simplify = FALSE)
edges <- do.call(
  rbind,
  lapply(pair_list, function(pair) {
    n <- sum(party_matrix[, pair[1]] & party_matrix[, pair[2]])
    if (n > 0) data.frame(from = pair[1], to = pair[2], weight = n)
  })
)

all_nodes <- data.frame(name = colnames(party_matrix))
g <- graph_from_data_frame(edges, directed = FALSE, vertices = all_nodes)

coalition_colors <- c(
  MORENA = "#B2182B",
  PVEM = "#B2182B",
  PT = "#B2182B",
  PAN = "#2166AC",
  PRI = "#2166AC",
  PRD = "#2166AC",
  MC = "#F28E2B",
  OTRO = "#888888"
)
coalition_labels <- c(
  MORENA = "MORENA/PT/PVEM",
  PVEM = "MORENA/PT/PVEM",
  PT = "MORENA/PT/PVEM",
  PAN = "PAN/PRI/PRD",
  PRI = "PAN/PRI/PRD",
  PRD = "PAN/PRI/PRD",
  MC = "MC",
  OTRO = "Otro"
)

V(g)$coalition <- coalition_labels[V(g)$name]
V(g)$node_color <- coalition_colors[V(g)$name]
V(g)$n_munis <- colSums(party_matrix)[V(g)$name]

node_positions <- list(
  MORENA = c(-0.5, 0.1),
  PT = c(-0.65, -0.2),
  PVEM = c(-0.35, -0.2),
  PAN = c(0.5, 0.1),
  PRI = c(0.65, -0.2),
  PRD = c(0.35, -0.2),
  MC = c(0.0, 0.4),
  OTRO = c(0.0, -0.45)
)
layout_coords <- do.call(rbind, node_positions[V(g)$name])
network_plot <- ggraph(g, layout = layout_coords) +
  geom_edge_link(aes(width = weight, alpha = weight), color = "gray40") +
  scale_edge_width(range = c(0.5, 5), name = "Co-governed\nmunicipalities") +
  scale_edge_alpha(range = c(0.2, 0.9), guide = "none") +
  geom_node_point(aes(color = coalition, size = n_munis)) +
  geom_node_label(
    aes(label = name),
    repel = TRUE,
    size = 3.5,
    fontface = "bold"
  ) +
  scale_color_manual(
    values = c(
      "MORENA/PT/PVEM" = "#B2182B",
      "PAN/PRI/PRD" = "#2166AC",
      "MC" = "#F28E2B",
      "Otro" = "#888888"
    ),
    name = "Coalition"
  ) +
  scale_size_continuous(range = c(4, 12), name = "Municipalities\ngoverned") +
  labs(
    title = "",
    subtitle = ""
  ) +
  theme_graph(base_family = "sans")

print(network_plot)
ggsave(
  "latex/images/network_plot.pdf",
  plot = network_plot,
  width = 8,
  height = 6
)

# ── Municipalities with both MORENA and MC in coalition ───────────────────────

cat(
  "\n── Municipalities with both MORENA and MC in coalition ──────────────────\n"
)
morena_mc <- magar2024 %>%
  filter(grepl("morena", l01) & grepl("mc", l01)) %>%
  mutate(CVEGEO = sprintf("%05d", inegi)) %>%
  left_join(
    d_geo %>% st_drop_geometry() %>% select(muni_id, NOMGEO, NOM_ENT),
    by = c("CVEGEO" = "muni_id")
  ) %>%
  select(CVEGEO, NOMGEO, NOM_ENT, l01)

cat("Count:", nrow(morena_mc), "\n\n")
print(morena_mc, n = Inf)

# ── Regression: robberies ~ coalition + controls ───────────────────────────────

robo_data <- readRDS("data/robo_2025.rds") %>%
  mutate(muni_id = sprintf("%05d", Cve..Municipio))

reg_data <- d %>%
  left_join(robo_data %>% select(muni_id, robos), by = "muni_id") %>%
  filter(
    !is.na(robos),
    !is.na(coalition_label),
    !is.na(POB_TOTAL),
    POB_TOTAL > 0,
    !is.na(area_km2),
    area_km2 > 0
  ) %>%
  mutate(
    coalition_label = relevel(factor(coalition_label), ref = "PAN/PRI/PRD"),
    robos_per_100k = robos / POB_TOTAL * 100000,
    log_pop = log(POB_TOTAL),
    log_area = log(area_km2)
  )

#no controls
fit0 <- feols(
  robos_per_100k ~ coalition_label,
  #cluster = "NOM_ENT",
  data = reg_data
)

# Without state FEs
fit1 <- feols(
  robos_per_100k ~ coalition_label + log_pop + log_area,
  #cluster = "NOM_ENT",
  data = reg_data
)

# With state fixed effects
fit2 <- feols(
  robos_per_100k ~ coalition_label + log_pop + log_area | NOM_ENT,
  vcov = "iid",
  data = reg_data
)

fit3 <- feols(
  robos_per_100k ~ coalition_label + log_pop + log_area | NOM_ENT,
  data = reg_data
)

etable(fit0, fit1, fit2, fit3, tex = T)
#stargazer::stargazer(fit0, fit1, fit2, type = "text")
