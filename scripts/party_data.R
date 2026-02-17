library(dplyr)
library(ggplot2)

magar_incumbents <- read.csv("~/IC_Survey/data/magar_incumbents.csv")

# Map state abbreviations to full names
state_names <- c(
  "ags" = "Aguascalientes",
  "bc" = "Baja California",
  "bcs" = "Baja California Sur",
  "cam" = "Campeche",
  "coa" = "Coahuila",
  "col" = "Colima",
  "chi" = "Chiapas",
  "chs" = "Chihuahua",
  "cin" = "Ciudad de México",
  "cdmx" = "Ciudad de México",
  "df" = "Ciudad de México",
  "dur" = "Durango",
  "gua" = "Guanajuato",
  "gue" = "Guerrero",
  "hid" = "Hidalgo",
  "jal" = "Jalisco",
  "mex" = "Estado de México",
  "mic" = "Michoacán",
  "mor" = "Morelos",
  "nay" = "Nayarit",
  "nl" = "Nuevo León",
  "oax" = "Oaxaca",
  "pue" = "Puebla",
  "que" = "Querétaro",
  "qui" = "Quintana Roo",
  "san" = "San Luis Potosí",
  "sin" = "Sinaloa",
  "son" = "Sonora",
  "tab" = "Tabasco",
  "tam" = "Tamaulipas",
  "tla" = "Tlaxcala",
  "ver" = "Veracruz",
  "yuc" = "Yucatán",
  "zac" = "Zacatecas"
)

magar_incumbents <- magar_incumbents %>%
  mutate(
    state_abbr = sub("-.*", "", emm),
    estado = state_names[state_abbr]
  )

magar_2024 <- filter(magar_incumbents, yr == 2024 & !grepl("^oax", emm))

# Group any coalition containing morena into the MORENA coalition (pvem-pt-morena)
magar_2024 <- magar_2024 %>%
  mutate(
    coalition = case_when(
      grepl("morena|pvem|pt", part) ~ "pvem-pt-morena",
      grepl("pan|pri|prd", part) ~ "pan-pri-prd",
      grepl("mc", part) ~ "mc",
      TRUE ~ part
    )
  )

# Check for cross-coalition cases (morena + pan/pri/prd in same coalition)
cross_coalition <- magar_2024 %>%
  filter(grepl("morena", part) & grepl("pan|pri|prd", part))
cat(
  "Cross-coalition cases (morena with pan/pri/prd):",
  nrow(cross_coalition),
  "\n"
)
if (nrow(cross_coalition) > 0) {
  print(cross_coalition %>% select(emm, mun, yr, part))
}

# Bar graph: frequency of each coalition
coalition_counts <- magar_2024 %>%
  count(estado, coalition, sort = TRUE)

ggplot(coalition_counts, aes(x = reorder(coalition, n), y = n)) +
  geom_col(fill = "#0072B2") +
  coord_flip() +
  labs(
    title = "Frequency of coalitions in municipal elections (2024)",
    x = "Coalition",
    y = "Count"
  ) +
  facet_wrap(~estado) +
  theme_minimal()

# --- Coalition map: CDMX, Estado de México, Morelos ---
library(sf)
library(ggnewscale)

d_geo <- st_read("data/00mun_simplified.geojson", quiet = TRUE)

# Filter to Estado de México (15), Morelos (17), Puebla (21)
d_map <- d_geo %>%
  filter(CVE_ENT %in% c("15", "17", "21"))

# Prepare party data for join — format inegi as 5-digit CVEGEO
magar_map <- magar_2024 %>%
  filter(edon %in% c(15, 17, 21)) %>%
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

all_parties <- magar_map

# Join party data to geometries
d_map <- d_map %>%
  left_join(all_parties, by = "CVEGEO") %>%
  mutate(
    coalition_label = ifelse(is.na(coalition_label), "No data", coalition_label)
  )

# Set factor order for consistent legend
d_map$coalition_label <- factor(
  d_map$coalition_label,
  levels = c(
    "MORENA/PVEM/PT",
    "PAN/PRI/PRD",
    "PAN",
    "PRI/PRD",
    "MC",
    "Other",
    "No data"
  )
)

coalition_colors <- c(
  "MORENA/PVEM/PT" = "#8B0000",
  "PAN/PRI/PRD" = "#00008B",
  "PAN" = "#1E90FF",
  "PRI/PRD" = "#006400",
  "MC" = "#FF8C00",
  "Other" = "#808080",
  "No data" = "grey90"
)

# Dissolve municipalities into state boundaries for border overlay
state_borders <- d_map %>%
  group_by(CVE_ENT, NOM_ENT) %>%
  summarise(geometry = st_union(geometry), .groups = "drop")

# State label positions (centroids)
state_labels <- state_borders %>%
  mutate(centroid = st_centroid(geometry)) %>%
  mutate(
    x = st_coordinates(centroid)[, 1],
    y = st_coordinates(centroid)[, 2]
  )

ggplot(d_map) +
  geom_sf(aes(fill = coalition_label), color = "white", linewidth = 0.05) +
  scale_fill_manual(
    name = "Governing Coalition",
    values = coalition_colors,
    drop = FALSE
  ) +
  new_scale_fill() +
  geom_sf(
    data = state_borders,
    aes(fill = NOM_ENT),
    color = "white",
    linewidth = 0.8,
    alpha = 0.15
  ) +
  scale_fill_manual(
    name = "State",
    values = c(
      "México" = "black",
      "Morelos" = "white",
      "Puebla" = "white"
    ),
    guide = "none"
  ) +
  geom_text(
    data = state_labels,
    aes(x = x, y = y, label = NOM_ENT),
    size = 3,
    fontface = "bold",
    color = "white"
  ) +
  theme_void() +
  theme(
    legend.position = "bottom",
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5, size = 11, color = "grey40")
  ) +
  labs(
    title = "Governing Party/Coalition by Municipality (2024)",
    subtitle = "Estado de M\u00e9xico, Morelos & Puebla"
  )

ggsave("output/coalition_map.pdf", width = 12, height = 8)
ggsave("output/coalition_map.png", width = 12, height = 8, dpi = 300)
