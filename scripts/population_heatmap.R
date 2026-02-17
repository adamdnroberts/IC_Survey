library(sf)
library(ggplot2)
library(dplyr)

# Load municipality geometries with population data
d_geo <- st_read("data/00mun_simplified.geojson", quiet = TRUE)

# Log-transform population for better color contrast
d_geo <- d_geo %>%
  mutate(pop_log = log10(ifelse(POB_TOTAL == 0, 1, POB_TOTAL)))

# ggplot(d_geo) +
#   geom_sf(aes(fill = pop_log), color = NA) +
#   scale_fill_viridis_c(
#     name = "Population",
#     breaks = c(3, 4, 5, 6, 7),
#     labels = c("1K", "10K", "100K", "1M", "10M"),
#     option = "inferno",
#     na.value = "grey80"
#   ) +
#   theme_void() +
#   theme(
#     legend.position = "bottom",
#     legend.key.width = unit(2, "cm"),
#     plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
#     plot.subtitle = element_text(hjust = 0.5, size = 11, color = "grey40")
#   ) +
#   labs(
#     title = "Population by Municipality in Mexico",
#     subtitle = "Source: INEGI"
#   )
#
# ggsave("output/population_heatmap.pdf", width = 12, height = 8)
# ggsave("output/population_heatmap.png", width = 12, height = 8, dpi = 300)

# Binned population map
d_geo <- d_geo %>%
  mutate(
    pop_bin = cut(
      POB_TOTAL,
      breaks = c(0, 10e3, 50e3, 100e3, 500e3, 1e6, 2e6, Inf),
      labels = c(
        "<10K",
        "10K-50K",
        "50K-100K",
        "100K-500K",
        "500K-1M",
        "1M-2M",
        ">2M"
      ),
      include.lowest = TRUE
    )
  )

# ggplot(d_geo) +
#   geom_sf(aes(fill = pop_bin), color = NA) +
#   scale_fill_viridis_d(
#     name = "Population",
#     option = "inferno",
#     na.value = "grey80"
#   ) +
#   theme_void() +
#   theme(
#     legend.position = "bottom",
#     plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
#     plot.subtitle = element_text(hjust = 0.5, size = 11, color = "grey40")
#   ) +
#   labs(
#     title = "Population by Municipality in Mexico (Binned)",
#     subtitle = "Source: INEGI"
#   )
#
# ggsave("output/population_heatmap_binned.pdf", width = 12, height = 8)
# ggsave("output/population_heatmap_binned.png", width = 12, height = 8, dpi = 300)

# Map of Estado de México, Morelos, and Puebla
border_states <- c("15", "17", "21")

d_border <- d_geo %>%
  filter(CVE_ENT %in% border_states) %>%
  mutate(
    pop_bin = cut(
      POB_TOTAL,
      breaks = c(0, 10e3, 50e3, 100e3, 500e3, 1e6, 2e6, Inf),
      labels = c(
        "<10K",
        "10K-50K",
        "50K-100K",
        "100K-500K",
        "500K-1M",
        "1M-2M",
        ">2M"
      ),
      include.lowest = TRUE
    )
  )

ggplot(d_border) +
  geom_sf(aes(fill = pop_bin), color = "white", linewidth = 0.05) +
  scale_fill_viridis_d(
    name = "Population",
    option = "inferno",
    na.value = "grey80"
  ) +
  theme_void() +
  theme(
    legend.position = "bottom",
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5, size = 11, color = "grey40")
  ) +
  labs(
    title = "Population by Municipality: Estado de M\u00e9xico, Morelos & Puebla",
    subtitle = "Source: INEGI"
  )

ggsave("output/population_heatmap_edomex_border.pdf", width = 12, height = 8)
ggsave(
  "output/population_heatmap_edomex_border.png",
  width = 12,
  height = 8,
  dpi = 300
)

# Population summary statistics by state (all states)
# Use AGEEML for complete population data (geojson has nulls for some states)
ageeml <- read.csv(
  "references/AGEEML_202512121054579_utf.csv",
  colClasses = c(CVEGEO = "character")
) %>%
  select(CVEGEO, POB_AGEEML = POB_TOTAL) %>%
  mutate(POB_AGEEML = as.numeric(POB_AGEEML))

pop_summary <- d_geo %>%
  st_drop_geometry() %>%
  left_join(ageeml, by = "CVEGEO") %>%
  mutate(pop = coalesce(POB_TOTAL, POB_AGEEML)) %>%
  filter(!is.na(pop)) %>%
  group_by(NOM_ENT) %>%
  summarise(
    n_munis = n(),
    mean_pop = round(mean(pop)),
    median_pop = round(median(pop)),
    min_pop = min(pop),
    max_pop = max(pop),
    .groups = "drop"
  )

cat("\nPopulation summary by state:\n")
print(as.data.frame(pop_summary))

# Box and whiskers plot of municipal population by state
pop_plot_data <- d_geo %>%
  st_drop_geometry() %>%
  left_join(ageeml, by = "CVEGEO") %>%
  mutate(pop = coalesce(POB_TOTAL, POB_AGEEML)) %>%
  filter(!is.na(pop))

# Order states by median population
state_order <- pop_plot_data %>%
  group_by(NOM_ENT) %>%
  summarise(med = median(pop)) %>%
  arrange(med) %>%
  pull(NOM_ENT)

pop_plot_data$NOM_ENT <- factor(pop_plot_data$NOM_ENT, levels = state_order)

ggplot(pop_plot_data, aes(x = NOM_ENT, y = pop)) +
  geom_boxplot(outlier.size = 0.8, outlier.alpha = 0.5, fill = "#0072B2", alpha = 0.3) +
  scale_y_log10(labels = scales::comma) +
  coord_flip() +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
    axis.text.y = element_text(size = 8)
  ) +
  labs(
    title = "Municipal Population by State",
    x = NULL,
    y = "Population (log scale)"
  )

ggsave("output/population_boxplot.pdf", width = 10, height = 10)
ggsave("output/population_boxplot.png", width = 10, height = 10, dpi = 300)
