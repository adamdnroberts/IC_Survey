# Load required libraries
library(ggplot2)
library(dplyr)

# Set seed for reproducibility
set.seed(123)

# Simulate data for municipalities
municipalities <- data.frame(
  municipality = c(
    "Your Municipality",
    "City A",
    "City B",
    "City C",
    "City D",
    "City E",
    "City F"
  ),
  homicides = c(45, 62, 45, 45, 28, 31, 38),
  stringsAsFactors = FALSE
)

# Categorize comparison
municipalities <- municipalities %>%
  mutate(
    comparison = case_when(
      municipality == "Your Municipality" ~ "Your Municipality",
      homicides > 45 ~ "Higher than yours",
      homicides == 45 ~ "Same as yours",
      homicides < 45 ~ "Lower than yours"
    ),
    comparison = factor(
      comparison,
      levels = c(
        "Your Municipality",
        "Lower than yours",
        "Same as yours",
        "Higher than yours"
      )
    )
  )

# Create the bar graph
mun_bar_graph <- ggplot(
  municipalities,
  aes(x = reorder(municipality, -homicides), y = homicides, fill = comparison)
) +
  geom_bar(stat = "identity", width = 0.7) +
  scale_fill_manual(
    values = c(
      "Your Municipality" = "#2E86AB",
      "Lower than yours" = "#A23B72",
      "Same as yours" = "#F18F01",
      "Higher than yours" = "#C73E1D"
    )
  ) +
  labs(
    #title = "Homicides in 2024: Municipal Comparison",
    #subtitle = "Your municipality compared to peer cities",
    x = "Municipality",
    y = "Number of Homicides",
    fill = "Comparison"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
    plot.title = element_text(face = "bold", size = 14),
    legend.position = "right"
  ) +
  geom_text(aes(label = homicides), vjust = -0.5, size = 3.5)

ggsave(
  "docs/mun_bar_graph.pdf",
  plot = mun_bar_graph,
  width = 6,
  height = 4
)

# Summary statistics for text
your_mun_homicides <- municipalities$homicides[
  municipalities$municipality == "Your Municipality"
]
better_than <- municipalities %>%
  filter(homicides > your_mun_homicides) %>%
  pull(municipality)
same_as <- municipalities %>%
  filter(
    homicides == your_mun_homicides,
    municipality != "Your Municipality"
  ) %>%
  pull(municipality)
worse_than <- municipalities %>%
  filter(homicides < your_mun_homicides) %>%
  pull(municipality)

# Print treatment message
cat("\n=== TREATMENT MESSAGE ===\n")
cat(sprintf(
  "In 2024, an audit found that your municipality had %d homicides.\n",
  your_mun_homicides
))
cat(sprintf(
  "This is better than %s, the same as %s, and worse than %s.\n",
  paste(better_than, collapse = ", "),
  paste(same_as, collapse = ", "),
  paste(worse_than, collapse = ", ")
))
