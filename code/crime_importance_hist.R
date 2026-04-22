library(dplyr)
library(ggplot2)

d <- readRDS("data/wave1_responses.rds")

crime_imp <- d %>%
  filter(!is.na(Importance_Crime)) %>%
  count(Importance_Crime) %>%
  mutate(pct = n / sum(n))

n_total <- sum(crime_imp$n)

crime_importance <- ggplot(
  crime_imp,
  aes(x = factor(Importance_Crime), y = n)
) +
  geom_col(fill = "#0072B2", width = 0.6) +
  # geom_text(
  #   aes(label = sprintf("%d\n(%.0f%%)", n, pct * 100)),
  #   vjust = -0.4,
  #   size = 3.5
  # ) +
  scale_x_discrete(
    labels = c("1\n(Most important)", "2", "3", "4", "5\n(Least important)")
  ) +
  labs(
    title = "Distribution of Crime Importance",
    #subtitle = "Rank assigned to 'Seguridad / Delincuencia' among five issues",
    caption = sprintf("N = %d", n_total),
    x = "Rank",
    y = "Number of respondents"
  ) +
  theme_minimal(base_size = 13) +
  theme(panel.grid.major.x = element_blank())

ggsave(
  "latex/images/crime_importance_hist.pdf",
  plot = crime_importance,
  width = 4,
  height = 4
)
