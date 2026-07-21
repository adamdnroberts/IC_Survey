# Distribution of the home municipality's true relative standing across the six
# treatment arms, as a randomization check. Under random assignment these should
# look the same in every arm. Uses the panel dataset, which already carries
# `actual_rank`, `home_rate`, and `Treatment_Group`.

library(dplyr)
library(ggplot2)

load("data/survey_panel_dataset.Rdata") # -> `panel`

# ── Actual rank distribution by treatment arm ─────────────────────────────────
#
# actual_rank = home municipality's robbery-rate rank among its 4 comparison
# municipalities (1 = highest rate, 5 = lowest).
p_rank <- ggplot(panel, aes(x = actual_rank, fill = Treatment_Group)) +
  geom_histogram(binwidth = 1, position = "dodge") +
  facet_wrap(~Treatment_Group) +
  labs(
    x = "Home municipality actual robbery-rate rank (1 = highest)",
    y = "Number of respondents",
    title = "Distribution of actual rank by treatment group"
  ) +
  theme_bw() +
  theme(legend.position = "none")

print(p_rank)

ggsave(
  "latex/images/actual_rank_by_treatment.pdf",
  p_rank,
  width = 7,
  height = 4.5
)

# ── Actual crime rate distribution by treatment arm ───────────────────────────
#
# home_rate = home municipality robbery rate per 100,000 residents. Right-skewed,
# so plot on a log10 x-axis.
p_crime <- ggplot(panel, aes(x = home_rate, fill = Treatment_Group)) +
  geom_histogram(bins = 30, position = "identity", alpha = 0.6) +
  facet_wrap(~Treatment_Group) +
  scale_x_log10() +
  labs(
    x = "Home municipality robbery rate (per 100,000, log scale)",
    y = "Number of respondents",
    title = "Distribution of actual crime rate by treatment group"
  ) +
  theme_bw() +
  theme(legend.position = "none")

print(p_crime)

ggsave(
  "latex/images/actual_crime_by_treatment.pdf",
  p_crime,
  width = 7,
  height = 4.5
)
