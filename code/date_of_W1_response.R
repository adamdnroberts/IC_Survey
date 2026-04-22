library(dplyr)
library(ggplot2)
library(lubridate)

responses <- readRDS("data/wave1_responses.rds")

responses <- responses %>%
  mutate(date = as.Date(as.POSIXct(Timestamp)))

daily_counts <- responses %>%
  count(date)

ggplot(daily_counts, aes(x = date, y = n)) +
  geom_col(fill = "#0072B2") +
  geom_text(aes(label = n), vjust = -0.4, size = 3.5) +
  scale_x_date(date_breaks = "1 day", date_labels = "%b %d") +
  labs(
    title = "Wave 1 survey responses by day",
    x = NULL,
    y = "Number of responses"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
