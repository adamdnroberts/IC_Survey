library(ggplot2)
library(dplyr)
library(tidyr)
library(quantreg)

survey_responses_wave1 <- readRDS("data/wave1_responses.rds")

coalition_colors <- c(
  "Home municipality" = "#0072B2",
  "MORENA" = "#8B0000",
  "PAN/PRI/PRD" = "#00308F",
  "MC" = "#FF5722"
)

plot_df <- survey_responses_wave1 %>%
  transmute(
    `Home municipality` = as.numeric(Home_Crime_Handling_Pre),
    MORENA = as.numeric(MORENA_Crime_Rating_Pre),
    `PAN/PRI/PRD` = as.numeric(Coalition_PAN_PRI_PRD_Crime_Rating_Pre),
    MC = as.numeric(MC_Crime_Rating_Pre)
  ) %>%
  pivot_longer(everything(), names_to = "coalition", values_to = "rating") %>%
  filter(!is.na(rating)) %>%
  mutate(
    coalition = factor(
      coalition,
      levels = c("Home municipality", "MORENA", "PAN/PRI/PRD", "MC")
    )
  )

# Quantile regression: median differences relative to Home municipality
# Bootstrap SEs (R = 500) avoid distributional assumptions
qr_fit <- rq(rating ~ coalition, data = plot_df, tau = 0.5)
qr_summary <- summary(qr_fit, se = "boot", R = 500)
print(qr_summary)

# Restore factor order for plotting (bottom to top)
plot_df <- plot_df %>%
  mutate(
    coalition = factor(
      coalition,
      levels = c("MC", "PAN/PRI/PRD", "MORENA", "Home municipality")
    )
  )

box_df <- plot_df %>%
  group_by(coalition) %>%
  summarise(
    q25 = quantile(rating, 0.25, na.rm = TRUE),
    median = median(rating, na.rm = TRUE),
    q75 = quantile(rating, 0.75, na.rm = TRUE),
    .groups = "drop"
  )

median_ci_df <- plot_df %>%
  group_by(coalition) %>%
  summarise(
    median = median(rating, na.rm = TRUE),
    iqr = IQR(rating, na.rm = TRUE),
    n = sum(!is.na(rating)),
    .groups = "drop"
  ) %>%
  mutate(
    ci_lo = median - 1.58 * iqr / sqrt(n),
    ci_hi = median + 1.58 * iqr / sqrt(n)
  )

ggplot(plot_df, aes(x = rating, y = coalition)) +
  geom_crossbar(
    data = box_df,
    aes(y = coalition, x = median, xmin = q25, xmax = q75),
    fill = "white",
    color = "grey30",
    linewidth = 0.5,
    width = 0.5,
    orientation = "y",
    inherit.aes = FALSE
  ) +
  geom_point(
    aes(color = coalition),
    size = 2,
    alpha = 0.35,
    position = position_jitter(height = 0.15, seed = 42)
  ) +
  geom_pointrange(
    data = median_ci_df,
    aes(y = coalition, x = median, xmin = ci_lo, xmax = ci_hi),
    color = "grey10",
    size = 0.5,
    linewidth = 0.7,
    orientation = "y",
    inherit.aes = FALSE
  ) +
  scale_color_manual(values = coalition_colors) +
  scale_x_continuous(
    limits = c(0, 100),
    breaks = seq(0, 100, 25),
    expand = expansion(mult = c(0, 0.02))
  ) +
  labs(
    title = "Pre-treatment crime handling ratings by coalition",
    x = "Crime handling rating (0–100)",
    y = NULL
  ) +
  theme_minimal(base_size = 13) +
  theme(
    legend.position = "none",
    axis.text.y = element_text(
      size = 14,
      color = "grey20",
      margin = margin(r = 6)
    ),
    axis.text.x = element_text(color = "grey40"),
    axis.title.x = element_text(size = 13, margin = margin(t = 10)),
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_line(color = "grey92", linewidth = 0.4),
    plot.title = element_text(face = "bold", size = 15),
    axis.line.x.bottom = element_line(color = "grey70"),
    plot.margin = margin(20, 20, 20, 20)
  )
