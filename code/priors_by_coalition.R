library(ggplot2)
library(dplyr)
library(tidyr)
library(ggridges)
library(emmeans)
library(fixest)

survey_responses_wave1 <- readRDS("data/wave1_responses.rds")

coalition_colors <- c(
  "Home municipality" = "#0072B2",
  "MORENA" = "#8B0000",
  "PAN/PRI/PRD" = "#00308F",
  "MC" = "#FF5722"
)

plot_df <- survey_responses_wave1 %>%
  transmute(
    respondent_id = Respondent_ID,
    `Home municipality` = as.numeric(Home_Crime_Handling_Pre),
    MORENA = as.numeric(MORENA_Crime_Rating_Pre),
    `PAN/PRI/PRD` = as.numeric(Coalition_PAN_PRI_PRD_Crime_Rating_Pre),
    MC = as.numeric(MC_Crime_Rating_Pre)
  ) %>%
  pivot_longer(-respondent_id, names_to = "coalition", values_to = "rating") %>%
  filter(!is.na(rating)) %>%
  mutate(
    coalition = factor(
      coalition,
      levels = c("Home municipality", "MORENA", "PAN/PRI/PRD", "MC")
    )
  )

m_clustered <- feols(
  rating ~ coalition,
  cluster = ~respondent_id,
  data = plot_df
)
# m_fe <- feols(rating ~ coalition | respondent_id, data = plot_df)
# etable(m_clustered, m_fe)

# library(marginaleffects)
#
# avg_predictions(m_clustered, variables = "coalition") |>
#   as.data.frame() |>
#   ggplot(aes(x = reorder(coalition, estimate), y = estimate)) +
#   geom_pointrange(aes(ymin = conf.low, ymax = conf.high)) +
#   coord_flip() +
#   labs(x = NULL, y = "Mean rating (95% CI)") +
#   theme_bw()

emm <- emmeans(m_clustered, ~coalition)
plot_means <- as.data.frame(emm)

mean_crime_priors <- ggplot(
  plot_means,
  aes(x = reorder(coalition, emmean), y = emmean)
) +
  geom_pointrange(aes(ymin = lower.CL, ymax = upper.CL)) +
  coord_flip() +
  labs(
    x = NULL,
    y = "Mean crime-handling rating, 0–100 (95% CI)",
    caption = paste0("n = ", length(unique(plot_df$respondent_id)))
    #title = "Predicted mean rating by governing coalition"
  ) +
  theme_bw()

print(mean_crime_priors)

ggsave(
  "latex/images/mean_crime_priors.pdf",
  plot = mean_crime_priors,
  width = 7,
  height = 4.5
)

# Restore factor order for plotting (bottom to top)
plot_df <- plot_df %>%
  mutate(
    coalition = factor(
      coalition,
      levels = c("MC", "PAN/PRI/PRD", "MORENA", "Home municipality")
    )
  )

mean_df <- plot_df %>%
  group_by(coalition) %>%
  summarise(
    mean = mean(rating, na.rm = TRUE),
    n = sum(!is.na(rating)),
    .groups = "drop"
  )

priors_ridge_plot <- ggplot(
  plot_df,
  aes(x = rating, y = coalition, fill = coalition)
) +
  geom_density_ridges(
    scale = 0.9,
    alpha = 0.7,
    color = "grey30",
    linewidth = 0.4,
    quantile_lines = TRUE,
    quantiles = c(0.25, 0.5, 0.75)
  ) +
  # geom_point(
  #   data = mean_df,
  #   aes(x = mean, y = coalition),
  #   shape = 23,
  #   size = 3,
  #   fill = "white",
  #   color = "grey10",
  #   inherit.aes = FALSE
  # ) +
  scale_fill_manual(values = coalition_colors) +
  scale_x_continuous(
    limits = c(0, 100),
    breaks = seq(0, 100, 25),
    expand = expansion(mult = c(0, 0.02))
  ) +
  labs(
    #title = "Pre-treatment crime handling ratings by coalition",
    x = "Crime handling rating (0–100)",
    y = NULL,
    caption = paste0("n = ", mean_df$n[1])
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
    plot.caption = element_text(color = "grey50", size = 11),
    plot.margin = margin(20, 20, 20, 20)
  )

print(priors_ridge_plot)

ggsave(
  "latex/images/priors_ridge_plot.pdf",
  plot = priors_ridge_plot,
  width = 7,
  height = 4.5
)

# Manual skewness calculation
n <- length(plot_df$coalition[plot_df$coalition == "MORENA"])
mean_data <- mean(plot_df$rating[plot_df$coalition == "MORENA"])
sd_data <- sd(plot_df$rating[plot_df$coalition == "MORENA"])

skewness_value <- (n *
  sum((plot_df$rating[plot_df$coalition == "MORENA"] - mean_data)^3)) /
  ((n - 1) * (n - 2) * sd_data^3)

print(skewness_value)

# ── Q-Q plots vs. home municipality ───────────────────────────────────────────

probs <- seq(0, 1, length.out = 500)

ref_quantiles <- quantile(
  plot_df$rating[plot_df$coalition == "Home municipality"],
  probs = probs
)

qq_pairwise <- plot_df %>%
  filter(coalition != "Home municipality") %>%
  group_by(coalition) %>%
  summarise(
    q_other = list(quantile(rating, probs = probs)),
    .groups = "drop"
  ) %>%
  mutate(q_home = list(ref_quantiles), prob = list(probs)) %>%
  unnest(c(q_other, q_home, prob))

label_probs <- c("MC" = 0.25, "PAN/PRI/PRD" = 0.50, "MORENA" = 0.8)

qq_labels <- qq_pairwise %>%
  group_by(coalition) %>%
  slice_min(
    abs(prob - label_probs[as.character(coalition[1])]),
    n = 1,
    with_ties = FALSE
  )

qq_plot <- ggplot(
  qq_pairwise,
  aes(x = q_home, y = q_other, color = coalition)
) +
  geom_abline(
    slope = 1,
    intercept = 0,
    color = "grey60",
    linewidth = 0.7,
    linetype = "dashed"
  ) +
  geom_line(linewidth = 0.9) +
  geom_text(
    data = qq_labels,
    aes(label = as.character(coalition)),
    vjust = -3.5,
    hjust = 0.75,
    size = 3.5,
    fontface = "bold"
  ) +
  scale_color_manual(values = coalition_colors) +
  scale_x_continuous(expand = expansion(mult = c(0.02, 0.02))) +
  labs(
    x = "Home municipality quantiles",
    y = "Coalition quantiles", #,
    # = "Q-Q plots of crime handling ratings vs. home municipality",
    caption = paste0("n = ", mean_df$n[1])
  ) +
  theme_bw(base_size = 12) +
  theme(legend.position = "none")

print(qq_plot)

ggsave(
  "latex/images/priors_qq_plot.pdf",
  plot = qq_plot,
  width = 4.5,
  height = 4.5
)
