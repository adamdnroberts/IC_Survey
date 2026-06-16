library(ggplot2)
library(dplyr)

W_seq <- seq(-2, 2, length.out = 300)

base_theme <- function() {
  list(
    scale_x_continuous(
      breaks = c(-2, 0, 2),
      labels = c("Optimistic", "Accurate", "Pessimistic"),
      name = "Prior belief about home municipality crime"
    ),
    labs(y = "Predicted effect on incumbent evaluation"),
    coord_cartesian(ylim = c(-2.4, 2.4), clip = "off"),
    theme_minimal(base_size = 13),
    theme(
      panel.grid.minor = element_blank(),
      legend.position = "none",
      plot.margin = margin(5, 80, 5, 5)
    )
  )
}

make_lines <- function(arms, slopes, intercepts = rep(0, length(arms))) {
  data.frame(
    treatment = rep(arms, each = length(W_seq)),
    W = rep(W_seq, length(arms))
  ) %>%
    left_join(
      data.frame(treatment = arms, slope = slopes, intercept = intercepts),
      by = "treatment"
    ) %>%
    mutate(
      effect = intercept + slope * W,
      treatment = factor(treatment, levels = arms)
    )
}

# ── Plot 1: Only Information Updating ─────────────────────────────────────────
# All arms slope the same — comparison context adds nothing

arms1 <- c("Plain Info")
colors1 <- c("#000000")
lines1 <- c("solid")
slopes1 <- c(0.5)
intercepts1 <- c(0)

dat1 <- make_lines(arms1, slopes1, intercepts1)

labs1 <- data.frame(
  W = 2.08,
  treatment = factor(arms1, levels = arms1),
  y = intercepts1 + slopes1 * 2,
  label = arms1
)

p1 <- ggplot(
  dat1,
  aes(x = W, y = effect, color = treatment, linetype = treatment)
) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey60") +
  geom_vline(xintercept = 0, linetype = "dotted", color = "grey60") +
  geom_line(linewidth = 1.2) +
  geom_text(
    data = labs1,
    aes(x = W, y = y, label = label, color = treatment),
    hjust = 0,
    size = 4,
    fontface = "bold",
    inherit.aes = FALSE
  ) +
  scale_color_manual(values = colors1) +
  scale_linetype_manual(values = lines1) +
  base_theme()

ggsave("latex/images/mechanism_plot_1.pdf", p1, width = 6, height = 4.5)
message("Saved: mechanism_plot_1.pdf")

# ── Plot 2: Comparison Additive Effect ────────────────────────────────────────
# Comparison arms steeper than T1 — benchmark context amplifies updating

arms2 <- c("Plain Info", "Comparison")
colors2 <- c("#000000", "#0072B2")
lines2 <- c("solid", "dashed")
slopes2 <- c(0.5, 0.5)
intercepts2 <- c(0, 0.8)

dat2 <- make_lines(arms2, slopes2, intercepts2)

labs2 <- data.frame(
  W = 2.08,
  treatment = factor(arms2, levels = arms2),
  y = intercepts2 + slopes2 * 2,
  label = arms2
)

p2 <- ggplot(
  dat2,
  aes(x = W, y = effect, color = treatment, linetype = treatment)
) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey60") +
  geom_vline(xintercept = 0, linetype = "dotted", color = "grey60") +
  geom_line(linewidth = 1.2) +
  geom_text(
    data = labs2,
    aes(x = W, y = y, label = label, color = treatment),
    hjust = 0,
    size = 4,
    fontface = "bold",
    inherit.aes = FALSE
  ) +
  scale_color_manual(values = colors2) +
  scale_linetype_manual(values = lines2) +
  base_theme()

ggsave("latex/images/mechanism_plot_2.pdf", p2, width = 6, height = 4.5)
message("Saved: mechanism_plot_2.pdf")

# ── Plot 3: Opposition Comparison ─────────────────────────────────────────────
# T3 (opposite coalition) steepest — partisan framing amplifies RG effect

arms3 <- c("Plain Info", "T2 (Non-partisan)", "T3 (Opp. coalition)")
colors3 <- c("#000000", "#0072B2", "#E69F00")
lines3 <- c("solid", "dashed", "dashed")
slopes3 <- c(0.5, 0.5, 0.5)
intercepts3 <- c(0, 0.5, 0.9)

dat3 <- make_lines(arms3, slopes3, intercepts3)

labs3 <- data.frame(
  W = 2.08,
  treatment = factor(arms3, levels = arms3),
  y = intercepts3 + slopes3 * 2,
  label = c("Plain Info", "T2", "T3")
)

p3 <- ggplot(
  dat3,
  aes(x = W, y = effect, color = treatment, linetype = treatment)
) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey60") +
  geom_vline(xintercept = 0, linetype = "dotted", color = "grey60") +
  geom_line(linewidth = 1.2) +
  geom_text(
    data = labs3,
    aes(x = W, y = y, label = label, color = treatment),
    hjust = 0,
    size = 4,
    fontface = "bold",
    inherit.aes = FALSE
  ) +
  scale_color_manual(values = colors3) +
  scale_linetype_manual(values = lines3) +
  base_theme() +
  list(theme(plot.title = element_text(face = "bold", size = 13, hjust = 0.5)))

ggsave("latex/images/mechanism_plot_3.pdf", p3, width = 6, height = 4.5)
message("Saved: mechanism_plot_3.pdf")

# ── Plot 4: Confusion ──────────────────────────────────────────────────────────
# Comparison arm flatter than Plain Info — comparison context dampens updating

arms4      <- c("Plain Info", "Comparison")
colors4    <- c("#000000", "#0072B2")
lines4     <- c("solid", "dashed")
slopes4    <- c(0.5, 0.2)
intercepts4 <- c(0, 0)

dat4 <- make_lines(arms4, slopes4, intercepts4)

labs4 <- data.frame(
  W         = 2.08,
  treatment = factor(arms4, levels = arms4),
  y         = intercepts4 + slopes4*2,
  label     = arms4
)

p4 <- ggplot(dat4, aes(x = W, y = effect, color = treatment, linetype = treatment)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey60") +
  geom_vline(xintercept = 0, linetype = "dotted", color = "grey60") +
  geom_line(linewidth = 1.2) +
  geom_text(data = labs4, aes(x = W, y = y, label = label, color = treatment),
            hjust = 0, size = 4, fontface = "bold", inherit.aes = FALSE) +
  scale_color_manual(values = colors4) +
  scale_linetype_manual(values = lines4) +
  base_theme()

ggsave("latex/images/mechanism_plot_4.pdf", p4, width = 6, height = 4.5)
message("Saved: mechanism_plot_4.pdf")
