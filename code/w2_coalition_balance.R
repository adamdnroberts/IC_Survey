library(dplyr)
library(tidyr)
library(ggplot2)

responses <- readRDS("data/wave2_responses.rds")
load("data/magar2024_coalitions.Rdata")

coalition_lookup <- magar2024 %>%
  mutate(
    muni_id = sprintf("%05d", inegi),
    coalition = case_when(
      grepl("morena|pvem|pt", l01) ~ "MORENA/PVEM/PT",
      grepl("pan|pri|prd", l01)    ~ "PAN/PRI/PRD",
      grepl("mc", l01)             ~ "MC",
      TRUE                          ~ NA_character_
    )
  ) %>%
  select(muni_id, coalition)

d <- responses %>%
  left_join(coalition_lookup, by = c("Found_Municipality_ID" = "muni_id"))

tg_order <- c("control", "control2", "T1", "T2", "T3", "T4")

# ── Count table ───────────────────────────────────────────────────────────────

counts <- d %>%
  filter(!is.na(coalition)) %>%
  count(Treatment_Group, coalition) %>%
  group_by(Treatment_Group) %>%
  mutate(
    total = sum(n),
    pct   = round(100 * n / total, 1)
  ) %>%
  ungroup() %>%
  mutate(Treatment_Group = factor(Treatment_Group, levels = tg_order))

cat("\n=== Home municipality coalition by treatment arm ===\n")
print(
  counts %>%
    select(Treatment_Group, coalition, n, total, pct) %>%
    arrange(Treatment_Group, coalition),
  row.names = FALSE
)

# ── Chi-square test of independence ──────────────────────────────────────────

ct <- d %>%
  filter(!is.na(coalition)) %>%
  with(table(Treatment_Group, coalition))

cat("\n=== Contingency table ===\n")
print(ct)

chi <- chisq.test(ct)
cat(sprintf(
  "\nChi-square test: X²(%d) = %.2f, p = %.3f\n",
  chi$parameter, chi$statistic, chi$p.value
))

# ── Plot ──────────────────────────────────────────────────────────────────────

coalition_colors <- c(
  "MORENA/PVEM/PT" = "#8B0000",
  "PAN/PRI/PRD"    = "#00308F",
  "MC"             = "#FF5722"
)

p <- ggplot(counts, aes(x = Treatment_Group, y = pct, fill = coalition)) +
  geom_col(position = "stack", width = 0.6) +
  scale_fill_manual(values = coalition_colors, name = "Home coalition") +
  scale_y_continuous(labels = function(x) paste0(x, "%"), limits = c(0, 100)) +
  labs(
    title = "Balance of home municipality coalition by treatment arm",
    x = NULL,
    y = "Share of respondents (%)"
  ) +
  theme_minimal(base_size = 13) +
  theme(legend.position = "bottom")

print(p)
ggsave("latex/images/coalition_balance.pdf", p, width = 7, height = 5)
cat("\nPlot saved to latex/images/coalition_balance.pdf\n")
