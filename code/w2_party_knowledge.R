library(dplyr)
library(tidyr)
library(ggplot2)

responses <- readRDS("data/wave2_responses.rds")
load("data/magar2024_coalitions.Rdata")

# Build municipality → actual coalition lookup
coalition_lookup <- magar2024 %>%
  mutate(
    muni_id = sprintf("%05d", inegi),
    actual_coalition = case_when(
      grepl("morena|pvem|pt", l01) ~ "MORENA/PVEM/PT",
      grepl("pan|pri|prd", l01) ~ "PAN/PRI/PRD",
      grepl("mc", l01) ~ "MC",
      TRUE ~ NA_character_
    )
  ) %>%
  select(muni_id, actual_coalition)

# Map belief response values to coalition labels (anything else = wrong)
belief_to_coalition <- c(
  "morena_pt_pvem" = "MORENA/PVEM/PT",
  "pan_pri_prd" = "PAN/PRI/PRD",
  "mc" = "MC"
)

# ── Reshape to long format: one row per respondent × municipality ─────────────

home_long <- responses %>%
  select(
    Respondent_ID,
    Treatment_Group,
    muni_id = Found_Municipality_ID,
    belief = Home_Governing_Party_Belief
  ) %>%
  mutate(muni_type = "home")

comp_long <- responses %>%
  select(
    Respondent_ID,
    Treatment_Group,
    muni_id_1 = Comparison_Muni_1_ID,
    belief_1 = Comp_Governing_Party_Belief_1,
    muni_id_2 = Comparison_Muni_2_ID,
    belief_2 = Comp_Governing_Party_Belief_2,
    muni_id_3 = Comparison_Muni_3_ID,
    belief_3 = Comp_Governing_Party_Belief_3,
    muni_id_4 = Comparison_Muni_4_ID,
    belief_4 = Comp_Governing_Party_Belief_4
  ) %>%
  pivot_longer(
    cols = -c(Respondent_ID, Treatment_Group),
    names_to = c(".value", "slot"),
    names_pattern = "(muni_id|belief)_(\\d)"
  ) %>%
  filter(!is.na(muni_id)) %>%
  mutate(muni_type = "comparison")

all_long <- bind_rows(home_long, comp_long) %>%
  left_join(coalition_lookup, by = "muni_id") %>%
  mutate(
    guessed_coalition = belief_to_coalition[belief],
    correct = case_when(
      is.na(belief) | belief == "" ~ NA,
      belief == "dont_know" ~ FALSE,
      is.na(actual_coalition) ~ NA,
      guessed_coalition == actual_coalition ~ TRUE,
      TRUE ~ FALSE
    )
  )

# ── Accuracy by treatment arm ─────────────────────────────────────────────────

tg_order <- c("control", "control2", "T1", "T2", "T3", "T4")

acc_overall <- all_long %>%
  filter(!is.na(correct)) %>%
  group_by(Treatment_Group) %>%
  summarise(
    n_obs = n(),
    n_correct = sum(correct),
    accuracy = mean(correct),
    se = sqrt(accuracy * (1 - accuracy) / n_obs),
    .groups = "drop"
  ) %>%
  mutate(
    Treatment_Group = factor(Treatment_Group, levels = tg_order),
    muni_type = "Overall"
  )

acc_by_type <- all_long %>%
  filter(!is.na(correct)) %>%
  group_by(Treatment_Group, muni_type) %>%
  summarise(
    n_obs = n(),
    n_correct = sum(correct),
    accuracy = mean(correct),
    se = sqrt(accuracy * (1 - accuracy) / n_obs),
    .groups = "drop"
  ) %>%
  mutate(Treatment_Group = factor(Treatment_Group, levels = tg_order))

cat("\n=== Overall accuracy by treatment arm ===\n")
print(
  acc_overall %>%
    select(Treatment_Group, n_obs, n_correct, accuracy, se) %>%
    mutate(across(c(accuracy, se), \(x) round(x, 3))),
  row.names = FALSE
)

cat("\n=== Accuracy by treatment arm and municipality type ===\n")
print(
  acc_by_type %>%
    select(Treatment_Group, muni_type, n_obs, n_correct, accuracy, se) %>%
    mutate(across(c(accuracy, se), \(x) round(x, 3))) %>%
    arrange(muni_type, Treatment_Group),
  row.names = FALSE
)

# ── Plot ──────────────────────────────────────────────────────────────────────

plot_df <- acc_by_type %>%
  mutate(muni_type = factor(muni_type, levels = c("home", "comparison")))

p <- ggplot(
  plot_df,
  aes(x = Treatment_Group, y = accuracy, fill = muni_type)
) +
  geom_col(position = position_dodge(width = 0.7), width = 0.6) +
  geom_hline(yintercept = 0.33, linetype = "dashed", color = "gray40") +
  geom_errorbar(
    aes(ymin = accuracy - 1.96 * se, ymax = accuracy + 1.96 * se),
    position = position_dodge(width = 0.7),
    width = 0.2
  ) +
  scale_y_continuous(labels = scales::percent_format(), limits = c(0, 1)) +
  scale_fill_manual(
    values = c(home = "#0072B2", comparison = "#E69F00", Overall = "#999999"),
    labels = c(
      home = "Home municipality",
      comparison = "Comparison municipalities",
      Overall = "Overall"
    ),
    name = NULL
  ) +
  labs(
    title = "Accuracy of governing coalition responses",
    x = NULL,
    y = "Proportion correct"
  ) +
  theme_minimal(base_size = 13) +
  theme(legend.position = "bottom")

print(p)

ggsave("latex/images/party_knowledge_accuracy.pdf", p, width = 8, height = 5)
