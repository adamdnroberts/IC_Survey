# Check overlap between wave 1 benchmark selections and wave 2 treatment comparisons
# Input:  data/survey_panel_dataset.Rdata (merged wave 1 + wave 2)
# Output: console summary

# VDD Claims:
# 1. Panel has both Benchmark_Selected_Municipalities (W1) and Comparison_Muni_*_ID (W2)
# 2. Only comparison treatments (T2, T3, T4, control2) have non-NA comparison muni IDs
# 3. Overlap count per respondent is between 0 and min(n_selected, n_shown)
# 4. Each respondent appears exactly once in the output

library(dplyr)

load("data/derived/survey_panel_dataset.Rdata")  # loads 'panel'

panel_full <- panel

panel_with_failures <- filter(panel_full, muni_changed == 0)
panel <- filter(
  panel_with_failures,
  Attention_Check == "somewhat_agree" & Treatment_Group != "control2"
)

# Claim 1: verify columns exist
stopifnot(

  "Benchmark_Selected_Municipalities" %in% names(panel),
  "Comparison_Muni_1_ID" %in% names(panel),
  "Treatment_Group" %in% names(panel)
)

# Restrict to comparison treatments
comp_panel <- panel %>%
  filter(Treatment_Group %in% c("T2", "T3", "T4", "control2"))

# Claim 2: comparison treatments should have at least some comparison muni IDs
stopifnot(sum(!is.na(comp_panel$Comparison_Muni_1_ID)) > 0)

overlap_df <- comp_panel %>%
  rowwise() %>%
  mutate(
    w1_selected = list(trimws(strsplit(Benchmark_Selected_Municipalities, ";")[[1]])),
    w2_shown = list(na.omit(c(Comparison_Muni_1_ID, Comparison_Muni_2_ID,
                               Comparison_Muni_3_ID, Comparison_Muni_4_ID))),
    n_w1_selected = length(w1_selected),
    n_w2_shown = length(w2_shown),
    overlap = list(intersect(w1_selected, w2_shown)),
    n_overlap = length(overlap)
  ) %>%
  ungroup()

# Claim 3: overlap bounded by min of selected and shown
stopifnot(all(overlap_df$n_overlap <= pmin(overlap_df$n_w1_selected, overlap_df$n_w2_shown)))

# Claim 4: one row per respondent (panel should already be deduplicated)
stopifnot(n_distinct(overlap_df$Netquest_PID) == nrow(overlap_df))

cat("=== Overlap between W1 benchmark selections and W2 treatment comparisons ===\n\n")
cat(sprintf("Respondents in comparison treatments: %d\n\n", nrow(overlap_df)))

cat("Distribution of overlap count:\n")
print(table(overlap_df$n_overlap))

cat(sprintf("\nMean overlap: %.2f municipalities\n", mean(overlap_df$n_overlap)))
cat(sprintf("Respondents with at least 1 overlap: %d (%.1f%%)\n",
            sum(overlap_df$n_overlap > 0),
            100 * mean(overlap_df$n_overlap > 0)))

cat("\nBy treatment group:\n")
overlap_df %>%
  group_by(Treatment_Group) %>%
  summarise(
    n = n(),
    mean_overlap = mean(n_overlap),
    pct_any_overlap = 100 * mean(n_overlap > 0),
    .groups = "drop"
  ) %>%
  print()
