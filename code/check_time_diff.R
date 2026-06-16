library(dplyr)
library(ggplot2)
library(readxl)
library(estimatr)

# ── 1. Load data ──────────────────────────────────────────────────────────────

wave1 <- readRDS("data/wave1_responses.rds")
wave2 <- readRDS("data/wave2_responses.rds")

# Drop within-wave duplicate respondents (same Netquest_PID submitted more than
# once) so the cross-wave join is strictly one-to-one. Keeps the first row.
wave1 <- distinct(wave1, Netquest_PID, .keep_all = TRUE)
wave2 <- distinct(wave2, Netquest_PID, .keep_all = TRUE)

match_ids <- read_excel("data/Match ID.xlsx")
match_ids <- janitor::clean_names(match_ids)
# Columns are now snake_case: wave_1, wave_2, status_wave_2

match_ids2 <- read.csv(
  "data/wave_match_ids.csv",
  stringsAsFactors = FALSE,
  fileEncoding = "UTF-8-BOM"
)

# ── 2. Cross-wave PID merge (same logic as belief_update_analysis.R) ──────────

match_ids <- match_ids %>%
  rename(
    pid_w1 = wave_1,
    pid_w2 = wave_2,
    status_w2 = status_wave_2
  ) %>%
  select(pid_w2, pid_w1) %>%
  mutate(across(c(pid_w2, pid_w1), as.character))

match_ids2 <- match_ids2 %>%
  rename(
    pid_w1 = Wave.1,
    pid_w2 = Wave.2
  ) %>%
  select(pid_w2, pid_w1) %>%
  mutate(across(c(pid_w2, pid_w1), as.character))

# Append the second match table, drop exact duplicate pairs, then keep only
# clean one-to-one matches before merging the two waves.
match_ids_new <- bind_rows(match_ids, match_ids2) %>%
  distinct(pid_w2, pid_w1) %>%
  add_count(pid_w2, name = "n_w2") %>%
  add_count(pid_w1, name = "n_w1")

test <- wave2 %>%
  full_join(match_ids_new, by = c("Netquest_PID" = "pid_w2")) %>%
  full_join(
    wave1,
    by = c("pid_w1" = "Netquest_PID"),
    suffix = c("_w2", "_w1")
  ) %>%
  # select(-pid_w1) %>%
  # filter(
  #   Found_Municipality_ID_w2 == Found_Municipality_ID_w1 &
  #     NQ_Region_w2 == NQ_Region_w1 &
  #     NQ_SEL_w2 == NQ_SEL_w1 &
  #     (as.numeric(NQ_Age_w2) == as.numeric(NQ_Age_w1) + 1 |
  #       NQ_Age_w2 == NQ_Age_w1)
  # ) %>%
  # # Found_Municipality_ID is in both waves, so it was suffixed; rebuild the
  # # unified column the downstream code expects.
  mutate(Found_Municipality_ID = Found_Municipality_ID_w2)

# ── 3. Time between waves ─────────────────────────────────────────────────────

# Timestamps are character strings like "2026-04-10 14:57:42.651886".
test <- test %>%
  mutate(
    ts_w1 = as.POSIXct(Timestamp_w1, format = "%Y-%m-%d %H:%M:%OS", tz = "UTC"),
    ts_w2 = as.POSIXct(Timestamp_w2, format = "%Y-%m-%d %H:%M:%OS", tz = "UTC"),
    days_between = as.numeric(difftime(ts_w2, ts_w1, units = "days"))
  )

cat(sprintf(
  "Days between waves (N = %d):\n  median = %.1f, mean = %.1f, range = %.1f-%.1f\n",
  sum(!is.na(test$days_between)),
  median(test$days_between, na.rm = TRUE),
  mean(test$days_between, na.rm = TRUE),
  min(test$days_between, na.rm = TRUE),
  max(test$days_between, na.rm = TRUE)
))

p_time_diff <- ggplot(test, aes(x = days_between)) +
  geom_histogram(binwidth = 1, fill = "#0072B2", color = "white") +
  geom_vline(
    aes(xintercept = median(days_between, na.rm = TRUE)),
    linetype = "dashed",
    color = "#E69F00"
  ) +
  labs(
    x = "Days between wave 1 and wave 2 response",
    y = "Number of respondents",
    title = "Time elapsed between waves"
  ) +
  theme_minimal()

print(p_time_diff)

ggsave(
  "latex/images/time_between_waves.pdf",
  p_time_diff,
  width = 7,
  height = 4.5
)

# ── 4. Merge days_between onto the multiple-w2-per-distinct-w1 pair list ───────
#
# multi_pairs <- readxl::read_excel(
#   "C:/Users/adamd/Documents/multiple_w2_for_distinct_w1.xlsx"
# )
#
# # Days between waves for every n_w2 > 1 pair (pid_w2 = wave-2 Netquest_PID).
# # demo_mismatch flags NQ-demographic inconsistency across waves (age allowed to
# # rise by 1 year). Found municipality is intentionally NOT part of this check.
# pair_days <- test %>%
#   filter(n_w2 > 1) %>%
#   transmute(
#     pid_w2 = Netquest_PID,
#     pid_w1 = pid_w1,
#     days_between,
#     demo_mismatch = !(NQ_Region_w2 == NQ_Region_w1 &
#       NQ_SEL_w2 == NQ_SEL_w1 &
#       NQ_Sex_w2 == NQ_Sex_w1 &
#       (as.numeric(NQ_Age_w2) == as.numeric(NQ_Age_w1) |
#         as.numeric(NQ_Age_w2) == as.numeric(NQ_Age_w1) + 1))
#   )
#
# multi_pairs_days <- multi_pairs %>%
#   left_join(pair_days, by = c("pid_w2", "pid_w1"))
#
# writexl::write_xlsx(
#   multi_pairs_days,
#   "C:/Users/adamd/Documents/multiple_w2_for_distinct_w1_with_days.xlsx"
# )
