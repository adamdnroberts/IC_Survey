library(dplyr)
library(readxl)

# Remove wave-1 responses that are linked (via the cross-wave crosswalk) to a
# wave-2 response recorded less than `min_days_after` days after the wave-1
# response. A genuine panelist should have a meaningful gap between waves, so
# too-short (or negative) gaps flag bad matches; the wave-1 response is dropped.

# ── 1. Load data ──────────────────────────────────────────────────────────────

wave1 <- readRDS("data/wave1_responses_w_duplicates.rds")
wave2 <- readRDS("data/wave2_responses.rds")

wave1 <- distinct(wave1, Netquest_PID, .keep_all = TRUE)
wave2 <- distinct(wave2, Netquest_PID, .keep_all = TRUE)

# ── 2. Build combined crosswalk (all three match files) ───────────────────────

match_ids1 <- janitor::clean_names(read_excel("data/Match ID.xlsx")) %>%
  rename(pid_w1 = wave_1, pid_w2 = wave_2) %>%
  select(pid_w2, pid_w1) %>%
  mutate(across(c(pid_w2, pid_w1), as.character))

match_ids2 <- read.csv(
  "data/wave_match_ids.csv",
  stringsAsFactors = FALSE,
  fileEncoding = "UTF-8-BOM"
) %>%
  rename(pid_w1 = Wave.1, pid_w2 = Wave.2) %>%
  select(pid_w2, pid_w1) %>%
  mutate(across(c(pid_w2, pid_w1), as.character))

match_ids3 <- janitor::clean_names(read_excel("data/match IDs 29 Jun.xlsx")) %>%
  rename(pid_w1 = wave_1, pid_w2 = wave_2) %>%
  select(pid_w2, pid_w1) %>%
  mutate(across(c(pid_w2, pid_w1), as.character))

match_ids <- bind_rows(match_ids1, match_ids2, match_ids3) %>%
  distinct(pid_w2, pid_w1)

# ── 3. Flag wave-1 responses linked to an earlier wave-2 response ─────────────

w1_times <- wave1 %>%
  transmute(
    pid_w1 = as.character(Netquest_PID),
    ts_w1 = as.POSIXct(Timestamp, format = "%Y-%m-%d %H:%M:%OS", tz = "UTC")
  )

w2_times <- wave2 %>%
  transmute(
    pid_w2 = as.character(Netquest_PID),
    ts_w2 = as.POSIXct(Timestamp, format = "%Y-%m-%d %H:%M:%OS", tz = "UTC")
  )

min_days_after <- 0

# Every crosswalk pair with both timestamps, flagging links where the wave-2
# response is less than `min_days_after` days after the wave-1 response. This
# also catches negative gaps (wave-2 recorded before wave-1).
linked <- match_ids %>%
  inner_join(w1_times, by = "pid_w1") %>%
  inner_join(w2_times, by = "pid_w2") %>%
  mutate(
    days_between = as.numeric(difftime(ts_w2, ts_w1, units = "days")),
    w2_too_soon = days_between <= min_days_after
  )

# wave-1 PIDs to drop: linked to ANY wave-2 response less than 4 days later.
bad_w1_pids <- linked %>%
  filter(w2_too_soon) %>%
  distinct(pid_w1) %>%
  pull(pid_w1)

write.csv(
  data.frame(pid_w1 = bad_w1_pids),
  "data/w1_pids_after_w2.csv",
  row.names = FALSE
)

# ── 4. Drop respondents who answered wave-1 twice before their wave-2 ─────────

# After the timing filter, a wave-2 response may still be linked to more than
# one surviving wave-1 response — i.e. the respondent answered the wave-1 survey
# twice (both > min_days_after before their wave-2). Having taken wave-1 twice
# contaminates their pre-treatment priors, so we drop ALL wave-1 responses in
# each such set (the respondent then drops out of the panel entirely).
surviving <- linked %>%
  filter(!w2_too_soon, !(pid_w1 %in% bad_w1_pids))

dup_sets <- surviving %>%
  group_by(pid_w2) %>%
  filter(n() > 1) %>%
  arrange(ts_w1, .by_group = TRUE)

dup_w1_pids <- dup_sets %>%
  ungroup() %>%
  distinct(pid_w1) %>%
  pull(pid_w1)

write.csv(
  data.frame(pid_w1 = dup_w1_pids),
  "data/dup_respondents_w1_pids.csv",
  row.names = FALSE
)

n_dup_w2 <- n_distinct(dup_sets$pid_w2)

# Gap (days) between the first and second wave-1 response in each duplicate set.
dup_gaps <- dup_sets %>%
  summarise(
    gap_days = as.numeric(difftime(ts_w1[2], ts_w1[1], units = "days")),
    .groups = "drop"
  )

# ── 5. Filter ─────────────────────────────────────────────────────────────────

# `wave1_filtered` is consumed by the caller (pull_responses_wave1.R), which
# saves it as the canonical data/wave1_responses.rds.
drop_w1_pids <- union(bad_w1_pids, dup_w1_pids)

wave1_filtered <- wave1 %>%
  filter(!(as.character(Netquest_PID) %in% drop_w1_pids))

# Combined list of every tossed wave-1 PID with its drop reason. A PID dropped
# for both reasons is labelled "implausible_timing_and_duplicate".
dropped_w1_ids <- data.frame(pid_w1 = drop_w1_pids, stringsAsFactors = FALSE) %>%
  mutate(
    reason = case_when(
      pid_w1 %in% bad_w1_pids & pid_w1 %in% dup_w1_pids ~
        "implausible_timing_and_duplicate",
      pid_w1 %in% bad_w1_pids ~ "implausible_timing",
      TRUE ~ "answered_wave1_twice"
    )
  ) %>%
  arrange(reason, pid_w1)

write.csv(
  dropped_w1_ids,
  "data/dropped_wave1_ids.csv",
  row.names = FALSE
)

# ── 6. Copy/pasteable report ──────────────────────────────────────────────────

n_start <- nrow(wave1)
n_end <- nrow(wave1_filtered)
n_dropped <- length(drop_w1_pids)

# Break the timing drops into wave-2-before-wave-1 vs short positive gaps,
# classifying each dropped wave-1 by its most extreme (minimum) too-soon gap.
timing_gaps <- linked %>%
  filter(w2_too_soon) %>%
  group_by(pid_w1) %>%
  summarise(min_gap = min(days_between), .groups = "drop")
n_neg <- sum(timing_gaps$min_gap < 0)
n_short <- sum(timing_gaps$min_gap >= 0)

# Of the date-filtered drops, how many were duplicates: their linked wave-2
# response was also linked to another wave-1 response (present in the data).
w2_link_counts <- count(linked, pid_w2, name = "n_w1_per_w2")
n_timing_dup <- linked %>%
  filter(w2_too_soon) %>%
  left_join(w2_link_counts, by = "pid_w2") %>%
  filter(n_w1_per_w2 > 1) %>%
  distinct(pid_w1) %>%
  nrow()
n_timing_nondup <- length(bad_w1_pids) - n_timing_dup

# Summary + distribution of the wave-1 gap within the surviving duplicate sets.
gq <- quantile(dup_gaps$gap_days, c(0, .25, .5, .75, 1), names = FALSE)
gap_summary_line <- sprintf(
  "    min=%.2f  Q1=%.2f  median=%.2f  mean=%.2f  Q3=%.2f  max=%.2f",
  gq[1],
  gq[2],
  gq[3],
  mean(dup_gaps$gap_days),
  gq[4],
  gq[5]
)
gap_buckets <- table(cut(
  dup_gaps$gap_days,
  breaks = c(-Inf, 1 / 24, 1, 7, 14, Inf),
  labels = c("<1 hour", "1h-1day", "1-7 days", "7-14 days", ">14 days")
))
gap_bucket_lines <- paste(
  sprintf("      %-10s %d", names(gap_buckets), as.integer(gap_buckets)),
  collapse = "\n"
)

report <- paste(
  strrep("=", 64),
  "WAVE-1 RESPONSE FILTERING REPORT",
  sprintf("Generated: %s", format(Sys.time(), "%Y-%m-%d %H:%M")),
  sprintf(
    "Crosswalk: combined (Match ID.xlsx + wave_match_ids.csv + match IDs 29 Jun.xlsx)"
  ),
  strrep("=", 64),
  "",
  sprintf("Starting wave-1 responses (distinct PID): %d", n_start),
  sprintf("Dropped: %d (%.1f%%)", n_dropped, 100 * n_dropped / n_start),
  sprintf("Remaining: %d", n_end),
  "",
  sprintf(
    "REASON 1 - Implausible match timing: %d responses",
    length(bad_w1_pids)
  ),
  sprintf(
    "    - %d linked to a wave-2 recorded BEFORE wave-1 (negative gap)",
    n_neg
  ),
  sprintf(
    "    - %d linked to a wave-2 recorded 0-%d days after wave-1",
    n_short,
    min_days_after
  ),
  sprintf(
    "  Of these %d, %d were duplicates (linked wave-2 also linked to",
    length(bad_w1_pids),
    n_timing_dup
  ),
  sprintf("  another wave-1 response); %d were not.", n_timing_nondup),
  "",
  sprintf(
    "REASON 2 - Respondent answered wave-1 twice: %d responses",
    length(dup_w1_pids)
  ),
  sprintf(
    "  %d wave-2 responses were each linked to >1 wave-1 response, all",
    n_dup_w2
  ),
  sprintf(
    "  recorded > %d days before the wave-2 (so not caught by the",
    min_days_after
  ),
  "  timing filter). Taking wave-1 twice contaminates the respondent's",
  "  pre-treatment priors, so ALL wave-1 responses in each set are",
  "  dropped and the respondent is excluded from the panel.",
  "  Gap between the two linked wave-1 responses (days):",
  gap_summary_line,
  gap_bucket_lines,
  "",
  "Result: each wave-2 response now links to at most one wave-1 response.",
  strrep("=", 64),
  sep = "\n"
)

cat(report, "\n")
