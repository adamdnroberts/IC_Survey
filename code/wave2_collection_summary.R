suppressPackageStartupMessages({
  library(dplyr)
  library(readxl)
})

# Counts are computed from the saved wave 2 dataset (data/wave2_responses.rds),
# not pulled from the S3 quota_counts JSON.
# Run code/census_quotas_wave2.R first to generate data/region_quotas_wave2.rds

# ── Targets ───────────────────────────────────────────────────────────────────
QUOTA_SEX <- c("1" = 1067L, "2" = 1113L)

QUOTA_AGE <- c(
  "18-24" = 377L,
  "25-34" = 491L,
  "35-44" = 441L,
  "45-54" = 371L,
  "55-64" = 255L,
  "65+" = 245L
)

QUOTA_SEL <- c(
  "1" = 196L, # AB
  "2" = 325L, # C+
  "3" = 416L, # C
  "4" = 443L, # C-
  "5" = 351L, # D+
  "6" = 449L # D/E merged
)

QUOTA_REGION <- readRDS("data/region_quotas_wave2.rds")

SEL_LABELS <- c(
  "1" = "AB",
  "2" = "C+",
  "3" = "C",
  "4" = "C-",
  "5" = "D+",
  "6" = "D/E"
)

# ── Counts (from saved wave 2 dataset) ──────────────────────────────────────────
wave2 <- readRDS("data/wave2_responses.rds") %>%
  distinct(Netquest_PID, .keep_all = TRUE)

age_bracket <- cut(
  as.integer(wave2$NQ_Age),
  breaks = c(18, 25, 35, 45, 55, 65, Inf),
  labels = names(QUOTA_AGE),
  right = FALSE
)

# SEL category 7 is merged into 6 (D/E)
sel <- as.integer(wave2$NQ_SEL)
sel[sel == 7L] <- 6L

counts <- list(
  sex = table(as.character(wave2$NQ_Sex)),
  age = table(age_bracket),
  sel = table(as.character(sel)),
  region = table(as.character(wave2$NQ_Region))
)

# ── Helper ────────────────────────────────────────────────────────────────────
print_quota <- function(title, counts_vec, targets, extra_labels = NULL) {
  cat("\n", title, "\n", rep("-", nchar(title)), "\n", sep = "")
  cells <- names(targets)
  n <- as.integer(counts_vec[cells])
  n[is.na(n)] <- 0L
  tgt <- as.integer(targets[cells])
  pct <- round(100 * n / tgt, 1)
  met <- ifelse(n >= tgt, "DONE", "")
  label_col <- if (!is.null(extra_labels)) {
    paste0(cells, " (", extra_labels[cells], ")")
  } else {
    cells
  }
  df <- data.frame(
    Cell = label_col,
    Count = n,
    Target = tgt,
    Pct = paste0(pct, "%"),
    Status = met,
    stringsAsFactors = FALSE
  )
  print(df, row.names = FALSE)
  invisible(df)
}

# ── Summary ───────────────────────────────────────────────────────────────────
total_n <- sum(as.integer(unlist(counts$sex)), na.rm = TRUE)
cat(sprintf(
  "\nWave 2 Quota Summary  (total completes tracked: %d / %d = %.1f%%)\n",
  total_n,
  2180L,
  100 * total_n / 2180
))
cat(rep("=", 60), "\n", sep = "")

print_quota("Sex (1=Male, 2=Female)", counts$sex, QUOTA_SEX)
print_quota("Age brackets", counts$age, QUOTA_AGE)
print_quota("SEL", counts$sel, QUOTA_SEL, SEL_LABELS)
print_quota("Region (state NQ code)", counts$region, QUOTA_REGION)

all_counts <- c(
  as.integer(counts$sex[names(QUOTA_SEX)]),
  as.integer(counts$age[names(QUOTA_AGE)]),
  as.integer(counts$sel[names(QUOTA_SEL)]),
  as.integer(counts$region[names(QUOTA_REGION)])
)
all_targets <- c(QUOTA_SEX, QUOTA_AGE, QUOTA_SEL, QUOTA_REGION)
all_counts[is.na(all_counts)] <- 0L
n_met <- sum(all_counts >= all_targets)
cat(sprintf(
  "\nCells met: %d / %d (%.0f%%)\n",
  n_met,
  length(all_targets),
  100 * n_met / length(all_targets)
))

# ── Wave 1 panel match ──────────────────────────────────────────────────────
# How many wave 2 responses can be linked back to a wave 1 respondent?
# Crosswalk logic mirrors code/create_panel_dataset.R.
wave1 <- readRDS("data/wave1_responses.rds") %>%
  distinct(Netquest_PID, .keep_all = TRUE)

match_ids <- read_excel("data/Match ID.xlsx") %>%
  janitor::clean_names() %>%
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

match_ids <- bind_rows(match_ids, match_ids2) %>%
  distinct(pid_w2, pid_w1) %>%
  add_count(pid_w2, name = "n_w2") %>%
  filter(n_w2 == 1) %>%
  select(pid_w2, pid_w1)

w2_pids <- as.character(wave2$Netquest_PID)
w1_pids <- as.character(wave1$Netquest_PID)

# pid_w2 present in the crosswalk
in_xwalk <- w2_pids %in% match_ids$pid_w2
# crosswalk maps to a pid_w1 that actually has a wave 1 response
xwalk_to_real_w1 <- match_ids$pid_w2[match_ids$pid_w1 %in% w1_pids]
linked_to_w1 <- w2_pids %in% xwalk_to_real_w1

cat("\nWave 1 panel match\n", rep("-", 18), "\n", sep = "")
cat(sprintf("Wave 2 responses (unique PID):        %d\n", length(w2_pids)))
cat(sprintf(
  "  in wave1<->wave2 crosswalk:         %d (%.1f%%)\n",
  sum(in_xwalk),
  100 * mean(in_xwalk)
))
cat(sprintf(
  "  linked to an actual wave 1 response: %d (%.1f%%)\n",
  sum(linked_to_w1),
  100 * mean(linked_to_w1)
))

# Days between waves for linked responses (mirrors create_panel_dataset.R)
min_days_between <- 5

linked <- wave2 %>%
  transmute(pid_w2 = as.character(Netquest_PID), ts_w2 = Timestamp) %>%
  inner_join(match_ids, by = "pid_w2") %>%
  inner_join(
    transmute(wave1, pid_w1 = as.character(Netquest_PID), ts_w1 = Timestamp),
    by = "pid_w1"
  ) %>%
  mutate(
    ts_w1 = as.POSIXct(ts_w1, format = "%Y-%m-%d %H:%M:%OS", tz = "UTC"),
    ts_w2 = as.POSIXct(ts_w2, format = "%Y-%m-%d %H:%M:%OS", tz = "UTC"),
    days_between = as.numeric(difftime(ts_w2, ts_w1, units = "days"))
  )

n_days_ok <- sum(linked$days_between >= min_days_between, na.rm = TRUE)
cat(sprintf(
  "  with days_between >= %d:             %d (%.1f%% of linked)\n",
  min_days_between,
  n_days_ok,
  100 * n_days_ok / nrow(linked)
))

# ── Analysis-panel funnel ────────────────────────────────────────────────────
# Why the linked count exceeds the N used in vote_update_analysis.R. Mirrors the
# filters in create_panel_dataset.R (home-muni match, days_between) plus the
# attention-check filter applied in vote_update_analysis.R.
min_days_panel <- 6 # create_panel_dataset.R threshold

# earliest wave 2 response per wave 1 PID (dedups multiple w2 -> same w1)
mi <- match_ids %>%
  left_join(
    transmute(wave2, pid_w2 = as.character(Netquest_PID), w2_time = as.POSIXct(Timestamp)),
    by = "pid_w2"
  ) %>%
  group_by(pid_w1) %>%
  slice_min(w2_time, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  select(pid_w2, pid_w1)

fpanel <- wave2 %>%
  inner_join(mi, by = c("Netquest_PID" = "pid_w2")) %>%
  inner_join(wave1, by = c("pid_w1" = "Netquest_PID"), suffix = c("_w2", "_w1"))
n_linked <- nrow(fpanel)

fpanel <- filter(fpanel, Found_Municipality_ID_w2 == Found_Municipality_ID_w1)
n_muni <- nrow(fpanel)

fpanel <- fpanel %>%
  mutate(days_between = as.numeric(difftime(
    as.POSIXct(Timestamp_w2, format = "%Y-%m-%d %H:%M:%OS", tz = "UTC"),
    as.POSIXct(Timestamp_w1, format = "%Y-%m-%d %H:%M:%OS", tz = "UTC"),
    units = "days"
  ))) %>%
  filter(!is.na(days_between) & days_between >= min_days_panel)
n_days <- nrow(fpanel)

n_attn <- sum(fpanel$Attention_Check == "somewhat_agree", na.rm = TRUE)

cat("\nAnalysis-panel funnel\n", rep("-", 21), "\n", sep = "")
cat(sprintf("Linked to a wave 1 response:           %d\n", n_linked))
cat(sprintf("  home muni matches across waves:      %d (-%d)\n",
            n_muni, n_linked - n_muni))
cat(sprintf("  days_between >= %d:                   %d (-%d)\n",
            min_days_panel, n_days, n_muni - n_days))
cat(sprintf("  passes attention check (vote model): %d (-%d)\n",
            n_attn, n_days - n_attn))
