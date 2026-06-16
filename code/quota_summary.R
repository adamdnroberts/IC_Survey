.script_start_time <- Sys.time()

library(dplyr)
library(jsonlite)
library(paws.storage)

larger_sample <- 1.4

# ── Targets ──────────────────────────────────────────────────────────────────
QUOTA_SEX <- ceiling(c("1" = 1067, "2" = 1113) * larger_sample)

QUOTA_AGE <- ceiling(
  c(
    "18-24" = 377,
    "25-34" = 491,
    "35-44" = 441,
    "45-54" = 371,
    "55-64" = 255,
    "65+" = 245
  ) *
    larger_sample
)

QUOTA_SEL <- ceiling(
  c(
    "1" = 189,
    "2" = 346,
    "3" = 415,
    "4" = 430,
    "5" = 800 # D+/D/E merged (469+600)
  ) *
    larger_sample
)

QUOTA_REGION <- ceiling(readRDS("data/region_quotas_wave1.rds") * larger_sample)

SEL_LABELS <- c(
  "1" = "AB",
  "2" = "C+",
  "3" = "C",
  "4" = "C-",
  "5" = "D+/D/E"
)

# ── Counts ────────────────────────────────────────────────────────────────────
s3 <- paws.storage::s3()
obj <- s3$get_object(
  Bucket = Sys.getenv("S3_BUCKET"),
  Key = "quota_counts/wave1_quota_counts.json"
)
counts <- jsonlite::fromJSON(rawToChar(obj$Body), simplifyVector = TRUE)

# Fold codes 6 and 7 into code 5 (D+/D/E combined)
if (!is.null(counts$sel[["6"]])) {
  counts$sel[["5"]] <- as.integer(counts$sel[["5"]]) +
    as.integer(counts$sel[["6"]])
  counts$sel[["6"]] <- NULL
}
if (!is.null(counts$sel[["7"]])) {
  counts$sel[["5"]] <- as.integer(counts$sel[["5"]]) +
    as.integer(counts$sel[["7"]])
  counts$sel[["7"]] <- NULL
}

# ── Helper: print a quota dimension ──────────────────────────────────────────
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

# ── Summary line ─────────────────────────────────────────────────────────────
total_n <- sum(as.integer(unlist(counts$age)), na.rm = TRUE)
quota_target_n <- sum(QUOTA_AGE)
cat(sprintf(
  "\nWave 1 Quota Summary  (total completes tracked: %d / %d = %.1f%%)\n",
  total_n,
  quota_target_n,
  100 * total_n / quota_target_n
))
cat(rep("=", 60), "\n", sep = "")

# print_quota("Sex (1=Male, 2=Female)", counts$sex, QUOTA_SEX)
print_quota("Age brackets", counts$age, QUOTA_AGE)
print_quota("SEL", counts$sel, QUOTA_SEL, SEL_LABELS)
print_quota("Region (state INEGI code)", counts$region, QUOTA_REGION)

# ── Overall percent across all cells ─────────────────────────────────────────
all_counts <- c(
  # as.integer(counts$sex[names(QUOTA_SEX)]),
  as.integer(counts$age[names(QUOTA_AGE)]),
  as.integer(counts$sel[names(QUOTA_SEL)]),
  as.integer(counts$region[names(QUOTA_REGION)])
)
all_targets <- c(
  # QUOTA_SEX,
  QUOTA_AGE,
  QUOTA_SEL,
  QUOTA_REGION
)
all_counts[is.na(all_counts)] <- 0L
n_met <- sum(all_counts >= all_targets)
cat(sprintf(
  "\nCells met: %d / %d (%.0f%%)\n",
  n_met,
  length(all_targets),
  100 * n_met / length(all_targets)
))

# ── Cross-check against actual responses ─────────────────────────────────────

# Pull fresh responses from S3 (writes data/wave1_responses.rds)
#source("code/pull_responses_wave1.R")
responses <- readRDS("data/wave1_responses.rds")

n_raw <- nrow(responses)
responses <- responses |>
  filter(
    !is.na(Netquest_PID),
    nchar(Netquest_PID) > 0,
    !is.na(NQ_Age),
    !is.na(NQ_Sex),
    !is.na(NQ_Region),
    !is.na(NQ_SEL)
  )
cat(sprintf(
  "\nScreened out %d obs with missing PID or demographics (kept %d / %d)\n",
  n_raw - nrow(responses),
  nrow(responses),
  n_raw
))

resp_counts <- function(col) {
  tbl <- table(col, useNA = "no")
  setNames(as.integer(tbl), names(tbl))
}

age_bracket <- function(age) {
  age <- as.integer(age)
  cut(
    age,
    breaks = c(18, 25, 35, 45, 55, 65, Inf),
    right = FALSE,
    labels = c("18-24", "25-34", "35-44", "45-54", "55-64", "65+")
  )
}

# resp_sex    <- resp_counts(responses$NQ_Sex)
resp_age <- resp_counts(age_bracket(responses$NQ_Age))
resp_sel <- resp_counts(responses$NQ_SEL)
resp_region <- resp_counts(responses$NQ_Region)

cat(sprintf(
  "\n\n%s\nWave 1 Response-Based Quota Check  (N = %d / %d = %.1f%% responses)\n%s\n",
  strrep("=", 60),
  nrow(responses),
  quota_target_n,
  100 * nrow(responses) / quota_target_n,
  strrep("=", 60)
))

# print_quota("Sex (1=Male, 2=Female)", resp_sex, QUOTA_SEX)
print_quota("Age brackets", resp_age, QUOTA_AGE)
print_quota("SEL", resp_sel, QUOTA_SEL, SEL_LABELS)
print_quota("Region (state INEGI code)", resp_region, QUOTA_REGION)

# ── Discrepancy: JSON counter vs responses ────────────────────────────────────

cat(sprintf(
  "\nDiscrepancy (responses − JSON counter): %+d\n",
  nrow(responses) - total_n
))

# ── Runtime ───────────────────────────────────────────────────────────────────
cat(sprintf(
  "\nScript runtime: %.1f mins\n",
  as.numeric(difftime(Sys.time(), .script_start_time, units = "mins"))
))
