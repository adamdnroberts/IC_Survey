library(jsonlite)
library(paws.storage)

# ── Targets ──────────────────────────────────────────────────────────────────
QUOTA_SEX <- c("1" = 1067L, "2" = 1113L)

QUOTA_AGE <- c(
  "18-24" = 377L,
  "25-34" = 491L,
  "35-44" = 441L,
  "45-54" = 371L,
  "55-64" = 255L,
  "65+"   = 245L
)

QUOTA_SEL <- c(
  "1" = 158L, "2" = 262L, "3" = 334L,
  "4" = 357L, "5" = 469L, "6" = 560L, "7" = 40L
)

QUOTA_REGION <- readRDS("data/region_quotas_wave1.rds")

SEL_LABELS <- c(
  "1" = "AB", "2" = "C+", "3" = "C",
  "4" = "C-", "5" = "D+", "6" = "D", "7" = "E"
)

# ── Counts ────────────────────────────────────────────────────────────────────
s3  <- paws.storage::s3()
obj <- s3$get_object(Bucket = Sys.getenv("S3_BUCKET"), Key = "quota_counts/wave1_quota_counts.json")
counts <- jsonlite::fromJSON(rawToChar(obj$Body), simplifyVector = TRUE)

# ── Helper: print a quota dimension ──────────────────────────────────────────
print_quota <- function(title, counts_vec, targets, extra_labels = NULL) {
  cat("\n", title, "\n", rep("-", nchar(title)), "\n", sep = "")

  cells <- names(targets)
  n     <- as.integer(counts_vec[cells])
  n[is.na(n)] <- 0L
  tgt   <- as.integer(targets[cells])
  pct   <- round(100 * n / tgt, 1)
  met   <- ifelse(n >= tgt, "DONE", "")

  label_col <- if (!is.null(extra_labels)) {
    paste0(cells, " (", extra_labels[cells], ")")
  } else {
    cells
  }

  df <- data.frame(
    Cell    = label_col,
    Count   = n,
    Target  = tgt,
    Pct     = paste0(pct, "%"),
    Status  = met,
    stringsAsFactors = FALSE
  )

  print(df, row.names = FALSE)
  invisible(df)
}

# ── Summary line ─────────────────────────────────────────────────────────────
total_n <- sum(as.integer(unlist(counts$sex)), na.rm = TRUE)
cat(sprintf("\nWave 1 Quota Summary  (total completes tracked: %d / %d = %.1f%%)\n",
            total_n, 2180L, 100 * total_n / 2180))
cat(rep("=", 60), "\n", sep = "")

print_quota("Sex (1=Male, 2=Female)", counts$sex, QUOTA_SEX)
print_quota("Age brackets", counts$age, QUOTA_AGE)
print_quota("SEL", counts$sel, QUOTA_SEL, SEL_LABELS)
print_quota("Region (state INEGI code)", counts$region, QUOTA_REGION)

# ── Overall percent across all cells ─────────────────────────────────────────
all_counts  <- c(
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
  n_met, length(all_targets), 100 * n_met / length(all_targets)
))

# ── Cross-check against actual responses ─────────────────────────────────────

responses <- readRDS("data/wave1_responses.rds")

resp_counts <- function(col) {
  tbl <- table(col, useNA = "no")
  setNames(as.integer(tbl), names(tbl))
}

age_bracket <- function(age) {
  age <- as.integer(age)
  cut(age,
    breaks = c(18, 25, 35, 45, 55, 65, Inf),
    right  = FALSE,
    labels = c("18-24", "25-34", "35-44", "45-54", "55-64", "65+")
  )
}

resp_sex    <- resp_counts(responses$NQ_Sex)
resp_age    <- resp_counts(age_bracket(responses$NQ_Age))
resp_sel    <- resp_counts(responses$NQ_SEL)
resp_region <- resp_counts(responses$NQ_Region)

cat(sprintf(
  "\n\n%s\nWave 1 Response-Based Quota Check  (N = %d responses)\n%s\n",
  strrep("=", 60), nrow(responses), strrep("=", 60)
))

print_quota("Sex (1=Male, 2=Female)", resp_sex, QUOTA_SEX)
print_quota("Age brackets", resp_age, QUOTA_AGE)
print_quota("SEL", resp_sel, QUOTA_SEL, SEL_LABELS)
print_quota("Region (state INEGI code)", resp_region, QUOTA_REGION)

# ── Discrepancy: JSON counter vs responses ────────────────────────────────────

cat(sprintf(
  "\nDiscrepancy (responses − JSON counter): %+d\n",
  nrow(responses) - total_n
))
