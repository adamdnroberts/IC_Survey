library(jsonlite)
library(paws.storage)

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
  "1" = 196L,   # AB
  "2" = 325L,   # C+
  "3" = 416L,   # C
  "4" = 443L,   # C-
  "5" = 351L,   # D+
  "6" = 449L    # D/E merged
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

# ── Counts ────────────────────────────────────────────────────────────────────
s3 <- paws.storage::s3()
obj <- s3$get_object(
  Bucket = Sys.getenv("S3_BUCKET"),
  Key = "quota_counts/wave2_quota_counts.json"
)
counts <- jsonlite::fromJSON(rawToChar(obj$Body), simplifyVector = TRUE)

if (!is.null(counts$sel[["7"]])) {
  counts$sel[["6"]] <- as.integer(counts$sel[["6"]]) + as.integer(counts$sel[["7"]])
  counts$sel[["7"]] <- NULL
}

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
    Cell = label_col, Count = n, Target = tgt,
    Pct = paste0(pct, "%"), Status = met,
    stringsAsFactors = FALSE
  )
  print(df, row.names = FALSE)
  invisible(df)
}

# ── Summary ───────────────────────────────────────────────────────────────────
total_n <- sum(as.integer(unlist(counts$sex)), na.rm = TRUE)
cat(sprintf(
  "\nWave 2 Quota Summary  (total completes tracked: %d / %d = %.1f%%)\n",
  total_n, 2180L, 100 * total_n / 2180
))
cat(rep("=", 60), "\n", sep = "")

print_quota("Sex (1=Male, 2=Female)", counts$sex, QUOTA_SEX)
print_quota("Age brackets", counts$age, QUOTA_AGE)
print_quota("SEL", counts$sel, QUOTA_SEL, SEL_LABELS)
print_quota("Region (state NQ code)", counts$region, QUOTA_REGION)

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
