library(paws.storage)
library(dplyr)

readRenviron("C:/Users/adamd/Documents/IC_Survey/.Renviron")

s3_client <- paws.storage::s3()
objects <- s3_client$list_objects_v2(
  Bucket = Sys.getenv("S3_BUCKET"),
  Prefix = "wave1_screenouts/"
)

if (length(objects$Contents) == 0) {
  stop("No screenout records found in S3 bucket.")
}

screenouts <- dplyr::bind_rows(lapply(objects$Contents, function(obj) {
  raw <- s3_client$get_object(Bucket = Sys.getenv("S3_BUCKET"), Key = obj$Key)
  read.csv(
    text = rawToChar(raw$Body),
    stringsAsFactors = FALSE,
    colClasses = "character"
  )
}))

cat(sprintf("Total screenout records pulled: %d\n\n", nrow(screenouts)))

# ── Counts by reason ──────────────────────────────────────────────────────────

reason_tbl <- screenouts %>%
  count(Screenout_Reason, name = "n") %>%
  arrange(desc(n)) %>%
  mutate(pct = sprintf("%.1f%%", 100 * n / sum(n)))
print(reason_tbl)

# ── Comparison with completions ───────────────────────────────────────────────

first_screenout_date <- as.Date(min(screenouts$Timestamp))

responses_path <- "C:/Users/adamd/Documents/IC_Survey/data/wave1_responses.rds"
if (file.exists(responses_path)) {
  completions <- readRDS(responses_path) %>%
    filter(as.Date(Timestamp) >= first_screenout_date)
  n_complete <- nrow(completions)
  n_screen <- nrow(screenouts)
  n_total <- n_complete + n_screen
  cat(sprintf(
    "  Completions : %d  (%.1f%%)\n  Screenouts  : %d  (%.1f%%)\n  Total       : %d\n",
    n_complete,
    100 * n_complete / n_total,
    n_screen,
    100 * n_screen / n_total,
    n_total
  ))
}


saveRDS(
  screenouts,
  "C:/Users/adamd/Documents/IC_Survey/data/wave1_screenouts.rds"
)
cat("\nSaved to data/wave1_screenouts.rds\n")
