library(dplyr)

# Build a per-page timing summary (all times in minutes) for one wave.
# `pages` is the vector of page numbers that have Time_Page_<n>_Sec columns.
timing_summary <- function(responses, pages) {
  rows <- c("Total", paste0("Page_", pages))
  cols <- c("Time_Total_Seconds", paste0("Time_Page_", pages, "_Sec"))

  lapply(seq_along(rows), function(i) {
    col <- cols[i]
    if (!col %in% names(responses)) {
      return(NULL)
    }

    vals <- as.numeric(responses[[col]]) / 60

    data.frame(
      Page = rows[i],
      N = sum(!is.na(vals)),
      Min = round(min(vals, na.rm = TRUE), 2),
      Q1 = round(quantile(vals, 0.25, na.rm = TRUE), 2),
      Median = round(median(vals, na.rm = TRUE), 2),
      Mean = round(mean(vals, na.rm = TRUE), 2),
      Q3 = round(quantile(vals, 0.75, na.rm = TRUE), 2),
      Max = round(max(vals, na.rm = TRUE), 2),
      row.names = NULL
    )
  }) |>
    Filter(Negate(is.null), x = _) |>
    bind_rows()
}

# Wave 1: timing columns exist for pages 0, 1, 3, 4, 6, 9
survey_responses_wave1 <- readRDS(
  "C:/Users/adamd/Documents/IC_Survey/data/wave1_responses.rds"
) %>%
  filter(!is.na(Netquest_PID), Netquest_PID != "")

cat("\n=== Wave 1 timing (minutes) ===\n")
print(
  timing_summary(survey_responses_wave1, pages = c(0, 1, 3, 4, 6, 9)),
  row.names = FALSE
)

# Wave 2: pages 1, 7, 9, 11 (page 8 doesn't exist; page 13 is the final page
# and its time is never accumulated into page_durations, so both are excluded)
survey_responses_wave2 <- readRDS(
  "C:/Users/adamd/Documents/IC_Survey/data/wave2_responses.rds"
) %>%
  filter(!is.na(Netquest_PID), Netquest_PID != "")

cat("\n=== Wave 2 timing (minutes) ===\n")
print(
  timing_summary(survey_responses_wave2, pages = c(1, 7, 9, 11)),
  row.names = FALSE
)

# Median total time per wave (minutes)
median_total_min <- function(responses) {
  round(median(as.numeric(responses$Time_Total_Seconds) / 60, na.rm = TRUE), 2)
}

cat("\n=== Median total time (minutes) ===\n")
cat(sprintf(
  "Wave 1: %s (N = %d)\n",
  median_total_min(survey_responses_wave1),
  nrow(survey_responses_wave1)
))
cat(sprintf(
  "Wave 2: %s (N = %d)\n",
  median_total_min(survey_responses_wave2),
  nrow(survey_responses_wave2)
))
