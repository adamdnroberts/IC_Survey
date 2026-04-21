library(dplyr)

survey_responses_wave1 <- readRDS(
  "C:/Users/adamd/Documents/IC_Survey/data/wave1_responses.rds"
)

survey_responses_wave1 <- survey_responses_wave1 %>%
  filter(as.Date(Timestamp) >= "2026-04-14")

# Build timing summary table (all times in minutes)
pages <- c("Total", paste0("Page_", 0:11))
col_names <- c("Time_Total_Seconds", paste0("Time_Page_", c(0:4, 6, 9), "_Sec"))

timing_summary <- lapply(seq_along(pages), function(i) {
  col <- col_names[i]
  if (!col %in% names(survey_responses_wave1)) {
    return(NULL)
  }

  vals <- as.numeric(survey_responses_wave1[[col]]) / 60

  data.frame(
    Page = pages[i],
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

print(timing_summary, row.names = FALSE)
