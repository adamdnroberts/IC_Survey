library(dplyr)

responses <- readRDS("data/wave1_responses.rds")

excluded_pids <- c(
  read.csv("data/first200_wave1.csv")$Netquest_PID,
  read.csv("data/batch2_wave1_sent.csv")$Netquest_PID
)

batch3 <- responses %>%
  filter(!Netquest_PID %in% excluded_pids) %>%
  filter(as.Date(as.POSIXct(Timestamp)) <= as.Date("2026-04-21")) %>%
  select(Netquest_PID, Timestamp)

overlap <- intersect(batch3$Netquest_PID, excluded_pids)
if (length(overlap) > 0) {
  stop(sprintf(
    "Overlap detected: %d PIDs appear in a previous batch.",
    length(overlap)
  ))
} else {
  cat("No overlap with previous batches.\n")
}

write.csv(batch3, "data/batch3_wave1.csv", row.names = FALSE)

cat(sprintf("Saved %d respondents → data/batch3_wave1.csv\n", nrow(batch3)))
