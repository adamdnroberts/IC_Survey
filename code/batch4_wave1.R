library(dplyr)

responses <- readRDS("data/wave1_responses.rds")

excluded_pids <- c(
  read.csv("data/first200_wave1.csv")$Netquest_PID,
  read.csv("data/batch2_wave1_sent.csv")$Netquest_PID,
  read.csv("data/batch3_wave1.csv")$Netquest_PID
)

w2 <- readRDS("C:/Users/adamd/Documents/IC_Survey/data/wave2_responses.rds")

print(paste0(
  "prop. recontacts achieved: ",
  round(nrow(w2) / length(excluded_pids), 2)
))

batch4 <- responses %>%
  filter(!Netquest_PID %in% excluded_pids) %>%
  filter(as.Date(as.POSIXct(Timestamp)) <= as.Date("2026-04-27")) %>%
  select(Netquest_PID, Timestamp)

overlap <- intersect(batch4$Netquest_PID, excluded_pids)
if (length(overlap) > 0) {
  stop(sprintf(
    "Overlap detected: %d PIDs appear in a previous batch.",
    length(overlap)
  ))
} else {
  cat("No overlap with previous batches.\n")
}

write.csv(batch4, "data/batch4_wave1_pids.csv", row.names = FALSE)
#
# cat(sprintf("Saved %d respondents → data/batch3_wave1.csv\n", nrow(batch3)))
