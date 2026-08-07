library(dplyr)

responses <- readRDS("data/derived/wave1_responses.rds")

excluded_pids <- c(
  read.csv("data/temp/first200_wave1.csv")$Netquest_PID,
  read.csv("data/raw/batch2_wave1_sent.csv")$Netquest_PID,
  read.csv("data/temp/batch3_wave1.csv")$Netquest_PID,
  read.csv("data/temp/batch4_wave1_pids.csv")$Netquest_PID,
  read.csv("data/temp/batch_5_18_wave1_pids.csv")$Netquest_PID,
  read.csv("data/temp/batch_5_22_wave1_pids.csv")$Netquest_PID,
  read.csv("data/temp/batch_5_26_wave1_pids.csv")$Netquest_PID,
  read.csv("data/temp/final_batch_5_29_wave1_pids.csv")$Netquest_PID,
  read.csv("data/temp/batch_6_22_wave1_pids.csv")$Netquest_PID,
  read.csv("data/temp/batch_6_23_wave1_pids.csv")$Netquest_PID
)

w2 <- readRDS("data/derived/wave2_responses.rds")

print(paste0(
  "prop. recontacts achieved: ",
  round(nrow(w2) / length(excluded_pids), 2)
))

batch_6_24 <- responses %>%
  filter(!Netquest_PID %in% excluded_pids) %>%
  filter(as.Date(as.POSIXct(Timestamp)) <= Sys.Date() - 5) %>%
  select(Netquest_PID, Timestamp)

overlap <- intersect(batch_6_24$Netquest_PID, excluded_pids)
if (length(overlap) > 0) {
  stop(sprintf(
    "Overlap detected: %d PIDs appear in a previous batch.",
    length(overlap)
  ))
} else {
  cat("No overlap with previous batches.\n")
}

write.csv(batch_6_24, "data/temp/batch_6_24_wave1_pids.csv", row.names = FALSE)
