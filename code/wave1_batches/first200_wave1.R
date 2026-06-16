library(dplyr)

responses <- readRDS("data/wave1_responses.rds")

first200 <- responses %>%
  arrange(as.numeric(Netquest_PID)) %>%
  slice_head(n = 200)

out <- first200 %>%
  select(Netquest_PID, Timestamp)

write.csv(out, "data/first200_wave1.csv", row.names = FALSE)

cat(sprintf("Saved %d respondents → data/first200_wave1.csv\n", nrow(out)))
cat(sprintf(
  "Netquest_PID range: %s – %s\n",
  min(out$Netquest_PID, na.rm = TRUE),
  max(out$Netquest_PID, na.rm = TRUE)
))

batch2 <- responses %>%
  filter(!Netquest_PID %in% first200$Netquest_PID) %>%
  filter(as.Date(as.POSIXct(Timestamp)) <= as.Date("2026-04-15"))

overlap <- intersect(first200$Netquest_PID, batch2$Netquest_PID)
if (length(overlap) > 0) {
  stop(sprintf(
    "Overlap detected: %d PIDs appear in both batches.",
    length(overlap)
  ))
} else {
  cat("No overlap between first200 and batch2.\n")
}

write.csv(batch2, "data/batch2_wave1.csv", row.names = FALSE)

cat(sprintf(
  "Saved %d respondents (on/before Apr 15, excl. first200) → data/batch2_wave1.csv\n",
  nrow(batch2)
))

length(intersect(first200$Netquest_PID, batch2$Netquest_PID))
