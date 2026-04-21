library(dplyr)

responses <- readRDS("data/wave1_responses.rds")

first200 <- responses %>%
  arrange(as.numeric(Netquest_PID)) %>%
  slice_head(n = 200)

out <- first200 %>%
  select(Netquest_PID, Timestamp)

write.csv(out, "data/first200_wave1.csv", row.names = FALSE)

cat(sprintf("Saved %d respondents → data/first200_wave1.csv\n", nrow(out)))
cat(sprintf("Netquest_PID range: %s – %s\n",
  min(out$Netquest_PID, na.rm = TRUE),
  max(out$Netquest_PID, na.rm = TRUE)))
