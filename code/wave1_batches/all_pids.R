library(dplyr)

responses <- readRDS("data/wave1_responses.rds")

all_pids <- responses %>%
  select(Netquest_PID, Timestamp)

write.csv(all_pids, "data/all_pids_wave1.csv", row.names = FALSE)
