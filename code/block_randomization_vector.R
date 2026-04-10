library(dplyr)

set.seed(20250409)

# Block of 10 matches your probability ratios exactly (2,1,1,2,2,2)
block <- c(
  rep("control", 2),
  rep("control2", 1),
  rep("T1", 1),
  rep("T2", 2),
  rep("T3", 2),
  rep("T4", 2)
)

coalitions <- c("MORENA/PT/PVEM", "PAN/PRI/PRD", "MC")

# 200 blocks × 10 = 2,000 slots per coalition
assignment_table <- do.call(
  rbind,
  lapply(coalitions, function(coal) {
    assignments <- unlist(lapply(1:200, function(b) sample(block)))
    data.frame(
      coalition = coal,
      slot = 1:2000,
      assignment = assignments
    )
  })
)

assignment_table %>%
  group_by(assignment) %>%
  summarize(count = n())

write.csv(assignment_table, "assignment_vector.csv", row.names = FALSE)
