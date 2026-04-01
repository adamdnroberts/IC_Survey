library(dplyr)
library(ggplot2)

mp_incumbents <- read.csv(
  "C:/Users/adamd/Documents/IC_Survey/data/aymu1989-on.incumbents.csv"
)

# Pre-compute incumbent MC data: correct name, runner-up, pre-filtered distractor pool
mp_latest <- mp_incumbents %>%
  mutate(CVEGEO = sprintf("%05d", as.numeric(inegi))) %>%
  group_by(CVEGEO) %>%
  slice_max(yr, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  filter(!is.na(incumbent) & incumbent != "")

all_incumbent_names <- unique(mp_latest$incumbent)

is_dissimilar <- function(name, targets, threshold = 0.4) {
  targets <- targets[!is.na(targets) & targets != ""]
  if (length(targets) == 0) {
    return(TRUE)
  }
  dists <- adist(name, targets, ignore.case = TRUE)
  max_lens <- pmax(nchar(name), nchar(targets))
  all(dists / max_lens >= threshold)
}

mp_mc_data <- setNames(
  lapply(seq_len(nrow(mp_latest)), function(i) {
    row <- mp_latest[i, ]
    correct <- trimws(row$incumbent)
    runnerup_raw <- trimws(row$runnerup)
    runner_up <- if (!is.na(runnerup_raw) & nchar(runnerup_raw) > 0) {
      runnerup_raw
    } else {
      NA_character_
    }
    exclude <- c(correct, runner_up)
    n_distractors <- if (is.na(runner_up)) 3 else 2
    pool <- sample(
      all_incumbent_names[
        all_incumbent_names != correct &
          sapply(all_incumbent_names, is_dissimilar, targets = exclude)
      ],
      n_distractors
    )
    list(correct = correct, runner_up = runner_up, pool = pool)
  }),
  mp_latest$CVEGEO
)

saveRDS(mp_mc_data, file = "data/mp_mc_data.rds")

test <- filter(mp_incumbents, yr == 2022)

test$race.after <- as.character(test$race.after)
unique(test$race.after)

# 1. Standardize the case so "Term" and "term" are counted together
test$race.after <- tolower(test$race.after)

# 2. Create the plot
ggplot(test, aes(x = race.after)) +
  geom_bar(fill = "steelblue") +
  theme_minimal() +
  labs(
    title = "Frequency of Re-election Eligibility Status",
    x = "Status Category",
    y = "Number of Municipalities"
  ) +
  # Rotate labels if they overlap
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
