library(dplyr)
library(sf)
library(ggplot2)

d <- readRDS("data/wave1_responses.rds")

# Build coalition lookup (mirrors app.R startup code)
load("data/magar2024_coalitions.Rdata")
all_parties <- magar2024 %>%
  mutate(
    CVEGEO = sprintf("%05d", inegi),
    coalition_label = case_when(
      grepl("morena|pvem|pt", l01) ~ "MORENA/PVEM/PT",
      grepl("pan|pri|prd", l01) ~ "PAN/PRI/PRD",
      grepl("mc", l01) ~ "MC",
      TRUE ~ NA_character_
    )
  ) %>%
  select(CVEGEO, coalition_label)

# Recode survey response values to match coalition_label format
d <- d %>%
  left_join(all_parties, by = c("Found_Municipality_ID" = "CVEGEO")) %>%
  mutate(
    belief_recoded = case_when(
      Home_Governing_Party_Belief == "morena_pt_pvem" ~ "MORENA/PVEM/PT",
      Home_Governing_Party_Belief == "pan_pri_prd" ~ "PAN/PRI/PRD",
      Home_Governing_Party_Belief == "mc" ~ "MC",
      Home_Governing_Party_Belief == "dont_know" ~ "dont_know"
    ),
    party_knowledge = case_when(
      belief_recoded == "dont_know" ~ "Don't know",
      belief_recoded == coalition_label ~ "Correct",
      is.na(coalition_label) ~ NA_character_,
      TRUE ~ "Incorrect"
    )
  )


# Difference in proportions: party coalition vs. municipal president
n <- nrow(d)
party_correct <- sum(d$party_knowledge == "Correct") / n
mun_pres_correct <- sum(d$Municipal_President_Correct == TRUE) / n

se_diff <- sqrt(
  party_correct *
    (1 - party_correct) /
    n +
    mun_pres_correct * (1 - mun_pres_correct) / n
)

ci95 <- c(
  diff = party_correct - mun_pres_correct,
  lower = party_correct - mun_pres_correct - 1.96 * se_diff,
  upper = party_correct - mun_pres_correct + 1.96 * se_diff
)

mdd <- c(
  mdd_80 = (1.96 + 0.84) * se_diff,
  mdd_90 = (1.96 + 1.28) * se_diff
)

print(round(ci95, 4))
print(round(mdd, 4))

# Binomial test: is party_correct different from random guessing (p = 1/3)?
print(binom.test(
  x = sum(d$party_knowledge == "Correct", na.rm = TRUE),
  n = nrow(d),
  p = 1 / 3
))

# Nearby municipality coalition knowledge
recode_belief <- function(x) {
  case_when(
    x == "morena_pt_pvem" ~ "MORENA/PVEM/PT",
    x == "pan_pri_prd" ~ "PAN/PRI/PRD",
    x == "mc" ~ "MC",
    x == "dont_know" ~ "dont_know",
    TRUE ~ NA_character_
  )
}

nearest3_long <- bind_rows(
  d %>%
    select(
      Respondent_ID,
      muni_id = Nearest3_Muni_1_ID,
      belief_raw = Nearest3_Governing_Party_Belief_1
    ) %>%
    mutate(slot = 1L),
  d %>%
    select(
      Respondent_ID,
      muni_id = Nearest3_Muni_2_ID,
      belief_raw = Nearest3_Governing_Party_Belief_2
    ) %>%
    mutate(slot = 2L),
  d %>%
    select(
      Respondent_ID,
      muni_id = Nearest3_Muni_3_ID,
      belief_raw = Nearest3_Governing_Party_Belief_3
    ) %>%
    mutate(slot = 3L)
) %>%
  filter(!is.na(muni_id)) %>%
  left_join(all_parties, by = c("muni_id" = "CVEGEO")) %>%
  mutate(
    belief_recoded = recode_belief(belief_raw),
    knowledge = case_when(
      belief_recoded == "dont_know" ~ "Don't know",
      belief_recoded == coalition_label ~ "Correct",
      is.na(coalition_label) ~ NA_character_,
      TRUE ~ "Incorrect"
    )
  )

round(mean(nearest3_long$knowledge == "Correct", na.rm = TRUE), 3)

print(binom.test(
  x = sum(nearest3_long$knowledge == "Correct", na.rm = TRUE),
  n = sum(!is.na(nearest3_long$knowledge)),
  p = 1 / 3
))

# Compare home vs. nearby accuracy
cat("\nHome municipality correct:  ", round(party_correct, 3), "\n")
cat(
  "Nearby municipalities correct:",
  round(mean(nearest3_long$knowledge == "Correct", na.rm = TRUE), 3),
  "\n"
)

print(binom.test(
  x = sum(d$Municipal_President_Correct == T, na.rm = TRUE),
  n = nrow(d),
  p = 1 / 3
))

# Bar chart of Vote_Intention_Pre frequencies
vote_counts <- d %>%
  filter(!is.na(Vote_Intention_Pre)) %>%
  count(Vote_Intention_Pre) %>%
  arrange(desc(n)) %>%
  mutate(
    Vote_Intention_Pre = factor(Vote_Intention_Pre, levels = Vote_Intention_Pre)
  )

ggplot(vote_counts, aes(x = Vote_Intention_Pre, y = n)) +
  geom_col() +
  geom_text(aes(label = n), vjust = -0.4, size = 3) +
  labs(
    x = "Vote intention (pre-treatment)",
    y = "Count",
    title = "Vote Intention Pre-Treatment"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Cross-coalition selections
# Coalition definitions (mirrors app.R)
party_to_coalition <- c(
  morena = "MORENA/PVEM/PT",
  pvem = "MORENA/PVEM/PT",
  pt = "MORENA/PVEM/PT",
  pan = "PAN/PRI/PRD",
  pri = "PAN/PRI/PRD",
  prd = "PAN/PRI/PRD",
  mc = "MC"
)

cross_coalition <- d %>%
  filter(!is.na(Vote_Intention_Pre), Vote_Intention_Pre != "other") %>%
  mutate(
    parties = strsplit(Vote_Intention_Pre, ";"),
    coalitions = lapply(parties, function(ps) {
      unique(na.omit(party_to_coalition[ps]))
    }),
    n_coalitions = sapply(coalitions, length),
    cross_coalition = n_coalitions > 1
  )

cat("\nCross-coalition vote intentions:\n")
print(table(cross_coalition$cross_coalition))
cat(
  "\nProportion cross-coalition:",
  round(mean(cross_coalition$cross_coalition), 3),
  "\n"
)

cat("\nBreakdown by number of coalitions selected:\n")
print(table(cross_coalition$n_coalitions))

cat("\nCross-coalition combinations and their frequencies:\n")
cross_coalition %>%
  filter(cross_coalition) %>%
  mutate(
    coalition_combo = sapply(coalitions, function(cs) {
      paste(sort(cs), collapse = " + ")
    })
  ) %>%
  count(coalition_combo) %>%
  arrange(desc(n)) %>%
  print()
