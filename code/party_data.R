library(dplyr)
library(ggplot2)

magar_coalition_full <- read.csv("data/raw/aymu-coalAgg2020s.csv")

# Map state abbreviations to full names
state_names <- c(
  "ags" = "Aguascalientes",
  "bc" = "Baja California",
  "bcs" = "Baja California Sur",
  "cam" = "Campeche",
  "coa" = "Coahuila",
  "col" = "Colima",
  "chi" = "Chiapas",
  "cps" = "Chiapas",
  "chs" = "Chihuahua",
  "cua" = "Chihuahua",
  "cin" = "Ciudad de México",
  "cdmx" = "Ciudad de México",
  "df" = "Ciudad de México",
  "dur" = "Durango",
  "dgo" = "Durango",
  "gua" = "Guanajuato",
  "gue" = "Guerrero",
  "hid" = "Hidalgo",
  "hgo" = "Hidalgo",
  "jal" = "Jalisco",
  "mex" = "Estado de México",
  "mic" = "Michoacán",
  "mor" = "Morelos",
  "nay" = "Nayarit",
  "nl" = "Nuevo León",
  "oax" = "Oaxaca",
  "pue" = "Puebla",
  "que" = "Querétaro",
  "qui" = "Quintana Roo",
  "san" = "San Luis Potosí",
  "sin" = "Sinaloa",
  "son" = "Sonora",
  "tab" = "Tabasco",
  "tam" = "Tamaulipas",
  "tla" = "Tlaxcala",
  "ver" = "Veracruz",
  "yuc" = "Yucatán",
  "zac" = "Zacatecas"
)

magar_coalition_full <- magar_coalition_full %>%
  mutate(
    state_abbr = sub("-.*", "", emm),
    estado = state_names[state_abbr]
  )

# Use most recent election year per municipality (covers all 32 states)
magar2024 <- magar_coalition_full %>%
  group_by(inegi) %>%
  slice_max(yr, n = 1, with_ties = FALSE) %>%
  ungroup()

save(magar2024, file = "data/magar2024_coalitions.Rdata")

# Check for cross-coalition cases (morena + pan/pri/prd in same coalition)
cross_coalition <- magar2024 %>%
  filter(grepl("morena", v01) & grepl("pan|pri|prd", v01))
cat(
  "Cross-coalition cases (morena with pan/pri/prd):",
  nrow(cross_coalition),
  "\n"
)
if (nrow(cross_coalition) > 0) {
  print(cross_coalition %>% select(emm, mun, yr, part))
}

############################################################################################
mp_incumbents <- read.csv(
  "data/raw/aymu1989-on.incumbents.csv"
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

saveRDS(mp_mc_data, file = "data/derived/mp_mc_data.rds")

test <- filter(mp_incumbents, yr == 2022)

test$race.after <- as.character(test$race.after)
unique(test$race.after)

library(ggplot2)

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
