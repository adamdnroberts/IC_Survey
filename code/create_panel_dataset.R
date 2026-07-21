library(dplyr)
library(readxl)

robbery_cap_mult <- 2
min_days_between <- 4
ci_alpha <- 0.01

wave1 <- readRDS("data/wave1_responses.rds")
wave2 <- readRDS("data/wave2_responses.rds")

wave1 <- distinct(wave1, Netquest_PID, .keep_all = TRUE)
wave2 <- distinct(wave2, Netquest_PID, .keep_all = TRUE)

match_ids1 <- read_excel("data/Match ID.xlsx")
match_ids1 <- janitor::clean_names(match_ids1)

match_ids2 <- read.csv(
  "data/wave_match_ids.csv",
  stringsAsFactors = FALSE,
  fileEncoding = "UTF-8-BOM"
)

match_ids3 <- read_excel("data/match IDs 29 Jun.xlsx")
match_ids3 <- janitor::clean_names(match_ids3)

match_ids4 <- read_excel(
  "C:/Users/adamd/Documents/IC_Survey/data/Match IDs final.xlsx"
)
match_ids4 <- janitor::clean_names(match_ids4)


match_ids1 <- match_ids1 %>%
  rename(
    pid_w1 = wave_1,
    pid_w2 = wave_2,
    status_w2 = status_wave_2
  ) %>%
  select(pid_w2, pid_w1) %>%
  mutate(across(c(pid_w2, pid_w1), as.character))

match_ids2 <- match_ids2 %>%
  rename(
    pid_w1 = Wave.1,
    pid_w2 = Wave.2
  ) %>%
  select(pid_w2, pid_w1) %>%
  mutate(across(c(pid_w2, pid_w1), as.character))

match_ids3 <- match_ids3 %>%
  rename(
    pid_w1 = wave_1,
    pid_w2 = wave_2
  ) %>%
  select(pid_w2, pid_w1) %>%
  mutate(across(c(pid_w2, pid_w1), as.character))

match_ids4 <- match_ids4 %>%
  rename(
    pid_w1 = wave_1,
    pid_w2 = wave_2
  ) %>%
  select(pid_w2, pid_w1) %>%
  mutate(across(c(pid_w2, pid_w1), as.character))

match_ids1_2 <- bind_rows(match_ids1, match_ids2) %>%
  distinct(pid_w2, pid_w1)

match_ids3_4 <- bind_rows(match_ids3, match_ids4) %>%
  distinct(pid_w2, pid_w1)

match_ids <- bind_rows(match_ids1_2, match_ids3_4) %>%
  distinct(pid_w2, pid_w1)

# wave1 is the filtered set (data/wave1_responses.rds), in which each wave-2
# response links to at most one surviving wave-1 response. Restrict the
# crosswalk to those surviving wave-1 PIDs so the spurious arm of each ambiguous
# match drops out, leaving the resolved one-to-one pairs.
match_ids_full <- match_ids %>%
  filter(pid_w1 %in% as.character(wave1$Netquest_PID))

wave1_times <- wave1 %>%
  transmute(
    pid_w1 = as.character(Netquest_PID),
    w1_time = as.POSIXct(Timestamp, format = "%Y-%m-%d %H:%M:%OS", tz = "UTC")
  )

wave2_times <- wave2 %>%
  transmute(
    pid_w2 = as.character(Netquest_PID),
    w2_time = as.POSIXct(Timestamp)
  )

# Safety: guarantee one wave-1 per wave-2 (earliest-recorded wave-1, mirroring
# filter_wave1_bad_links.R) in case both arms of an ambiguous match survive.
match_ids <- match_ids_full %>%
  left_join(wave1_times, by = "pid_w1") %>%
  group_by(pid_w2) %>%
  slice_min(w1_time, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  select(pid_w2, pid_w1)

# A wave-1 respondent matched to multiple wave-2 responses keeps the earliest
# wave-2 response.
match_ids <- match_ids %>%
  left_join(wave2_times, by = "pid_w2") %>%
  group_by(pid_w1) %>%
  slice_min(w2_time, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  select(pid_w2, pid_w1)

panel <- wave2 %>%
  inner_join(match_ids, by = c("Netquest_PID" = "pid_w2")) %>%
  inner_join(
    wave1,
    by = c("pid_w1" = "Netquest_PID"),
    suffix = c("_w2", "_w1")
  ) %>%
  select(-pid_w1) %>%
  mutate(
    muni_changed = Found_Municipality_ID_w2 != Found_Municipality_ID_w1,
    Found_Municipality_ID = Found_Municipality_ID_w2,
    ts_w1 = as.POSIXct(Timestamp_w1, format = "%Y-%m-%d %H:%M:%OS", tz = "UTC"),
    ts_w2 = as.POSIXct(Timestamp_w2, format = "%Y-%m-%d %H:%M:%OS", tz = "UTC"),
    days_between = as.numeric(difftime(ts_w2, ts_w1, units = "days"))
  )

sum(panel$NQ_Age_w1 == panel$NQ_Age_w2) / nrow(panel)
sum(panel$NQ_Sex_w1 == panel$NQ_Sex_w2) / nrow(panel)
sum(panel$NQ_Region_w1 == panel$NQ_Region_w2) / nrow(panel)
sum(panel$NQ_SEL_w1 == panel$NQ_SEL_w2) / nrow(panel)

rank_cols <- c(
  "Crime_Rank_Comp_1",
  "Crime_Rank_Comp_2",
  "Crime_Rank_Comp_3",
  "Crime_Rank_Comp_4"
)

panel$rank_prior <- 1 +
  rowSums(
    sapply(panel[rank_cols], function(x) x %in% c("fewer")),
    na.rm = TRUE
  )

robo <- readRDS("data/robo_2025.rds") %>%
  mutate(Cve..Municipio = sprintf("%05d", as.integer(Cve..Municipio))) %>%
  select(Cve..Municipio, rate_per_100k)

panel <- panel %>%
  left_join(robo, by = c("Found_Municipality_ID" = "Cve..Municipio")) %>%
  rename(home_rate = rate_per_100k)

for (i in 1:4) {
  id_col <- paste0("Comparison_Muni_", i, "_ID")
  rate_col <- paste0("comp_rate_", i)
  panel <- panel %>%
    left_join(
      rename(robo, !!rate_col := rate_per_100k),
      by = setNames("Cve..Municipio", id_col)
    )
}

panel$actual_rank <- 1 +
  rowSums(
    sapply(paste0("comp_rate_", 1:4), function(col) {
      panel[[col]] < panel$home_rate
    }),
    na.rm = TRUE
  )

panel$Robbery_Estimate_wins <- pmin(
  as.numeric(panel$Robbery_Estimate),
  max(panel$home_rate, na.rm = TRUE) * robbery_cap_mult
)

panel$crime_gap <- panel$home_rate - as.numeric(panel$Robbery_Estimate)
panel$crime_gap_wins <- panel$home_rate - panel$Robbery_Estimate_wins
panel$rank_gap <- panel$actual_rank - panel$rank_prior

panel$log_crime_gap <- sign(panel$crime_gap) * log(abs(panel$crime_gap))

panel$Home_Crime_Handling_Change <- as.numeric(panel$Home_Crime_Handling_Post) -
  as.numeric(panel$Home_Crime_Handling_Pre)

load("data/magar2024_coalitions.Rdata")
all_parties_tmp <- magar2024 %>%
  mutate(
    muni_id = sprintf("%05d", inegi),
    home_coalition = case_when(
      grepl("morena|pvem|pt", l01) ~ "MORENA/PVEM/PT",
      grepl("pan|pri|prd", l01) ~ "PAN/PRI/PRD",
      grepl("mc", l01) ~ "MC",
      TRUE ~ NA_character_
    )
  ) %>%
  select(muni_id, home_coalition)

coalition_vec <- setNames(
  all_parties_tmp$home_coalition,
  all_parties_tmp$muni_id
)

belief_to_coalition <- c(
  "morena_pt_pvem" = "MORENA/PVEM/PT",
  "pan_pri_prd" = "PAN/PRI/PRD",
  "mc" = "MC"
)

comp_correct <- sapply(1:4, function(i) {
  belief <- panel[[paste0("Comp_Governing_Party_Belief_", i)]]
  muni <- sprintf(
    "%05d",
    as.integer(panel[[paste0("Comparison_Muni_", i, "_ID")]])
  )
  guessed <- belief_to_coalition[belief]
  actual <- coalition_vec[muni]
  as.integer(
    !is.na(belief) &
      belief != "" &
      belief != "dont_know" &
      !is.na(actual) &
      guessed == actual
  )
})

panel$comp_party_known <- rowSums(comp_correct, na.rm = TRUE)

party_to_coalition <- c(
  morena = "MORENA/PVEM/PT",
  pvem = "MORENA/PVEM/PT",
  pt = "MORENA/PVEM/PT",
  pan = "PAN/PRI/PRD",
  pri = "PAN/PRI/PRD",
  prd = "PAN/PRI/PRD",
  mc = "MC"
)

to_coalition <- function(x) {
  sapply(
    x,
    function(s) {
      if (is.na(s) || s == "") {
        return(NA_character_)
      }
      parties <- trimws(strsplit(s, ";")[[1]])
      coalitions <- unique(na.omit(party_to_coalition[parties]))
      # No recognized party (e.g. "other" on its own) => its own category.
      # A single coalition resolves; a cross-coalition ticket stays NA.
      if (length(coalitions) == 0) "Other"
      else if (length(coalitions) == 1) coalitions else NA_character_
    },
    USE.NAMES = FALSE
  )
}

panel$coalition_pre <- to_coalition(panel$Vote_Intention_Pre)
panel$coalition_post <- to_coalition(panel$Vote_Intention_Post)

panel$home_coalition <- coalition_vec[
  sprintf("%05d", as.integer(panel$Found_Municipality_ID))
]

panel <- filter(panel, !is.na(days_between) & days_between > min_days_between)

save(panel, file = "data/survey_panel_dataset.Rdata")
