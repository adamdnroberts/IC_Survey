library(dplyr)
library(sf)
library(data.table)

n <- 2000 # number of synthetic responses to generate

# ── Load reference data ───────────────────────────────────────────────────────

d_geo <- st_read("data/00mun_simplified.geojson", quiet = TRUE)
d_geo$muni_id <- d_geo$CVEGEO

excluded_states <- c(
  "Ciudad de México",
  "Durango",
  "Oaxaca",
  "Veracruz de Ignacio de la Llave"
)
d_geo <- d_geo %>%
  st_drop_geometry() %>%
  filter(!NOM_ENT %in% excluded_states)

nearest10 <- readRDS("data/nearest10.rds")

ageeml <- data.table::fread(
  "references/AGEEML_202512121054579_utf.csv",
  encoding = "Latin-1"
)
pop_lookup <- setNames(
  as.numeric(ageeml$POB_TOTAL),
  sprintf("%05d", suppressWarnings(as.integer(ageeml$CVEGEO)))
)
pop_lookup <- pop_lookup[!is.na(names(pop_lookup))]

# ── Helper: generate one respondent ID ───────────────────────────────────────

make_id <- function(i) {
  ts <- format(Sys.time() + i, "%Y%m%d_%H%M%S")
  sfx <- paste(sample(c(letters, 0:9), 8, replace = TRUE), collapse = "")
  paste0(ts, "_", sfx)
}

# ── Simulate ──────────────────────────────────────────────────────────────────

parties <- c("MORENA", "PAN", "PRI", "PRD", "MC", "PVEM", "PT")
vote_choices <- c(parties, "otro", "no_vote", "dont_remember")
gov_choices <- c("MORENA/PT/PVEM", "PAN/PRI/PRD", "MC", "no_se")
issue_labels <- c(
  "Seguridad / Delincuencia",
  "Econom\u00eda / Inflaci\u00f3n",
  "Desempleo / Bajos salarios",
  "Corrupci\u00f3n",
  "Educaci\u00f3n y servicios de salud"
)
all_munis <- d_geo$muni_id
top20_munis <- names(sort(
  pop_lookup[names(pop_lookup) %in% all_munis],
  decreasing = TRUE
))[1:20]

rows <- vector("list", n)

for (i in seq_len(n)) {
  respondent_id <- make_id(i)
  home_id <- sample(all_munis, 1)
  home_row <- d_geo[d_geo$muni_id == home_id, ]

  # Nearest 5 neighbors
  nn5 <- nearest10 %>%
    filter(muni_id == home_id, rank <= 5) %>%
    arrange(rank)
  nn_ids <- nn5$neighbor_id

  # Benchmark candidates: 5 nearest + 5 largest + 5 random (matching app logic)
  nn10_ids <- nearest10 %>% filter(muni_id == home_id) %>% pull(neighbor_id)
  pool1 <- if (length(nn10_ids) >= 5) sample(nn10_ids, 5) else nn10_ids

  top20_cands <- setdiff(top20_munis, c(home_id, pool1))
  pool2 <- if (length(top20_cands) >= 5) sample(top20_cands, 5) else top20_cands

  remaining <- setdiff(all_munis, c(home_id, pool1, pool2))
  pool3 <- if (length(remaining) >= 5) sample(remaining, 5) else remaining

  bench_cands <- c(pool1, pool2, pool3)
  n_sel <- sample(seq_len(length(bench_cands)), 1)
  bench_sel <- sample(bench_cands, n_sel)

  # Issue importance: a random permutation of ranks 1–5
  issue_perm <- sample(seq_along(issue_labels))

  rows[[i]] <- data.frame(
    Respondent_ID = respondent_id,
    Netquest_PID = paste0("NQ", sample(100000:999999, 1)),
    NQ_Age = sample(18:70, 1),
    NQ_Sex = sample(c("M", "F"), 1),
    NQ_Region = sample(sprintf("%02d", 1:32), 1),
    NQ_SEL = sample(c("A", "B", "C", "D", "E"), 1),
    Found_Municipality = home_row$NOMGEO,
    Found_Municipality_ID = home_id,
    Benchmark_Candidate_Municipalities = paste(bench_cands, collapse = ";"),
    Benchmark_Selected_Municipalities = paste(bench_sel, collapse = ";"),
    Nearest5_Muni_1_ID = if (length(nn_ids) >= 1) nn_ids[1] else NA_character_,
    Nearest5_Muni_2_ID = if (length(nn_ids) >= 2) nn_ids[2] else NA_character_,
    Nearest5_Muni_3_ID = if (length(nn_ids) >= 3) nn_ids[3] else NA_character_,
    Nearest5_Muni_4_ID = if (length(nn_ids) >= 4) nn_ids[4] else NA_character_,
    Nearest5_Muni_5_ID = if (length(nn_ids) >= 5) nn_ids[5] else NA_character_,
    Age = sample(18:70, 1),
    Gender = sample(
      c("hombre", "mujer", "otro"),
      1,
      prob = c(0.48, 0.48, 0.04)
    ),
    Indigenous = sample(c("si", "no"), 1, prob = c(0.15, 0.85)),
    Municipal_President_Name = sample(
      c("Correct answer", "Wrong A", "Wrong B", "No s\u00e9"),
      1
    ),
    Municipal_President_Correct = sample(c(TRUE, FALSE), 1, prob = c(0.4, 0.6)),
    Left_Right_Scale = sample(0:10, 1),
    Last_Election_Vote = sample(vote_choices, 1),
    Last_Election_Vote_Other = NA_character_,
    Home_Governing_Party_Belief = sample(gov_choices, 1),
    Nearest5_Governing_Party_Belief_1 = sample(gov_choices, 1),
    Nearest5_Governing_Party_Belief_2 = sample(gov_choices, 1),
    Nearest5_Governing_Party_Belief_3 = sample(gov_choices, 1),
    Nearest5_Governing_Party_Belief_4 = sample(gov_choices, 1),
    Nearest5_Governing_Party_Belief_5 = sample(gov_choices, 1),
    Importance_Crime = issue_perm[1],
    Importance_Economy = issue_perm[2],
    Importance_Employment_Poverty = issue_perm[3],
    Importance_Corruption = issue_perm[4],
    Importance_Education_Health = issue_perm[5],
    Robbery_Estimate = sample(0:500, 1),
    Home_Crime_Handling_Pre = sample(0:100, 1),
    MORENA_Crime_Rating_Pre = sample(0:100, 1),
    Coalition_PAN_PRI_PRD_Crime_Rating_Pre = sample(0:100, 1),
    MC_Crime_Rating_Pre = sample(0:100, 1),
    Turnout_Likelihood_Pre = sample(0:100, 1),
    Vote_Intention_Pre = sample(vote_choices, 1),
    Vote_Intention_Pre_Other = NA_character_,
    Timestamp = as.character(Sys.time() + i * 60),
    Time_Total_Seconds = round(runif(1, 300, 1800), 1),
    Time_Page_0_Sec = round(runif(1, 20, 120), 1),
    Time_Page_1_Sec = round(runif(1, 30, 300), 1),
    Time_Page_3_Sec = round(runif(1, 20, 120), 1),
    Time_Page_19_Sec = round(runif(1, 20, 120), 1),
    Time_Page_4_Sec = round(runif(1, 30, 180), 1),
    Time_Page_20_Sec = round(runif(1, 30, 180), 1),
    Time_Page_5_Sec = round(runif(1, 30, 300), 1),
    Time_Page_6_Sec = round(runif(1, 20, 120), 1),
    Is_Mobile = sample(c(TRUE, FALSE), 1, prob = c(0.6, 0.4)),
    stringsAsFactors = FALSE
  )
}

responses <- bind_rows(rows)

out_path <- "data/survey_responses_wave1.csv"
write.csv(responses, out_path, row.names = FALSE)

cat(sprintf("Generated %d synthetic responses → %s\n", n, out_path))
