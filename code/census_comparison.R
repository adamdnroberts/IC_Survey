# census_comparison.R
#
# Compares the achieved samples (wave 1 and wave 2) against population
# benchmarks on four dimensions: sex, age bracket, SEL, and state (region).
# Prints one block of tables per wave and writes them to
# data/census_comparison_wave1.csv and data/census_comparison_wave2.csv.
#
# Benchmark provenance differs by dimension:
#   - sex, region: computed directly from data/raw/INEGI_censo_sexo_estado.xlsx
#   - age:         INEGI 2020 age distribution of the 18+ population in the
#                  eligible states, as stated in the PAP. Identical to the field
#                  quota targets.
#   - SEL:         AMAI NSE shares from ENIGH 2022, as stated in the PAP. These
#                  are NOT the field quota targets: the SEL quotas allocated
#                  more of the sample to the upper strata than their population
#                  share (e.g. AB at 9.0% of the wave 2 quota vs 7.3% of the
#                  population), so comparing against the quotas would understate
#                  how far the achieved sample departs from the population.
# Population figures are from latex/other files/pap.tex, Population and Sample.
#
# The wave 2 sample is the analysis sample, not the raw responses: it drops
# attention-check failures and respondents whose home municipality changed
# between waves (muni_changed), mirroring the filters in
# code/vote_update_analysis.R. Because muni_changed only exists for respondents
# linked across waves, this necessarily restricts wave 2 to the linked panel,
# which also carries create_panel_dataset.R's days_between > 4 filter. The
# funnel is printed so the cost of each filter is visible. Wave 1 is the full
# set of wave 1 responses.
#
# Inputs (run code/pull_responses_wave1.R / _wave2.R and
# code/create_panel_dataset.R first):
#   data/wave1_responses.rds
#   data/survey_panel_dataset.Rdata
#   data/raw/INEGI_censo_sexo_estado.xlsx

suppressPackageStartupMessages({
  library(dplyr)
  library(readxl)
})

# ── Sampling frame ────────────────────────────────────────────────────────────
# Four states were excluded from the survey frame (see code/census_quotas.R).
EXCLUDED_INEGI <- c("09", "10", "20", "30")

# Netquest region code -> INEGI state code (mirrors nq_to_inegi_state in
# app_wave1.R / app_wave2.R). NQ_Region is stored in the NQ coding in both waves.
NQ_TO_INEGI <- c(
  "20" = "01", "21" = "02", "22" = "03", "23" = "04", "24" = "05",
  "25" = "06", "26" = "07", "27" = "08", "30" = "11", "31" = "12",
  "32" = "13", "33" = "14", "34" = "15", "35" = "16", "36" = "17",
  "37" = "18", "38" = "19", "40" = "21", "41" = "22", "42" = "23",
  "43" = "24", "44" = "25", "45" = "26", "46" = "27", "47" = "28",
  "48" = "29", "50" = "31", "51" = "32"
)
INEGI_TO_NQ <- setNames(names(NQ_TO_INEGI), NQ_TO_INEGI)

AGE_BREAKS <- c(18, 25, 35, 45, 55, 65, Inf)
AGE_LABELS <- c("18-24", "25-34", "35-44", "45-54", "55-64", "65+")

SEX_LABELS <- c("1" = "Male", "2" = "Female")

# ── Census benchmark (state x sex) ────────────────────────────────────────────
census <- read_excel("data/raw/INEGI_censo_sexo_estado.xlsx") %>%
  filter(!is.na(...1), !(...1 %in% EXCLUDED_INEGI)) %>%
  transmute(
    inegi = ...1,
    state = ...2,
    total = as.numeric(gsub(",", "", Total)),
    hombres = as.numeric(gsub(",", "", Hombres)),
    mujeres = as.numeric(gsub(",", "", Mujeres))
  )

stopifnot(nrow(census) == length(NQ_TO_INEGI))

# Census sex shares cover the whole population; the sample is 18+ only. The
# adult sex ratio is close enough to the all-ages ratio that this is a minor
# caveat, but it is a caveat.
census_sex <- c(
  "1" = sum(census$hombres) / sum(census$total),
  "2" = sum(census$mujeres) / sum(census$total)
)

census_region <- census %>%
  mutate(nq_code = INEGI_TO_NQ[inegi], share = total / sum(total)) %>%
  select(nq_code, state, share)

REGION_LABELS <- setNames(census_region$state, census_region$nq_code)
CENSUS_REGION_SHARE <- setNames(census_region$share, census_region$nq_code)

# ── Population benchmarks from the PAP (age, SEL) ────────────────────────────
# INEGI 2020, 18+ population, eligible states.
POP_AGE <- c(
  "18-24" = 17.3, "25-34" = 22.5, "35-44" = 20.2,
  "45-54" = 17.0, "55-64" = 11.7, "65+" = 11.2
)

# AMAI NSE, ENIGH 2022: AB 7.3, C+ 12.0, C 15.3, C- 16.4, D+ 14.9, D 25.4, E 8.7.
# Wave 1 collapsed D+/D/E into code 5 at collection time (app_wave1.R), so its
# bottom cell is 14.9 + 25.4 + 8.7; wave 2 kept D+ separate and folded only
# code 7 into 6, so its bottom cell is 25.4 + 8.7.
POP_SEL_W1 <- c(
  "1" = 7.3, "2" = 12.0, "3" = 15.3, "4" = 16.4, "5" = 49.0
)
POP_SEL_W2 <- c(
  "1" = 7.3, "2" = 12.0, "3" = 15.3, "4" = 16.4, "5" = 14.9, "6" = 34.1
)

SEL_LABELS_W1 <- c(
  "1" = "AB", "2" = "C+", "3" = "C", "4" = "C-", "5" = "D+/D/E"
)
SEL_LABELS_W2 <- c(
  "1" = "AB", "2" = "C+", "3" = "C", "4" = "C-", "5" = "D+", "6" = "D/E"
)

# ── Helpers ───────────────────────────────────────────────────────────────────

# Compare one dimension: observed counts vs a benchmark share vector.
# `cells` fixes the row order; missing cells count as zero.
compare_dimension <- function(
  dimension,
  source_label,
  observed,
  benchmark_share,
  labels = NULL
) {
  cells <- names(benchmark_share)
  n <- as.integer(observed[cells])
  n[is.na(n)] <- 0L
  total <- sum(n)

  sample_pct <- 100 * n / total
  bench_pct <- 100 * as.numeric(benchmark_share[cells]) /
    sum(as.numeric(benchmark_share))

  data.frame(
    Dimension = dimension,
    Source = source_label,
    Cell = if (is.null(labels)) cells else paste0(cells, " (", labels[cells], ")"),
    N = n,
    Sample_pct = round(sample_pct, 1),
    Benchmark_pct = round(bench_pct, 1),
    Diff_pp = round(sample_pct - bench_pct, 1),
    Ratio = round(sample_pct / bench_pct, 2),
    stringsAsFactors = FALSE
  )
}

count_cells <- function(x) {
  tbl <- table(as.character(x), useNA = "no")
  setNames(as.integer(tbl), names(tbl))
}

age_bracket <- function(age) {
  cut(
    suppressWarnings(as.integer(age)),
    breaks = AGE_BREAKS,
    labels = AGE_LABELS,
    right = FALSE
  )
}

print_block <- function(tbl) {
  for (dim in unique(tbl$Dimension)) {
    part <- tbl[tbl$Dimension == dim, ]
    header <- sprintf("%s  [benchmark: %s]", dim, part$Source[1])
    cat("\n", header, "\n", strrep("-", nchar(header)), "\n", sep = "")
    print(
      part[, c("Cell", "N", "Sample_pct", "Benchmark_pct", "Diff_pp", "Ratio")],
      row.names = FALSE
    )
  }
}

# Sum of absolute percentage-point deviations / 2 = share of the sample that
# would have to be reallocated to match the benchmark (dissimilarity index).
dissimilarity <- function(part) round(sum(abs(part$Diff_pp)) / 2, 1)

# ── Per-wave comparison ───────────────────────────────────────────────────────
# `responses` must already carry unsuffixed NQ_Age / NQ_Sex / NQ_Region / NQ_SEL.
compare_wave <- function(wave_label, responses, pop_sel, sel_labels) {
  n_raw <- nrow(responses)

  responses <- responses %>%
    filter(
      !is.na(Netquest_PID), nchar(Netquest_PID) > 0,
      !is.na(NQ_Age), !is.na(NQ_Sex), !is.na(NQ_Region), !is.na(NQ_SEL)
    )

  # Match the collection-time SEL recode so observed cells line up with quotas.
  sel <- as.integer(responses$NQ_SEL)
  if (identical(names(pop_sel), names(POP_SEL_W1))) {
    sel[sel %in% c(6L, 7L)] <- 5L # wave 1: D+/D/E merged
  } else {
    sel[sel == 7L] <- 6L # wave 2: D/E merged
  }

  tbl <- bind_rows(
    compare_dimension(
      "Sex", "census", count_cells(responses$NQ_Sex), census_sex, SEX_LABELS
    ),
    compare_dimension(
      "Age", "INEGI 2020 (18+)",
      count_cells(age_bracket(responses$NQ_Age)), POP_AGE
    ),
    compare_dimension(
      "SEL", "AMAI/ENIGH 2022", count_cells(sel), pop_sel, sel_labels
    ),
    compare_dimension(
      "Region (state)", "census",
      count_cells(responses$NQ_Region), CENSUS_REGION_SHARE, REGION_LABELS
    )
  )
  tbl$Wave <- wave_label

  cat(sprintf("\n\n%s\n%s\n%s\n", strrep("=", 68), wave_label, strrep("=", 68)))
  cat(sprintf(
    "Unique respondents: %d (dropped %d with missing PID or demographics)\n",
    nrow(responses),
    n_raw - nrow(responses)
  ))
  print_block(tbl)

  cat("\nDissimilarity index (pp of sample to reallocate to match benchmark)\n")
  for (dim in unique(tbl$Dimension)) {
    cat(sprintf(
      "  %-16s %4.1f%%\n", dim, dissimilarity(tbl[tbl$Dimension == dim, ])
    ))
  }

  tbl
}

# ── Wave 1: all responses ─────────────────────────────────────────────────────
wave1 <- readRDS("data/derived/wave1_responses.rds") %>%
  distinct(Netquest_PID, .keep_all = TRUE)

w1 <- compare_wave("Wave 1", wave1, POP_SEL_W1, SEL_LABELS_W1)

# ── Wave 2: analysis sample ───────────────────────────────────────────────────
# Filters mirror code/vote_update_analysis.R (control2 is NOT dropped here —
# that exclusion is specific to the treatment-effect models, not the sample).
panel <- get(load("data/derived/survey_panel_dataset.Rdata"))

n_panel <- nrow(panel)
panel_nomove <- filter(panel, muni_changed == 0)
n_nomove <- nrow(panel_nomove)
# NA attention check counts as a failure, matching the == comparison used in the
# analysis scripts.
wave2 <- filter(panel_nomove, Attention_Check %in% "somewhat_agree")

cat("\nWave 2 sample funnel\n", strrep("-", 20), "\n", sep = "")
cat(sprintf("Linked panel (days_between > 4):   %d\n", n_panel))
cat(sprintf(
  "  home municipality unchanged:    %d (-%d)\n",
  n_nomove, n_panel - n_nomove
))
cat(sprintf(
  "  passes attention check:         %d (-%d)\n",
  nrow(wave2), n_nomove - nrow(wave2)
))

# Panel columns are suffixed by wave; use the wave 2 profile.
wave2 <- wave2 %>%
  mutate(
    NQ_Age = NQ_Age_w2,
    NQ_Sex = NQ_Sex_w2,
    NQ_Region = NQ_Region_w2,
    NQ_SEL = NQ_SEL_w2
  )

w2 <- compare_wave("Wave 2 (analysis sample)", wave2, POP_SEL_W2, SEL_LABELS_W2)

write.csv(w1, "data/derived/census_comparison_wave1.csv", row.names = FALSE)
write.csv(w2, "data/derived/census_comparison_wave2.csv", row.names = FALSE)

cat("\n\nWrote data/census_comparison_wave1.csv and data/census_comparison_wave2.csv\n")
