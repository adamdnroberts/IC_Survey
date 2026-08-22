# census_benchmarks.R
#
# Population benchmarks for the survey frame, plus the coding helpers that go
# with them. Sourced by code/census_comparison.R (sample-vs-population tables)
# and code/vote_update_weighted.R (raking weights) so the two never drift.
#
# Sourcing this reads data/raw/INEGI_censo_sexo_estado.xlsx and defines:
#   census_sex           sex shares (names "1" male, "2" female)
#   CENSUS_REGION_SHARE  state shares, named by Netquest region code
#   REGION_LABELS        state names, named by Netquest region code
#   POP_AGE              age-bracket percentages (names = AGE_LABELS)
#   POP_SEL_W1/_W2       SEL percentages in each wave's collapsed coding
#   SEL_LABELS_W1/_W2    SEL labels
#   age_bracket()        integer age -> AGE_LABELS factor
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

suppressPackageStartupMessages({
  library(dplyr)
  library(readxl)
})

# ── Sampling frame ────────────────────────────────────────────────────────────
# Four states were excluded from the survey frame (see code/fielding/census_quotas.R).
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
# data/raw/ is gitignored, so this file is local-only. Fail loudly here rather
# than inside read_excel(), since the analysis stage now depends on it.
CENSUS_XLSX <- "data/raw/INEGI_censo_sexo_estado.xlsx"
if (!file.exists(CENSUS_XLSX)) {
  stop(
    CENSUS_XLSX,
    " is missing. It is an untracked raw input (see .gitignore) needed by ",
    "code/census_comparison.R and code/vote_update_weighted.R.",
    call. = FALSE
  )
}

census <- read_excel(CENSUS_XLSX) %>%
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

age_bracket <- function(age) {
  cut(
    suppressWarnings(as.integer(age)),
    breaks = AGE_BREAKS,
    labels = AGE_LABELS,
    right = FALSE
  )
}
