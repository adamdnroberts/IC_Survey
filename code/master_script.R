# Master script: runs all analysis for the main paper
# Run from project root directory

# ── 1. Data preparation ───────────────────────────────────────────────────────

source("code/pull_responses_wave1.R")
source("code/pull_responses_wave2.R")
source("code/create_panel_dataset.R")
source("code/build_benchmark_panel.R")

# ── 2. Benchmark municipality selection analysis ──────────────────────────────

source("code/benchmark_analysis.R")
source("code/benchmark_crime_analysis.R")

# ── 3. Manipulation checks ────────────────────────────────────────────────────

source("code/manipulation_check.R")

# ── 4. Main experimental analysis ─────────────────────────────────────────────

source("code/experimental_analysis.R")
source("code/belief_update_analysis.R")
source("code/vote_update_analysis.R")

# ── 5. Robustness and extensions ──────────────────────────────────────────────

source("code/belief_update_tobit_analysis.R")
source("code/belief_2sls.R")
source("code/triple_interaction_vote.R")
source("code/home_party_update_analysis.R")
