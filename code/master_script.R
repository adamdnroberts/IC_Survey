# Master script: runs all analysis for the main paper
# Run from project root directory
#
# latex/ is a git submodule (the Overleaf project). On a fresh clone run
#   git submodule update --init
# first, or every ggsave("latex/images/...") below will fail on a missing
# directory.

# ── 0. Derived data ───────────────────────────────────────────────────────────
# Built once from the raw shapefiles, crime counts and WorldClim rasters. Slow,
# so it is skipped when the outputs are already present. Delete any of the four
# files to force a rebuild.

derived_data <- c(
  "data/robo_2025.rds",
  "data/00mun_simplified.geojson",
  "data/precip_data.rds",
  "data/nearest10.rds"
)

if (!all(file.exists(derived_data))) {
  message(
    "Building derived data (missing: ",
    paste(basename(derived_data[!file.exists(derived_data)]), collapse = ", "),
    ")"
  )
  source("code/build_data.R")
} else {
  message("Derived data present; skipping code/build_data.R")
}

# ── 1. Data preparation ───────────────────────────────────────────────────────
# The S3 pulls are slow and only matter when new responses have come in, so
# they run when the response files are missing or when refresh_responses is
# TRUE. Fielding is closed, so the pulls are normally a no-op; flip the flag
# after any further data collection.

refresh_responses <- FALSE

response_files <- c("data/wave1_responses.rds", "data/wave2_responses.rds")

if (refresh_responses || !all(file.exists(response_files))) {
  source("code/pull_responses_wave1.R")
  source("code/pull_responses_wave2.R")
} else {
  message("Response files present; skipping S3 pulls (refresh_responses = FALSE)")
}

source("code/create_panel_dataset.R")
source("code/build_benchmark_panel.R")

# ── 2. Benchmark municipality selection analysis ──────────────────────────────

source("code/benchmark_analysis.R")        # comparison_coef_plot.pdf, benchmark_model.tex
source("code/benchmark_crime_analysis.R")

# ── 3. Manipulation checks ────────────────────────────────────────────────────

source("code/manipulation_check.R")

# ── 4. Main experimental analysis ─────────────────────────────────────────────

source("code/belief_update_analysis.R")
source("code/vote_update_analysis.R")      # vote_coef_update_log.pdf

# ── 5. Robustness and extensions ──────────────────────────────────────────────

source("code/belief_update_tobit_analysis.R")
source("code/belief_2sls.R")
source("code/triple_interaction_vote.R")   # vote_treatment_by_importance{,_factor}.pdf
source("code/home_party_update_analysis.R")# inc_vs_other_coalitions.tex

# ── 6. Remaining paper figures and tables ─────────────────────────────────────
# Everything else \includegraphics'd or \input by latex/main.tex and
# latex/appendix.tex. Grouped here because none of it feeds later steps.

source("code/priors_by_coalition.R")       # priors_ridge_plot.pdf, priors_qq_plot.pdf,
                                           # mean_crime_priors.pdf
source("code/plots/t4_belief_updates_comparison.R") # t4_belief_updates_comparison_cg.pdf
source("code/plots/t4_mediated_direct_by_gap.R")    # t4_mediated_direct_by_crimegap_full.pdf
# NB: the line above attaches MASS (via mediation), which masks dplyr::select
# for everything sourced after it. Scripts below qualify their select() calls.
source("code/equivalence_tests.R")         # balance_equivalence.tex
source("code/w2_party_knowledge.R")        # party_knowledge.tex
source("code/descriptive_coalitions.R")    # network_plot.pdf
source("code/test_dist_of_comparisons.R")  # rank_dist_plot.pdf

# Design-stage power analysis. Does not depend on the response data, but the
# appendix reports its two figures.
source("code/power_analysis/power_analysis_simulation_v2.R") # power_graph_t2_rw.pdf,
                                                             # power_graph_t1_cw.pdf

# ── Paper artifacts with no producing script ──────────────────────────────────
# These are referenced by the paper but cannot be regenerated from this repo:
#   images/benchmark_selection.png        hand-taken screenshot of the Shiny app
#   images/treatment_bar_graph_english.png  hand-taken screenshot of the Shiny app
#   images/comparison_posterior.pdf       produced by latex/create_posterior_plot.m
#   tables/sample_sel.tex                 hand-typed
#   tables/sample_sex_age.tex             hand-typed
#   tables/sample_region.tex              hand-typed
# The three sample_*.tex tables report quantities that code/census_comparison.R
# already computes into data/census_comparison_wave1.csv.
