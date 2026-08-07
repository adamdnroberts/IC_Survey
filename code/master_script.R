# master_script.R -- rebuild every dataset, figure and table in the paper.
#
#   Rscript code/master_script.R                 # data + analysis + figures
#   Rscript code/master_script.R figures         # any combination of stages
#   Rscript code/master_script.R build pull data analysis figures
#
# Run from the project root; every path in the project is relative to it.
#
# Each script runs in its own R process, so results cannot depend on what ran
# before. Neither objects nor the package search path carry over. This matters:
# sourcing these into one session leaves MASS (attached by mediation, via
# plots/t4_mediated_direct_by_gap.R) masking dplyr::select, which broke
# equivalence_tests.R with an "unused arguments" error rather than a wrong
# number. The same hazard is why scripts guarding on `if (!exists("ci_alpha"))`
# now always get their documented default instead of whatever a previous script
# happened to leave behind.
#
# latex/ is a git submodule (the Overleaf project). On a fresh clone run
#   git submodule update --init
# first, or every ggsave("latex/images/...") will fail on a missing directory.
#
# Stages: `build` regenerates the derived data from the raw shapefiles, crime
# counts and WorldClim rasters -- slow, and skipped automatically when its four
# outputs are present. `pull` re-fetches survey responses from S3; fielding is
# closed, so it is off by default and only runs when the response files are
# missing. `data`, `analysis` and `figures` need no network access.

# ---- Configuration ---------------------------------------------------------

STAGES <- list(
  build = list(
    label = "Stage 0 -- derived data from raw inputs",
    scripts = c(
      # Robbery statistics. Independent of the geodata below.
      "code/crime_data.R",
      # Simplified municipality geometry. Must precede the two that follow:
      # both read data/00mun_simplified.geojson.
      "code/make_geojson_file.R",
      "code/weather_data.R",
      "code/precompute_nearest10.R"
    )
  ),

  pull = list(
    label = "Stage 0b -- pull survey responses from S3",
    scripts = c(
      "code/pull_responses_wave1.R",
      "code/pull_responses_wave2.R"
    )
  ),

  data = list(
    label = "Stage 1 -- build the analysis panels",
    scripts = c(
      "code/create_panel_dataset.R",   # -> data/survey_panel_dataset.Rdata
      "code/build_benchmark_panel.R"   # -> data/benchmark_panel.rds
    )
  ),

  analysis = list(
    label = "Stage 2 -- main results",
    scripts = c(
      "code/benchmark_analysis.R",          # comparison_coef_plot.pdf,
                                            # benchmark_model.tex
      "code/benchmark_crime_analysis.R",
      "code/manipulation_check.R",
      "code/belief_update_analysis.R",
      "code/vote_update_analysis.R",        # vote_coef_update_log.pdf
      "code/belief_update_tobit_analysis.R",
      "code/belief_2sls.R",
      "code/triple_interaction_vote.R",     # vote_treatment_by_importance*.pdf
      "code/home_party_update_analysis.R"   # inc_vs_other_coalitions.tex
    )
  ),

  figures = list(
    label = "Stage 3 -- remaining paper figures and tables",
    scripts = c(
      "code/priors_by_coalition.R",         # priors_ridge_plot.pdf,
                                            # priors_qq_plot.pdf,
                                            # mean_crime_priors.pdf
      "code/plots/t4_belief_updates_comparison.R", # t4_belief_updates_comparison_cg.pdf
      "code/plots/t4_mediated_direct_by_gap.R",    # t4_mediated_direct_by_crimegap_full.pdf
      "code/equivalence_tests.R",           # balance_equivalence.tex
      "code/w2_party_knowledge.R",          # party_knowledge.tex
      "code/descriptive_coalitions.R",      # network_plot.pdf
      "code/test_dist_of_comparisons.R",    # rank_dist_plot.pdf
      # Design-stage power analysis. Uses no response data, but the appendix
      # reports its two figures.
      "code/power_analysis/power_analysis_simulation_v2.R" # power_graph_t2_rw.pdf,
                                                           # power_graph_t1_cw.pdf
    )
  )
)

DEFAULT_STAGES <- c("data", "analysis", "figures")

# Outputs that let `build` and `pull` be skipped when they are already present.
BUILD_OUTPUTS <- c(
  "data/robo_2025.rds",
  "data/00mun_simplified.geojson",
  "data/precip_data.rds",
  "data/nearest10.rds"
)
PULL_OUTPUTS <- c("data/wave1_responses.rds", "data/wave2_responses.rds")

# Packages the pipeline loads. Checked up front so a missing one fails in the
# first second rather than forty minutes in.
REQUIRED_PACKAGES <- c(
  "AER", "TOSTER", "brms", "broom", "data.table", "dplyr", "emmeans",
  "estimatr", "fixest", "geodata", "ggplot2", "ggraph", "ggrepel", "ggridges",
  "igraph", "lmtest", "mediation", "modelsummary", "paws.storage", "readxl",
  "sandwich", "sf", "terra", "tidyr"
)

# ---- Environment checks ----------------------------------------------------

check_root <- function() {
  root <- getwd()
  markers <- c("code/master_script.R", "data", "latex")
  missing <- markers[!file.exists(file.path(root, markers))]
  if (length(missing) > 0) {
    stop(
      "\n  The working directory does not look like the project root.\n",
      "    working directory : ", root, "\n",
      "    not found here    : ", paste(missing, collapse = ", "), "\n\n",
      "  cd into the folder containing code/ and try again.\n",
      "  (A missing latex/ usually means: git submodule update --init)\n",
      call. = FALSE
    )
  }
  normalizePath(root, winslash = "/", mustWork = TRUE)
}

check_packages <- function() {
  missing <- REQUIRED_PACKAGES[
    !vapply(REQUIRED_PACKAGES, requireNamespace, logical(1), quietly = TRUE)
  ]
  if (length(missing) > 0) {
    stop(
      "\n  Missing packages: ", paste(missing, collapse = ", "), "\n",
      '  install.packages(c("', paste(missing, collapse = '", "'), '"))\n',
      call. = FALSE
    )
  }
}

# ---- Runner ----------------------------------------------------------------

RSCRIPT <- file.path(R.home("bin"), "Rscript")

# Where each script's console output goes. Keeping it out of the terminal is
# what makes a failure visible: the summary at the end names the log to read.
LOG_DIR <- file.path(tempdir(), "ic_survey_logs")

run_script <- function(rel_path, root) {
  full <- file.path(root, rel_path)
  if (!file.exists(full)) {
    stop("script not found: ", rel_path, call. = FALSE)
  }

  log_file <- file.path(LOG_DIR, paste0(basename(rel_path), ".log"))
  message("  -> ", basename(rel_path), appendLF = FALSE)
  started <- Sys.time()

  status <- system2(
    RSCRIPT,
    args = shQuote(full),
    stdout = log_file,
    stderr = log_file
  )

  elapsed <- as.numeric(difftime(Sys.time(), started, units = "secs"))
  ok <- identical(as.integer(status), 0L)

  if (ok) {
    message(sprintf("  (%.1fs)", elapsed))
  } else {
    message("")
    message("     FAILED (exit ", status, ") -- see ", log_file)
    tail_lines <- tryCatch(
      readLines(log_file, warn = FALSE),
      error = function(e) character()
    )
    for (l in utils::tail(tail_lines, 8)) message("       | ", l)
  }

  list(script = rel_path, ok = ok, seconds = elapsed, log = log_file)
}

run_stage <- function(name, root) {
  stage <- STAGES[[name]]
  message("")
  message(strrep("=", 72))
  message(stage$label)
  message(strrep("=", 72))
  lapply(stage$scripts, run_script, root = root)
}

# `build` and `pull` are expensive and rarely needed, so they are added only
# when their outputs are missing -- unless the caller asked for them by name.
resolve_stages <- function(requested) {
  auto <- character()
  if (!"build" %in% requested && !all(file.exists(BUILD_OUTPUTS))) {
    message(
      "Derived data missing (",
      paste(basename(BUILD_OUTPUTS[!file.exists(BUILD_OUTPUTS)]), collapse = ", "),
      ") -- adding the build stage"
    )
    auto <- c(auto, "build")
  }
  if (!"pull" %in% requested && !all(file.exists(PULL_OUTPUTS))) {
    message("Response files missing -- adding the pull stage")
    auto <- c(auto, "pull")
  }
  ordered <- names(STAGES)
  ordered[ordered %in% c(auto, requested)]
}

# ---- Main ------------------------------------------------------------------

main <- function(stage_names) {
  unknown <- setdiff(stage_names, names(STAGES))
  if (length(unknown) > 0) {
    stop(
      "unknown stage(s): ", paste(unknown, collapse = ", "),
      "\n  valid stages: ", paste(names(STAGES), collapse = ", "),
      call. = FALSE
    )
  }

  root <- check_root()
  check_packages()
  stage_names <- resolve_stages(stage_names)

  dir.create(LOG_DIR, recursive = TRUE, showWarnings = FALSE)

  message("IC_Survey -- full rebuild")
  message("  root:   ", root)
  message("  stages: ", paste(stage_names, collapse = ", "))
  message("  logs:   ", LOG_DIR)
  message("  start:  ", format(Sys.time(), "%Y-%m-%d %H:%M:%S"))

  started <- Sys.time()
  results <- do.call(c, lapply(stage_names, run_stage, root = root))

  failed <- Filter(function(r) !r$ok, results)
  total <- as.numeric(difftime(Sys.time(), started, units = "mins"))

  message("")
  message(strrep("=", 72))
  message(sprintf(
    "%d of %d scripts succeeded in %.1f minutes",
    length(results) - length(failed), length(results), total
  ))
  if (length(failed) > 0) {
    message("")
    message("Failed:")
    for (f in failed) message("  ", f$script, "\n    log: ", f$log)
    message(strrep("=", 72))
    if (!interactive()) quit(status = 1, save = "no")
  }
  message(strrep("=", 72))
  invisible(results)
}

# ---- Paper artifacts with no producing script ------------------------------
# Referenced by the paper but not regenerable from this repo:
#   images/benchmark_selection.png          hand-taken screenshot of the app
#   images/treatment_bar_graph_english.png  hand-taken screenshot of the app
#   images/comparison_posterior.pdf         latex/create_posterior_plot.m (MATLAB)
#   tables/sample_sel.tex                   hand-typed
#   tables/sample_sex_age.tex               hand-typed
#   tables/sample_region.tex                hand-typed
# The three sample_*.tex tables report quantities code/census_comparison.R
# already computes into data/census_comparison_wave1.csv.

args <- commandArgs(trailingOnly = TRUE)
main(if (length(args) > 0) args else DEFAULT_STAGES)
