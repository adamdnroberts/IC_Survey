# analysis.R
# Main analysis script implementing the pre-analysis plan (PAP):
# "Beyond Borders: Evaluating the Efficacy of Sub-National Informational Comparisons"
#
# Sections:
#   1.  Setup
#   2.  Load data
#   3.  Exclusions and treatment indicators
#   4.  Outcome construction (ΔY = Y_post - Y_pre)
#   5.  Vote intention binary
#   6.  Crime importance (C_i)
#   7.  Wrongness (W_i) — primary (rank-based) and alternative (log-based)
#   8.  Attention check filter
#   9.  Primary analysis — Equation 2 (OLS, HC2 SEs)
#   10. Adaptive shrinkage (ash)
#   11. RQ tests (Wald contrasts)
#   12. Secondary analysis — crime importance moderation, Equation 3
#   13. Robustness: attention-check passers only
#   14. Output

# ── 1. Setup ──────────────────────────────────────────────────────────────────

library(dplyr)
library(tidyr)
library(estimatr) # lm_robust() with HC2 SEs
library(ashr) # adaptive shrinkage
library(ggplot2)
library(sf)

# ── 2. Load data ───────────────────────────────────────────────────────────────

# Run pull_responses.R first to fetch data from S3 and save data/responses.rds
if (!file.exists("data/responses.rds")) {
  stop("data/responses.rds not found. Run pull_responses.R first.")
}
d <- readRDS("data/responses.rds")

# Reference data
d_geo <- st_read("data/00mun_simplified.geojson", quiet = TRUE) |>
  st_drop_geometry()
robo_data <- readRDS("data/robo_2025.rds")
load("data/magar2024_coalitions.Rdata") # loads governing_party df

# Standardise the CVEGEO key in robo_data (zero-pad to 5 chars if needed)
robo_data <- robo_data |>
  mutate(CVEGEO = formatC(Cve..Municipio, width = 5, flag = "0", format = "d"))

# ── 3. Exclusions and treatment indicators ─────────────────────────────────────

# Exclude respondents without a valid home municipality
d <- d |> filter(!is.na(Found_Municipality) & Found_Municipality != "")

# Treatment factor and dummies (control = baseline)
d <- d |>
  mutate(
    Treatment_Group = factor(
      Treatment_Group,
      levels = c("control", "T1", "T2", "T3", "T4")
    ),
    T1 = as.integer(Treatment_Group == "T1"),
    T2 = as.integer(Treatment_Group == "T2"),
    T3 = as.integer(Treatment_Group == "T3"),
    T4 = as.integer(Treatment_Group == "T4")
  )

# ── 4. Outcome construction ────────────────────────────────────────────────────

# Crime-rank categories to numeric (1 = comp has most crime; 5 = comp has least crime)
rank_cat_to_num <- function(x) {
  case_when(
    x == "more_than_double" ~ 1L,
    x == "more" ~ 2L,
    x == "same" ~ 3L,
    x == "fewer" ~ 4L,
    x == "less_than_half" ~ 5L,
    TRUE ~ NA_integer_
  )
}

d <- d |>
  mutate(
    # Continuous outcomes (0-100 sliders)
    dY_home = Home_Crime_Handling_Post - Home_Crime_Handling_Pre,
    dY_morena = MORENA_Crime_Rating_Post - MORENA_Crime_Rating_Pre,
    dY_nonmorena = Coalition_PAN_PRI_PRD_Crime_Rating_Post -
      Coalition_PAN_PRI_PRD_Crime_Rating_Pre,
    dY_mc = Coalition_MC_Crime_Rating_Post - Coalition_MC_Crime_Rating_Pre,
    dY_turnout = Turnout_Likelihood_Post - Turnout_Likelihood_Pre,
    # Relative robbery ranking: average change in perceived rank across comparison munis
    rank_pre_1 = rank_cat_to_num(Crime_Rank_Comp_1),
    rank_pre_2 = rank_cat_to_num(Crime_Rank_Comp_2),
    rank_pre_3 = rank_cat_to_num(Crime_Rank_Comp_3),
    rank_pre_4 = rank_cat_to_num(Crime_Rank_Comp_4),
    rank_post_1 = rank_cat_to_num(Crime_Rank_Comp_1_Post),
    rank_post_2 = rank_cat_to_num(Crime_Rank_Comp_2_Post),
    rank_post_3 = rank_cat_to_num(Crime_Rank_Comp_3_Post),
    rank_post_4 = rank_cat_to_num(Crime_Rank_Comp_4_Post),
    rank_pre_avg = rowMeans(
      cbind(rank_pre_1, rank_pre_2, rank_pre_3, rank_pre_4),
      na.rm = TRUE
    ),
    rank_post_avg = rowMeans(
      cbind(rank_post_1, rank_post_2, rank_post_3, rank_post_4),
      na.rm = TRUE
    ),
    dY_rank = rank_post_avg - rank_pre_avg
  )

# ── 5. Vote intention binary ───────────────────────────────────────────────────

# Coalitions
coalition_a <- c("MORENA", "PT", "PVEM")
coalition_b <- c("PAN", "PRI", "PRD", "MC")

# Join governing coalition to home municipality
# governing_party is derived from magar2024_coalitions.Rdata (real data).
gov_party_df <- governing_party |>
  select(CVEGEO, governing_party, coalition_label)

d <- d |>
  left_join(gov_party_df, by = c("Found_Municipality_ID" = "CVEGEO"))

# Binary: 1 if post vote intention includes any party in the governing coalition
vote_includes_coalition <- function(vote_str, coalition) {
  if (is.na(vote_str) || vote_str == "") {
    return(NA_integer_)
  }
  parties <- trimws(strsplit(vote_str, ";")[[1]])
  as.integer(any(parties %in% coalition))
}

d <- d |>
  rowwise() |>
  mutate(
    incumbent_coalition = if_else(
      coalition_label == "A",
      list(coalition_a),
      list(coalition_b)
    ),
    dY_vote_pre = vote_includes_coalition(
      Vote_Intention_Pre,
      unlist(incumbent_coalition)
    ),
    dY_vote_post = vote_includes_coalition(
      Vote_Intention_Post,
      unlist(incumbent_coalition)
    ),
    dY_vote = dY_vote_post - dY_vote_pre
  ) |>
  ungroup()

# ── 6. Crime importance (C_i) ─────────────────────────────────────────────────

# C_i = 6 - rank(crime); higher = crime more salient for this respondent
d <- d |>
  mutate(
    C_i = 6L - Importance_Crime,
    C_i_std = as.numeric(scale(C_i))
  )

# ── 7. Wrongness (W_i) ────────────────────────────────────────────────────────

winsorize <- function(x, probs = c(0.01, 0.99)) {
  q <- quantile(x, probs, na.rm = TRUE)
  pmin(pmax(x, q[1]), q[2])
}

# --- Primary W_i: rank-based ---
# R_prior: home's perceived rank among the 5 displayed munis (1 = home has fewest crimes)
# Derived from pre-treatment comparison ranking: count how many comp munis have
# LESS crime than home (each such comp worsens home's rank by 1).
d <- d |>
  mutate(
    n_comp_fewer = rowSums(
      cbind(
        rank_pre_1 > 3, # "fewer" or "less_than_half" → comp < home
        rank_pre_2 > 3,
        rank_pre_3 > 3,
        rank_pre_4 > 3
      ),
      na.rm = TRUE
    ),
    R_prior = n_comp_fewer + 1L # 1 = best (fewest crimes); 5 = worst
  )

# R_actual: home's actual rank among home + 4 comparison munis by 2025 robbery count
# (rank 1 = fewest robberies = best performance)
robo_lookup <- robo_data |> select(CVEGEO, robos)

get_actual_rank <- function(home_id, c1, c2, c3, c4) {
  ids <- c(home_id, c1, c2, c3, c4)
  counts <- robo_lookup$robos[match(ids, robo_lookup$CVEGEO)]
  if (all(is.na(counts))) {
    return(NA_real_)
  }
  rank(counts, ties.method = "average", na.last = "keep")[1]
}

d <- d |>
  rowwise() |>
  mutate(
    R_actual = get_actual_rank(
      Found_Municipality_ID,
      Comparison_Muni_1_ID,
      Comparison_Muni_2_ID,
      Comparison_Muni_3_ID,
      Comparison_Muni_4_ID
    )
  ) |>
  ungroup()

d <- d |>
  mutate(
    W_i_raw = R_actual - R_prior,
    W_i_wins = winsorize(W_i_raw),
    W_i = as.numeric(scale(W_i_wins)) # standardised; used in all models
  )

# --- Alternative W_i: log-based (PAP robustness check) ---
d <- d |>
  left_join(
    robo_lookup |> rename(robos_home = robos),
    by = c("Found_Municipality_ID" = "CVEGEO")
  ) |>
  mutate(
    W_i_alt_raw = log(robos_home + 1) - log(pmax(Robbery_Estimate, 1)),
    W_i_alt_wins = winsorize(W_i_alt_raw),
    W_i_alt = as.numeric(scale(W_i_alt_wins))
  )

# ── 8. Attention check filter ─────────────────────────────────────────────────

d_full <- d
d_attn <- d |> filter(Attention_Check == "Somewhat agree")

# ── 9. Primary analysis — Equation 2 ──────────────────────────────────────────
#
# ΔY_i = α + Σ δ_k T_ik + γ W_i + Σ β_k (W_i × T_ik) + ε_i
#
# β_k are the key coefficients (differential belief updating per unit wrongness)

outcomes <- c(
  "dY_home",
  "dY_morena",
  "dY_nonmorena",
  "dY_mc",
  "dY_turnout",
  "dY_rank",
  "dY_vote"
)
outcome_labels <- c(
  "Home crime handling",
  "MORENA coalition",
  "Non-MORENA coalition",
  "MC",
  "Turnout",
  "Relative robbery ranking",
  "Vote intention (incumbent)"
)

fit_eq2 <- function(outcome, data) {
  fmla <- as.formula(paste0(
    outcome,
    " ~ T1 + T2 + T3 + T4 + W_i +",
    " W_i:T1 + W_i:T2 + W_i:T3 + W_i:T4"
  ))
  lm_robust(fmla, data = data, se_type = "HC2")
}

models <- setNames(
  lapply(outcomes, fit_eq2, data = d_full),
  outcomes
)

# ── 10. Adaptive shrinkage (ash) ───────────────────────────────────────────────
# Applied to β_k (interaction coefficients) across all 7 outcomes for each arm.

extract_betas <- function(model, label) {
  nms <- c("W_i:T1", "W_i:T2", "W_i:T3", "W_i:T4")
  idx <- match(nms, names(coef(model)))
  data.frame(
    outcome = label,
    arm = c("T1", "T2", "T3", "T4"),
    beta_hat = coef(model)[idx],
    se = model$std.error[idx],
    row.names = NULL
  )
}

beta_df <- do.call(rbind, Map(extract_betas, models, outcome_labels))

apply_ash <- function(arm_label) {
  sub <- beta_df |> filter(arm == arm_label)
  fit <- ash(sub$beta_hat, sub$se, mixcompdist = "normal")
  data.frame(
    outcome = sub$outcome,
    arm = arm_label,
    beta_hat = sub$beta_hat,
    se = sub$se,
    beta_post = fit$result$PosteriorMean,
    lfsr = fit$result$lfsr,
    row.names = NULL
  )
}

ash_results <- do.call(rbind, lapply(c("T1", "T2", "T3", "T4"), apply_ash))

# ── 11. RQ tests — Wald contrasts on primary outcome (dY_home) ────────────────

wald_test <- function(model, label, contrast) {
  # contrast: named numeric vector (e.g. c("W_i:T2" = 1, "W_i:T1" = -1))
  b <- coef(model)[names(contrast)]
  V <- vcov(model)[names(contrast), names(contrast)]
  ct <- as.numeric(contrast %*% b)
  se <- sqrt(as.numeric(contrast %*% V %*% contrast))
  data.frame(
    test = label,
    estimate = ct,
    se = se,
    p_value = 2 * pnorm(-abs(ct / se))
  )
}

pm <- models[["dY_home"]]
rq_tests <- rbind(
  wald_test(pm, "RQ1: β_T1 = 0", c("W_i:T1" = 1)),
  wald_test(pm, "RQ2: β_T2 − β_T1 = 0", c("W_i:T1" = -1, "W_i:T2" = 1)),
  wald_test(pm, "RQ3: β_T3 − β_T2 = 0", c("W_i:T2" = -1, "W_i:T3" = 1)),
  wald_test(pm, "RQ4: β_T4 − β_T3 = 0", c("W_i:T3" = -1, "W_i:T4" = 1))
)

# ── 12. Secondary analysis — crime importance moderation (Equation 3) ──────────
# Exploratory (RQ7); primary outcome only

model_crime_importance <- lm_robust(
  dY_home ~
    T1 +
      T2 +
      T3 +
      T4 +
      W_i +
      W_i:T1 +
      W_i:T2 +
      W_i:T3 +
      W_i:T4 +
      C_i_std +
      C_i_std:T1 +
      C_i_std:T2 +
      C_i_std:T3 +
      C_i_std:T4,
  data = d_full,
  se_type = "HC2"
)

# ── 13. Robustness — alternative W_i and attention-check passers ───────────────

# Refit primary models with log-based W_i
fit_eq2_alt <- function(outcome, data) {
  fmla <- as.formula(paste0(
    outcome,
    " ~ T1 + T2 + T3 + T4 + W_i_alt +",
    " W_i_alt:T1 + W_i_alt:T2 + W_i_alt:T3 + W_i_alt:T4"
  ))
  lm_robust(fmla, data = data, se_type = "HC2")
}

models_alt <- setNames(lapply(outcomes, fit_eq2_alt, data = d_full), outcomes)
models_attn <- setNames(lapply(outcomes, fit_eq2, data = d_attn), outcomes)

# ── 14. Output ─────────────────────────────────────────────────────────────────

cat("\n=== Sample sizes ===\n")
cat("Full sample:              ", nrow(d_full), "\n")
cat("Attention-check passers:  ", nrow(d_attn), "\n")
cat("By treatment arm:\n")
print(table(d_full$Treatment_Group))

cat("\n=== Primary RQ tests (dY_home, HC2 SEs) ===\n")
print(rq_tests, digits = 3)

cat("\n=== ASH results — posterior means and lfsr across all outcomes ===\n")
print(ash_results, digits = 3)

cat("\n=== Crime importance moderation (exploratory, RQ7) ===\n")
print(summary(model_crime_importance))
