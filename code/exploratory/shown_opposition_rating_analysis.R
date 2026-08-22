# Does the opposite-coalition comparison treatment (T3) change beliefs about the
# OPPOSITION COALITION THAT WAS SHOWN IN THE TREATMENT?
#
# Existing scripts get close but never condition on the treatment target:
#   - coalition_rating_post_analysis.R regresses the three fixed coalition
#     ratings on arm, pooling T3 respondents shown PAN/PRI/PRD with those shown
#     MORENA/PVEM/PT.
#   - home_party_update_analysis.R builds "opposition" benchmarks from
#     home_coalition (the coalitions that do not govern the home municipality),
#     which is a respondent attribute, not a treatment attribute.
#
# ── What T3 shows ────────────────────────────────────────────────────────────
# app_wave2.R draws T3's four comparison municipalities from a SINGLE opposite
# coalition and names it in the treatment text ("municipios similares gobernados
# por PAN/PRI/PRD"), with party labels on the bar chart. So every T3 respondent
# has exactly one named opposition target.
#
# ── Defining that target for the control arms ────────────────────────────────
# The pooled control group never saw a named opposition coalition, so the target
# has to be the one T3 WOULD have named. That is recoverable, because
# get_opposite_parties() is deterministic: coalition A (MORENA/PVEM/PT) maps to
# coalition B (PAN/PRI/PRD) and vice versa. The single exception is MC home
# municipalities, where app_wave2.R draws one of the two at random
# (mc_opp_coalition_rv):
#   - home coalition is MORENA/PVEM/PT or PAN/PRI/PRD -> target is the other one
#   - home coalition is MC, and the arm's draw was opposite-coalition by design
#     (T3, or control/T1 in the "opposite" cell) -> target is the coalition
#     governing those municipalities, which reveals the draw
#   - home coalition is MC otherwise -> the draw is unobserved, so the target
#     rating is the mean of both non-home coalitions, which is its expectation
#     over the 50/50 draw. Flagged by target_is_expectation so it can be dropped.
# The MC exception is not applied to nonpartisan arms even when their displayed
# municipalities happen to fall in one coalition: that is coincidence, not the
# T3 draw.
#
# Counterpart coalition = the non-home coalition that is NOT the target, i.e.
# the out-party T3 said nothing about. Differencing target against counterpart
# gives a within-respondent placebo that nets out any general drift in how
# respondents rate out-parties between waves (model C).
#
# ── Pooling the controls ─────────────────────────────────────────────────────
# The baseline is control + T1 + control2 pooled. All three withhold crime
# information about the comparison municipalities; they differ in what else was
# on screen (control/T1 name comparison municipalities drawn from a randomly
# assigned cell -- nonpartisan / opposite / same -- and control2 adds a rainfall
# comparison chart). Those differences are randomized, so pooling is unbiased
# for the T3-vs-no-crime-information contrast, at the cost of a more
# heterogeneous control group. Model F re-runs the headline contrast against the
# control/T1 "opposite" cell alone, where the municipalities named on screen are
# drawn exactly as T3's, as the stricter comparison.
#
# T2 is not pooled into either side: its comparisons are drawn party-blind, so
# respondents see the target coalition in a dose that varies 0-4 at random, and
# neither its text nor its chart mentions party. It gets a dose model (model G).
#
# ── Which coefficient carries the hypothesis ─────────────────────────────────
# The main analyses interact treatment with log_crime_gap, the surprise about
# the HOME municipality's robbery rate. For beliefs about the comparison
# coalition the relevant surprise is rank_gap (actual_rank - rank_prior): how
# the home municipality actually ranked against the shown comparisons versus
# where the respondent expected it to rank. A positive rank_gap means the
# comparisons turned out safer than expected, i.e. the news flatters the
# opposition. rank_gap x T3 is the coefficient of interest; log_crime_gap x T3
# is carried along for comparability with the main spec.
#
# Sign convention: ratings are 0-100 sliders, higher = handles crime better, so
# a positive outcome means the target coalition was rated better after treatment
# than before.
#
# Outputs: printed model summaries, plus
#   latex/images/opp_target_rating_coef_plot.pdf

library(estimatr)
library(dplyr)
library(ggplot2)
library(broom)

load("data/derived/survey_panel_dataset.Rdata")

if (!exists("ci_alpha")) {
  ci_alpha <- 0.01
}

num <- function(x) suppressWarnings(as.numeric(x))
pad5 <- function(x) sprintf("%05d", as.integer(x))

# ── Coalition governing each comparison municipality ─────────────────────────
# Same municipality -> coalition mapping used in create_panel_dataset.R. The
# panel stores the comparison municipality IDs but not their coalitions, so the
# lookup is rebuilt here rather than read from a stored column.
load("data/magar2024_coalitions.Rdata")
coalition_lookup <- magar2024 %>%
  mutate(
    muni_id = sprintf("%05d", inegi),
    coalition = case_when(
      grepl("morena|pvem|pt", l01) ~ "MORENA/PVEM/PT",
      grepl("pan|pri|prd", l01) ~ "PAN/PRI/PRD",
      grepl("mc", l01) ~ "MC",
      TRUE ~ NA_character_
    )
  )
coalition_vec <- setNames(coalition_lookup$coalition, coalition_lookup$muni_id)

comp_coalition <- sapply(1:4, function(i) {
  unname(coalition_vec[pad5(panel[[paste0("Comparison_Muni_", i, "_ID")]])])
})

coalition_names <- c("MORENA/PVEM/PT", "PAN/PRI/PRD", "MC")

rating_post <- cbind(
  "MORENA/PVEM/PT" = num(panel$MORENA_Crime_Rating_Post),
  "PAN/PRI/PRD" = num(panel$Coalition_PAN_PRI_PRD_Crime_Rating_Post),
  "MC" = num(panel$MC_Crime_Rating_Post)
)

rating_pre <- cbind(
  "MORENA/PVEM/PT" = num(panel$MORENA_Crime_Rating_Pre),
  "PAN/PRI/PRD" = num(panel$Coalition_PAN_PRI_PRD_Crime_Rating_Pre),
  "MC" = num(panel$MC_Crime_Rating_Pre)
)

# A coalition's rating is usable only if BOTH waves are observed, so every
# pre/post pair below runs over the same coalitions and their difference is a
# genuine within-respondent update rather than a change in composition.
rating_ok <- !is.na(rating_pre) & !is.na(rating_post)

# Arms whose comparison municipalities are drawn from the opposite coalition by
# design, so the displayed municipalities reveal the draw for MC home cases.
opposite_by_design <- panel$Treatment_Group == "T3" |
  (panel$Treatment_Group %in% c("control", "T1") &
    !is.na(panel$Control_T1_Comp_Type) &
    panel$Control_T1_Comp_Type == "opposite")

# ── Per-respondent target and counterpart coalitions ─────────────────────────
target_coalition <- rep(NA_character_, nrow(panel))
counterpart_coalition <- rep(NA_character_, nrow(panel))
target_is_expectation <- rep(FALSE, nrow(panel))

for (i in seq_len(nrow(panel))) {
  hc <- panel$home_coalition[i]
  if (is.na(hc)) {
    next
  }
  if (hc == "MORENA/PVEM/PT") {
    target_coalition[i] <- "PAN/PRI/PRD"
  } else if (hc == "PAN/PRI/PRD") {
    target_coalition[i] <- "MORENA/PVEM/PT"
  } else if (opposite_by_design[i]) {
    shown <- comp_coalition[i, ]
    shown <- unique(shown[!is.na(shown) & shown != hc])
    if (length(shown) == 1) {
      target_coalition[i] <- shown
    }
  } else {
    target_is_expectation[i] <- TRUE
  }
  if (!is.na(target_coalition[i])) {
    counterpart_coalition[i] <- setdiff(
      coalition_names,
      c(hc, target_coalition[i])
    )
  }
}

panel$target_coalition <- target_coalition
panel$counterpart_coalition <- counterpart_coalition
panel$target_is_expectation <- target_is_expectation

# Rating of a named coalition, per respondent (NA unless both waves observed).
rating_of <- function(mat, coalition) {
  vapply(
    seq_len(nrow(panel)),
    function(i) {
      k <- coalition[i]
      if (is.na(k) || !rating_ok[i, k]) NA_real_ else mat[i, k]
    },
    numeric(1)
  )
}

# Expectation fallback for MC home municipalities in arms that do not reveal the
# draw: the mean of both non-home coalitions.
mean_non_home <- function(mat) {
  vapply(
    seq_len(nrow(panel)),
    function(i) {
      hc <- panel$home_coalition[i]
      if (is.na(hc)) {
        return(NA_real_)
      }
      k <- setdiff(coalition_names, hc)
      k <- k[rating_ok[i, k]]
      if (length(k) == 0) NA_real_ else mean(mat[i, k])
    },
    numeric(1)
  )
}

panel$Target_Pre <- ifelse(
  target_is_expectation,
  mean_non_home(rating_pre),
  rating_of(rating_pre, target_coalition)
)
panel$Target_Post <- ifelse(
  target_is_expectation,
  mean_non_home(rating_post),
  rating_of(rating_post, target_coalition)
)
panel$Target_Change <- panel$Target_Post - panel$Target_Pre

# Counterpart is undefined wherever the target is only an expectation.
panel$Counterpart_Pre <- rating_of(rating_pre, counterpart_coalition)
panel$Counterpart_Change <- rating_of(rating_post, counterpart_coalition) -
  panel$Counterpart_Pre

# Within-respondent placebo: target net of the out-party the treatment said
# nothing about. Positive => the named coalition gained relative to it.
panel$Target_Minus_Counterpart <- panel$Target_Change -
  panel$Counterpart_Change

# Did the target gain ground on the respondent's own governing coalition?
# Home_Party_Crime_Handling_Change is built in create_panel_dataset.R.
panel$Target_Minus_Home_Party <- panel$Target_Change -
  panel$Home_Party_Crime_Handling_Change

# How many displayed municipalities were governed by the target coalition (0-4).
# Fixed at 4 in T3 by construction; random in the party-blind arms.
panel$n_target_munis_shown <- vapply(
  seq_len(nrow(panel)),
  function(i) {
    k <- target_coalition[i]
    if (is.na(k)) NA_real_ else sum(comp_coalition[i, ] == k, na.rm = TRUE)
  },
  numeric(1)
)

# Knew the governing party of at least one comparison municipality. Believing
# the displayed municipalities are opposition-run is a precondition for the
# partisan-attribution channel; comp_party_known is built in
# create_panel_dataset.R by scoring Comp_Governing_Party_Belief_* against truth.
panel$knew_comp_party <- as.integer(panel$comp_party_known > 0)

# Base restrictions match belief_update_analysis.R.
base <- filter(panel, muni_changed == 0, Attention_Check == "somewhat_agree")

# ── Diagnostics ──────────────────────────────────────────────────────────────
cat("\n===== Target coalition, by home coalition =====\n")
print(table(base$home_coalition, base$target_coalition, useNA = "ifany"))

cat("\n===== Target identified from the draw vs. expectation =====\n")
print(table(base$Treatment_Group, base$target_is_expectation))

cat("\n===== Municipalities shown from the target coalition, by arm =====\n")
print(table(base$Treatment_Group, base$n_target_munis_shown, useNA = "ifany"))
# T3 should be 4 across the board. T4 should be 0 (same-coalition comparisons).
# T2, control2 and the nonpartisan/opposite/same cells of control and T1 vary.

# ── Sample: T3 versus the pooled controls ────────────────────────────────────
matched <- base %>%
  filter(Treatment_Group %in% c("control", "control2", "T1", "T3")) %>%
  mutate(
    saw_crime_comparison = as.integer(Treatment_Group == "T3"),
    arm = if_else(Treatment_Group == "T3", "T3", "pooled control")
  )

cat("\n===== Pooled-control sample =====\n")
print(table(matched$arm, matched$Treatment_Group))
cat("Outcome defined (target):              ",
    sum(!is.na(matched$Target_Change)), "\n")
cat("Outcome defined (target - counterpart):",
    sum(!is.na(matched$Target_Minus_Counterpart)), "\n")

# ── Model A: did T3 move the named opposition's rating at all? ───────────────
m_a <- lm_robust(
  Target_Change ~ saw_crime_comparison + Target_Pre + coalition_pre,
  data = matched,
  alpha = ci_alpha,
  se_type = "HC2"
)

cat("\n===== A. T3 vs pooled controls (control + control2 + T1) =====\n")
print(summary(m_a))

# ── Model B: the effect should run through the ranking surprise ─────────────
m_b <- lm_robust(
  Target_Change ~ rank_gap * saw_crime_comparison +
    log_crime_gap * saw_crime_comparison + Target_Pre + coalition_pre,
  data = matched,
  alpha = ci_alpha,
  se_type = "HC2"
)

cat("\n===== B. T3 x surprise (rank_gap is the hypothesis-carrying term) =====\n")
print(summary(m_b))

# ── Model C: within-respondent placebo ──────────────────────────────────────
m_c <- lm_robust(
  Target_Minus_Counterpart ~ rank_gap * saw_crime_comparison +
    log_crime_gap * saw_crime_comparison + Target_Pre + Counterpart_Pre +
    coalition_pre,
  data = matched,
  alpha = ci_alpha,
  se_type = "HC2"
)

cat("\n===== C. Within-respondent placebo: target minus counterpart out-party =====\n")
print(summary(m_c))

# ── Model D: relative to the respondent's own governing coalition ────────────
m_d <- lm_robust(
  Target_Minus_Home_Party ~ rank_gap * saw_crime_comparison +
    log_crime_gap * saw_crime_comparison + Target_Pre + coalition_pre,
  data = matched,
  alpha = ci_alpha,
  se_type = "HC2"
)

cat("\n===== D. Target minus home party =====\n")
print(summary(m_d))

# ── Model E: does it require knowing who governs the comparisons? ───────────
m_e <- lm_robust(
  Target_Change ~ saw_crime_comparison * knew_comp_party + Target_Pre +
    coalition_pre,
  data = matched,
  alpha = ci_alpha,
  se_type = "HC2"
)

cat("\n===== E. Moderation by knowledge of the comparison municipalities' party =====\n")
print(summary(m_e))

# ── Model F: stricter control group ─────────────────────────────────────────
# Only the control/T1 respondents randomly assigned the "opposite" cell, whose
# comparison municipalities are drawn exactly as T3's and named on screen. Same
# estimand as model B, cleaner comparison, smaller N.
strict <- matched %>%
  filter(
    Treatment_Group == "T3" |
      (Treatment_Group %in% c("control", "T1") &
        !is.na(Control_T1_Comp_Type) &
        Control_T1_Comp_Type == "opposite")
  )

cat("\n===== F. Strict control: control/T1 'opposite' cell only =====\n")
print(table(strict$Treatment_Group))

m_f <- lm_robust(
  Target_Change ~ rank_gap * saw_crime_comparison +
    log_crime_gap * saw_crime_comparison + Target_Pre + coalition_pre,
  data = strict,
  alpha = ci_alpha,
  se_type = "HC2"
)
print(summary(m_f))

# ── Coefficient plot: the T3 contrast across outcomes ───────────────────────
# Interactions are rescaled to a 1 SD change in the moderator, as in
# belief_update_analysis.R.
log_crime_gap_sd <- sd(matched$log_crime_gap, na.rm = TRUE)
rank_gap_sd <- sd(matched$rank_gap, na.rm = TRUE)

# `note` names what each row isolates, so the plot can be read without the
# model summaries alongside it. It goes on a second line of the axis label.
pull_interactions <- function(model, label, note) {
  tidy(model, conf.int = TRUE) %>%
    filter(grepl("saw_crime_comparison", term), grepl(":", term)) %>%
    mutate(
      outcome = sprintf("%s\n%s (N=%d)", label, note, model$nobs),
      group = if_else(grepl("log_crime_gap", term), "CG × T3", "RG × T3"),
      sd = if_else(group == "CG × T3", log_crime_gap_sd, rank_gap_sd),
      across(c(estimate, conf.low, conf.high, std.error), ~ . * sd),
      conf.low95 = estimate - qt(0.975, df) * std.error,
      conf.high95 = estimate + qt(0.975, df) * std.error
    )
}

coef_all <- bind_rows(
  pull_interactions(m_b, "Target coalition", "headline outcome"),
  pull_interactions(m_c, "Target − counterpart", "within-respondent placebo"),
  pull_interactions(m_d, "Target − home party", "relative repositioning"),
  pull_interactions(m_f, "Target, strict control", "robustness to pooling")
) %>%
  mutate(outcome = factor(outcome, levels = rev(unique(outcome))))

opp_target_coef_plot <- ggplot(coef_all, aes(y = outcome, x = estimate)) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey50") +
  geom_errorbar(
    aes(xmin = conf.low, xmax = conf.high),
    orientation = "y",
    width = 0,
    linewidth = 0.5,
    color = "#E69F00"
  ) +
  geom_errorbar(
    aes(xmin = conf.low95, xmax = conf.high95),
    orientation = "y",
    width = 0,
    linewidth = 1.2,
    color = "#E69F00"
  ) +
  geom_point(size = 2.5, color = "#E69F00") +
  facet_wrap(~group) +
  labs(
    title = "Updating about the opposition coalition named in the treatment",
    subtitle = "T3 vs pooled controls (control + control2 + T1)",
    y = NULL,
    x = "Standardized coefficient (1 SD increase in predictor)",
    caption = "Thick bar 95% CI, thin 99% CI"
  ) +
  theme_minimal() +
  # Axis labels carry a second descriptor line; tighten the leading so the two
  # lines read as one label rather than two rows.
  theme(axis.text.y = element_text(size = 8, lineheight = 0.95))

print(opp_target_coef_plot)

ggsave(
  "latex/images/opp_target_rating_coef_plot.pdf",
  plot = opp_target_coef_plot,
  width = 8,
  height = 5
)

# ── Model G: T2's unlabeled, variable-dose exposure ─────────────────────────
# T2's comparisons are drawn party-blind, so the number governed by the target
# coalition varies 0-4 at random. That is a dose rather than a named target, and
# neither the treatment text nor the chart mentions party -- so this is a
# deliberately weak test, and only respondents who already knew who governs the
# displayed municipalities could update partisan beliefs from it. Baseline is
# the same pooled control group.
t2_sample <- base %>%
  filter(Treatment_Group %in% c("control", "control2", "T1", "T2")) %>%
  mutate(saw_crime_comparison = as.integer(Treatment_Group == "T2"))

cat("\n===== G. T2 dose sample: target municipalities shown =====\n")
print(table(t2_sample$Treatment_Group, t2_sample$n_target_munis_shown))

m_g <- lm_robust(
  Target_Change ~ n_target_munis_shown * saw_crime_comparison + Target_Pre +
    coalition_pre,
  data = t2_sample,
  alpha = ci_alpha,
  se_type = "HC2"
)

cat("\n===== G. T2 dose x arm =====\n")
print(summary(m_g))

m_g_known <- lm_robust(
  Target_Change ~ n_target_munis_shown * saw_crime_comparison * knew_comp_party +
    Target_Pre + coalition_pre,
  data = t2_sample,
  alpha = ci_alpha,
  se_type = "HC2"
)

cat("\n===== G. T2 dose x arm x knew comparison parties =====\n")
print(summary(m_g_known))