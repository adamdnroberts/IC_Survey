# Does the same-coalition comparison treatment (T4) change beliefs about the
# COALITION SHOWN IN THE TREATMENT -- which, for T4, is the respondent's own?
#
# ── Relationship to home_party_update_analysis.R ─────────────────────────────
# T4 draws its four comparison municipalities from the coalition that governs
# the respondent's home municipality (get_same_coalition_parties in
# app_wave2.R) and names it in the treatment text with party labels on the bar
# chart. So T4's target coalition is the home coalition, and the natural
# outcome is Home_Party_Crime_Handling_Change -- which
# home_party_update_analysis.R already regresses on
#   log_crime_gap * Treatment_Group + rank_gap * Treatment_Group + coalition_pre
# along with home-party-minus-incumbent, post-on-post home-party-vs-opposition
# contrasts, a T1-dropped refit, and a test of CG x T4 against the other arms.
# None of that is repeated here. Read that script for the main T4 results.
#
# This script adds only what that one cannot do, all of it design-side:
#
#   1. A no-information placebo. app_wave2.R assigns control and T1 respondents
#      a comparison type at random (control_t1_comp_type: "nonpartisan" /
#      "opposite" / "same") and NAMES those municipalities in the governance
#      grid and crime-ranking questions -- they simply never receive crime
#      information about them. The "same" cell is therefore a placebo for T4:
#      identical draw, identical names on screen, no crime comparison.
#      home_party_update_analysis.R treats control and T1 as undifferentiated.
#   2. control2 in the control group. home_party_update_analysis.R excludes it;
#      here control + T1 + control2 are pooled as the no-crime-information
#      baseline, with the strict "same"-cell comparison retained as model F.
#   3. A dose model for the party-blind arms (model G), where the number of
#      displayed municipalities governed by the home coalition varies 0-4 at
#      random rather than being fixed at 4.
#
# It is the T4 counterpart of shown_opposition_rating_analysis.R and follows the
# same structure, with one simplification: T4's target is the home coalition,
# which is always observed, so none of the MC-home random-draw machinery in that
# script is needed here.
#
# ── What the treatment could do ─────────────────────────────────────────────
# T4's comparisons are co-partisans of the home incumbent, so the news bears on
# the coalition BRAND rather than on an out-party. Two outcomes separate the
# possibilities: the brand rating itself (model B), and the brand net of the two
# out-parties, neither of which was mentioned (model C) -- the within-respondent
# placebo that nets out general drift in how respondents rate parties between
# waves.
#
# ── Which coefficient carries the hypothesis ────────────────────────────────
# As in the T3 script, rank_gap (actual_rank - rank_prior) is the surprise that
# bears on the comparisons: positive means the displayed municipalities turned
# out safer than the respondent expected. Under T4 those municipalities are
# co-partisans, so a positive rank_gap is good news about the respondent's own
# coalition, and the prediction is a positive rank_gap x T4 interaction.
# log_crime_gap x T4 is carried along for comparability with the main spec.
#
# Sign convention: ratings are 0-100 sliders, higher = handles crime better.
#
# Outputs: printed model summaries, plus
#   latex/images/same_coalition_target_coef_plot.pdf

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
# panel stores the comparison municipality IDs but not their coalitions.
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
# pre/post pair runs over the same coalitions and their difference is a genuine
# within-respondent update rather than a change in composition.
rating_ok <- !is.na(rating_pre) & !is.na(rating_post)

# ── Target and counterpart ───────────────────────────────────────────────────
# Target = home coalition, always observed. Counterpart = the mean of the two
# out-parties, neither of which T4 mentions.
#
# The target's pre/post/change are Home_Party_Crime_Handling_*, already built in
# create_panel_dataset.R by the same home_coalition selection, so they are
# reused rather than rebuilt.
panel$Target_Pre <- panel$Home_Party_Crime_Handling_Pre
panel$Target_Change <- panel$Home_Party_Crime_Handling_Change

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

panel$Counterpart_Pre <- mean_non_home(rating_pre)
panel$Counterpart_Change <- mean_non_home(rating_post) - panel$Counterpart_Pre

# Within-respondent placebo: the shown coalition net of the out-parties the
# treatment said nothing about. Positive => the brand gained relative to them.
panel$Target_Minus_Counterpart <- panel$Target_Change -
  panel$Counterpart_Change

# Brand versus the local incumbent. T4's comparisons are co-partisans elsewhere,
# not the respondent's own government, so the two can move apart: news about
# the coalition need not be news about the mayor. Positive => the coalition
# brand gained on the home municipality's own government.
# (home_party_update_analysis.R fits this outcome across all arms as m_diff;
# what is new here is only the pooled-control contrast.)
panel$Target_Minus_Incumbent <- panel$Target_Change -
  panel$Home_Crime_Handling_Change

# How many displayed municipalities were governed by the home coalition (0-4).
# Fixed at 4 in T4 by construction; 0 in T3; random in the party-blind arms.
panel$n_target_munis_shown <- vapply(
  seq_len(nrow(panel)),
  function(i) {
    hc <- panel$home_coalition[i]
    if (is.na(hc)) NA_real_ else sum(comp_coalition[i, ] == hc, na.rm = TRUE)
  },
  numeric(1)
)

# Knew the governing party of at least one comparison municipality. Recognizing
# the displayed municipalities as co-partisans is a precondition for the
# brand-attribution channel; comp_party_known is built in
# create_panel_dataset.R by scoring Comp_Governing_Party_Belief_* against truth.
panel$knew_comp_party <- as.integer(panel$comp_party_known > 0)

# Base restrictions match belief_update_analysis.R.
base <- filter(panel, muni_changed == 0, Attention_Check == "somewhat_agree")

# ── Diagnostics ──────────────────────────────────────────────────────────────
cat("\n===== Home-coalition municipalities shown, by arm =====\n")
print(table(base$Treatment_Group, base$n_target_munis_shown, useNA = "ifany"))
# T4 should be 4 across the board and T3 should be 0. T2, control2 and the
# control/T1 cells vary -- the "same" cell should be 4, "opposite" 0.

cat("\n===== Control/T1 comparison cell =====\n")
print(table(
  base$Treatment_Group,
  base$Control_T1_Comp_Type,
  useNA = "ifany"
))

# ── Sample: T4 versus the pooled controls ────────────────────────────────────
matched <- base %>%
  filter(Treatment_Group %in% c("control", "control2", "T1", "T4")) %>%
  mutate(
    saw_crime_comparison = as.integer(Treatment_Group == "T4"),
    arm = if_else(Treatment_Group == "T4", "T4", "pooled control")
  )

cat("\n===== Pooled-control sample =====\n")
print(table(matched$arm, matched$Treatment_Group))
cat("Outcome defined (target):              ",
    sum(!is.na(matched$Target_Change)), "\n")
cat("Outcome defined (target - counterpart):",
    sum(!is.na(matched$Target_Minus_Counterpart)), "\n")

# ── Model A: did T4 move the coalition brand at all? ────────────────────────
m_a <- lm_robust(
  Target_Change ~ saw_crime_comparison + Target_Pre + coalition_pre,
  data = matched,
  alpha = ci_alpha,
  se_type = "HC2"
)

cat("\n===== A. T4 vs pooled controls (control + control2 + T1) =====\n")
print(summary(m_a))

# ── Model B: the effect should run through the ranking surprise ─────────────
m_b <- lm_robust(
  Target_Change ~ rank_gap * saw_crime_comparison +
    log_crime_gap * saw_crime_comparison + Target_Pre + coalition_pre,
  data = matched,
  alpha = ci_alpha,
  se_type = "HC2"
)

cat("\n===== B. T4 x surprise (rank_gap is the hypothesis-carrying term) =====\n")
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

cat("\n===== C. Within-respondent placebo: brand minus the unmentioned out-parties =====\n")
print(summary(m_c))

# ── Model D: brand versus the local incumbent ───────────────────────────────
m_d <- lm_robust(
  Target_Minus_Incumbent ~ rank_gap * saw_crime_comparison +
    log_crime_gap * saw_crime_comparison + Target_Pre + coalition_pre,
  data = matched,
  alpha = ci_alpha,
  se_type = "HC2"
)

cat("\n===== D. Coalition brand minus home incumbent =====\n")
print(summary(m_d))

# ── Model E: does it require recognizing the comparisons as co-partisans? ───
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
# Only the control/T1 respondents randomly assigned the "same" cell, whose
# comparison municipalities are drawn exactly as T4's and named on screen.
strict <- matched %>%
  filter(
    Treatment_Group == "T4" |
      (Treatment_Group %in% c("control", "T1") &
        !is.na(Control_T1_Comp_Type) &
        Control_T1_Comp_Type == "same")
  )

cat("\n===== F. Strict control: control/T1 'same' cell only =====\n")
print(table(strict$Treatment_Group))

m_f <- lm_robust(
  Target_Change ~ rank_gap * saw_crime_comparison +
    log_crime_gap * saw_crime_comparison + Target_Pre + coalition_pre,
  data = strict,
  alpha = ci_alpha,
  se_type = "HC2"
)
print(summary(m_f))

# ── Coefficient plot: the T4 contrast across outcomes ───────────────────────
# Interactions are rescaled to a 1 SD change in the moderator, as in
# belief_update_analysis.R. Row labels carry a second line naming what each
# outcome isolates.
log_crime_gap_sd <- sd(matched$log_crime_gap, na.rm = TRUE)
rank_gap_sd <- sd(matched$rank_gap, na.rm = TRUE)

pull_interactions <- function(model, label, note) {
  tidy(model, conf.int = TRUE) %>%
    filter(grepl("saw_crime_comparison", term), grepl(":", term)) %>%
    mutate(
      outcome = sprintf("%s\n%s (N=%d)", label, note, model$nobs),
      group = if_else(grepl("log_crime_gap", term), "CG × T4", "RG × T4"),
      sd = if_else(group == "CG × T4", log_crime_gap_sd, rank_gap_sd),
      across(c(estimate, conf.low, conf.high, std.error), ~ . * sd),
      conf.low95 = estimate - qt(0.975, df) * std.error,
      conf.high95 = estimate + qt(0.975, df) * std.error
    )
}

coef_all <- bind_rows(
  pull_interactions(m_b, "Coalition brand", "headline outcome"),
  pull_interactions(m_c, "Brand − out-parties", "within-respondent placebo"),
  pull_interactions(m_d, "Brand − incumbent", "brand vs. local government"),
  pull_interactions(m_f, "Brand, strict control", "robustness to pooling")
) %>%
  mutate(outcome = factor(outcome, levels = rev(unique(outcome))))

same_coalition_coef_plot <- ggplot(coef_all, aes(y = outcome, x = estimate)) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey50") +
  geom_errorbar(
    aes(xmin = conf.low, xmax = conf.high),
    orientation = "y",
    width = 0,
    linewidth = 0.5,
    color = "#0072B2"
  ) +
  geom_errorbar(
    aes(xmin = conf.low95, xmax = conf.high95),
    orientation = "y",
    width = 0,
    linewidth = 1.2,
    color = "#0072B2"
  ) +
  geom_point(size = 2.5, color = "#0072B2") +
  facet_wrap(~group) +
  labs(
    title = "Updating about the coalition named in the same-coalition treatment",
    subtitle = "T4 vs pooled controls (control + control2 + T1)",
    y = NULL,
    x = "Standardized coefficient (1 SD increase in predictor)",
    caption = "Thick bar 95% CI, thin 99% CI"
  ) +
  theme_minimal() +
  theme(axis.text.y = element_text(size = 8, lineheight = 0.95))

print(same_coalition_coef_plot)

ggsave(
  "latex/images/same_coalition_target_coef_plot.pdf",
  plot = same_coalition_coef_plot,
  width = 8,
  height = 5
)

# ── Model G: party-blind, variable-dose exposure to co-partisans ────────────
# T2's comparisons are drawn without regard to party, so the number governed by
# the home coalition varies 0-4 at random. That is a dose rather than a named
# target, and neither T2's text nor its chart mentions party -- so this is a
# deliberately weak test, and only respondents who already knew who governs the
# displayed municipalities could update brand beliefs from it. Baseline is the
# same pooled control group.
t2_sample <- base %>%
  filter(Treatment_Group %in% c("control", "control2", "T1", "T2")) %>%
  mutate(saw_crime_comparison = as.integer(Treatment_Group == "T2"))

cat("\n===== G. Dose sample: home-coalition municipalities shown =====\n")
print(table(t2_sample$Treatment_Group, t2_sample$n_target_munis_shown))

m_g <- lm_robust(
  Target_Change ~ n_target_munis_shown * saw_crime_comparison + Target_Pre +
    coalition_pre,
  data = t2_sample,
  alpha = ci_alpha,
  se_type = "HC2"
)

cat("\n===== G. Dose x arm =====\n")
print(summary(m_g))

m_g_known <- lm_robust(
  Target_Change ~ n_target_munis_shown * saw_crime_comparison * knew_comp_party +
    Target_Pre + coalition_pre,
  data = t2_sample,
  alpha = ci_alpha,
  se_type = "HC2"
)

cat("\n===== G. Dose x arm x knew comparison parties =====\n")
print(summary(m_g_known))
