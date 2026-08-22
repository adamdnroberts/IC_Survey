# Descriptives: do respondents rate the three coalitions as EQUALLY good at
# handling crime?
#
# This is a within-respondent question. Every wave 1 respondent rates all three
# coalitions on the same 0-100 slider, so the quantity of interest is the SPREAD
# of a respondent's three ratings, not whether the three sample means are equal.
# Partisans who rate their own coalition high and the others low cancel out in
# the aggregate: pooled means can look identical while nobody is indifferent.
# Section 4 below checks exactly that by splitting on vote intention.
#
# Sample: all of wave 1 (these are pre-treatment priors, so no treatment filter
# is needed). Uses data/derived/wave1_responses.rds, matching priors_by_coalition.R.
#
# CAVEAT: all three sliders default to 50 (app_wave1.R). The app warns about
# untouched sliders but does not block, and the touched flags are not stored, so
# a respondent who left all three alone is indistinguishable from one who
# deliberately rated all three at 50. Every "identical" statistic below is
# therefore reported twice: including and excluding the all-at-50 cases.
#
# Output: console tables + latex/images/coalition_rating_spread.pdf
#                          latex/images/coalition_rating_ingroup.pdf

library(dplyr)
library(tidyr)
library(ggplot2)

survey_responses_wave1 <- readRDS("data/derived/wave1_responses.rds")

coalition_levels <- c("MORENA", "PAN/PRI/PRD", "MC")

coalition_colors <- c(
  "MORENA" = "#8B0000",
  "PAN/PRI/PRD" = "#00308F",
  "MC" = "#FF5722"
)

slider_default <- 50

# ── Vote-intention coalition ─────────────────────────────────────────────────
# Same mapping as create_panel_dataset.R's to_coalition(): a single-coalition
# ticket resolves, a cross-coalition ticket stays NA, an unrecognised party
# becomes "Other".

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
      if (length(coalitions) == 0) {
        "Other"
      } else if (length(coalitions) == 1) {
        coalitions
      } else {
        NA_character_
      }
    },
    USE.NAMES = FALSE
  )
}

# ── Build the respondent-level wide frame ────────────────────────────────────
# The sliders are stored as character; coerce and report anything lost.

ratings_wide <- survey_responses_wave1 %>%
  transmute(
    respondent_id = Respondent_ID,
    MORENA = suppressWarnings(as.numeric(MORENA_Crime_Rating_Pre)),
    `PAN/PRI/PRD` = suppressWarnings(
      as.numeric(Coalition_PAN_PRI_PRD_Crime_Rating_Pre)
    ),
    MC = suppressWarnings(as.numeric(MC_Crime_Rating_Pre)),
    voter_coalition = to_coalition(Vote_Intention_Pre)
  )

n_total <- nrow(ratings_wide)

# Within-respondent spread is only defined when all three ratings are present.
ratings_wide <- ratings_wide %>%
  filter(!is.na(MORENA), !is.na(`PAN/PRI/PRD`), !is.na(MC))

n_complete <- nrow(ratings_wide)

ratings_wide <- ratings_wide %>%
  rowwise() %>%
  mutate(
    rating_min = min(c(MORENA, `PAN/PRI/PRD`, MC)),
    rating_max = max(c(MORENA, `PAN/PRI/PRD`, MC)),
    rating_mean = mean(c(MORENA, `PAN/PRI/PRD`, MC)),
    rating_sd = sd(c(MORENA, `PAN/PRI/PRD`, MC))
  ) %>%
  ungroup() %>%
  mutate(
    spread = rating_max - rating_min,
    all_identical = spread == 0,
    all_default = all_identical & rating_mean == slider_default
  )

cat("\n== Coalition crime-handling ratings, wave 1 (0-100 sliders) ==\n\n")
cat(sprintf("Wave 1 respondents:                 %d\n", n_total))
cat(sprintf(
  "With all three ratings non-missing: %d (%.1f%%)\n\n",
  n_complete,
  100 * n_complete / n_total
))

# ── 1. How much does each respondent spread the three coalitions? ────────────

cat("-- 1. Within-respondent spread (max - min of the three ratings) --\n\n")

spread_quantiles <- quantile(
  ratings_wide$spread,
  probs = c(0, 0.1, 0.25, 0.5, 0.75, 0.9, 1)
)
print(round(spread_quantiles, 1))

cat(sprintf(
  "\nMean spread: %.1f points | Mean within-respondent SD: %.1f points\n\n",
  mean(ratings_wide$spread),
  mean(ratings_wide$rating_sd)
))

# Share of respondents whose three ratings all fall inside a given window. Read
# this as "how many respondents treat the coalitions as interchangeable, up to a
# tolerance of `w` points on a 100-point slider".
spread_windows <- c(0, 5, 10, 20)
spread_share <- data.frame(
  window = spread_windows,
  n = sapply(spread_windows, function(w) sum(ratings_wide$spread <= w)),
  pct = sapply(
    spread_windows,
    function(w) 100 * mean(ratings_wide$spread <= w)
  )
)
spread_share$label <- sprintf("spread <= %d pts", spread_share$window)
print(spread_share[, c("label", "n", "pct")], row.names = FALSE, digits = 3)

n_identical <- sum(ratings_wide$all_identical)
n_default <- sum(ratings_wide$all_default)

cat(sprintf(
  "\nAll three ratings identical: %d (%.1f%%)\n",
  n_identical,
  100 * n_identical / n_complete
))
cat(sprintf(
  "  ...of which all three = %d (the slider default): %d (%.1f%% of all respondents)\n",
  slider_default,
  n_default,
  100 * n_default / n_complete
))
cat(sprintf(
  "  ...identical at some OTHER value:                %d (%.1f%% of all respondents)\n\n",
  n_identical - n_default,
  100 * (n_identical - n_default) / n_complete
))

# The same headline number with the all-at-default cases dropped, since those
# are plausibly non-response rather than considered indifference.
no_default <- ratings_wide %>% filter(!all_default)
cat(sprintf(
  "Excluding the %d all-at-%d respondents (n = %d):\n",
  n_default,
  slider_default,
  nrow(no_default)
))
cat(sprintf(
  "  mean spread %.1f pts | share with spread <= 5 pts: %.1f%% | identical: %.1f%%\n\n",
  mean(no_default$spread),
  100 * mean(no_default$spread <= 5),
  100 * mean(no_default$all_identical)
))

# ── 2. Pairwise differences ──────────────────────────────────────────────────

cat("-- 2. Pairwise within-respondent differences --\n\n")

pairs <- list(
  c("MORENA", "PAN/PRI/PRD"),
  c("MORENA", "MC"),
  c("PAN/PRI/PRD", "MC")
)

pairwise <- do.call(
  rbind,
  lapply(pairs, function(p) {
    d <- ratings_wide[[p[1]]] - ratings_wide[[p[2]]]
    data.frame(
      pair = paste(p[1], "-", p[2]),
      mean_diff = mean(d),
      median_diff = median(d),
      sd_diff = sd(d),
      mean_abs_diff = mean(abs(d)),
      pct_equal = 100 * mean(d == 0),
      pct_within_5 = 100 * mean(abs(d) <= 5)
    )
  })
)

print(pairwise, row.names = FALSE, digits = 3)

cat(
  "\nNote: mean_diff near zero with a large mean_abs_diff is the aggregation\n",
  "artifact -- respondents disagree strongly but in offsetting directions.\n\n",
  sep = ""
)

# ── 3. Marginal (pooled) distribution, for contrast ──────────────────────────

cat("-- 3. Pooled distribution by coalition (ignores the pairing) --\n\n")

ratings_long <- ratings_wide %>%
  select(respondent_id, voter_coalition, all_of(coalition_levels)) %>%
  pivot_longer(
    all_of(coalition_levels),
    names_to = "coalition",
    values_to = "rating"
  ) %>%
  mutate(coalition = factor(coalition, levels = coalition_levels))

pooled <- ratings_long %>%
  group_by(coalition) %>%
  summarise(
    n = n(),
    mean = mean(rating),
    sd = sd(rating),
    median = median(rating),
    .groups = "drop"
  )
print(as.data.frame(pooled), row.names = FALSE, digits = 3)

# ── 4. In-group favouritism: does the spread come from partisanship? ─────────

cat("\n-- 4. Ratings by the respondent's own vote-intention coalition --\n\n")

# Map the voter's coalition label onto the rated-coalition labels so "own party"
# can be identified. Voters with no resolvable coalition are excluded here.
voter_to_rated <- c(
  "MORENA/PVEM/PT" = "MORENA",
  "PAN/PRI/PRD" = "PAN/PRI/PRD",
  "MC" = "MC"
)

ingroup <- ratings_long %>%
  filter(voter_coalition %in% names(voter_to_rated)) %>%
  mutate(
    own_coalition = voter_to_rated[voter_coalition] == as.character(coalition),
    voter_label = voter_to_rated[voter_coalition]
  )

cat("Mean rating GIVEN (rows = rater's coalition, cols = rated coalition):\n\n")
ingroup_matrix <- ingroup %>%
  group_by(voter_label, coalition) %>%
  summarise(mean = mean(rating), .groups = "drop") %>%
  pivot_wider(names_from = coalition, values_from = mean) %>%
  rename(rater = voter_label)
print(as.data.frame(ingroup_matrix), row.names = FALSE, digits = 3)

cat("\nOwn coalition vs. others, by rater:\n\n")
ingroup_gap <- ingroup %>%
  group_by(voter_label, own_coalition) %>%
  summarise(mean = mean(rating), .groups = "drop") %>%
  mutate(which = ifelse(own_coalition, "own", "other")) %>%
  select(-own_coalition) %>%
  pivot_wider(names_from = which, values_from = mean) %>%
  mutate(gap = own - other) %>%
  rename(rater = voter_label)
print(as.data.frame(ingroup_gap), row.names = FALSE, digits = 3)

cat("\nWithin-respondent spread by rater's coalition:\n\n")
spread_by_voter <- ratings_wide %>%
  mutate(
    voter_label = ifelse(
      voter_coalition %in% names(voter_to_rated),
      voter_to_rated[voter_coalition],
      "No / mixed / other"
    )
  ) %>%
  group_by(voter_label) %>%
  summarise(
    n = n(),
    mean_spread = mean(spread),
    median_spread = median(spread),
    pct_spread_le_5 = 100 * mean(spread <= 5),
    .groups = "drop"
  )
print(as.data.frame(spread_by_voter), row.names = FALSE, digits = 3)

# ── Figures ──────────────────────────────────────────────────────────────────

spread_plot <- ggplot(ratings_wide, aes(x = spread)) +
  geom_histogram(binwidth = 5, boundary = 0, fill = "#0072B2", color = "white") +
  geom_vline(
    xintercept = median(ratings_wide$spread),
    linetype = "dashed",
    color = "grey30"
  ) +
  annotate(
    "text",
    x = median(ratings_wide$spread),
    y = Inf,
    label = sprintf(" median = %.0f", median(ratings_wide$spread)),
    hjust = 0,
    vjust = 1.5,
    size = 3.5,
    color = "grey30"
  ) +
  scale_x_continuous(limits = c(-2.5, 102.5), breaks = seq(0, 100, 25)) +
  labs(
    x = "Within-respondent spread in coalition ratings (max - min, 0-100)",
    y = "Respondents",
    caption = sprintf(
      "n = %d. Spread of 0 means the respondent rated all three coalitions identically.",
      n_complete
    )
  ) +
  theme_bw(base_size = 12)

print(spread_plot)

ggsave(
  "latex/images/coalition_rating_spread.pdf",
  plot = spread_plot,
  width = 7,
  height = 4.5
)

ingroup_plot <- ingroup %>%
  group_by(voter_label, coalition) %>%
  summarise(
    mean = mean(rating),
    se = sd(rating) / sqrt(n()),
    .groups = "drop"
  ) %>%
  ggplot(aes(x = coalition, y = mean, color = coalition)) +
  geom_pointrange(aes(ymin = mean - 1.96 * se, ymax = mean + 1.96 * se)) +
  facet_wrap(~voter_label, labeller = label_both) +
  scale_color_manual(values = coalition_colors, guide = "none") +
  scale_y_continuous(limits = c(0, 100)) +
  labs(
    x = "Coalition rated",
    y = "Mean crime-handling rating, 0-100 (95% CI)",
    caption = sprintf(
      "Panels are the rater's own vote-intention coalition. n = %d.",
      nrow(ingroup) / length(coalition_levels)
    )
  ) +
  theme_bw(base_size = 12) +
  theme(axis.text.x = element_text(angle = 30, hjust = 1))

print(ingroup_plot)

ggsave(
  "latex/images/coalition_rating_ingroup.pdf",
  plot = ingroup_plot,
  width = 8,
  height = 4
)
