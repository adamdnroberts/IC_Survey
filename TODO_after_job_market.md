# To do after the job market

Extensions and robustness work deliberately deferred. Nothing here is needed for
the current draft — these are things worth doing once there is time to do them
properly.

1. [Respondent-level heterogeneity in benchmark preferences](#respondent-level-heterogeneity-in-benchmark-preferences)
   — a new result, roughly a week of work.
2. [Prior sensitivity for the benchmark models](#prior-sensitivity-for-the-benchmark-models)
   — an appendix that closes off a likely referee question. Half a day.
3. [asinh robustness check for the crime gap](#asinh-robustness-check-for-the-crime-gap)
   — an hour, and almost certainly a null result. Lowest priority.

---

## Respondent-level heterogeneity in benchmark preferences

**Status:** not started. Idea only — no code written.

**The gap.** `code/benchmark_analysis.R` fits the wave 1 benchmark selection
model with a random intercept only, `(1 | Respondent_ID)`. That allows
respondents to differ in *how many* municipalities they select, but forces every
respondent to weight distance, size and copartisanship identically. The natural
descriptive question the current model cannot answer: do some respondents care a
lot about whether a benchmark shares their governing coalition while others
ignore it entirely?

This matters beyond the descriptive section. A respondent-level measure of
copartisan benchmark preference is the theoretically right moderator for the T4
(same-coalition comparison) effect in wave 2 — it would connect the two halves of
the paper directly, rather than through the current assertion that benchmark
relevance shapes updating.

### Stage 1 — fit varying slopes

New script, e.g. `code/benchmark_heterogeneity.R`. Leave `benchmark_analysis.R`
alone so the paper's existing table and figure do not move.

Only predictors that vary *within* respondent can take a random slope. From
`code/build_benchmark_panel.R:213-236` those are `log_dist_km`,
`log_pop_ratio`, `same_state`, `same_coalition`, `vote_coalition_match`, `pool`
and `cand_coalition`. Not `home_coalition` or `log_home_pop` — those are constant
across a respondent's 15 rows, so a random slope on them is collinear with the
random intercept and will not identify.

Center the continuous predictors first. Uncentered `log_dist_km` has a mean
around 6-7, which induces a strong intercept-slope posterior correlation, hurts
sampling, and makes the LKJ correlation uninterpretable. Center rather than
scale, so the per-unit interpretation behind the `plogis(draw) - 0.5` transform
in `benchmark_analysis.R:89` still holds.

```r
library(dplyr); library(brms)

long_df <- readRDS("data/derived/benchmark_panel.rds") %>%
  mutate(
    log_dist_km_c   = log_dist_km   - mean(log_dist_km,   na.rm = TRUE),
    log_pop_ratio_c = log_pop_ratio - mean(log_pop_ratio, na.rm = TRUE)
  )

priors_het <- c(
  prior(student_t(2, 0, 2.5), class = b),
  prior(student_t(2, 0, 2.5), class = Intercept),
  prior(exponential(1), class = sd),
  prior(lkj(2), class = cor)          # new: needed once slopes are correlated
)

fit_het <- brm(
  bf(
    Selected ~
      log_dist_km_c + log_pop_ratio_c + log_dist_km_c:log_pop_ratio_c +
      same_state + same_coalition + vote_coalition_match +
      cand_coalition + home_coalition + log_home_pop + pool +
      (1 + same_coalition + log_dist_km_c | Respondent_ID),
    decomp = "QR"
  ),
  data = long_df,
  family = bernoulli(link = "logit"),
  prior = priors_het,
  chains = 4, cores = 4, iter = 2000, warmup = 1000, seed = 42,
  file = "data/derived/fit_benchmark_het",
  file_refit = "on_change"
)
```

Practical notes:

- **Start with two slopes, not seven.** Each respondent contributes only 15
  binary observations, so slope SDs are weakly identified. Every added slope
  costs sampling time and adds a row/column to the correlation matrix.
  `same_coalition` is the substantively interesting one; `log_dist_km_c` is the
  one most likely to show real spread.
- `lkj(2)` mildly favors correlations away from ±1, which is what thin per-group
  data needs — flat `lkj(1)` will park at ±0.95 on noise.
- brms already uses the non-centered parameterization for group-level effects.
- `decomp = "QR"` only touches population-level effects, so it is fine to keep,
  but the CLAUDE.md rule still applies: use `fixef()` / `ranef()` / `coef()`,
  never `as_draws_df()`, for anything in original coefficient space.
- Expect this to be much slower than the intercept-only fit. Test on
  `filter(long_df, Respondent_ID %in% sample(unique(Respondent_ID), 300))`
  before committing to a full run.

### Stage 2 — check the heterogeneity is real

Before building anything on top of it:

```r
VarCorr(fit_het)$Respondent_ID$sd     # posterior for sd(same_coalition)
loo(fit_benchmark, fit_het)
```

Compare that SD to the `same_coalition` fixed effect. If the SD posterior is
concentrated near zero there is nothing to exploit, and that is itself
reportable — the copartisan preference is uniform across respondents. Stop here
if so.

Check first how many respondents can even inform the slope. Some may have been
shown 15 candidates with no variation in `same_coalition`; they contribute
nothing and shrink fully to the population mean.

```r
long_df %>% group_by(Respondent_ID) %>%
  summarise(v = n_distinct(same_coalition)) %>% count(v)
```

### Stage 3 — use it as a wave 2 moderator (mind the trap)

The obvious move — pull `coef(fit_het)$Respondent_ID[, "Estimate",
"same_coalition"]`, merge on `Respondent_ID`, interact with `Treatment_Group` —
is **wrong**. Those slopes are shrunken posterior means estimated with error.
Using them as a regressor is a generated-regressor problem: second-stage SEs come
out too small, and shrinkage pulls low-information respondents toward the mean,
mechanically attenuating the very moderation being tested. That combination
biases toward a null while reporting overconfident intervals about it.

Carry the uncertainty through instead — run the second stage once per posterior
draw and pool:

```r
library(estimatr)
slope_draws <- ranef(fit_het, summary = FALSE)$Respondent_ID[, , "same_coalition"]
# dims: draws × respondents
fe <- fixef(fit_het, summary = FALSE)[, "same_coalition"]

load("data/derived/survey_panel_dataset.Rdata")   # build `panel` as in vote_update_analysis.R

draw_idx <- sample(nrow(slope_draws), 200)
res <- lapply(draw_idx, function(i) {
  sl <- data.frame(
    Respondent_ID = as.integer(colnames(slope_draws)),
    copartisan_pref = fe[i] + slope_draws[i, ]
  )
  d <- left_join(panel, sl, by = "Respondent_ID")
  m <- lm_robust(
    Vote_home_post ~ log_crime_gap * as.factor(Treatment_Group) * copartisan_pref +
      rank_gap * as.factor(Treatment_Group) + inc_vote,
    data = d, se_type = "HC2"
  )
  data.frame(term = names(coef(m)), est = coef(m), se = sqrt(diag(vcov(m))))
}) %>% bind_rows()
```

Combine by Rubin's rules — total variance is the mean within-draw variance plus
the between-draw variance of the estimates:

```r
res %>% group_by(term) %>%
  summarise(
    estimate = mean(est),
    se = sqrt(mean(se^2) + (1 + 1/n()) * var(est))
  )
```

The `var(est)` term is the point: it is the uncertainty in each respondent's
preference propagating into the moderation estimate, and the plug-in approach
throws it away.

**Check `Respondent_ID` types before that join.** `colnames()` on the `ranef`
array returns character, and CLAUDE.md flags the zero-padded-ID hazard. A type
mismatch here produces an all-`NA` moderator silently rather than erroring.

### If it works

Add to `master_script.R` in the `analysis` stage, after
`benchmark_crime_analysis.R`.

---

## Prior sensitivity for the benchmark models

**Status:** not started.

**The gap.** `benchmark_analysis.R:13-17` and `benchmark_crime_analysis.R` both
use `student_t(2, 0, 2.5)` on the coefficients and intercept. That is a heavy-
tailed choice — two degrees of freedom means the prior has no finite variance —
and nothing in the paper currently justifies it or shows it does not matter. It
is a cheap question for a referee to ask and an awkward one to answer after the
fact.

The expected answer is that it makes no difference: ~45,000 rows against ~14
population-level parameters means the data dominate everywhere except in sparse
cells. Worth confirming rather than asserting, particularly for the levels that
might be thin — the `"Other"` catch-all in both `cand_coalition` and
`home_coalition` (`build_benchmark_panel.R:226-233`), crossed with the 3-level
`pool`. If any of those cells is near-separated, the prior *is* doing real work
there, which is a point in the model's favor but needs to be stated rather than
hidden.

**What to do.** Refit `fit_benchmark` under two alternative priors and compare
the population-level coefficients:

- `normal(0, 1.5)` — a conventional weakly-informative choice on the logit scale,
  and noticeably tighter than the current prior.
- `student_t(3, 0, 2.5)` — same shape, finite variance, tests whether the `df = 2`
  choice specifically matters.

Use different `file =` paths so the cached main fit is not disturbed. Report as a
small appendix table: coefficient, posterior mean and 95% CI under each of the
three priors. One paragraph of text noting that the estimates are unchanged, plus
the cell counts for the sparse levels.

Also check first whether separation is actually in play, which decides how much
of this discussion the paper needs:

```r
long_df <- readRDS("data/derived/benchmark_panel.rds")
with(long_df, table(cand_coalition, home_coalition, Selected))
with(long_df, table(cand_coalition, pool, Selected))
```

This is worth doing regardless of the heterogeneity extension above — and if that
extension happens, its `lkj(2)` and `exponential(1)` priors should be checked the
same way, where they will matter considerably more, since the random-slope SDs
are weakly identified at 15 binary observations per respondent.

---

## asinh robustness check for the crime gap

**Status:** not started. Noted inline at `code/vote_update_analysis.R:4`.

**The gap.** The main specifications use `log_crime_gap`, defined at
`create_panel_dataset.R:210` as `sign(x) * log(1 + |x|)`. The panel already
carries `asinh_crime_gap` (line 211) and nothing uses it.

**Expect a null result.** The two transforms agree to first order — both have
slope 1 at the origin, and for large `|x|` they differ by roughly a constant
`log(2)` shift. A constant shift in a regressor that is interacted with treatment
moves the main effects but leaves the interaction coefficients — which are what
the paper reports — essentially untouched. So this is a robustness check whose
value is in being able to say it was run, not in what it will find. Hence lowest
priority.

**What to do.** Refit `m_log` in `vote_update_analysis.R` and `m_log` in
`belief_update_analysis.R` with `asinh_crime_gap` substituted for
`log_crime_gap`, and report the interaction coefficients alongside the main
specification in the appendix.

Two things that will silently produce wrong numbers if missed:

- The coefficient plots rescale by the regressor's SD (`log_crime_gap_sd` in
  `vote_update_analysis.R:41`). Recompute that from `asinh_crime_gap`, or the
  plotted magnitudes will be scaled by the wrong constant — and because the two
  transforms are so similar, the resulting numbers will look plausible.
- The multiple-comparisons scripts match term names on the literal string
  `log_crime_gap` (see `multiple comparisons/fwer_inc_other.R:31-37`). If the
  asinh version is ever put through the FWER machinery, that family definition
  needs updating too, or it will silently select zero terms.

Keep this as an appendix robustness check only. Do not change the main
specification — `log_crime_gap` is what was pre-registered.
