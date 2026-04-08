library(dplyr)
library(data.table)

ageeml <- data.table::fread(
  "references/AGEEML_202512121054579_utf.csv",
  encoding = "Latin-1",
  colClasses = c(CVEGEO = "character")
)

d <- ageeml %>%
  mutate(pop = as.numeric(POB_TOTAL)) %>%
  filter(!is.na(pop)) %>%
  arrange(pop)

total_pop <- sum(d$pop)
cat(sprintf(
  "Total population (all municipalities): %s\n\n",
  format(total_pop, big.mark = ",")
))

# ── Share of population by municipality size threshold ───────────────────────

thresholds <- c(1000, 2500, 5000, 10000, 25000, 50000, 100000)

cat(sprintf(
  "%-20s  %8s  %8s  %10s  %10s\n",
  "Threshold",
  "N munis",
  "% munis",
  "Pop below",
  "% pop"
))
cat(strrep("-", 62), "\n")

for (thr in thresholds) {
  below <- d %>% filter(pop < thr)
  n_below <- nrow(below)
  pct_munis <- 100 * n_below / nrow(d)
  pop_below <- sum(below$pop)
  pct_pop <- 100 * pop_below / total_pop
  cat(sprintf(
    "< %-17s  %8d  %7.1f%%  %10s  %9.1f%%\n",
    format(thr, big.mark = ","),
    n_below,
    pct_munis,
    format(pop_below, big.mark = ","),
    pct_pop
  ))
}

# ── Cumulative distribution table ────────────────────────────────────────────

cat("\n── Population percentiles by municipality size ──────────────────────\n")
quantile_pops <- quantile(d$pop, probs = seq(0.1, 0.9, by = 0.1))
for (p in names(quantile_pops)) {
  cat(sprintf(
    "  %s of municipalities have population below %s\n",
    p,
    format(round(quantile_pops[p]), big.mark = ",")
  ))
}

# ── Population-weighted threshold: 75% of population in municipalities ABOVE X ─
# Sort ascending, cumsum until we hit 25% of total pop → that's the crossing point

d_sorted <- d %>% arrange(pop) %>% mutate(cum_pop = cumsum(pop))
threshold_row <- d_sorted %>%
  filter(cum_pop <= 0.1 * total_pop) %>%
  slice_tail(n = 1)
cat(sprintf(
  "\n90%% of the population lives in municipalities larger than %s residents\n",
  format(threshold_row$pop, big.mark = ",")
))

# ── Share of national population captured by the nearest10 candidate pool ───
# nearest10 restricts to pop >= 5,000; show what share of national pop that covers

above5k <- d %>% filter(pop >= 10000)
cat(sprintf(
  "\nMunicipalities with pop >= 5,000: %d (%.1f%% of munis, %.1f%% of national pop)\n",
  nrow(above5k),
  100 * nrow(above5k) / nrow(d),
  100 * sum(above5k$pop) / total_pop
))
