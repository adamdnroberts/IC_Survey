library(dplyr)
library(ggplot2)
library(readxl)
library(scales)

wave1 <- readRDS("data/wave1_responses.rds")
wave2 <- readRDS("data/wave2_responses.rds")

# Drop within-wave duplicate respondents (same Netquest_PID submitted more than
# once) so the cross-wave join is strictly one-to-one. Keeps the first row.
wave1 <- distinct(wave1, Netquest_PID, .keep_all = TRUE)
wave2 <- distinct(wave2, Netquest_PID, .keep_all = TRUE)

match_ids <- read_excel("data/Match ID.xlsx")
match_ids <- janitor::clean_names(match_ids)
# Columns are now snake_case: wave_1, wave_2, status_wave_2

match_ids2 <- read.csv(
  "data/wave_match_ids.csv",
  stringsAsFactors = FALSE,
  fileEncoding = "UTF-8-BOM"
)

# Columns: "Wave 1" = wave 1 PID, "Wave 2" = wave 2 PID, "Status Wave 2" =
# wave 2 completion status (a "..._0" suffix means they did not take wave 2).
match_ids <- match_ids %>%
  rename(
    pid_w1 = wave_1,
    pid_w2 = wave_2,
    status_w2 = status_wave_2
  ) %>%
  select(pid_w2, pid_w1) %>%
  mutate(across(c(pid_w2, pid_w1), as.character))

match_ids2 <- match_ids2 %>%
  rename(
    pid_w1 = Wave.1,
    pid_w2 = Wave.2
  ) %>%
  select(pid_w2, pid_w1) %>%
  mutate(across(c(pid_w2, pid_w1), as.character))

# Append the second match table to the first, drop exact duplicate pairs, then
# keep only clean one-to-one matches: drop any rows whose wave-1 or wave-2 PID
# is duplicated across the combined set before merging the two waves.
match_ids <- bind_rows(match_ids, match_ids2) %>%
  distinct(pid_w2, pid_w1) %>%
  add_count(pid_w2, name = "n_w2") %>%
  add_count(pid_w1, name = "n_w1") %>%
  filter(n_w2 == 1, n_w1 == 1) %>%
  select(pid_w2, pid_w1)

test <- wave2 %>%
  inner_join(match_ids, by = c("Netquest_PID" = "pid_w2")) %>%
  inner_join(
    wave1,
    by = c("pid_w1" = "Netquest_PID"),
    suffix = c("_w2", "_w1")
  ) %>%
  select(-pid_w1) %>%
  filter(Found_Municipality_ID_w2 == Found_Municipality_ID_w1) %>%
  # Found_Municipality_ID is present in both waves, so the join suffixed it;
  # the two are equal after the filter, so rebuild the unified column the
  # downstream code expects.
  mutate(Found_Municipality_ID = Found_Municipality_ID_w2)

rank_cols <- c(
  "Crime_Rank_Comp_1",
  "Crime_Rank_Comp_2",
  "Crime_Rank_Comp_3",
  "Crime_Rank_Comp_4"
)

test$rank_prior <- 1 +
  rowSums(
    sapply(test[rank_cols], function(x) x %in% c("fewer")),
    na.rm = TRUE
  )

rank_cols_post <- c(
  "Crime_Rank_Comp_1_Post",
  "Crime_Rank_Comp_2_Post",
  "Crime_Rank_Comp_3_Post",
  "Crime_Rank_Comp_4_Post"
)

test$rank_post <- 1 +
  rowSums(
    sapply(test[rank_cols_post], function(x) x %in% c("fewer")),
    na.rm = TRUE
  )

robo <- readRDS("data/robo_2025.rds") %>%
  mutate(Cve..Municipio = sprintf("%05d", as.integer(Cve..Municipio))) %>%
  select(Cve..Municipio, rate_per_100k)

test <- test %>%
  left_join(robo, by = c("Found_Municipality_ID" = "Cve..Municipio")) %>%
  rename(home_rate = rate_per_100k)

for (i in 1:4) {
  id_col <- paste0("Comparison_Muni_", i, "_ID")
  rate_col <- paste0("comp_rate_", i)
  test <- test %>%
    left_join(
      rename(robo, !!rate_col := rate_per_100k),
      by = setNames("Cve..Municipio", id_col)
    )
}

test$actual_rank <- 1 +
  rowSums(
    sapply(paste0("comp_rate_", 1:4), function(col) {
      test[[col]] < test$home_rate
    }),
    na.rm = TRUE
  )

# Drop implausible robbery estimates (> 100,000) before computing gaps and
# updates, matching crime_rate_accuracy_update.R and manipulation_check.R.
est_cap <- 100000
test$est_pre <- as.numeric(test$Robbery_Estimate)
test$est_post <- as.numeric(test$Robbery_Estimate_Post)
test$est_pre[!is.na(test$est_pre) & test$est_pre > est_cap] <- NA_real_
test$est_post[!is.na(test$est_post) & test$est_post > est_cap] <- NA_real_

test$CG <- test$est_pre - test$home_rate
test$RG <- test$rank_prior - test$actual_rank

test <- filter(test, Attention_Check == "somewhat_agree")

test$info_treatment <- as.factor(ifelse(
  test$Treatment_Group %in% c("T1", "T2", "T3", "T4"),
  1,
  0
))

test$comparison_treatment <- as.factor(ifelse(
  test$Treatment_Group %in% c("T2", "T3", "T4"),
  1,
  0
))

test$robbery_estimate_update <- test$est_post - test$est_pre
test$rank_update <- test$rank_post - test$rank_prior

# Robbery estimate update vs. prior accuracy (learning-rate framing)
#   x = prior error = true rate - prior estimate (neg = over-, pos = under-est.)
#   y = estimate update = post - pre estimate
# The through-origin slope is the learning rate: 0 = ignore the news, 1 = fully
# update to the truth (reference lines mark both). Both axes use a signed-log
# transform applied to the data, then displayed on a linear scale with breaks
# relabeled in real units (robberies per 100k) so the heavy right skew is
# compressed without dropping any points. The fit is estimated in raw units and
# transformed for display, so its slope remains the raw learning rate.
signed_log10 <- function(x) sign(x) * log10(abs(x) + 1)

rob_sub <- test %>%
  mutate(
    prior_error = home_rate - est_pre,
    est_update = robbery_estimate_update
  ) %>%
  filter(!is.na(prior_error), !is.na(est_update)) %>%
  mutate(
    xt = signed_log10(prior_error),
    yt = signed_log10(est_update)
  )

# Per-group through-origin fit, predicted on a raw-unit grid then signed-log
# transformed for display (so the line is the raw learning-rate fit).
fit_df <- rob_sub %>%
  group_by(info_treatment) %>%
  group_modify(
    ~ {
      m <- lm(yt ~ 0 + xt, data = .x)
      grid <- seq(min(.x$xt), max(.x$xt), length.out = 200)
      pr <- predict(m, newdata = data.frame(xt = grid), se.fit = TRUE)
      tibble(
        xt = signed_log10(grid),
        yt = signed_log10(pr$fit),
        lo = signed_log10(pr$fit - 1.96 * pr$se.fit),
        hi = signed_log10(pr$fit + 1.96 * pr$se.fit)
      )
    }
  ) %>%
  ungroup()

info_label_df <- rob_sub %>%
  group_by(info_treatment) %>%
  summarise(
    slope = coef(lm(est_update ~ 0 + prior_error))[1],
    x_raw = quantile(prior_error, 0.8, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    x_pos = signed_log10(x_raw),
    y_pos = signed_log10(slope * x_raw),
    label = ifelse(
      info_treatment == "1",
      sprintf("Information (rate %.2f)", slope),
      sprintf("Control (rate %.2f)", slope)
    )
  )

# Binscatter: within each group, split prior_error into 10 equal-count bins and
# summarise the mean update (+/- 95% CI) per bin. The summary is computed in the
# signed-log display space (not raw units) so the heavy right tail does not blow
# up the standard error, and the CI stays symmetric around the plotted point.
bin_df <- rob_sub %>%
  group_by(info_treatment) %>%
  mutate(bin = ntile(prior_error, 10)) %>%
  group_by(info_treatment, bin) %>%
  summarise(
    xt = mean(xt),
    yt_mean = mean(yt),
    yt_se = sd(yt) / sqrt(n()),
    .groups = "drop"
  ) %>%
  mutate(
    yt = yt_mean,
    lo = yt_mean - 1.96 * yt_se,
    hi = yt_mean + 1.96 * yt_se
  )

# Real-unit tick positions on the signed-log scale.
plog_breaks <- c(-1e5, -1e4, -1e3, -1e2, -1e1, 0, 1e1, 1e2, 1e3, 1e4, 1e5)
plog_pos <- signed_log10(plog_breaks)
plog_lab <- format(plog_breaks, big.mark = ",", scientific = FALSE, trim = TRUE)

rob_est_update <- ggplot(
  subset(
    rob_sub,
    as.numeric(Robbery_Estimate) <= max(test$home_rate) * 10
  ),
  aes(x = xt, y = yt, color = info_treatment)
) +
  geom_hline(yintercept = 0, linetype = "dotted", color = "grey60") +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "grey40") +
  geom_point(alpha = 0.12) +
  geom_ribbon(
    data = fit_df,
    aes(x = xt, ymin = lo, ymax = hi, fill = info_treatment),
    inherit.aes = FALSE,
    alpha = 0.2
  ) +
  geom_line(
    data = fit_df,
    aes(x = xt, y = yt, color = info_treatment),
    linewidth = 0.9
  ) +
  geom_pointrange(
    data = bin_df,
    aes(x = xt, y = yt, ymin = lo, ymax = hi, color = info_treatment),
    inherit.aes = FALSE,
    size = 0.4,
    fatten = 2
  ) +
  geom_text(
    data = info_label_df,
    aes(x = x_pos, y = y_pos, label = label, color = info_treatment),
    vjust = -1.2,
    fontface = "bold",
    show.legend = FALSE
  ) +
  scale_x_continuous(breaks = plog_pos, labels = plog_lab) +
  scale_y_continuous(breaks = plog_pos, labels = plog_lab) +
  facet_wrap(~info_treatment) +
  labs(
    x = "Prior error = true rate - prior estimate (per 100k): negative = overestimate, positive = underestimate",
    y = "Estimate update = post - pre (per 100k)",
    caption = paste0(
      "n = ",
      nrow(rob_sub),
      " (estimates > 100,000 dropped). ",
      "Dashed = full updating (slope 1); dotted = no updating (slope 0). ",
      "Points = binned means (10 quantile bins) +/- 95% CI; raw data faded. ",
      "Axes signed-log; fit estimated in raw units."
    )
  ) +
  theme_bw() +
  theme(legend.position = "none")

rob_est_update <- ggplot(
  subset(
    rob_sub,
    as.numeric(Robbery_Estimate) <= max(test$home_rate) * 10
  ),
  aes(x = xt, y = yt, color = info_treatment)
) +
  geom_hline(yintercept = 0, linetype = "dotted", color = "grey60") +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "grey40") +
  geom_point(alpha = 0.12) +
  geom_ribbon(
    data = fit_df,
    aes(x = xt, ymin = lo, ymax = hi, fill = info_treatment),
    inherit.aes = FALSE,
    alpha = 0.2
  ) +
  geom_line(
    data = fit_df,
    aes(x = xt, y = yt, color = info_treatment),
    linewidth = 0.9
  ) +
  # geom_pointrange(
  #   data = bin_df,
  #   aes(x = xt, y = yt, ymin = lo, ymax = hi, color = info_treatment),
  #   inherit.aes = FALSE,
  #   size = 0.4,
  #   fatten = 2
  # ) +
  # geom_text(
  #   data = info_label_df,
  #   aes(x = x_pos, y = y_pos, label = label, color = info_treatment),
  #   vjust = -1.2,
  #   fontface = "bold",
  #   show.legend = FALSE
  # ) +
  # scale_x_continuous(breaks = plog_pos, labels = plog_lab) +
  # scale_y_continuous(breaks = plog_pos, labels = plog_lab) +
  # facet_wrap(~info_treatment) +
  # labs(
  #   x = "Prior error = true rate - prior estimate (per 100k): negative = overestimate, positive = underestimate",
  #   y = "Estimate update = post - pre (per 100k)",
  #   caption = paste0(
  #     "n = ",
  #     nrow(rob_sub),
  #     " (estimates > 100,000 dropped). ",
  #     "Dashed = full updating (slope 1); dotted = no updating (slope 0). ",
  #     "Points = binned means (10 quantile bins) +/- 95% CI; raw data faded. ",
  #     "Axes signed-log; fit estimated in raw units."
  #   )
  # ) +
  theme_bw() +
  theme(legend.position = "none")

print(rob_est_update)

ggsave(
  "latex/images/robbery_estimate_update.pdf",
  plot = rob_est_update,
  width = 8,
  height = 5
)

# Difference binscatter: common pooled bins of prior_error; within each bin,
# mean update for Information minus mean update for Control (signed-log space),
# with a 95% CI. A series above zero = information induces more updating than the
# placebo at that level of prior error. Differencing the arms cancels the
# regression-to-the-mean pedestal they share (both axes contain -est_pre), so
# what remains is the treatment contrast.
n_diff_bins <- 5
diff_df <- rob_sub %>%
  filter(as.numeric(est_pre) <= max(test$home_rate) * 10) %>%
  mutate(bin = ntile(prior_error, n_diff_bins)) %>%
  group_by(bin) %>%
  summarise(
    x_raw = mean(prior_error),
    n_1 = sum(info_treatment == "1"),
    n_0 = sum(info_treatment == "0"),
    m_1 = mean(yt[info_treatment == "1"]),
    m_0 = mean(yt[info_treatment == "0"]),
    v_1 = var(yt[info_treatment == "1"]) / n_1,
    v_0 = var(yt[info_treatment == "0"]) / n_0,
    .groups = "drop"
  ) %>%
  filter(
    n_1 >= 2,
    n_0 >= 2
  ) %>%
  mutate(
    xt = signed_log10(x_raw),
    diff = m_1 - m_0,
    se = sqrt(v_1 + v_0),
    lo = diff - 1.96 * se,
    hi = diff + 1.96 * se
  )

rob_update_diff <- ggplot(diff_df, aes(x = xt, y = diff)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey40") +
  geom_ribbon(aes(ymin = lo, ymax = hi), alpha = 0.15, fill = "#0072B2") +
  geom_line(color = "#0072B2", linewidth = 0.8) +
  geom_pointrange(
    aes(ymin = lo, ymax = hi),
    color = "#0072B2",
    size = 0.4,
    fatten = 2
  ) +
  scale_x_continuous(breaks = plog_pos, labels = plog_lab) +
  labs(
    x = "Prior error = true rate - prior estimate (per 100k): negative = overestimate, positive = underestimate",
    y = "Information - Control: difference in mean update (signed-log)",
    caption = paste0(
      "Pooled into ",
      n_diff_bins,
      " quantile bins of prior error. ",
      "Points = Information mean update minus Control mean update per bin +/- 95% CI. ",
      "Above 0 = information updates more than placebo; differencing removes the ",
      "regression-to-mean component common to both arms."
    )
  ) +
  theme_bw()

print(rob_update_diff)

ggsave(
  "latex/images/robbery_estimate_update_diff.pdf",
  plot = rob_update_diff,
  width = 8,
  height = 5
)

# Distribution of prior errors: how many respondents over- vs under-estimate the
# true home robbery rate. Overestimate = guessed higher than truth (prior_error
# < 0); underestimate = guessed lower (prior_error > 0). Plotted on the signed-
# log axis so the heavy tail is visible alongside the over/under split at zero.
ou_df <- rob_sub %>%
  mutate(
    err_dir = ifelse(prior_error < 0, "Overestimate", "Underestimate")
  )

dir_counts <- ou_df %>%
  count(err_dir) %>%
  mutate(pct = round(100 * n / sum(n)))
get_dir <- function(dir, col) dir_counts[[col]][dir_counts$err_dir == dir]

over_under_hist <- ggplot(ou_df, aes(x = xt, fill = err_dir)) +
  geom_histogram(bins = 40, color = "white", linewidth = 0.1) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey30") +
  scale_x_continuous(breaks = plog_pos, labels = plog_lab) +
  scale_fill_manual(
    values = c("Overestimate" = "#E69F00", "Underestimate" = "#0072B2"),
    name = NULL
  ) +
  labs(
    x = "Prior error = true rate - prior estimate (per 100k)",
    y = "Respondents",
    subtitle = sprintf(
      "Overestimators: %d (%d%%)    |    Underestimators: %d (%d%%)",
      get_dir("Overestimate", "n"),
      get_dir("Overestimate", "pct"),
      get_dir("Underestimate", "n"),
      get_dir("Underestimate", "pct")
    ),
    caption = paste0(
      "Overestimate = guessed higher than the true rate (prior error < 0); ",
      "underestimate = guessed lower (> 0). n = ",
      nrow(ou_df),
      "."
    )
  ) +
  theme_bw() +
  theme(legend.position = "top")

print(over_under_hist)

ggsave(
  "latex/images/prior_error_distribution.pdf",
  plot = over_under_hist,
  width = 8,
  height = 5
)

# Rank update vs. prior rank accuracy
lim2 <- max(abs(c(test$RG, test$rank_update)), na.rm = TRUE)

rank_sub <- subset(test, Treatment_Group != "T1") %>%
  filter(!is.na(RG), !is.na(rank_update))

comp_label_df <- rank_sub %>%
  group_by(comparison_treatment) %>%
  summarise(
    x_pos = quantile(-RG, 0.2, na.rm = TRUE),
    slope = coef(lm(rank_update ~ 1 + I(-RG)))[1],
    .groups = "drop"
  ) %>%
  mutate(
    y_pos = slope * x_pos,
    label = ifelse(comparison_treatment == "1", "Comparison", "No comparison")
  )

rank_update <- ggplot(
  rank_sub,
  aes(x = -RG, y = rank_update, color = comparison_treatment)
) +
  geom_jitter(width = 0.2, height = 0.2, alpha = 0.5) +
  geom_smooth(method = "lm", formula = y ~ 1 + x, se = TRUE) +
  geom_text(
    data = comp_label_df,
    aes(x = x_pos, y = y_pos, label = label, color = comparison_treatment),
    vjust = -1.2,
    hjust = 1.5,
    fontface = "bold",
    show.legend = FALSE
  ) +
  labs(
    x = "Prior rank belief error: negative = overestimate, positive = underestimate",
    y = "Rank belief update",
    caption = paste0("n = ", nrow(rank_sub))
  ) +
  scale_x_continuous(limits = c(-lim2, lim2)) +
  scale_y_continuous(limits = c(-lim2, lim2)) +
  theme_bw() +
  theme(legend.position = "none")

print(rank_update)

ggsave(
  "latex/images/rank_update.pdf",
  plot = rank_update,
  width = 6,
  height = 6
)
