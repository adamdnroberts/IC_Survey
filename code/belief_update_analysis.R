library(estimatr)
library(modelsummary)
library(dplyr)
library(ggplot2)
library(broom)
library(readxl)

robbery_cap_mult <- 10
min_days_between <- 6
ci_alpha <- 0.01

wave1 <- readRDS("data/wave1_responses.rds")
wave2 <- readRDS("data/wave2_responses.rds")

wave1 <- distinct(wave1, Netquest_PID, .keep_all = TRUE)
wave2 <- distinct(wave2, Netquest_PID, .keep_all = TRUE)

match_ids <- read_excel("data/Match ID.xlsx")
match_ids <- janitor::clean_names(match_ids)

match_ids2 <- read.csv(
  "data/wave_match_ids.csv",
  stringsAsFactors = FALSE,
  fileEncoding = "UTF-8-BOM"
)

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

match_ids <- bind_rows(match_ids, match_ids2) %>%
  distinct(pid_w2, pid_w1)

match_ids <- match_ids %>%
  add_count(pid_w2, name = "n_w2") %>%
  filter(n_w2 == 1) %>%
  select(pid_w2, pid_w1)

wave2_times <- wave2 %>%
  transmute(
    pid_w2 = as.character(Netquest_PID),
    w2_time = as.POSIXct(Timestamp)
  )

match_ids <- match_ids %>%
  left_join(wave2_times, by = "pid_w2") %>%
  group_by(pid_w1) %>%
  slice_min(w2_time, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  select(pid_w2, pid_w1)

panel <- wave2 %>%
  inner_join(match_ids, by = c("Netquest_PID" = "pid_w2")) %>%
  inner_join(
    wave1,
    by = c("pid_w1" = "Netquest_PID"),
    suffix = c("_w2", "_w1")
  ) %>%
  select(-pid_w1) %>%
  filter(Found_Municipality_ID_w2 == Found_Municipality_ID_w1) %>%
  mutate(
    Found_Municipality_ID = Found_Municipality_ID_w2,
    ts_w1 = as.POSIXct(Timestamp_w1, format = "%Y-%m-%d %H:%M:%OS", tz = "UTC"),
    ts_w2 = as.POSIXct(Timestamp_w2, format = "%Y-%m-%d %H:%M:%OS", tz = "UTC"),
    days_between = as.numeric(difftime(ts_w2, ts_w1, units = "days"))
  )

sum(panel$NQ_Age_w1 == panel$NQ_Age_w2) / nrow(panel)
sum(panel$NQ_Sex_w1 == panel$NQ_Sex_w2) / nrow(panel)
sum(panel$NQ_Region_w1 == panel$NQ_Region_w2) / nrow(panel)
sum(panel$NQ_SEL_w1 == panel$NQ_SEL_w2) / nrow(panel)

rank_cols <- c(
  "Crime_Rank_Comp_1",
  "Crime_Rank_Comp_2",
  "Crime_Rank_Comp_3",
  "Crime_Rank_Comp_4"
)

panel$rank_prior <- 1 +
  rowSums(
    sapply(panel[rank_cols], function(x) x %in% c("fewer")),
    na.rm = TRUE
  )

robo <- readRDS("data/robo_2025.rds") %>%
  mutate(Cve..Municipio = sprintf("%05d", as.integer(Cve..Municipio))) %>%
  select(Cve..Municipio, rate_per_100k)

panel <- panel %>%
  left_join(robo, by = c("Found_Municipality_ID" = "Cve..Municipio")) %>%
  rename(home_rate = rate_per_100k)

for (i in 1:4) {
  id_col <- paste0("Comparison_Muni_", i, "_ID")
  rate_col <- paste0("comp_rate_", i)
  panel <- panel %>%
    left_join(
      rename(robo, !!rate_col := rate_per_100k),
      by = setNames("Cve..Municipio", id_col)
    )
}

panel$actual_rank <- 1 +
  rowSums(
    sapply(paste0("comp_rate_", 1:4), function(col) {
      panel[[col]] < panel$home_rate
    }),
    na.rm = TRUE
  )

panel$Robbery_Estimate_wins <- pmin(
  as.numeric(panel$Robbery_Estimate),
  max(panel$home_rate, na.rm = TRUE) * robbery_cap_mult
)

panel$crime_gap <- as.numeric(panel$Robbery_Estimate) - panel$home_rate
panel$crime_gap_wins <- panel$Robbery_Estimate_wins - panel$home_rate
panel$rank_gap <- panel$rank_prior - panel$actual_rank

ggplot(panel, aes(x = actual_rank, fill = Treatment_Group)) +
  geom_histogram(binwidth = 1, position = "dodge") +
  facet_wrap(~Treatment_Group) +
  theme_bw()

panel$log_crime_gap <- sign(panel$crime_gap) * log(abs(panel$crime_gap))

panel$Home_Crime_Handling_Change <- as.numeric(panel$Home_Crime_Handling_Post) -
  as.numeric(panel$Home_Crime_Handling_Pre)

load("data/magar2024_coalitions.Rdata")
all_parties_tmp <- magar2024 %>%
  mutate(
    muni_id = sprintf("%05d", inegi),
    home_coalition = case_when(
      grepl("morena|pvem|pt", l01) ~ "MORENA/PVEM/PT",
      grepl("pan|pri|prd", l01) ~ "PAN/PRI/PRD",
      grepl("mc", l01) ~ "MC",
      TRUE ~ NA_character_
    )
  ) %>%
  select(muni_id, home_coalition)

coalition_vec <- setNames(all_parties_tmp$home_coalition, all_parties_tmp$muni_id)

belief_to_coalition <- c(
  "morena_pt_pvem" = "MORENA/PVEM/PT",
  "pan_pri_prd" = "PAN/PRI/PRD",
  "mc" = "MC"
)

comp_correct <- sapply(1:4, function(i) {
  belief <- panel[[paste0("Comp_Governing_Party_Belief_", i)]]
  muni <- sprintf("%05d", as.integer(panel[[paste0("Comparison_Muni_", i, "_ID")]]))
  guessed <- belief_to_coalition[belief]
  actual <- coalition_vec[muni]
  as.integer(
    !is.na(belief) & belief != "" & belief != "dont_know" &
      !is.na(actual) & guessed == actual
  )
})

panel$comp_party_known <- rowSums(comp_correct, na.rm = TRUE)

panel <- filter(panel, !is.na(days_between) & days_between >= min_days_between)

panel_with_failures <- panel
panel <- filter(panel, Attention_Check == "somewhat_agree")

m_winsorized <- lm_robust(
  Home_Crime_Handling_Change ~
    crime_gap_wins * as.factor(Treatment_Group) + rank_gap * as.factor(Treatment_Group) +
    comp_party_known,
  alpha = ci_alpha,
  data = panel,
  se_type = "HC2"
)

panel_exclude_extreme <- subset(
  panel,
  as.numeric(Robbery_Estimate) <= max(panel$home_rate, na.rm = TRUE) * robbery_cap_mult
)

m_exclude_extreme <- lm_robust(
  Home_Crime_Handling_Change ~
    log_crime_gap * as.factor(Treatment_Group) + rank_gap * as.factor(Treatment_Group) +
    comp_party_known,
  alpha = ci_alpha,
  data = panel_exclude_extreme,
  se_type = "HC2"
)

m_log <- lm_robust(
  Home_Crime_Handling_Change ~
    log_crime_gap *
      as.factor(Treatment_Group) +
      rank_gap * as.factor(Treatment_Group) +
      comp_party_known,
  alpha = ci_alpha,
  data = panel,
  se_type = "HC2"
)

log_crime_gap_sd <- sd(panel$log_crime_gap, na.rm = TRUE)
crime_gap_wins_sd <- sd(panel$crime_gap_wins, na.rm = TRUE)
log_crime_gap_exclude_sd <- sd(panel_exclude_extreme$log_crime_gap, na.rm = TRUE)
rank_gap_sd <- sd(panel$rank_gap, na.rm = TRUE)

extract_coef_plot <- function(model, cg_pattern, model_label, cg_sd, rg_sd) {
  tidy(model, conf.int = TRUE) %>%
    filter(grepl(
      paste0(
        cg_pattern,
        ":as\\.factor|as\\.factor.*:rank_gap(?!:)|(?<=:)rank_gap:as\\.factor"
      ),
      term,
      perl = TRUE
    )) %>%
    filter(!grepl(paste0(cg_pattern, ":rank_gap:as\\.factor"), term)) %>%
    mutate(
      model = model_label,
      group = case_when(
        grepl(paste0("^", cg_pattern, ":as\\.factor"), term) ~ "CG × Treatment",
        TRUE ~ "RG × Treatment"
      ),
      treatment = sub(".*as\\.factor\\([^)]*\\)", "", term) %>%
        sub(":.*$", "", .)
    ) %>%
    mutate(
      sd = if_else(group == "CG × Treatment", cg_sd, rg_sd),
      across(c(estimate, conf.low, conf.high, std.error), ~ . * sd),
      conf.low95 = estimate - qt(0.975, df) * std.error,
      conf.high95 = estimate + qt(0.975, df) * std.error
    ) %>%
    select(-sd)
}

coef_plot_both <- bind_rows(
  extract_coef_plot(m_winsorized, "crime_gap_wins", "m_winsorized", crime_gap_wins_sd, rank_gap_sd),
  extract_coef_plot(
    m_exclude_extreme,
    "log_crime_gap",
    "m_exclude_extreme",
    log_crime_gap_exclude_sd,
    rank_gap_sd
  ),
  extract_coef_plot(m_log, "log_crime_gap", "m_log", log_crime_gap_sd, rank_gap_sd)
)

inc_update_coef_plot <- ggplot(
  subset(
    coef_plot_both,
    model == "m_winsorized" &
      treatment != "control2"
  ),
  aes(
    y = treatment,
    x = estimate
  )
) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey50") +
  geom_errorbar(
    aes(xmin = conf.low, xmax = conf.high),
    orientation = "y",
    width = 0,
    linewidth = 0.5,
    position = position_dodge(width = 0.5)
  ) +
  geom_errorbar(
    aes(xmin = conf.low95, xmax = conf.high95),
    orientation = "y",
    width = 0,
    linewidth = 2,
    alpha = 0.4,
    position = position_dodge(width = 0.5)
  ) +
  geom_point(position = position_dodge(width = 0.5)) +
  facet_wrap(~group, scales = "free_x") +
  labs(
    y = "Treatment group",
    x = "Standardized coefficient (1 SD increase in predictor)",
    caption = paste0(
      "N = ",
      m_winsorized$nobs,
      ", thick bar 95% CI, thin 99% CI"
    )
  ) +
  theme_minimal()

print(inc_update_coef_plot)

ggsave(
  "latex/images/inc_update_coef_plot.pdf",
  plot = inc_update_coef_plot,
  width = 7,
  height = 4.5
)

spec_differences <- ggplot(
  coef_plot_both,
  aes(
    y = treatment,
    x = estimate,
    color = model
  )
) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey50") +
  geom_errorbar(
    aes(xmin = conf.low, xmax = conf.high),
    orientation = "y",
    width = 0,
    linewidth = 0.5,
    position = position_dodge(width = 0.5)
  ) +
  geom_errorbar(
    aes(xmin = conf.low95, xmax = conf.high95),
    orientation = "y",
    width = 0,
    linewidth = 2,
    alpha = 0.4,
    position = position_dodge(width = 0.5)
  ) +
  geom_point(position = position_dodge(width = 0.5)) +
  facet_wrap(~group, scales = "free_x") +
  scale_color_brewer(
    palette = "Dark2",
    breaks = c("m_winsorized", "m_log", "m_exclude_extreme"),
    labels = c(
      "m_winsorized" = "Winsorized level gap",
      "m_log" = "Log gap (all)",
      "m_exclude_extreme" = "Log gap (extremes dropped)"
    )
  ) +
  labs(
    y = "Treatment group",
    x = "Standardized coefficient (1 SD increase in predictor)",
    color = "Specification",
    caption = paste0(
      "N = ",
      m_winsorized$nobs,
      ", thick bar 95% CI, thin 99% CI"
    )
  ) +
  theme_minimal()

print(spec_differences)

ggsave(
  "latex/images/inc_update_spec_differences_plot.pdf",
  plot = spec_differences,
  width = 7,
  height = 4.5
)

m_attn_all <- lm_robust(
  Home_Crime_Handling_Change ~
    crime_gap_wins * as.factor(Treatment_Group) + rank_gap * as.factor(Treatment_Group) +
    comp_party_known,
  alpha = ci_alpha,
  data = panel_with_failures,
  se_type = "HC2"
)

attn_check_compare <- bind_rows(
  extract_coef_plot(
    m_winsorized,
    "crime_gap_wins",
    "Excludes attn-check failures",
    crime_gap_wins_sd,
    rank_gap_sd
  ),
  extract_coef_plot(
    m_attn_all,
    "crime_gap_wins",
    "Includes attn-check failures",
    crime_gap_wins_sd,
    rank_gap_sd
  )
)

attn_check_coef_compare <- ggplot(
  subset(attn_check_compare, treatment != "control2"),
  aes(
    y = treatment,
    x = estimate,
    color = model
  )
) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey50") +
  geom_errorbar(
    aes(xmin = conf.low, xmax = conf.high),
    orientation = "y",
    width = 0,
    linewidth = 0.5,
    position = position_dodge(width = 0.5)
  ) +
  geom_errorbar(
    aes(xmin = conf.low95, xmax = conf.high95),
    orientation = "y",
    width = 0,
    linewidth = 2,
    alpha = 0.4,
    position = position_dodge(width = 0.5)
  ) +
  geom_point(position = position_dodge(width = 0.5)) +
  facet_wrap(~group, scales = "free_x") +
  scale_color_brewer(palette = "Dark2") +
  labs(
    y = "Treatment group",
    x = "Standardized coefficient (1 SD increase in predictor)",
    color = "Sample",
    caption = paste0(
      "N = ",
      m_winsorized$nobs,
      " (excl. failures) vs ",
      m_attn_all$nobs,
      " (incl. failures)",
      ", thick bar 95% CI, thin 99% CI"
    )
  ) +
  theme_minimal()

print(attn_check_coef_compare)

ggsave(
  "latex/images/inc_update_incl_failed_attn_plot.pdf",
  plot = attn_check_coef_compare,
  width = 7,
  height = 4.5
)

panel$t_pooled_control <- panel$Treatment_Group
panel$t_pooled_control[panel$Treatment_Group == "control2"] <- "control"

m_log_pooled <- lm_robust(
  Home_Crime_Handling_Change ~
    log_crime_gap *
      as.factor(t_pooled_control) +
      rank_gap * as.factor(t_pooled_control) +
      comp_party_known,
  alpha = ci_alpha,
  data = panel,
  se_type = "HC2"
)

coef_plot_pooled <- extract_coef_plot(
  m_log_pooled,
  "log_crime_gap",
  "m_log_pooled",
  log_crime_gap_sd,
  rank_gap_sd
)

inc_update_coef_plot_pooled <- ggplot(
  coef_plot_pooled,
  aes(
    y = treatment,
    x = estimate
  )
) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey50") +
  geom_errorbar(
    aes(xmin = conf.low, xmax = conf.high),
    orientation = "y",
    width = 0,
    linewidth = 0.5,
    position = position_dodge(width = 0.5)
  ) +
  geom_errorbar(
    aes(xmin = conf.low95, xmax = conf.high95),
    orientation = "y",
    width = 0,
    linewidth = 2,
    alpha = 0.4,
    position = position_dodge(width = 0.5)
  ) +
  geom_point(position = position_dodge(width = 0.5)) +
  facet_wrap(~group, scales = "free_x") +
  labs(
    y = "Treatment group",
    x = "Standardized coefficient (1 SD increase in predictor)",
    caption = paste0(
      "N = ",
      m_log_pooled$nobs,
      ", thick bar 95% CI, thin 99% CI"
    )
  ) +
  theme_minimal()

print(inc_update_coef_plot_pooled)

ggsave(
  "latex/images/incumbent_rank_coef_update_pooled.pdf",
  plot = inc_update_coef_plot_pooled,
  width = 7,
  height = 4.5
)

rg_thresh <- 0.25

panel$actual_rank_25 <- 1 +
  rowSums(
    sapply(paste0("comp_rate_", 1:4), function(col) {
      panel[[col]] < (1 - rg_thresh) * panel$home_rate
    }),
    na.rm = TRUE
  )

panel$rank_gap_25 <- panel$rank_prior - panel$actual_rank_25

rank_gap_25_sd <- sd(panel$rank_gap_25, na.rm = TRUE)

m_rg25 <- lm_robust(
  Home_Crime_Handling_Change ~
    crime_gap_wins * as.factor(Treatment_Group) + rank_gap_25 * as.factor(Treatment_Group) +
    comp_party_known,
  alpha = ci_alpha,
  data = panel,
  se_type = "HC2"
)

coef_plot_rg25 <- extract_coef_plot(
  m_rg25,
  "crime_gap_wins",
  "m_rg25",
  crime_gap_wins_sd,
  rank_gap_25_sd
)

inc_update_coef_plot_rg25 <- ggplot(
  coef_plot_rg25,
  aes(
    y = treatment,
    x = estimate
  )
) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey50") +
  geom_errorbar(
    aes(xmin = conf.low, xmax = conf.high),
    orientation = "y",
    width = 0,
    linewidth = 0.5,
    position = position_dodge(width = 0.5)
  ) +
  geom_errorbar(
    aes(xmin = conf.low95, xmax = conf.high95),
    orientation = "y",
    width = 0,
    linewidth = 2,
    alpha = 0.4,
    position = position_dodge(width = 0.5)
  ) +
  geom_point(position = position_dodge(width = 0.5)) +
  facet_wrap(~group, scales = "free_x") +
  labs(
    y = "Treatment group",
    x = "Standardized coefficient (1 SD increase in predictor)",
    caption = paste0("N = ", m_rg25$nobs, ", thick bar 95% CI, thin 99% CI")
  ) +
  theme_minimal()

print(inc_update_coef_plot_rg25)

ggsave(
  "latex/images/inc_update_alt_rank_gap_plot.pdf",
  plot = inc_update_coef_plot_rg25,
  width = 7,
  height = 4.5
)

car::linearHypothesis(
  m_rg25,
  "as.factor(Treatment_Group)control2:rank_gap_25 = as.factor(Treatment_Group)T3:rank_gap_25"
)
