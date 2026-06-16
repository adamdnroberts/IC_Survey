library(estimatr)
library(modelsummary)
library(dplyr)
library(ggplot2)
library(broom)
library(readxl)

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

# Append the second match table to the first and drop exact duplicate pairs.
match_ids <- bind_rows(match_ids, match_ids2) %>%
  distinct(pid_w2, pid_w1)

# Drop wave-2 PIDs that map to more than one wave-1 PID (ambiguous match).
match_ids <- match_ids %>%
  add_count(pid_w2, name = "n_w2") %>%
  filter(n_w2 == 1) %>%
  select(pid_w2, pid_w1)

# When one wave-1 PID maps to multiple wave-2 PIDs, keep the wave-2 response
# with the earliest timestamp.
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

test <- wave2 %>%
  inner_join(match_ids, by = c("Netquest_PID" = "pid_w2")) %>%
  inner_join(
    wave1,
    by = c("pid_w1" = "Netquest_PID"),
    suffix = c("_w2", "_w1")
  ) %>%
  select(-pid_w1) %>%
  filter(Found_Municipality_ID_w2 == Found_Municipality_ID_w1) %>%
  # # Found_Municipality_ID is present in both waves, so the join suffixed it;
  # # the two are equal after the filter, so rebuild the unified column the
  # # downstream code expects.
  mutate(
    Found_Municipality_ID = Found_Municipality_ID_w2,
    # Days between wave-1 and wave-2 responses (timestamps are character strings
    # like "2026-04-10 14:57:42.651886").
    ts_w1 = as.POSIXct(Timestamp_w1, format = "%Y-%m-%d %H:%M:%OS", tz = "UTC"),
    ts_w2 = as.POSIXct(Timestamp_w2, format = "%Y-%m-%d %H:%M:%OS", tz = "UTC"),
    days_between = as.numeric(difftime(ts_w2, ts_w1, units = "days"))
  )

sum(test$NQ_Age_w1 == test$NQ_Age_w2) / nrow(test)
sum(test$NQ_Sex_w1 == test$NQ_Sex_w2) / nrow(test)
sum(test$NQ_Region_w1 == test$NQ_Region_w2) / nrow(test)
sum(test$NQ_SEL_w1 == test$NQ_SEL_w2) / nrow(test)


# match_keys <- c(
#   "NQ_Sex",
#   "NQ_Age",
#   "NQ_Region",
#   "NQ_SEL",
#   "Found_Municipality_ID"
# )
#
# wave1_unique <- wave1 %>%
#   filter(as.POSIXct(Timestamp) <= as.POSIXct("2026-04-27 23:59:59")) %>%
#   group_by(across(all_of(match_keys))) %>%
#   filter(n() == 1) %>%
#   ungroup()
#
# wave2_unique <- wave2 %>%
#   group_by(across(all_of(match_keys))) %>%
#   filter(n() == 1) %>%
#   ungroup()
#
# test <- inner_join(
#   wave2_unique,
#   wave1_unique,
#   by = match_keys,
#   suffix = c("_w2", "_w1")
# )

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

# winsorize <- function(x, probs = c(0.05, 0.95)) {
#   qs <- quantile(x, probs, na.rm = TRUE)
#   pmax(pmin(x, qs[2]), qs[1])
# }

test$Robbery_Estimate_wins <- pmin(
  as.numeric(test$Robbery_Estimate),
  max(test$home_rate, na.rm = TRUE) * 10
)

test$CG <- as.numeric(test$Robbery_Estimate) - test$home_rate
test$CG_wins <- test$Robbery_Estimate_wins - test$home_rate
test$RG <- test$rank_prior - test$actual_rank

ggplot(test, aes(x = actual_rank, fill = Treatment_Group)) +
  geom_histogram(binwidth = 1, position = "dodge") +
  facet_wrap(~Treatment_Group) +
  theme_bw()

test$log_CG <- sign(test$CG) * log(abs(test$CG))

test$CG_decile <- dplyr::ntile(test$CG, 10)

test$Home_Crime_Handling_Change <- as.numeric(test$Home_Crime_Handling_Post) -
  as.numeric(test$Home_Crime_Handling_Pre)

test$CI <- 6 - as.numeric(test$Importance_Crime)

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
      if (length(coalitions) == 1) coalitions else NA_character_
    },
    USE.NAMES = FALSE
  )
}

test$coalition_pre <- to_coalition(test$Vote_Intention_Pre)
test$coalition_post <- to_coalition(test$Vote_Intention_Post)

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

test <- test %>%
  left_join(all_parties_tmp, join_by("Found_Municipality_ID" == "muni_id"))

test$Vote_Switch <- case_when(
  is.na(test$coalition_pre) | is.na(test$coalition_post) ~ NA_integer_,
  test$coalition_pre == test$coalition_post ~ 0L,
  test$coalition_post == test$home_coalition ~ 1L,
  test$coalition_pre == test$home_coalition ~ -1L,
  TRUE ~ 0L
)

test$Vote_home_post <- as.integer(
  !is.na(test$coalition_post) &
    !is.na(test$home_coalition) &
    test$home_coalition == test$coalition_post
)

test$MORENA_Change <- as.numeric(test$MORENA_Crime_Rating_Post) -
  as.numeric(test$MORENA_Crime_Rating_Pre)
test$PAN_PRI_PRD_Change <- as.numeric(
  test$Coalition_PAN_PRI_PRD_Crime_Rating_Post
) -
  as.numeric(test$Coalition_PAN_PRI_PRD_Crime_Rating_Pre)
test$MC_Change <- as.numeric(test$MC_Crime_Rating_Post) -
  as.numeric(test$MC_Crime_Rating_Pre)

# Keep a copy that still includes attention-check failures for the
# sensitivity comparison at the end of the script.
# Require at least 6 days between the two waves (drop fast re-takes / mismatches).
test <- filter(test, !is.na(days_between) & days_between >= 6)

test_with_failures <- test
test <- filter(test, Attention_Check == "somewhat_agree")
#test <- filter(test, abs(CG) < 100000)

m1_lm <- lm(
  Home_Crime_Handling_Change ~
    CG_wins * as.factor(Treatment_Group) + RG * as.factor(Treatment_Group),
  data = test
)

m_log_lm <- lm(
  Home_Crime_Handling_Change ~
    log_CG *
      as.factor(Treatment_Group) +
      RG * as.factor(Treatment_Group),
  data = test
)

#plot(m_log_lm)

stargazer::stargazer(m1_lm, type = "text")

cooks_thresh <- 0.5
test_no_influential <- test[cooks.distance(m1_lm) <= cooks_thresh, ]

m1 <- lm_robust(
  Home_Crime_Handling_Change ~
    CG_wins * as.factor(Treatment_Group) + RG * as.factor(Treatment_Group),
  alpha = 0.01,
  data = test,
  se_type = "HC2"
)

m1_no_inf <- lm_robust(
  Home_Crime_Handling_Change ~
    CG * as.factor(Treatment_Group) + RG * as.factor(Treatment_Group),
  alpha = 0.01,
  data = test_no_influential,
  se_type = "HC2"
)

m1_exclude_extreme <- lm_robust(
  Home_Crime_Handling_Change ~
    log_CG * as.factor(Treatment_Group) + RG * as.factor(Treatment_Group),
  alpha = 0.01,
  data = subset(
    test,
    as.numeric(Robbery_Estimate) <= max(test$home_rate) * 10
  ),
  se_type = "HC2"
)

summary(m1_exclude_extreme)

test$negative_CG <- ifelse(sign(test$CG) == -1, 1, 0)

m1_log <- lm_robust(
  Home_Crime_Handling_Change ~
    log_CG *
      as.factor(Treatment_Group) +
      RG * as.factor(Treatment_Group),
  alpha = 0.01,
  data = test[-c(20, 325, 370), ], # log_CG < 30),
  se_type = "HC2"
)

m1_decile <- lm_robust(
  Home_Crime_Handling_Change ~
    CG_decile * as.factor(Treatment_Group) + RG * as.factor(Treatment_Group),
  alpha = 0.01,
  data = subset(test, abs(CG) < 50000),
  se_type = "HC2"
)

m2 <- lm_robust(
  Home_Crime_Handling_Change ~
    log_CG * as.factor(Treatment_Group) + RG * as.factor(Treatment_Group),
  alpha = 0.01,
  data = subset(test, Treatment_Group != "control" & Treatment_Group != "T1"),
  se_type = "HC2"
)

m3 <- lm_robust(
  Home_Crime_Handling_Change ~
    log_CG *
      CI *
      as.factor(Treatment_Group) +
      RG * CI * as.factor(Treatment_Group),
  alpha = 0.01,
  data = test,
  se_type = "HC2"
)

summary(m3)

#modelsummary(list(m1, m1_log), stars = TRUE)

outcome_sd <- sd(test$Home_Crime_Handling_Change, na.rm = TRUE)
logCG_sd <- sd(subset(test, abs(CG) < 50000)$log_CG, na.rm = TRUE)
CG_wins_sd <- sd(test$CG_wins, na.rm = TRUE)

test_exclude_extreme <- subset(
  test,
  as.numeric(Robbery_Estimate) <= max(test$home_rate) * 10
)

CG_exclude_sd <- sd(test_exclude_extreme$CG)
logCG_exclude_sd <- sd(test_exclude_extreme$log_CG, na.rm = TRUE)
RG_sd <- sd(test$RG, na.rm = TRUE)

extract_coef_plot <- function(model, cg_pattern, model_label, cg_sd, rg_sd) {
  tidy(model, conf.int = TRUE) %>%
    filter(grepl(
      paste0(
        cg_pattern,
        ":as\\.factor|as\\.factor.*:RG(?!:)|(?<=:)RG:as\\.factor"
      ),
      term,
      perl = TRUE
    )) %>%
    filter(!grepl(paste0(cg_pattern, ":RG:as\\.factor"), term)) %>%
    mutate(
      model = model_label,
      group = case_when(
        grepl(paste0("^", cg_pattern, ":as\\.factor"), term) ~ "CG × Treatment",
        TRUE ~ "RG × Treatment"
      ),
      treatment = sub(".*Treatment_Group\\)", "", term) %>% sub(":.*$", "", .)
    ) %>%
    mutate(
      sd = if_else(group == "CG × Treatment", cg_sd, rg_sd),
      across(c(estimate, conf.low, conf.high), ~ . * sd)
    ) %>%
    select(-sd)
}

coef_plot_both <- bind_rows(
  extract_coef_plot(m1, "CG_wins", "m1 (wins CG)", CG_wins_sd, RG_sd),
  extract_coef_plot(
    m1_exclude_extreme,
    "log_CG",
    "m1 (exclude extreme)",
    logCG_exclude_sd,
    RG_sd
  ),
  extract_coef_plot(m1_log, "log_CG", "m1 (log CG)", logCG_sd, RG_sd)
)

ggplot(
  coef_plot_both,
  aes(
    y = treatment,
    x = estimate,
    xmin = conf.low,
    xmax = conf.high,
    color = model
  )
) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey50") +
  geom_pointrange(position = position_dodge(width = 0.5)) +
  facet_wrap(~group, scales = "free_x") +
  labs(
    y = "Treatment group",
    x = "Standardized coefficient (1 SD increase in predictor)",
    color = "Model",
    title = "m1 vs m1_log interaction coefficients",
    caption = paste0("N = ", m1$nobs)
  ) +
  theme_minimal()

inc_rank_coef_update <- ggplot(
  subset(
    coef_plot_both,
    model == "m1 (wins CG)" &
      treatment != "control2"
  ),
  aes(
    y = treatment,
    x = estimate,
    xmin = conf.low,
    xmax = conf.high
  )
) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey50") +
  geom_pointrange(position = position_dodge(width = 0.5)) +
  facet_wrap(~group, scales = "free_x") +
  labs(
    y = "Treatment group",
    x = "Standardized coefficient (1 SD increase in predictor)",
    #color = "Model",
    #title = "m1 vs m1_log interaction coefficients",
    caption = paste0("N = ", m1$nobs)
  ) +
  theme_minimal()

print(inc_rank_coef_update)

ggsave(
  "latex/images/incumbent_rank_coef_update.pdf",
  plot = inc_rank_coef_update,
  width = 7,
  height = 4.5
)

# ── Attention-check sensitivity: include vs. exclude attn-check failures ──────
# Re-fit the plotted model (m1, winsorized CG) on the full sample that still
# contains attention-check failures, and overlay it against the passers-only
# fit. Both are standardized by the same SDs so the comparison is on one scale.
m1_attn_all <- lm_robust(
  Home_Crime_Handling_Change ~
    CG_wins * as.factor(Treatment_Group) + RG * as.factor(Treatment_Group),
  alpha = 0.01,
  data = test_with_failures,
  se_type = "HC2"
)

attn_check_compare <- bind_rows(
  extract_coef_plot(
    m1,
    "CG_wins",
    "Excludes attn-check failures",
    CG_wins_sd,
    RG_sd
  ),
  extract_coef_plot(
    m1_attn_all,
    "CG_wins",
    "Includes attn-check failures",
    CG_wins_sd,
    RG_sd
  )
)

attn_check_coef_compare <- ggplot(
  subset(attn_check_compare, treatment != "control2"),
  aes(
    y = treatment,
    x = estimate,
    xmin = conf.low,
    xmax = conf.high,
    color = model
  )
) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey50") +
  geom_pointrange(position = position_dodge(width = 0.5)) +
  facet_wrap(~group, scales = "free_x") +
  labs(
    y = "Treatment group",
    x = "Standardized coefficient (1 SD increase in predictor)",
    color = "Sample",
    title = "Attention-check sensitivity: belief-updating model",
    caption = paste0(
      "N = ",
      m1$nobs,
      " (excl. failures) vs ",
      m1_attn_all$nobs,
      " (incl. failures)"
    )
  ) +
  theme_minimal()

print(attn_check_coef_compare)

ggsave(
  "latex/images/attn_check_coef_compare.pdf",
  plot = attn_check_coef_compare,
  width = 7,
  height = 4.5
)
