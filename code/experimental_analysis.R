library(estimatr)
library(modelsummary)
library(dplyr)
library(ggplot2)
library(broom)

wave1 <- readRDS("data/wave1_responses.rds")
wave2 <- readRDS("data/wave2_responses.rds")

# match_ids <- read.csv(
#   "data/wave_match_ids.csv",
#   stringsAsFactors = FALSE,
#   fileEncoding = "UTF-8-BOM"
# )
# names(match_ids) <- trimws(names(match_ids))
# names(match_ids) <- c("pid_w2", "pid_w1")
#
# test <- wave2 %>%
#   inner_join(match_ids, by = c("Netquest_PID" = "pid_w2")) %>%
#   inner_join(
#     wave1,
#     by = c("pid_w1" = "Netquest_PID"),
#     suffix = c("_w2", "_w1")
#   ) %>%
#   select(-pid_w1) %>%
#   filter(Found_Municipality_ID_w2 == Found_Municipality_ID_w1)

match_keys <- c(
  "NQ_Sex",
  "NQ_Age",
  "NQ_Region",
  "NQ_SEL",
  "Found_Municipality_ID"
)

wave1_unique <- wave1 %>%
  filter(as.POSIXct(Timestamp) <= as.POSIXct("2026-04-21 23:59:59")) %>%
  group_by(across(all_of(match_keys))) %>%
  filter(n() == 1) %>%
  ungroup()

wave2_unique <- wave2 %>%
  group_by(across(all_of(match_keys))) %>%
  filter(n() == 1) %>%
  ungroup()

test <- inner_join(
  wave2_unique,
  wave1_unique,
  by = match_keys,
  suffix = c("_w2", "_w1")
)

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

test$CG <- as.numeric(test$Robbery_Estimate) - test$home_rate
test$RG <- test$rank_prior - test$actual_rank

test$log_CG <- sign(test$CG) * log(abs(test$CG) + 1)

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
test$Vote_Switch <- as.integer(
  !is.na(test$coalition_pre) &
    !is.na(test$coalition_post) &
    test$coalition_pre != test$coalition_post
)

test$MORENA_Change <- as.numeric(test$MORENA_Crime_Rating_Post) -
  as.numeric(test$MORENA_Crime_Rating_Pre)
test$PAN_PRI_PRD_Change <- as.numeric(
  test$Coalition_PAN_PRI_PRD_Crime_Rating_Post
) -
  as.numeric(test$Coalition_PAN_PRI_PRD_Crime_Rating_Pre)
test$MC_Change <- as.numeric(test$MC_Crime_Rating_Post) -
  as.numeric(test$MC_Crime_Rating_Pre)

test <- filter(test, Attention_Check == "somewhat_agree")

m1_lm <- lm(
  Home_Crime_Handling_Change ~
    CG * as.factor(Treatment_Group) + RG * as.factor(Treatment_Group),
  data = test
)

cooks_thresh <- 0.5
test_no_influential <- test[cooks.distance(m1_lm) <= cooks_thresh, ]

m1 <- lm_robust(
  Home_Crime_Handling_Change ~
    CG * as.factor(Treatment_Group) + RG * as.factor(Treatment_Group),
  data = test,
  se_type = "HC2"
)

m1_no_inf <- lm_robust(
  Home_Crime_Handling_Change ~
    CG * as.factor(Treatment_Group) + RG * as.factor(Treatment_Group),
  data = test_no_influential,
  se_type = "HC2"
)

m1_log <- lm_robust(
  Home_Crime_Handling_Change ~
    log_CG *
      as.factor(Treatment_Group) +
      RG * as.factor(Treatment_Group),
  data = subset(test, log_CG < 30),
  se_type = "HC2"
)

m1_decile <- lm_robust(
  Home_Crime_Handling_Change ~
    CG_decile * as.factor(Treatment_Group) + RG * as.factor(Treatment_Group),
  data = test,
  se_type = "HC2"
)

m2 <- lm_robust(
  Home_Crime_Handling_Change ~
    log_CG * as.factor(Treatment_Group) + RG * as.factor(Treatment_Group),
  data = subset(test, Treatment_Group != "control" & Treatment_Group != "T1"),
  se_type = "HC2"
)

m3 <- lm_robust(
  Home_Crime_Handling_Change ~
    CG * CI * as.factor(Treatment_Group) + RG * CI * as.factor(Treatment_Group),
  data = test,
  se_type = "HC2"
)

#modelsummary(list(m1, m1_log), stars = TRUE)

coef_plot_data <- tidy(m1_log, conf.int = TRUE) %>%
  filter(grepl(
    "log_CG:as\\.factor|as\\.factor.*:RG(?!:)|(?<=:)RG:as\\.factor",
    term,
    perl = TRUE
  )) %>%
  filter(!grepl("log_CG:RG:as\\.factor", term)) %>%
  mutate(
    group = case_when(
      grepl("^log_CG:as\\.factor", term) ~ "CG × Treatment",
      TRUE ~ "RG × Treatment"
    ),
    treatment = sub(".*Treatment_Group\\)", "", term) %>% sub(":.*$", "", .)
  )

ggplot(
  coef_plot_data,
  aes(y = treatment, x = estimate, xmin = conf.low, xmax = conf.high)
) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey50") +
  geom_pointrange() +
  facet_wrap(~group, scales = "free_x") +
  labs(
    y = "Treatment group",
    x = "Coefficient estimate",
    title = "Model 1 interaction coefficients",
    caption = paste0("N = ", m1_log$nobs)
  ) +
  theme_minimal()

# ── Vote intention switch model ───────────────────────────────────────────────

m_vote <- lm_robust(
  Vote_Switch ~
    log_CG * as.factor(Treatment_Group) + RG * as.factor(Treatment_Group),
  data = subset(test, !is.na(Vote_Switch)),
  se_type = "HC2"
)

summary(m_vote)

coef_plot_data_vote <- tidy(m_vote, conf.int = TRUE) %>%
  filter(grepl(
    "log_CG:as\\.factor|as\\.factor.*:RG(?!:)|(?<=:)RG:as\\.factor",
    term,
    perl = TRUE
  )) %>%
  filter(!grepl("log_CG:RG:as\\.factor", term)) %>%
  mutate(
    group = case_when(
      grepl("^log_CG:as\\.factor", term) ~ "CG × Treatment",
      TRUE ~ "RG × Treatment"
    ),
    treatment = sub(".*Treatment_Group\\)", "", term) %>% sub(":.*$", "", .)
  )

ggplot(
  coef_plot_data_vote,
  aes(y = treatment, x = estimate, xmin = conf.low, xmax = conf.high)
) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey50") +
  geom_pointrange() +
  facet_wrap(~group, scales = "free_x") +
  labs(
    y = "Treatment group",
    x = "Coefficient estimate",
    title = "Coalition switch: interaction coefficients",
    caption = paste0("N = ", m_vote$nobs)
  ) +
  theme_minimal()

# ── Coalition crime handling change models ────────────────────────────────────

coalition_interaction_formula <- function(outcome) {
  as.formula(paste0(
    outcome,
    " ~ log_CG * as.factor(Treatment_Group) + RG * as.factor(Treatment_Group)"
  ))
}

m_morena <- lm_robust(
  coalition_interaction_formula("MORENA_Change"),
  data = test,
  se_type = "HC2"
)

m_pan <- lm_robust(
  coalition_interaction_formula("PAN_PRI_PRD_Change"),
  data = test,
  se_type = "HC2"
)

m_mc <- lm_robust(
  coalition_interaction_formula("MC_Change"),
  data = test,
  se_type = "HC2"
)

coalition_models <- list(
  "MORENA/PVEM/PT" = m_morena,
  "PAN/PRI/PRD" = m_pan,
  "MC" = m_mc
)

coef_plot_data_coalitions <- bind_rows(lapply(
  names(coalition_models),
  function(nm) {
    tidy(coalition_models[[nm]], conf.int = TRUE) %>%
      filter(grepl(
        "log_CG:as\\.factor|as\\.factor.*:RG(?!:)|(?<=:)RG:as\\.factor",
        term,
        perl = TRUE
      )) %>%
      filter(!grepl("log_CG:RG:as\\.factor", term)) %>%
      mutate(
        coalition = nm,
        group = case_when(
          grepl("^log_CG:as\\.factor", term) ~ "CG × Treatment",
          TRUE ~ "RG × Treatment"
        ),
        treatment = sub(".*Treatment_Group\\)", "", term) %>% sub(":.*$", "", .)
      )
  }
))

ggplot(
  coef_plot_data_coalitions,
  aes(
    y = treatment,
    x = estimate,
    xmin = conf.low,
    xmax = conf.high,
    color = coalition
  )
) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey50") +
  geom_pointrange(position = position_dodge(width = 0.5)) +
  facet_wrap(~group, scales = "free_x") +
  labs(
    y = "Treatment group",
    x = "Coefficient estimate",
    color = "Coalition",
    title = "Coalition crime handling change: interaction coefficients"
  ) +
  theme_minimal()

# ── Adaptive shrinkage: joint mashr across outcomes ───────────────────────────

library(mashr)

extract_terms <- function(model, pattern) {
  terms <- grep(pattern, names(coef(model)), value = TRUE)
  V <- vcov(model)
  list(
    betas = coef(model)[terms],
    ses = sqrt(diag(V)[terms]),
    terms = terms
  )
}

cg_pattern <- "log_CG:as\\.factor|as\\.factor.*:log_CG"
rg_pattern <- "^RG:as\\.factor|as\\.factor.*:RG$"

cg_crime <- extract_terms(m1_log, cg_pattern)
cg_vote <- extract_terms(m_vote, cg_pattern)
cg_morena <- extract_terms(m_morena, cg_pattern)
cg_pan <- extract_terms(m_pan, cg_pattern)
cg_mc <- extract_terms(m_mc, cg_pattern)

rg_crime <- extract_terms(m1_log, rg_pattern)
rg_vote <- extract_terms(m_vote, rg_pattern)
rg_morena <- extract_terms(m_morena, rg_pattern)
rg_pan <- extract_terms(m_pan, rg_pattern)
rg_mc <- extract_terms(m_mc, rg_pattern)

# Rows = treatment arms, cols = outcomes
arm_labels <- sub(".*Treatment_Group\\)", "", cg_crime$terms)

B_cg <- cbind(
  crime_handling = cg_crime$betas,
  vote_switch = cg_vote$betas,
  morena = cg_morena$betas,
  pan_pri_prd = cg_pan$betas,
  mc = cg_mc$betas
)
S_cg <- cbind(
  crime_handling = cg_crime$ses,
  vote_switch = cg_vote$ses,
  morena = cg_morena$ses,
  pan_pri_prd = cg_pan$ses,
  mc = cg_mc$ses
)
rownames(B_cg) <- rownames(S_cg) <- arm_labels

B_rg <- cbind(
  crime_handling = rg_crime$betas,
  vote_switch = rg_vote$betas,
  morena = rg_morena$betas,
  pan_pri_prd = rg_pan$betas,
  mc = rg_mc$betas
)
S_rg <- cbind(
  crime_handling = rg_crime$ses,
  vote_switch = rg_vote$ses,
  morena = rg_morena$ses,
  pan_pri_prd = rg_pan$ses,
  mc = rg_mc$ses
)
rownames(B_rg) <- rownames(S_rg) <- sub(
  ".*Treatment_Group\\)|:RG$",
  "",
  rg_crime$terms
)

mash_cg <- mash(
  mash_set_data(B_cg, S_cg),
  cov_canonical(mash_set_data(B_cg, S_cg))
)
mash_rg <- mash(
  mash_set_data(B_rg, S_rg),
  cov_canonical(mash_set_data(B_rg, S_rg))
)

cat(
  "── mashr posterior means: CG × Treatment ────────────────────────────────\n"
)
print(round(get_pm(mash_cg), 3))
cat(
  "\n── mashr lfsr: CG × Treatment ───────────────────────────────────────────\n"
)
print(round(get_lfsr(mash_cg), 3))

cat(
  "\n── mashr posterior means: RG × Treatment ────────────────────────────────\n"
)
print(round(get_pm(mash_rg), 3))
cat(
  "\n── mashr lfsr: RG × Treatment ───────────────────────────────────────────\n"
)
print(round(get_lfsr(mash_rg), 3))

library(car)

linearHypothesis(
  m1_log,
  "as.factor(Treatment_Group)T2:RG - as.factor(Treatment_Group)T1:RG = 0"
)

linearHypothesis(
  m1_log,
  "as.factor(Treatment_Group)T3:RG - as.factor(Treatment_Group)T2:RG = 0"
)

linearHypothesis(
  m1_log,
  "as.factor(Treatment_Group)T4:RG - as.factor(Treatment_Group)T2:RG = 0"
)

linearHypothesis(
  m1_log,
  "as.factor(Treatment_Group)T4:RG - as.factor(Treatment_Group)T3:RG"
)
