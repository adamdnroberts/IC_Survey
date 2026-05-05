library(estimatr)
library(modelsummary)
library(dplyr)
library(ggplot2)
library(broom)
library(car)
library(mgcv)

wave1 <- readRDS("data/wave1_responses.rds")
wave2 <- readRDS("data/wave2_responses.rds")

match_keys <- c(
  "NQ_Sex",
  "NQ_Age",
  "NQ_Region",
  "NQ_SEL",
  "Found_Municipality_ID"
)

wave1_unique <- wave1 %>%
  filter(as.POSIXct(Timestamp) <= as.POSIXct("2026-04-27 23:59:59")) %>%
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

winsorize <- function(x, probs = c(0.05, 0.95)) {
  qs <- quantile(x, probs, na.rm = TRUE)
  pmax(pmin(x, qs[2]), qs[1])
}

test$CG <- as.numeric(test$Robbery_Estimate) - round(test$home_rate)
test$CG_wins <- winsorize(test$CG)
test$RG <- test$rank_prior - test$actual_rank

test$log_CG <- sign(test$CG) * log(abs(test$CG))

test$Home_Crime_Handling_Change <- as.numeric(test$Home_Crime_Handling_Post) -
  as.numeric(test$Home_Crime_Handling_Pre)

test$CI <- 6 - as.numeric(test$Importance_Crime)

test <- filter(test, Attention_Check == "somewhat_agree" & abs(CG) < 100000)

m <- lm_robust(
  Home_Crime_Handling_Change ~
    CG *
      as.factor(Treatment_Group) +
      RG * as.factor(Treatment_Group),
  data = test,
  se_type = "HC2"
)

m_wins <- lm_robust(
  Home_Crime_Handling_Change ~
    CG_wins * as.factor(Treatment_Group) + RG * as.factor(Treatment_Group),
  data = test,
  se_type = "HC2"
)

#test$negative_CG <- ifelse(sign(test$CG) == -1, 1, 0)

m_log <- lm_robust(
  Home_Crime_Handling_Change ~
    log_CG *
      as.factor(Treatment_Group) +
      RG * as.factor(Treatment_Group),
  data = test,
  se_type = "HC2"
)

outcome_sd <- sd(test$Home_Crime_Handling_Change, na.rm = TRUE)
CG_sd <- sd(test$CG, na.rm = TRUE)
logCG_sd <- sd(test$log_CG, na.rm = TRUE)
CG_wins_sd <- sd(test$CG_wins, na.rm = TRUE)
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

coef_data_both <- bind_rows(
  extract_coef_plot(m_wins, "CG_wins", "Winsorized CG", CG_wins_sd, RG_sd),
  extract_coef_plot(m_log, "log_CG", "Signed Log CG", logCG_sd, RG_sd)
)

coef_plot_both <- ggplot(
  coef_data_both,
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
    #title = "winsorized vs signed log interaction coefficients",
    y = "Treatment group",
    x = "Standardized coefficient (1 SD increase in perrception gap)",
    color = "Model",
    caption = paste0("N = ", m_wins$nobs)
  ) +
  theme_minimal()

print(coef_plot_both)

ggsave(
  "latex/images/incumbent_coef_update.pdf",
  plot = coef_plot_both,
  width = 6,
  height = 6
)

coef_data_all <- bind_rows(
  extract_coef_plot(m, "CG", "m1", CG_sd, RG_sd),
  extract_coef_plot(m_wins, "CG_wins", "m1 (wins CG)", CG_wins_sd, RG_sd),
  extract_coef_plot(m_log, "log_CG", "m1 (log CG)", logCG_sd, RG_sd)
)

coef_plot_all <- ggplot(
  coef_data_all,
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
    # title = "m1 vs m1_log interaction coefficients",
    y = "Treatment group",
    x = "Standardized coefficient (1 SD increase in perrception gap)",
    color = "Model",
    caption = paste0("N = ", m_wins$nobs)
  ) +
  theme_minimal()

print(coef_plot_all)

linearHypothesis(
  m_wins,
  "CG_wins:as.factor(Treatment_Group)T1 = 0"
)

linearHypothesis(
  m_wins,
  "as.factor(Treatment_Group)T2:RG - as.factor(Treatment_Group)T1:RG = 0"
)

linearHypothesis(
  m_wins,
  "as.factor(Treatment_Group)T3:RG - as.factor(Treatment_Group)T2:RG = 0"
)

linearHypothesis(
  m_wins,
  "as.factor(Treatment_Group)T4:RG - as.factor(Treatment_Group)T2:RG = 0"
)

linearHypothesis(
  m_wins,
  "as.factor(Treatment_Group)T4:RG - as.factor(Treatment_Group)T3:RG"
)

# GAM model

test$Treatment_Group <- as.factor(test$Treatment_Group)

m1_gam <- gam(
  Home_Crime_Handling_Change ~
    Treatment_Group +
      s(CG_wins, by = Treatment_Group) +
      RG * Treatment_Group,
  data = test,
  method = "REML"
)

summary(m1_gam)
plot(m1_gam, pages = 1, shade = TRUE)
