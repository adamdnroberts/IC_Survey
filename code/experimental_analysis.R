library(estimatr)
library(modelsummary)
library(dplyr)
library(ggplot2)
library(broom)

wave1 <- readRDS("data/wave1_responses.rds")
wave2 <- readRDS("data/wave2_responses.rds")

match_ids <- read.csv(
  "data/wave_match_ids.csv",
  stringsAsFactors = FALSE,
  fileEncoding = "UTF-8-BOM"
)
names(match_ids) <- trimws(names(match_ids))
names(match_ids) <- c("pid_w2", "pid_w1")

test <- wave2 %>%
  inner_join(match_ids, by = c("Netquest_PID" = "pid_w2")) %>%
  inner_join(
    wave1,
    by = c("pid_w1" = "Netquest_PID"),
    suffix = c("_w2", "_w1")
  ) %>%
  select(-pid_w1) %>%
  filter(Found_Municipality_ID_w2 == Found_Municipality_ID_w1)

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
  left_join(robo, by = c("Found_Municipality_ID_w2" = "Cve..Municipio")) %>%
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
    log_CG * as.factor(Treatment_Group) + RG * as.factor(Treatment_Group),
  data = test,
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

modelsummary(list(m1, m1_log), stars = TRUE)

coef_plot_data <- tidy(m1_log, conf.int = TRUE) %>%
  filter(grepl("log_CG:as\\.factor|as\\.factor.*:RG", term)) %>%
  mutate(
    group = ifelse(
      grepl("^log_CG:", term),
      "CG × Treatment",
      "RG × Treatment"
    ),
    treatment = sub(":.*$", "", sub(".*Treatment_Group\\)", "", term))
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
    title = "Model 1 interaction coefficients"
  ) +
  theme_minimal()

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

lim1 <- max(abs(c(test$log_CG, test$log_pcg)), na.rm = TRUE)

test$test <- as.numeric(test$Robbery_Estimate_Post) -
  as.numeric(test$Robbery_Estimate)
test$rank_update <- test$rank_post - test$rank_prior

ggplot(
  test,
  aes(
    x = sign(CG) * log(abs(CG) + 1),
    y = sign(test) * log(abs(test) + 1),
    color = info_treatment
  )
) +
  geom_point(alpha = 0.4) +
  geom_smooth(method = "lm", formula = y ~ x, se = FALSE) +
  labs(
    x = "Prior accuracy (signed log), negative = optimism",
    y = "Update (Posterior - Prior) (signed log)"
  ) +
  theme_bw()

lim2 <- max(abs(c(test$RG, test$rank_update)), na.rm = TRUE)

ggplot(
  subset(test, Treatment_Group %in% c("control2", "T2", "T3", "T4")),
  aes(x = RG, y = rank_update, color = comparison_treatment)
) +
  geom_jitter(width = 0.2, height = 0, alpha = 0.5) +
  geom_smooth(method = "lm", formula = y ~ x, se = FALSE) +
  labs(
    x = "Prior rank accuracy (negative = optimism)",
    y = "Update (Posterior - Prior)"
  ) +
  coord_fixed(xlim = c(-lim2, lim2), ylim = c(-lim2, lim2)) +
  theme_bw()
