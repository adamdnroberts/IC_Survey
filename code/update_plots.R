library(dplyr)
library(ggplot2)

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

test$CG <- as.numeric(test$Robbery_Estimate) - test$home_rate
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

test$robbery_estimate_update <- as.numeric(test$Robbery_Estimate_Post) -
  as.numeric(test$Robbery_Estimate)
test$rank_update <- test$rank_post - test$rank_prior

# Robbery estimate update vs. prior accuracy
ggplot(
  subset(test, log(abs(CG)) < 20),
  aes(
    x = sign(CG) * log(abs(CG) + 1),
    y = sign(robbery_estimate_update) * log(abs(robbery_estimate_update) + 1),
    color = info_treatment
  )
) +
  geom_point(alpha = 0.4) +
  geom_smooth(method = "lm", formula = y ~ 0 + x, se = FALSE) +
  labs(
    x = "Prior accuracy (signed log), negative = optimism",
    y = "Update (Posterior - Prior) (signed log)",
    caption = paste0("n = ", nrow(test) - 1, " (1 outlier removed)")
  ) +
  theme_bw()

# Rank update vs. prior rank accuracy
lim2 <- max(abs(c(test$RG, test$rank_update)), na.rm = TRUE)

ggplot(
  test,
  aes(x = RG, y = rank_update, color = comparison_treatment)
) +
  geom_jitter(width = 0.2, height = 0, alpha = 0.5) +
  geom_smooth(method = "lm", formula = y ~ 0 + x, se = FALSE) +
  labs(
    x = "Prior rank accuracy (negative = optimism)",
    y = "Update (Posterior - Prior)",
    caption = paste0("n = ", nrow(test))
  ) +
  coord_fixed(xlim = c(-lim2, lim2), ylim = c(-lim2, lim2)) +
  theme_bw()
