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
rob_sub <- subset(test, log(abs(CG)) < 20) %>%
  mutate(
    xv = sign(CG) * -log(abs(CG) + 1),
    yv = sign(robbery_estimate_update) * log(abs(robbery_estimate_update) + 1)
  ) %>%
  filter(!is.na(xv), !is.na(yv))

info_label_df <- rob_sub %>%
  group_by(info_treatment) %>%
  summarise(
    x_pos = quantile(xv, 0.2, na.rm = TRUE),
    slope = coef(lm(yv ~ 0 + xv))[1],
    .groups = "drop"
  ) %>%
  mutate(
    y_pos = slope * x_pos,
    label = ifelse(info_treatment == "1", "Information", "Control")
  )

rob_est_update <- ggplot(
  rob_sub,
  aes(x = xv, y = yv, color = info_treatment)
) +
  geom_point(alpha = 0.4) +
  geom_smooth(method = "lm", formula = y ~ 0 + x, se = TRUE) +
  geom_text(
    data = info_label_df,
    aes(x = x_pos, y = y_pos, label = label, color = info_treatment),
    vjust = -1.2,
    fontface = "bold",
    show.legend = FALSE
  ) +
  labs(
    x = "Prior belief error (signed log): negative = overestimate, positive = underestimate",
    y = "Robbery estimate update (signed log)",
    caption = paste0("n = ", nrow(rob_sub), " (1 outlier removed)")
  ) +
  theme_bw() +
  theme(legend.position = "none")

print(rob_est_update)

ggsave(
  "latex/images/robbery_estimate_update.pdf",
  plot = rob_est_update,
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
    slope = coef(lm(rank_update ~ 0 + I(-RG)))[1],
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
  geom_smooth(method = "lm", formula = y ~ 0 + x, se = TRUE) +
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
