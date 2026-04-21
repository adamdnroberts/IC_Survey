library(stargazer)

test <- readRDS(
  "C:/Users/adamd/Documents/IC_Survey/data/wave2_responses.rds"
)

rank_cols <- c(
  "Crime_Rank_Comp_1",
  "Crime_Rank_Comp_2",
  "Crime_Rank_Comp_3",
  "Crime_Rank_Comp_4"
)

test$rank_prior <- 1 +
  rowSums(
    sapply(test[rank_cols], function(x) x %in% c("more", "more_than_double")),
    na.rm = TRUE
  )
test <- slice_sample(test, n = 2000, replace = TRUE)

test$RG <- test$rank_prior - sample(test$rank_prior)
test$CG <- sample(
  as.numeric(test$Robbery_Estimate_Post),
  nrow(test),
  replace = TRUE
) -
  as.numeric(test$Robbery_Estimate_Post)
test$CI <- 6 - sample(1:5, nrow(test), replace = TRUE)

test$Home_Crime_Handling_Change <- as.numeric(test$Home_Crime_Handling_Post) +
  sample(as.numeric(test$Home_Crime_Handling_Post))

m1 <- lm(
  Home_Crime_Handling_Change ~
    CG * as.factor(Treatment_Group) + RG * as.factor(Treatment_Group),
  data = test
)

m2 <- lm(
  Home_Crime_Handling_Change ~
    CG * CI * as.factor(Treatment_Group) + RG * CI * as.factor(Treatment_Group),
  data = test
)

stargazer(m1, m2, type = "text")
