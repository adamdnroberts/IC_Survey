library(ggplot2)

survey_responses_wave1 <- readRDS("data/wave1_responses.rds")

survey_responses_wave1$Home_Crime_Handling_Pre <- as.numeric(
  survey_responses_wave1$Home_Crime_Handling_Pre
)
survey_responses_wave1$MORENA_Crime_Rating_Pre <- as.numeric(
  survey_responses_wave1$MORENA_Crime_Rating_Pre
)
survey_responses_wave1$Coalition_PAN_PRI_PRD_Crime_Rating_Pre <- as.numeric(
  survey_responses_wave1$Coalition_PAN_PRI_PRD_Crime_Rating_Pre
)
survey_responses_wave1$MC_Crime_Rating_Pre <- as.numeric(
  survey_responses_wave1$MC_Crime_Rating_Pre
)

ggplot(survey_responses_wave1) +
  geom_density(aes(x = Home_Crime_Handling_Pre)) +
  geom_density(
    (aes(x = MORENA_Crime_Rating_Pre)),
    color = "maroon",
    alpha = 0.5
  ) +
  geom_density(
    (aes(x = MC_Crime_Rating_Pre)),
    color = "orange",
    alpha = 0.5
  ) +
  geom_density(
    (aes(x = Coalition_PAN_PRI_PRD_Crime_Rating_Pre)),
    color = "blue",
    alpha = 0.5
  ) +
  theme_bw()
