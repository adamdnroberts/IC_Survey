library(estimatr)
library(modelsummary)
library(dplyr)
library(ggplot2)
library(broom)
library(mediation)

load("~/IC_Survey/data/survey_panel_dataset.Rdata")

panel_full <- panel

if (!exists("ci_alpha")) {
  ci_alpha <- 0.01
}

panel_with_failures <- filter(panel_full, muni_changed == 0)
panel <- filter(panel_with_failures, Attention_Check == "somewhat_agree")

panel$Vote_home_post <- as.integer(
  !is.na(panel$coalition_post) &
    !is.na(panel$home_coalition) &
    panel$home_coalition == panel$coalition_post
)

panel$Treatment_Group <- factor(panel$Treatment_Group)

m_mediatorT1 <- lm(
  Home_Crime_Handling_Change ~ crime_gap_wins *
    Treatment_Group +
    rank_gap * Treatment_Group +
    comp_party_known,
  data = filter(panel, Treatment_Group == "T1" | Treatment_Group == "control")
)
m_outcomeT1 <- lm(
  Vote_home_post ~ Home_Crime_Handling_Change +
    crime_gap_wins * Treatment_Group +
    rank_gap * Treatment_Group +
    comp_party_known,
  data = filter(panel, Treatment_Group == "T1" | Treatment_Group == "control")
)

M_control <- m_mediatorT1$coefficients["(Intercept)"] +
  m_mediatorT1$coefficients["crime_gap_wins"] *
    panel$crime_gap_wins +
  m_mediatorT1$coefficients["rank_gap"] *
    panel$rank_gap +
  m_mediatorT1$coefficients["comp_party_known"] *
    panel$comp_party_known
M_T1 <- m_mediatorT1$coefficients["(Intercept)"] +
  m_mediatorT1$coefficients["Treatment_GroupT1"] +
  m_mediatorT1$coefficients["crime_gap_wins"] *
    panel$crime_gap_wins +
  m_mediatorT1$coefficients["rank_gap"] *
    panel$rank_gap +
  m_mediatorT1$coefficients["crime_gap_wins:Treatment_GroupT1"] *
    panel$crime_gap_wins +
  m_mediatorT1$coefficients["Treatment_GroupT1:rank_gap"] *
    panel$rank_gap +
  m_mediatorT1$coefficients["comp_party_known"] *
    panel$comp_party_known
Y_control <- m_outcomeT1$coefficients["(Intercept)"] +
  m_outcomeT1$coefficients["Home_Crime_Handling_Change"] * M_control +
  m_outcomeT1$coefficients["crime_gap_wins"] *
    panel$crime_gap_wins +
  m_outcomeT1$coefficients["rank_gap"] *
    panel$rank_gap +
  m_outcomeT1$coefficients["comp_party_known"] *
    panel$comp_party_known
Y_T1 <- m_outcomeT1$coefficients["(Intercept)"] +
  m_outcomeT1$coefficients["Home_Crime_Handling_Change"] * M_T1 +
  m_outcomeT1$coefficients["crime_gap_wins"] *
    panel$crime_gap_wins +
  m_outcomeT1$coefficients["rank_gap"] *
    panel$rank_gap +
  m_outcomeT1$coefficients["comp_party_known"] *
    panel$comp_party_known

mean(Y_T1 - Y_control)

med.outT1 <- mediate(
  m_mediatorT1,
  m_outcomeT1,
  treat = "Treatment_Group",
  mediator = "Home_Crime_Handling_Change",
  control.value = "control",
  treat.value = "T1",
  robustSE = TRUE,
  sims = 1000
)

print(paste0(med.outT1$d0, ", p-value= ", med.outT1$d0.p))

m_mediatorT2 <- lm(
  Home_Crime_Handling_Change ~ crime_gap_wins *
    Treatment_Group +
    rank_gap * Treatment_Group +
    comp_party_known,
  data = filter(panel, Treatment_Group == "T2" | Treatment_Group == "control")
)
m_outcomeT2 <- lm(
  Vote_home_post ~ Home_Crime_Handling_Change +
    crime_gap_wins * Treatment_Group +
    rank_gap * Treatment_Group +
    comp_party_known,
  data = filter(panel, Treatment_Group == "T2" | Treatment_Group == "control")
)

M_control <- m_mediatorT2$coefficients["(Intercept)"] +
  m_mediatorT2$coefficients["crime_gap_wins"] *
    panel$crime_gap_wins +
  m_mediatorT2$coefficients["rank_gap"] *
    panel$rank_gap +
  m_mediatorT2$coefficients["comp_party_known"] *
    panel$comp_party_known
M_T2 <- m_mediatorT2$coefficients["(Intercept)"] +
  m_mediatorT2$coefficients["Treatment_GroupT2"] +
  m_mediatorT2$coefficients["crime_gap_wins"] *
    panel$crime_gap_wins +
  m_mediatorT2$coefficients["rank_gap"] *
    panel$rank_gap +
  m_mediatorT2$coefficients["crime_gap_wins:Treatment_GroupT2"] *
    panel$crime_gap_wins +
  m_mediatorT2$coefficients["Treatment_GroupT2:rank_gap"] *
    panel$rank_gap +
  m_mediatorT2$coefficients["comp_party_known"] *
    panel$comp_party_known
Y_control <- m_outcomeT2$coefficients["(Intercept)"] +
  m_outcomeT2$coefficients["Home_Crime_Handling_Change"] * M_control +
  m_outcomeT2$coefficients["crime_gap_wins"] *
    panel$crime_gap_wins +
  m_outcomeT2$coefficients["rank_gap"] *
    panel$rank_gap +
  m_outcomeT2$coefficients["comp_party_known"] *
    panel$comp_party_known
Y_T2 <- m_outcomeT2$coefficients["(Intercept)"] +
  m_outcomeT2$coefficients["Home_Crime_Handling_Change"] * M_T2 +
  m_outcomeT2$coefficients["crime_gap_wins"] *
    panel$crime_gap_wins +
  m_outcomeT2$coefficients["rank_gap"] *
    panel$rank_gap +
  m_outcomeT2$coefficients["comp_party_known"] *
    panel$comp_party_known

mean(Y_T2 - Y_control)

med.outT2 <- mediate(
  m_mediatorT2,
  m_outcomeT2,
  treat = "Treatment_Group",
  mediator = "Home_Crime_Handling_Change",
  control.value = "control",
  treat.value = "T2",
  robustSE = TRUE,
  sims = 1000
)

print(paste0(med.outT2$d0, ", p-value= ", med.outT2$d0.p))

m_mediatorT3 <- lm(
  Home_Crime_Handling_Change ~ crime_gap_wins *
    Treatment_Group +
    rank_gap * Treatment_Group +
    comp_party_known,
  data = filter(panel, Treatment_Group == "T3" | Treatment_Group == "control")
)
m_outcomeT3 <- lm(
  Vote_home_post ~ Home_Crime_Handling_Change +
    crime_gap_wins * Treatment_Group +
    rank_gap * Treatment_Group +
    comp_party_known,
  data = filter(panel, Treatment_Group == "T3" | Treatment_Group == "control")
)

M_control <- m_mediatorT3$coefficients["(Intercept)"] +
  m_mediatorT3$coefficients["crime_gap_wins"] *
    panel$crime_gap_wins +
  m_mediatorT3$coefficients["rank_gap"] *
    panel$rank_gap +
  m_mediatorT3$coefficients["comp_party_known"] *
    panel$comp_party_known
M_T3 <- m_mediatorT3$coefficients["(Intercept)"] +
  m_mediatorT3$coefficients["Treatment_GroupT3"] +
  m_mediatorT3$coefficients["crime_gap_wins"] *
    panel$crime_gap_wins +
  m_mediatorT3$coefficients["rank_gap"] *
    panel$rank_gap +
  m_mediatorT3$coefficients["crime_gap_wins:Treatment_GroupT3"] *
    panel$crime_gap_wins +
  m_mediatorT3$coefficients["Treatment_GroupT3:rank_gap"] *
    panel$rank_gap +
  m_mediatorT3$coefficients["comp_party_known"] *
    panel$comp_party_known
Y_control <- m_outcomeT3$coefficients["(Intercept)"] +
  m_outcomeT3$coefficients["Home_Crime_Handling_Change"] * M_control +
  m_outcomeT3$coefficients["crime_gap_wins"] *
    panel$crime_gap_wins +
  m_outcomeT3$coefficients["rank_gap"] *
    panel$rank_gap +
  m_outcomeT3$coefficients["comp_party_known"] *
    panel$comp_party_known
Y_T3 <- m_outcomeT3$coefficients["(Intercept)"] +
  m_outcomeT3$coefficients["Home_Crime_Handling_Change"] * M_T3 +
  m_outcomeT3$coefficients["crime_gap_wins"] *
    panel$crime_gap_wins +
  m_outcomeT3$coefficients["rank_gap"] *
    panel$rank_gap +
  m_outcomeT3$coefficients["comp_party_known"] *
    panel$comp_party_known

mean(Y_T3 - Y_control)
med.outT3 <- mediate(
  m_mediatorT3,
  m_outcomeT3,
  treat = "Treatment_Group",
  mediator = "Home_Crime_Handling_Change",
  control.value = "control",
  treat.value = "T3",
  robustSE = TRUE,
  sims = 1000
)
print(paste0(med.outT3$d0, ", p-value= ", med.outT3$d0.p))

m_mediatorT4 <- lm(
  Home_Crime_Handling_Change ~ crime_gap_wins *
    Treatment_Group +
    rank_gap * Treatment_Group +
    comp_party_known,
  data = filter(panel, Treatment_Group == "T4" | Treatment_Group == "control")
)
m_outcomeT4 <- lm(
  Vote_home_post ~ Home_Crime_Handling_Change +
    crime_gap_wins * Treatment_Group +
    rank_gap * Treatment_Group +
    comp_party_known,
  data = filter(panel, Treatment_Group == "T4" | Treatment_Group == "control")
)

M_control <- m_mediatorT4$coefficients["(Intercept)"] +
  m_mediatorT4$coefficients["crime_gap_wins"] *
    panel$crime_gap_wins +
  m_mediatorT4$coefficients["rank_gap"] *
    panel$rank_gap +
  m_mediatorT4$coefficients["comp_party_known"] *
    panel$comp_party_known
M_T4 <- m_mediatorT4$coefficients["(Intercept)"] +
  m_mediatorT4$coefficients["Treatment_GroupT4"] +
  m_mediatorT4$coefficients["crime_gap_wins"] *
    panel$crime_gap_wins +
  m_mediatorT4$coefficients["rank_gap"] *
    panel$rank_gap +
  m_mediatorT4$coefficients["crime_gap_wins:Treatment_GroupT4"] *
    panel$crime_gap_wins +
  m_mediatorT4$coefficients["Treatment_GroupT4:rank_gap"] *
    panel$rank_gap +
  m_mediatorT4$coefficients["comp_party_known"] *
    panel$comp_party_known
Y_control <- m_outcomeT4$coefficients["(Intercept)"] +
  m_outcomeT4$coefficients["Home_Crime_Handling_Change"] * M_control +
  m_outcomeT4$coefficients["crime_gap_wins"] *
    panel$crime_gap_wins +
  m_outcomeT4$coefficients["rank_gap"] *
    panel$rank_gap +
  m_outcomeT4$coefficients["comp_party_known"] *
    panel$comp_party_known
Y_T4 <- m_outcomeT4$coefficients["(Intercept)"] +
  m_outcomeT4$coefficients["Home_Crime_Handling_Change"] * M_T4 +
  m_outcomeT4$coefficients["crime_gap_wins"] *
    panel$crime_gap_wins +
  m_outcomeT4$coefficients["rank_gap"] *
    panel$rank_gap +
  m_outcomeT4$coefficients["comp_party_known"] *
    panel$comp_party_known

mean(Y_T4 - Y_control)
med.outT4 <- mediate(
  m_mediatorT4,
  m_outcomeT4,
  treat = "Treatment_Group",
  mediator = "Home_Crime_Handling_Change",
  control.value = "control",
  treat.value = "T4",
  robustSE = TRUE,
  sims = 1000
)
print(paste0(med.outT4$d0, ", p-value= ", med.outT4$d0.p))
