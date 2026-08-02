library(dplyr)
library(tidyr)
library(ggplot2)
library(fixest)

responses_all <- readRDS("data/wave2_responses.rds")
load("data/magar2024_coalitions.Rdata")

# muni_changed is only defined for respondents matched across waves, so this
# drops the known movers and leaves wave-2-only respondents in the sample.
load("data/survey_panel_dataset.Rdata")
movers <- panel$Netquest_PID[panel$muni_changed %in% TRUE]

# Sample restriction: attention-check failures, control2, and movers
responses <- responses_all %>%
  filter(
    Attention_Check == "somewhat_agree",
    Treatment_Group != "control2",
    !Netquest_PID %in% movers
  )

cat(
  "Dropped",
  nrow(responses_all) - nrow(responses),
  "of",
  nrow(responses_all),
  "respondents (attention-check failures, control2, changed municipality)\n"
)

# Build municipality → actual coalition lookup
coalition_lookup <- magar2024 %>%
  mutate(
    muni_id = sprintf("%05d", inegi),
    actual_coalition = case_when(
      grepl("morena|pvem|pt", l01) ~ "MORENA/PVEM/PT",
      grepl("pan|pri|prd", l01) ~ "PAN/PRI/PRD",
      grepl("mc", l01) ~ "MC",
      TRUE ~ NA_character_
    )
  ) %>%
  dplyr::select(muni_id, actual_coalition)

# Map belief response values to coalition labels (anything else = wrong)
belief_to_coalition <- c(
  "morena_pt_pvem" = "MORENA/PVEM/PT",
  "pan_pri_prd" = "PAN/PRI/PRD",
  "mc" = "MC"
)

# ── Reshape to long format: one row per respondent × municipality ─────────────

home_long <- responses %>%
  dplyr::select(
    Respondent_ID,
    Treatment_Group,
    muni_id = Found_Municipality_ID,
    belief = Home_Governing_Party_Belief
  ) %>%
  mutate(muni_type = "home")

comp_long <- responses %>%
  dplyr::select(
    Respondent_ID,
    Treatment_Group,
    muni_id_1 = Comparison_Muni_1_ID,
    belief_1 = Comp_Governing_Party_Belief_1,
    muni_id_2 = Comparison_Muni_2_ID,
    belief_2 = Comp_Governing_Party_Belief_2,
    muni_id_3 = Comparison_Muni_3_ID,
    belief_3 = Comp_Governing_Party_Belief_3,
    muni_id_4 = Comparison_Muni_4_ID,
    belief_4 = Comp_Governing_Party_Belief_4
  ) %>%
  pivot_longer(
    cols = -c(Respondent_ID, Treatment_Group),
    names_to = c(".value", "slot"),
    names_pattern = "(muni_id|belief)_(\\d)"
  ) %>%
  filter(!is.na(muni_id)) %>%
  mutate(muni_type = "comparison")

all_long <- bind_rows(home_long, comp_long) %>%
  left_join(coalition_lookup, by = "muni_id") %>%
  mutate(
    guessed_coalition = belief_to_coalition[belief],
    correct = case_when(
      is.na(belief) | belief == "" ~ NA,
      belief == "dont_know" ~ FALSE,
      is.na(actual_coalition) ~ NA,
      guessed_coalition == actual_coalition ~ TRUE,
      TRUE ~ FALSE
    )
  )

# ── Accuracy by treatment arm ─────────────────────────────────────────────────

tg_order <- c("control", "T1", "T2", "T3", "T4")

all_long$arm <- all_long$Treatment_Group

acc_overall <- all_long %>%
  filter(!is.na(correct)) %>%
  group_by(arm) %>%
  summarise(
    n_obs = n(),
    n_correct = sum(correct),
    accuracy = mean(correct),
    se = sqrt(accuracy * (1 - accuracy) / n_obs),
    .groups = "drop"
  ) %>%
  mutate(
    Treatment_Group = factor(arm, levels = tg_order),
    muni_type = "Overall"
  )

acc_by_type <- all_long %>%
  filter(!is.na(correct)) %>%
  group_by(arm, muni_type) %>%
  summarise(
    n_obs = n(),
    n_correct = sum(correct),
    accuracy = mean(correct),
    se = sqrt(accuracy * (1 - accuracy) / n_obs),
    .groups = "drop"
  ) %>%
  mutate(arm = factor(arm, levels = tg_order))

cat("\n=== Overall accuracy by treatment arm ===\n")
print(
  acc_overall %>%
    dplyr::select(arm, n_obs, n_correct, accuracy, se) %>%
    mutate(across(c(accuracy, se), \(x) round(x, 3))),
  row.names = FALSE
)

cat("\n=== Accuracy by treatment arm and municipality type ===\n")
print(
  acc_by_type %>%
    dplyr::select(
      arm,
      muni_type,
      n_obs,
      n_correct,
      accuracy,
      se
    ) %>%
    mutate(across(c(accuracy, se), \(x) round(x, 3))) %>%
    arrange(muni_type, arm),
  row.names = FALSE
)

# ── Plot ──────────────────────────────────────────────────────────────────────

# Okabe-Ito colorblind-friendly palette (base R >= 4.0)
ok_palette <- palette.colors(palette = "Okabe-Ito")

plot_df <- acc_by_type %>%
  mutate(muni_type = factor(muni_type, levels = c("home", "comparison")))

# Respondents contributing at least one non-missing response, plus the
# municipality-level row counts the bars are actually estimated from.
n_respondents <- n_distinct(all_long$Respondent_ID[!is.na(all_long$correct)])
n_rows <- all_long %>%
  filter(!is.na(correct)) %>%
  count(muni_type)

plot_caption <- paste0(
  n_respondents,
  " respondents; ",
  n_rows$n[n_rows$muni_type == "home"],
  " home and ",
  n_rows$n[n_rows$muni_type == "comparison"],
  " comparison municipality responses"
)

p <- ggplot(
  plot_df,
  aes(x = arm, y = accuracy, fill = muni_type)
) +
  geom_col(position = position_dodge(width = 0.7), width = 0.6) +
  geom_hline(yintercept = 0.33, linetype = "dashed", color = "gray40") +
  # Baseline: control respondents are assigned comparison municipalities and
  # asked the coalition question, but are never shown the comparison chart, so
  # their accuracy is knowledge held prior to any treatment information.
  geom_hline(
    yintercept = plot_df$accuracy[
      plot_df$arm == "control" & plot_df$muni_type == "comparison"
    ],
    linetype = "dashed",
    color = "black"
  ) +
  geom_errorbar(
    aes(ymin = accuracy - 1.96 * se, ymax = accuracy + 1.96 * se),
    position = position_dodge(width = 0.7),
    width = 0.2
  ) +
  scale_y_continuous(labels = scales::percent_format(), limits = c(0, 1)) +
  scale_fill_manual(
    values = c(
      home = ok_palette[6], # blue   #0072B2
      comparison = ok_palette[2], # orange #E69F00
      Overall = ok_palette[9] # gray   #999999
    ),
    labels = c(
      home = "Home municipality",
      comparison = "Comparison municipalities",
      Overall = "Overall"
    ),
    name = NULL
  ) +
  labs(
    #title = "Accuracy of governing coalition responses",
    x = NULL,
    y = "Proportion correct",
    caption = plot_caption
  ) +
  theme_minimal(base_size = 13) +
  theme(legend.position = "bottom")

print(p)

ggsave("latex/images/party_knowledge_accuracy.pdf", p, width = 8, height = 5)

# ── Pairwise differences between comparison-municipality arms ─────────────────
# All arms are assigned comparison municipalities and asked the coalition
# question; control and T1 are simply never shown the comparison chart, so they
# serve as the unexposed baseline. control is the reference category.
# SEs clustered by respondent because each respondent contributes up to 4 rows.

comp_arms <- c("control", "T1", "T2", "T3", "T4")

# Flag straightliners: respondents who gave the same coalition response for all
# municipalities (excluding dont_know and missing)
# Also compute proportion of "dont_know" responses per respondent
resp_patterns <- all_long %>%
  group_by(Respondent_ID) %>%
  summarise(
    n_total = n(),
    n_dont_know = sum(belief == "dont_know", na.rm = TRUE),
    prop_dont_know = n_dont_know / n_total,
    .groups = "drop"
  )

straightliners <- all_long %>%
  filter(!is.na(belief), belief != "", belief != "dont_know") %>%
  group_by(Respondent_ID) %>%
  summarise(
    n_responses = n(),
    n_unique = n_distinct(guessed_coalition),
    straightliner = n_responses > 1 & n_unique == 1,
    .groups = "drop"
  ) %>%
  dplyr::select(Respondent_ID, straightliner) %>%
  left_join(resp_patterns, by = "Respondent_ID")

cat(
  "\nStraightliners:",
  sum(straightliners$straightliner),
  "of",
  nrow(straightliners),
  "respondents\n"
)

comp_long_fit <- all_long %>%
  filter(
    muni_type == "comparison",
    !is.na(correct),
    Treatment_Group %in% comp_arms
  ) %>%
  left_join(straightliners, by = "Respondent_ID") %>%
  mutate(Treatment_Group = factor(Treatment_Group, levels = comp_arms))

m_comp <- estimatr::lm_robust(
  as.numeric(correct) ~ Treatment_Group,
  data = comp_long_fit,
  clusters = Respondent_ID
)

cat("\n=== Comparison-muni accuracy by arm (LPM, control baseline) ===\n")
print(summary(m_comp))

m_comp_sl <- estimatr::lm_robust(
  as.numeric(correct) ~ Treatment_Group + straightliner + prop_dont_know,
  data = comp_long_fit,
  clusters = Respondent_ID
)

cat("\n=== With straightliner and dont_know controls ===\n")
print(summary(m_comp_sl))

# All pairwise contrasts, Holm-adjusted
arm_pairs <- t(combn(comp_arms, 2))

pairwise_diffs <- lapply(seq_len(nrow(arm_pairs)), function(i) {
  a <- arm_pairs[i, 1]
  b <- arm_pairs[i, 2]
  # Contrast vector: b - a in the coefficient space of m_comp
  cvec <- rep(0, length(coef(m_comp)))
  names(cvec) <- names(coef(m_comp))
  if (a != comp_arms[1]) {
    cvec[paste0("Treatment_Group", a)] <- -1
  }
  if (b != comp_arms[1]) {
    cvec[paste0("Treatment_Group", b)] <- 1
  }
  est <- sum(cvec * coef(m_comp))
  se <- sqrt(drop(t(cvec) %*% vcov(m_comp) %*% cvec))
  data.frame(
    arm_1 = a,
    arm_2 = b,
    diff = est,
    se = se,
    t = est / se,
    p = 2 * pt(abs(est / se), df = m_comp$df.residual, lower.tail = FALSE)
  )
}) %>%
  bind_rows() %>%
  mutate(
    p_holm = p.adjust(p, method = "holm"),
    across(c(diff, se, t), \(x) round(x, 3)),
    across(c(p, p_holm), \(x) round(x, 4))
  )

cat(
  "\n=== Pairwise differences in comparison-muni accuracy (Holm-adjusted) ===\n"
)
print(arrange(pairwise_diffs, p), row.names = FALSE)

# ── Same-coalition vs opposite-coalition accuracy ────────────────────────────
# For each respondent, get home municipality coalition, then check if comparison
# municipalities are same or opposite coalition.

home_coalition <- responses %>%
  dplyr::select(Respondent_ID, home_muni_id = Found_Municipality_ID) %>%
  left_join(coalition_lookup, by = c("home_muni_id" = "muni_id")) %>%
  rename(home_coalition = actual_coalition)

coalition_match <- all_long %>%
  filter(muni_type == "comparison", !is.na(correct)) %>%

  left_join(home_coalition, by = "Respondent_ID") %>%
  mutate(
    same_coalition = case_when(
      is.na(home_coalition) | is.na(actual_coalition) ~ NA,
      home_coalition == actual_coalition ~ TRUE,
      TRUE ~ FALSE
    )
  ) %>%
  filter(!is.na(same_coalition))

acc_by_coalition <- coalition_match %>%
  group_by(same_coalition) %>%
  summarise(
    n_obs = n(),
    accuracy = mean(correct),
    se = sqrt(accuracy * (1 - accuracy) / n_obs),
    .groups = "drop"
  )

cat(
  "\n=== Accuracy by same vs opposite coalition (comparison munis only) ===\n"
)
print(
  acc_by_coalition %>%
    mutate(
      coalition_match = ifelse(same_coalition, "Same", "Opposite"),
      across(c(accuracy, se), \(x) round(x, 3))
    ) %>%
    dplyr::select(coalition_match, n_obs, accuracy, se),
  row.names = FALSE
)

m_coalition <- feols(
  correct ~ same_coalition | Respondent_ID,
  data = coalition_match
)

cat("\n=== Same-coalition effect on accuracy (LPM) ===\n")
print(summary(m_coalition))

# ── Accuracy for benchmark-selected municipalities ───────────────────────────
# Check if respondents who received one of their W1 benchmark selections as a
# W2 comparison municipality correctly identified the governing coalition.

load("data/survey_panel_dataset.Rdata") # loads 'panel'

# Identify which comparison munis were W1 benchmark selections
benchmark_matches <- panel %>%
  filter(!is.na(Benchmark_Selected_Municipalities)) %>%
  dplyr::select(
    Netquest_PID,
    Benchmark_Selected_Municipalities,
    Comparison_Muni_1_ID,
    Comparison_Muni_2_ID,
    Comparison_Muni_3_ID,
    Comparison_Muni_4_ID
  ) %>%
  tidyr::pivot_longer(
    cols = starts_with("Comparison_Muni_"),
    names_to = "slot",
    values_to = "muni_id"
  ) %>%
  filter(!is.na(muni_id)) %>%
  rowwise() %>%
  mutate(
    w1_selected = list(trimws(strsplit(Benchmark_Selected_Municipalities, ";")[[
      1
    ]])),
    was_benchmark = muni_id %in% w1_selected
  ) %>%
  ungroup() %>%
  dplyr::select(Netquest_PID, muni_id, was_benchmark)

# Add Netquest_PID to all_long for joining
all_long_pid <- all_long %>%
  left_join(
    responses %>% dplyr::select(Respondent_ID, Netquest_PID),
    by = "Respondent_ID"
  )

# Join with party knowledge data (exclude dont_know responses)
benchmark_accuracy <- all_long_pid %>%
  filter(muni_type == "comparison", !is.na(correct)) %>%
  left_join(benchmark_matches, by = c("Netquest_PID", "muni_id")) %>%
  mutate(was_benchmark = coalesce(was_benchmark, FALSE))

acc_by_benchmark <- benchmark_accuracy %>%
  group_by(was_benchmark) %>%
  summarise(
    n_obs = n(),
    accuracy = mean(correct),
    se = sqrt(accuracy * (1 - accuracy) / n_obs),
    .groups = "drop"
  )

cat("\n=== Accuracy by W1 benchmark selection status ===\n")
print(
  acc_by_benchmark %>%
    mutate(
      muni_type = ifelse(was_benchmark, "W1 benchmark", "Not benchmark"),
      across(c(accuracy, se), \(x) round(x, 3))
    ) %>%
    dplyr::select(muni_type, n_obs, accuracy, se),
  row.names = FALSE
)

m_benchmark <- feols(
  correct ~ was_benchmark | Respondent_ID,
  data = benchmark_accuracy
)

cat("\n=== Benchmark selection effect on accuracy (LPM) ===\n")
print(summary(m_benchmark))

# ── Accuracy by vote-coalition match ─────────────────────────────────────────
# Check if respondents are more accurate at identifying governing parties for
# municipalities governed by the coalition they voted for (pre-treatment).

vote_coalition <- panel %>%
  dplyr::select(Netquest_PID, respondent_coalition = coalition_pre)

vote_match_accuracy <- all_long_pid %>%
  filter(muni_type == "comparison", !is.na(correct)) %>%
  left_join(vote_coalition, by = "Netquest_PID") %>%
  mutate(
    vote_coalition_match = case_when(
      is.na(respondent_coalition) | is.na(actual_coalition) ~ NA,
      respondent_coalition == actual_coalition ~ TRUE,
      TRUE ~ FALSE
    )
  ) %>%
  filter(!is.na(vote_coalition_match))

acc_by_vote_match <- vote_match_accuracy %>%
  group_by(vote_coalition_match) %>%
  summarise(
    n_obs = n(),
    accuracy = mean(correct),
    se = sqrt(accuracy * (1 - accuracy) / n_obs),
    .groups = "drop"
  )

cat("\n=== Accuracy by vote-coalition match (excl. dont_know) ===\n")
print(
  acc_by_vote_match %>%
    mutate(
      match_type = ifelse(
        vote_coalition_match,
        "Voted for coalition",
        "Did not vote for"
      ),
      across(c(accuracy, se), \(x) round(x, 3))
    ) %>%
    dplyr::select(match_type, n_obs, accuracy, se),
  row.names = FALSE
)

m_vote_match <- feols(
  correct ~ vote_coalition_match | Netquest_PID,
  data = vote_match_accuracy
)

cat("\n=== Vote-coalition match effect on accuracy (LPM) ===\n")
print(summary(m_vote_match))

# ── LaTeX table ──────────────────────────────────────────────────────────────

fmt <- function(x) sprintf("%.3f", x)
fmt_se <- function(x) sprintf("(%.3f)", x)
stars <- function(p) {
  if (p < 0.001) {
    return("$^{***}$")
  }
  if (p < 0.01) {
    return("$^{**}$")
  }
  if (p < 0.05) {
    return("$^{*}$")
  }
  return("")
}
fmt_coef <- function(est, p) paste0(fmt(est), stars(p))

coefs_bench <- coeftable(m_benchmark)
coefs_vote <- coeftable(m_vote_match)
coefs_coal <- coeftable(m_coalition)

# Baseline accuracy (when variable = FALSE)
baseline_bench <- mean(benchmark_accuracy$correct[!benchmark_accuracy$was_benchmark])
baseline_vote <- mean(vote_match_accuracy$correct[!vote_match_accuracy$vote_coalition_match])
baseline_coal <- mean(coalition_match$correct[!coalition_match$same_coalition])

n_resp_benchmark <- n_distinct(benchmark_accuracy$Respondent_ID)
n_resp_vote <- n_distinct(vote_match_accuracy$Netquest_PID)
n_resp_coalition <- n_distinct(coalition_match$Respondent_ID)

tex_combined <- paste0(
  "\\begin{table}[htbp]\n",
  "\\centering\n",
  "\\caption{Governing coalition knowledge: predictors of accuracy}\n",
  "\\label{tab:party_knowledge}\n",
  "\\begin{tabular}{lccc}\n",
  "\\toprule\n",
  " & (1) & (2) & (3) \\\\\n",
  "\\midrule\n",
  "W1 benchmark & ",
  fmt_coef(
    coefs_bench["was_benchmarkTRUE", "Estimate"],
    coefs_bench["was_benchmarkTRUE", "Pr(>|t|)"]
  ),
  " & & \\\\\n",
  " & ",
  fmt_se(coefs_bench["was_benchmarkTRUE", "Std. Error"]),
  " & & \\\\\n",
  "Vote-coalition match & & ",
  fmt_coef(
    coefs_vote["vote_coalition_matchTRUE", "Estimate"],
    coefs_vote["vote_coalition_matchTRUE", "Pr(>|t|)"]
  ),
  " & \\\\\n",
  " & & ",
  fmt_se(coefs_vote["vote_coalition_matchTRUE", "Std. Error"]),
  " & \\\\\n",
  "Home-coalition match & & & ",
  fmt_coef(
    coefs_coal["same_coalitionTRUE", "Estimate"],
    coefs_coal["same_coalitionTRUE", "Pr(>|t|)"]
  ),
  " \\\\\n",
  " & & & ",
  fmt_se(coefs_coal["same_coalitionTRUE", "Std. Error"]),
  " \\\\\n",
  "\\midrule\n",
  sprintf("Baseline accuracy & %.3f & %.3f & %.3f \\\\\n",
          baseline_bench, baseline_vote, baseline_coal),
  "Respondent FE & Yes & Yes & Yes \\\\\n",
  sprintf(
    "Observations & %s & %s & %s \\\\\n",
    format(nobs(m_benchmark), big.mark = ","),
    format(nobs(m_vote_match), big.mark = ","),
    format(nobs(m_coalition), big.mark = ",")
  ),
  sprintf(
    "Respondents & %s & %s & %s \\\\\n",
    format(n_resp_benchmark, big.mark = ","),
    format(n_resp_vote, big.mark = ","),
    format(n_resp_coalition, big.mark = ",")
  ),
  "\\bottomrule\n",
  "\\multicolumn{4}{l}{\\footnotesize $^{*}p<0.05$; $^{**}p<0.01$; $^{***}p<0.001$} \\\\\n",
  "\\end{tabular}\n",
  "\\end{table}\n"
)

writeLines(tex_combined, "latex/tables/party_knowledge.tex")
cat("\nWrote latex/tables/party_knowledge.tex\n")
