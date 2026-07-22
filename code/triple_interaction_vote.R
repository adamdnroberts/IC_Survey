library(estimatr)
library(dplyr)
library(ggplot2)

load("~/IC_Survey/data/survey_panel_dataset.Rdata")

if (!exists("ci_alpha")) {
  ci_alpha <- 0.01
}

# Colorblind-friendly (Okabe-Ito) palette, matching crime_rate_accuracy_update.R
arm_colors <- c(
  control2 = "#999999",
  T1 = "#56B4E9",
  T2 = "#009E73",
  T3 = "#E69F00",
  T4 = "#0072B2"
)

panel$Vote_home_post <- as.integer(
  !is.na(panel$coalition_post) &
    !is.na(panel$home_coalition) &
    panel$home_coalition == panel$coalition_post
)

panel_full <- panel

panel_full$coalition_pre[is.na(panel_full$coalition_pre)] <- "Other"

panel_full$inc_vote <- as.numeric(
  panel_full$coalition_pre == panel_full$home_coalition
)

panel_with_failures <- filter(panel_full, muni_changed == 0)
panel <- filter(panel_with_failures, Attention_Check == "somewhat_agree" & Treatment_Group != "control2")

crime_gap_wins_sd <- sd(panel$crime_gap_wins, na.rm = TRUE)
log_crime_gap_sd <- sd(panel$log_crime_gap, na.rm = TRUE)
rank_gap_sd <- sd(panel$rank_gap, na.rm = TRUE)

m_log_vote <- lm_robust(
  Vote_home_post ~
    log_crime_gap *
    as.factor(Treatment_Group) * as.numeric(Importance_Crime) +
    rank_gap * as.factor(Treatment_Group) * as.numeric(Importance_Crime) +
    #as.factor(coalition_pre) +
    inc_vote,
  alpha = ci_alpha,
  data = panel,
  se_type = "HC2"
)

# Treatment effect (each arm vs control) on P(vote incumbent) at EACH value of
# Importance_Crime, with log_crime_gap and rank_gap held at their medians. For
# arm k the contrast is the sum of every coefficient activated by switching from
# control to arm k, evaluated at (cg_at, rg_at, Importance = v):
#   ME_k(v) = b[Tk] + b[cg:Tk]*cg_at + b[rg:Tk]*rg_at
#           + (b[Tk:Imp] + b[cg:Tk:Imp]*cg_at + b[rg:Tk:Imp]*rg_at) * v
# SE from the full HC2 vcov. NOTE: Importance_Crime enters the model linearly
# (as.numeric), so each arm's curve is a straight line across importance.
b <- coef(m_log_vote)
V <- vcov(m_log_vote)
df_resid <- m_log_vote$df.residual
cg_at <- median(panel$log_crime_gap, na.rm = TRUE) # gaps held at their medians
rg_at <- median(panel$rank_gap, na.rm = TRUE)
imp_vals <- sort(unique(as.numeric(panel$Importance_Crime)))
imp_vals <- imp_vals[!is.na(imp_vals)]

# Contrast vector for the arm-vs-control effect at Importance_Crime = v.
arm_contrast_L <- function(arm, v) {
  vapply(names(b), function(nm) {
    if (!grepl(paste0("Treatment_Group)", arm), nm, fixed = TRUE)) return(0)
    val <- 1
    if (grepl("log_crime_gap", nm, fixed = TRUE)) val <- val * cg_at
    if (grepl("rank_gap", nm, fixed = TRUE)) val <- val * rg_at
    if (grepl("Importance_Crime", nm, fixed = TRUE)) val <- val * v
    val
  }, numeric(1))
}

arms <- c("T1", "T2", "T3", "T4")
coef_plot_data_log <- do.call(rbind, lapply(arms, function(a) {
  do.call(rbind, lapply(imp_vals, function(v) {
    L <- arm_contrast_L(a, v)
    est <- sum(L * b)
    se <- sqrt(as.numeric(t(L) %*% V %*% L))
    data.frame(
      treatment = a,
      importance = v,
      estimate = est,
      std.error = se,
      conf.low = est - qt(0.975, df_resid) * se, # 95%
      conf.high = est + qt(0.975, df_resid) * se,
      row.names = NULL
    )
  }))
}))
coef_plot_data_log$treatment <- factor(coef_plot_data_log$treatment, levels = arms)

vote_coef_update_log <- ggplot(
  coef_plot_data_log,
  aes(importance, estimate, color = treatment, fill = treatment)
) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.12, color = NA) +
  geom_line(linewidth = 0.8) +
  geom_point() +
  scale_color_manual(values = arm_colors, name = NULL) +
  scale_fill_manual(values = arm_colors, name = NULL) +
  facet_wrap(~treatment) +
  labs(
    x = "Crime importance (Importance_Crime)",
    y = "Treatment effect vs. control on P(vote incumbent)",
    title = "Treatment effect on incumbent vote by crime importance (gaps at median)",
    caption = paste0(
      "N = ", m_log_vote$nobs,
      "; each arm vs control, gaps at median; 95% CI ribbons"
    )
  ) +
  theme_minimal() +
  theme(legend.position = "none")

print(vote_coef_update_log)

ggsave(
  "latex/images/vote_treatment_by_importance.pdf",
  plot = vote_coef_update_log,
  width = 7,
  height = 4.5
)

# ── Non-linear version: Importance_Crime as a factor ─────────────────────────
# Same estimand (arm-vs-control effect on P(vote incumbent) at each importance
# level, gaps at median), but Importance_Crime enters as a FACTOR, so each level
# is free to differ rather than lying on a straight line. Effects are plotted as
# points + 95% CIs per level.
m_fac_vote <- lm_robust(
  Vote_home_post ~
    log_crime_gap *
    as.factor(Treatment_Group) * factor(Importance_Crime) +
    rank_gap * as.factor(Treatment_Group) * factor(Importance_Crime) +
    inc_vote,
  alpha = ci_alpha,
  data = panel,
  se_type = "HC2"
)

b_f <- coef(m_fac_vote)
V_f <- vcov(m_fac_vote)
df_f <- m_fac_vote$df.residual
imp_ref <- min(imp_vals) # factor reference level (no dummy)

# Level referenced by a coefficient name, or NA if it carries no importance dummy.
imp_level_of <- function(nm) {
  if (!grepl("factor(Importance_Crime)", nm, fixed = TRUE)) return(NA_real_)
  as.numeric(sub("[^0-9].*$", "", sub(".*factor\\(Importance_Crime\\)", "", nm)))
}

# Contrast vector: arm-vs-control effect at Importance_Crime level v (gaps at
# median). Base (reference-level) arm terms enter at every level; level-v dummy
# arm terms enter only when v matches.
arm_contrast_fac_L <- function(arm, v) {
  vapply(names(b_f), function(nm) {
    if (!grepl(paste0("Treatment_Group)", arm), nm, fixed = TRUE)) return(0)
    lv <- imp_level_of(nm)
    if (!is.na(lv) && lv != v) return(0) # drop other-level dummy terms
    # base arm terms (lv = NA) enter at every level; level-v dummies enter here.
    val <- 1
    if (grepl("log_crime_gap", nm, fixed = TRUE)) val <- val * cg_at
    if (grepl("rank_gap", nm, fixed = TRUE)) val <- val * rg_at
    val
  }, numeric(1))
}

coef_plot_data_fac <- do.call(rbind, lapply(arms, function(a) {
  do.call(rbind, lapply(imp_vals, function(v) {
    L <- arm_contrast_fac_L(a, v)
    est <- sum(L * b_f)
    se <- sqrt(as.numeric(t(L) %*% V_f %*% L))
    data.frame(
      treatment = a,
      importance = v,
      estimate = est,
      std.error = se,
      conf.low = est - qt(0.975, df_f) * se, # 95%
      conf.high = est + qt(0.975, df_f) * se,
      row.names = NULL
    )
  }))
}))
coef_plot_data_fac$treatment <- factor(coef_plot_data_fac$treatment, levels = arms)

vote_fac_plot <- ggplot(
  coef_plot_data_fac,
  aes(importance, estimate, color = treatment)
) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0, linewidth = 0.5) +
  geom_point() +
  scale_color_manual(values = arm_colors, name = NULL) +
  facet_wrap(~treatment) +
  labs(
    x = "Crime importance (Importance_Crime, factor)",
    y = "Treatment effect vs. control on P(vote incumbent)",
    title = "Treatment effect on incumbent vote by crime importance (factor; gaps at median)",
    caption = paste0(
      "N = ", m_fac_vote$nobs,
      "; each arm vs control, gaps at median; 95% CIs"
    )
  ) +
  theme_minimal() +
  theme(legend.position = "none")

print(vote_fac_plot)

ggsave(
  "latex/images/vote_treatment_by_importance_factor.pdf",
  plot = vote_fac_plot,
  width = 7,
  height = 4.5
)

# ── Does crime importance moderate the treatment effects? ─────────────────────
# The plots show treatment-effect LEVELS; whether the effect DIFFERS by crime
# importance is a test of the Treatment x Importance terms (the slopes). We
# report (1) each arm's importance-slope of the treatment effect (gaps at
# median), (2) a joint Wald test of all Treatment x Importance interactions, and
# (3) a joint test of only the gap x Treatment x Importance (three-way) terms --
# i.e. whether importance changes how voters respond to the crime information.
b <- coef(m_log_vote)
V <- vcov(m_log_vote)
df_resid <- m_log_vote$df.residual
nm <- names(b)

# (1) Per-arm slope: d ME_k / d Imp = b[Tk:Imp] + b[cg:Tk:Imp]*cg_at + b[rg:Tk:Imp]*rg_at
slope_L <- function(arm) {
  vapply(nm, function(t) {
    if (!grepl(paste0("Treatment_Group)", arm), t, fixed = TRUE)) return(0)
    if (!grepl("Importance_Crime", t, fixed = TRUE)) return(0)
    val <- 1
    if (grepl("log_crime_gap", t, fixed = TRUE)) val <- val * cg_at
    if (grepl("rank_gap", t, fixed = TRUE)) val <- val * rg_at
    val
  }, numeric(1))
}
cat("\nPer-arm importance slope of the treatment effect (gaps at median):\n")
for (a in arms) {
  L <- slope_L(a)
  est <- sum(L * b)
  se <- sqrt(as.numeric(t(L) %*% V %*% L))
  cat(sprintf(
    "  %s: slope=% .5f  se=%.5f  p=%.3f\n",
    a, est, se, 2 * pt(-abs(est / se), df_resid)
  ))
}

wald <- function(keep) {
  R <- diag(length(b))[keep, , drop = FALSE]
  th <- as.numeric(R %*% b)
  W <- as.numeric(t(th) %*% solve(R %*% V %*% t(R)) %*% th)
  data.frame(chi2 = W, df = nrow(R), p = pchisq(W, nrow(R), lower.tail = FALSE))
}

# (2) all Treatment x Importance terms; (3) only the three-way gap terms.
idx_all <- grepl("Treatment_Group", nm) & grepl("Importance_Crime", nm)
idx_3way <- idx_all & grepl("log_crime_gap|rank_gap", nm)
w_all <- wald(idx_all)
w_3 <- wald(idx_3way)
cat(sprintf(
  "\nJoint Wald test, all %d Treatment x Importance terms = 0: chi2=%.2f, df=%d, p=%.4f\n",
  sum(idx_all), w_all$chi2, w_all$df, w_all$p
))
cat(sprintf(
  "Joint Wald test, only the %d gap x Treatment x Importance terms = 0: chi2=%.2f, df=%d, p=%.4f\n",
  sum(idx_3way), w_3$chi2, w_3$df, w_3$p
))

