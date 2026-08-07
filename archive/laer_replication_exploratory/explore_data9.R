library(haven)
library(fixest)
library(dplyr)

d <- read_dta("C:/Users/adamd/Downloads/ANALYSISedited.dta")

panelA <- d %>% filter(!is.na(treatment), !is.na(incumbent_vote_reg_share))

cat("=== treatment_nonsocial and treatment_social ===\n")
cat("treatment_nonsocial=1:", sum(panelA$treatment_nonsocial, na.rm=TRUE),
    " NA:", sum(is.na(panelA$treatment_nonsocial)), "\n")
cat("treatment_social=1:", sum(panelA$treatment_social, na.rm=TRUE),
    " NA:", sum(is.na(panelA$treatment_social)), "\n")
cat("\nCross-tab with treatment:\n")
print(table(panelA$treatment, panelA$treatment_nonsocial, useNA="always"))
print(table(panelA$treatment, panelA$treatment_social, useNA="always"))

# Check Tin Tcn Tis Tcs
cat("\n=== Tin/Tcn/Tis/Tcs ===\n")
for (v in c("Tin", "Tcn", "Tis", "Tcs", "Ti", "Tc")) {
  cat(v, "=1:", sum(panelA[[v]]==1, na.rm=TRUE),
      " NA:", sum(is.na(panelA[[v]])), "\n")
}

# Cross-tab treatment vs Tin/Tcn
cat("\ntreatment vs Tin:\n")
print(table(panelA$treatment, panelA$Tin, useNA="always"))
cat("\ntreatment vs Tcn:\n")
print(table(panelA$treatment, panelA$Tcn, useNA="always"))

# Try treatment_nonsocial as "local" and treatment_comp+treatment_social as something else
# Check: does Tin = local non-social (treatment=2)?
cat("\n=== Candidate: trt_local=Tin, trt_comp=Tc ===\n")
panelA2 <- panelA %>%
  mutate(
    trt_local = case_when(!is.na(Tin) ~ as.integer(Tin), TRUE ~ 0L),
    trt_comp  = case_when(!is.na(Tc)  ~ as.integer(Tc),  TRUE ~ 0L)
  )
cat("trt_local=1:", sum(panelA2$trt_local),
    " trt_comp=1:", sum(panelA2$trt_comp),
    " Panel A n:", nrow(panelA2), "\n")
panelB2 <- panelA2 %>% filter(trt_local==1 | trt_comp==1)
cat("Panel B n:", nrow(panelB2), "\n")

m <- feols(incumbent_vote_reg_share ~ trt_local + trt_comp | id_block,
           data = panelA2, weights = ~share_received, cluster = ~cluster2)
cat(sprintf("Tin/Tc + sr weight: trt_local=%.4f (%.4f), trt_comp=%.4f (%.4f)\n",
            coef(m)["trt_local"], se(m)["trt_local"],
            coef(m)["trt_comp"], se(m)["trt_comp"]))

# Check indi/indicador as potential weight
cat("\n=== indi and indicador ===\n")
cat("indi summary:", "\n"); print(summary(panelA$indi))
cat("indicador summary:", "\n"); print(summary(panelA$indicador))
cat("indi/listanominal summary:\n"); print(summary(panelA$indi / panelA$listanominal))

# Try weight = indi/listanominal
panelA3 <- panelA %>%
  mutate(
    trt_local = case_when(
      !is.na(treatment_local) ~ as.integer(treatment_local),
      treatment %in% c(2,4) ~ 1L, TRUE ~ 0L),
    trt_comp  = case_when(
      !is.na(treatment_comp) ~ as.integer(treatment_comp),
      treatment %in% c(3,5) ~ 1L, TRUE ~ 0L),
    wt_indi = indi / listanominal
  )
m2 <- feols(incumbent_vote_reg_share ~ trt_local + trt_comp | id_block,
            data = panelA3, weights = ~wt_indi, cluster = ~cluster2)
cat(sprintf("\norig trt + indi/listanominal wt: trt_local=%.4f (%.4f), trt_comp=%.4f (%.4f), n=%d\n",
            coef(m2)["trt_local"], se(m2)["trt_local"],
            coef(m2)["trt_comp"], se(m2)["trt_comp"], nobs(m2)))

cat("\n[Published: trt_local=0.012 (0.004), trt_comp=0.015 (0.005), n=675]\n")
