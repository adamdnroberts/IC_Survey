library(haven)
library(fixest)
library(dplyr)

d <- read_dta("C:/Users/adamd/Downloads/ANALYSISedited.dta")

panelA <- d %>%
  filter(!is.na(treatment), !is.na(incumbent_vote_reg_share)) %>%
  mutate(
    trt_local = case_when(
      !is.na(treatment_local) ~ as.integer(treatment_local),
      treatment %in% c(2, 4) ~ 1L,
      TRUE ~ 0L
    ),
    trt_comp = case_when(
      !is.na(treatment_comp) ~ as.integer(treatment_comp),
      treatment %in% c(3, 5) ~ 1L,
      TRUE ~ 0L
    )
  )

n <- nrow(panelA)
mean_sr <- mean(panelA$share_received, na.rm = TRUE)

# Stata aweight normalization: w_aw = w / mean(w) (scales to have mean = 1)
# In Stata [aw=w]: internally scales to sum(w) = n, so w_scaled = w / mean(w)
panelA$wt_aw <- panelA$share_received / mean_sr

cat("trt_local=1:", sum(panelA$trt_local), " trt_comp=1:", sum(panelA$trt_comp), "\n")
cat("Scaled weight mean:", mean(panelA$wt_aw, na.rm=TRUE), "\n\n")

# 1. Stata-style aweight normalization
m1 <- feols(incumbent_vote_reg_share ~ trt_local + trt_comp | id_block,
            data = panelA, weights = ~wt_aw, cluster = ~cluster2)
cat(sprintf("Stata-aw normalized: trt_local=%.4f (%.4f), trt_comp=%.4f (%.4f)\n",
            coef(m1)["trt_local"], se(m1)["trt_local"],
            coef(m1)["trt_comp"], se(m1)["trt_comp"]))

# 2. Dose-response: Y ~ share_received_local + share_received_comp | block_FE
# For control precincts: both shares ≈ 0 → baseline
# Published: trt_local=0.012 at binary 0/1 ≈ dose-response coef × mean_share
m2 <- feols(incumbent_vote_reg_share ~ share_received_local + share_received_comp | id_block,
            data = panelA, cluster = ~cluster2)
cat(sprintf("Dose-response (unwtd): share_local=%.4f (%.4f), share_comp=%.4f (%.4f)\n",
            coef(m2)["share_received_local"], se(m2)["share_received_local"],
            coef(m2)["share_received_comp"], se(m2)["share_received_comp"]))
cat(sprintf("  → at mean delivery(%.3f): local=%.4f, comp=%.4f\n",
            mean(c(panelA$share_received_local[panelA$trt_local==1],
                   panelA$share_received_comp[panelA$trt_comp==1]), na.rm=TRUE),
            coef(m2)["share_received_local"] * 0.52,
            coef(m2)["share_received_comp"] * 0.52))

# 3. Dose-response WITH share_received weight
m3 <- feols(incumbent_vote_reg_share ~ share_received_local + share_received_comp | id_block,
            data = panelA, weights = ~share_received, cluster = ~cluster2)
cat(sprintf("Dose-response (wtd):  share_local=%.4f (%.4f), share_comp=%.4f (%.4f)\n",
            coef(m3)["share_received_local"], se(m3)["share_received_local"],
            coef(m3)["share_received_comp"], se(m3)["share_received_comp"]))

# 4. Using share_received as BOTH treatment variable and no weight
m4 <- feols(incumbent_vote_reg_share ~ share_received_local + share_received_comp | id_block,
            data = panelA, weights = ~wt_aw, cluster = ~cluster2)
cat(sprintf("Dose-response (aw wt): share_local=%.4f (%.4f), share_comp=%.4f (%.4f)\n",
            coef(m4)["share_received_local"], se(m4)["share_received_local"],
            coef(m4)["share_received_comp"], se(m4)["share_received_comp"]))

# 5. What if treatment coding uses nonsocial only (treatment 2 and 3)?
panelA2 <- d %>%
  filter(!is.na(treatment), !is.na(incumbent_vote_reg_share)) %>%
  mutate(
    trt_local = as.integer(treatment == 2),  # non-social local only
    trt_comp  = as.integer(treatment == 3),  # non-social comp only
    wt_aw     = share_received / mean(share_received, na.rm=TRUE)
  )
m5 <- feols(incumbent_vote_reg_share ~ trt_local + trt_comp | id_block,
            data = panelA2, weights = ~wt_aw, cluster = ~cluster2)
cat(sprintf("\nNonsocial only + aw wt: trt_local=%.4f (%.4f), trt_comp=%.4f (%.4f), n=%d\n",
            coef(m5)["trt_local"], se(m5)["trt_local"],
            coef(m5)["trt_comp"], se(m5)["trt_comp"], nobs(m5)))

cat("\n[Published: trt_local=0.012 (0.004), trt_comp=0.015 (0.005), n=675]\n")
