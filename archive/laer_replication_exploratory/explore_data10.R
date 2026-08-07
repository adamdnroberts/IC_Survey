library(haven)
library(fixest)
library(dplyr)

d <- read_dta("C:/Users/adamd/Downloads/ANALYSISedited.dta")

# Use original treatment_local/treatment_comp with NA → 0
panelA <- d %>%
  filter(!is.na(treatment), !is.na(incumbent_vote_reg_share)) %>%
  mutate(
    trt_local = if_else(is.na(treatment_local), 0L, as.integer(treatment_local)),
    trt_comp  = if_else(is.na(treatment_comp),  0L, as.integer(treatment_comp))
  )

cat("trt_local=1:", sum(panelA$trt_local),
    " trt_comp=1:", sum(panelA$trt_comp), "\n")

# Panel B: local + comp only
panelB <- panelA %>% filter(trt_local==1 | trt_comp==1)
cat("Panel B n:", nrow(panelB), "\n\n")

# Weight = share_received
m_a1 <- feols(incumbent_vote_reg_share ~ trt_local + trt_comp | id_block,
              data = panelA, weights = ~share_received, cluster = ~cluster2)
cat(sprintf("Panel A col1: trt_local=%.4f (%.4f), trt_comp=%.4f (%.4f), n=%d\n",
            coef(m_a1)["trt_local"], se(m_a1)["trt_local"],
            coef(m_a1)["trt_comp"], se(m_a1)["trt_comp"], nobs(m_a1)))

m_b1 <- feols(incumbent_vote_reg_share ~ trt_comp | id_block,
              data = panelB, weights = ~share_received, cluster = ~cluster2)
cat(sprintf("Panel B col1: trt_comp=%.4f (%.4f), n=%d\n",
            coef(m_b1)["trt_comp"], se(m_b1)["trt_comp"], nobs(m_b1)))

cat("\n[Published: Panel A trt_local=0.012 (0.004), trt_comp=0.015 (0.005), n=675]\n")
cat("[Published: Panel B trt_comp=0.005 (0.007), n=398]\n")

# Also try using Ti and Tc (identical to treatment_local/comp but no NAs)
panelA2 <- panelA %>% mutate(trt_l2 = Ti, trt_c2 = Tc)
m2 <- feols(incumbent_vote_reg_share ~ trt_l2 + trt_c2 | id_block,
            data = panelA2, weights = ~share_received, cluster = ~cluster2)
cat(sprintf("\nUsing Ti/Tc: trt_local=%.4f (%.4f), trt_comp=%.4f (%.4f), n=%d\n",
            coef(m2)["trt_l2"], se(m2)["trt_l2"],
            coef(m2)["trt_c2"], se(m2)["trt_c2"], nobs(m2)))
