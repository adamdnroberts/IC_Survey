library(haven)
library(fixest)
library(dplyr)

d <- read_dta("C:/Users/adamd/Downloads/ANALYSISedited.dta")

panelA <- d %>%
  filter(!is.na(treatment), !is.na(incumbent_vote_reg_share)) %>%
  mutate(
    trt_local = as.integer(treatment %in% c(2, 4)),
    trt_comp  = as.integer(treatment %in% c(3, 5)),
    inc_mal   = intensity_own,
    chl_mal   = intensity_others,
    upd_inc   = newchange_i3,
    upd_chl   = newchangeCC,
    # wt_share_received: the variable with max > 1
    wt_share  = share_received,
    # treatment-specific weight
    wt_ts     = ifelse(trt_local==1, share_received_local,
                       ifelse(trt_comp==1, share_received_comp,
                              mean(c(share_received_local[trt_local==1],
                                     share_received_comp[trt_comp==1]), na.rm=TRUE))),
    wt_ts     = pmax(wt_ts, 0.001)
  )

cat("share_received summary:\n")
print(summary(panelA$share_received))

cat("\ntrt_local=1:", sum(panelA$trt_local), " trt_comp=1:", sum(panelA$trt_comp), "\n")

# Try 4 weight specifications for col 1 only
cat("\n=== Col 1: trt_local coef by weight specification (target: 0.012) ===\n")

# Unweighted
m1 <- feols(incumbent_vote_reg_share ~ trt_local + trt_comp | id_block,
            data = panelA, cluster = ~cluster2)
cat(sprintf("Unweighted:        trt_local=%.4f (%.4f)  trt_comp=%.4f (%.4f)  n=%d\n",
            coef(m1)["trt_local"], se(m1)["trt_local"],
            coef(m1)["trt_comp"], se(m1)["trt_comp"],
            nobs(m1)))

# Weight = share_received (as-is, even if > 1)
m2 <- feols(incumbent_vote_reg_share ~ trt_local + trt_comp | id_block,
            data = panelA, weights = ~wt_share, cluster = ~cluster2)
cat(sprintf("share_received:    trt_local=%.4f (%.4f)  trt_comp=%.4f (%.4f)  n=%d\n",
            coef(m2)["trt_local"], se(m2)["trt_local"],
            coef(m2)["trt_comp"], se(m2)["trt_comp"],
            nobs(m2)))

# Treatment-specific weight with floor
m3 <- feols(incumbent_vote_reg_share ~ trt_local + trt_comp | id_block,
            data = panelA, weights = ~wt_ts, cluster = ~cluster2)
cat(sprintf("treat-specific wt: trt_local=%.4f (%.4f)  trt_comp=%.4f (%.4f)  n=%d\n",
            coef(m3)["trt_local"], se(m3)["trt_local"],
            coef(m3)["trt_comp"], se(m3)["trt_comp"],
            nobs(m3)))

# Weight = share_received_local + share_received_comp
panelA$wt_sum <- panelA$share_received_local + panelA$share_received_comp
m4 <- feols(incumbent_vote_reg_share ~ trt_local + trt_comp | id_block,
            data = panelA, weights = ~wt_sum, cluster = ~cluster2)
cat(sprintf("sum_received:      trt_local=%.4f (%.4f)  trt_comp=%.4f (%.4f)  n=%d\n",
            coef(m4)["trt_local"], se(m4)["trt_local"],
            coef(m4)["trt_comp"], se(m4)["trt_comp"],
            nobs(m4)))

# Check if TR variable is the weight
cat("\nTR variable summary:\n")
print(summary(panelA$TR))
cat("TR by treatment group:\n")
for(t in 1:7) {
  sub <- panelA[panelA$treatment==t,]
  cat(sprintf("  treatment=%d: TR mean=%.4f sd=%.4f\n",
              t, mean(sub$TR, na.rm=TRUE), sd(sub$TR, na.rm=TRUE)))
}

# Try weight = TR
m5 <- feols(incumbent_vote_reg_share ~ trt_local + trt_comp | id_block,
            data = panelA, weights = ~TR, cluster = ~cluster2)
cat(sprintf("\nTR weight:         trt_local=%.4f (%.4f)  trt_comp=%.4f (%.4f)  n=%d\n",
            coef(m5)["trt_local"], se(m5)["trt_local"],
            coef(m5)["trt_comp"], se(m5)["trt_comp"],
            nobs(m5)))
