library(haven)
library(fixest)
library(dplyr)

d <- read_dta("C:/Users/adamd/Downloads/ANALYSISedited.dta")

# Use original treatment_local/treatment_comp, replacing NAs with group-based imputation
panelA <- d %>%
  filter(!is.na(treatment), !is.na(incumbent_vote_reg_share)) %>%
  mutate(
    # Original variables, NAs replaced using treatment group
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

cat("trt_local=1:", sum(panelA$trt_local),
    " trt_comp=1:", sum(panelA$trt_comp),
    " Panel A n:", nrow(panelA), "\n")

# Weight specifications to try
for (wt_name in c("share_received", "share_received_local", "share_received_comp")) {
  cat(sprintf("\n--- Weight = %s ---\n", wt_name))
}

# Try: orig trt vars + share_received weight
m <- feols(incumbent_vote_reg_share ~ trt_local + trt_comp | id_block,
           data = panelA, weights = ~share_received, cluster = ~cluster2)
cat(sprintf("\nOrig trt + share_received: trt_local=%.4f (%.4f), trt_comp=%.4f (%.4f), n=%d\n",
            coef(m)["trt_local"], se(m)["trt_local"],
            coef(m)["trt_comp"], se(m)["trt_comp"], nobs(m)))

# Try: orig trt vars + no weight
m2 <- feols(incumbent_vote_reg_share ~ trt_local + trt_comp | id_block,
            data = panelA, cluster = ~cluster2)
cat(sprintf("Orig trt + unweighted:     trt_local=%.4f (%.4f), trt_comp=%.4f (%.4f), n=%d\n",
            coef(m2)["trt_local"], se(m2)["trt_local"],
            coef(m2)["trt_comp"], se(m2)["trt_comp"], nobs(m2)))

# Check: what if clustering is by municipality (not cluster2)?
cat("\n=== Checking cluster variable ===\n")
cat("cluster2 unique values:", length(unique(panelA$cluster2)), "\n")
cat("municipality unique values:", length(unique(panelA$municipality)), "\n")
cat("id_block unique values:", length(unique(panelA$id_block)), "\n")

# Try municipality-level clustering
m3 <- feols(incumbent_vote_reg_share ~ trt_local + trt_comp | id_block,
            data = panelA, weights = ~share_received, cluster = ~municipality)
cat(sprintf("\nOrig trt + share_received + cluster=municipality:\n  trt_local=%.4f (%.4f), trt_comp=%.4f (%.4f)\n",
            coef(m3)["trt_local"], se(m3)["trt_local"],
            coef(m3)["trt_comp"], se(m3)["trt_comp"]))

# Check share_received range more carefully
cat("\nshare_received outliers (>2):\n")
print(panelA[panelA$share_received > 2, c("treatment", "share_received",
                                           "share_received_local", "share_received_comp",
                                           "trt_local", "trt_comp")])

# Try winsorizing share_received at 1
panelA$wt_win <- pmin(panelA$share_received, 1)
m4 <- feols(incumbent_vote_reg_share ~ trt_local + trt_comp | id_block,
            data = panelA, weights = ~wt_win, cluster = ~cluster2)
cat(sprintf("\nOrig trt + share_received (winsorized at 1):\n  trt_local=%.4f (%.4f), trt_comp=%.4f (%.4f)\n",
            coef(m4)["trt_local"], se(m4)["trt_local"],
            coef(m4)["trt_comp"], se(m4)["trt_comp"]))

# Published targets: trt_local=0.012***, trt_comp=0.015***
cat("\n[Published: trt_local=0.012 (0.004), trt_comp=0.015 (0.005)]\n")
