library(haven)
d <- read_dta("C:/Users/adamd/Downloads/ANALYSISedited.dta")

# Find full estimation sample: all 7 treatment groups, non-missing outcome
panelA <- d[!is.na(d$treatment) & !is.na(d$incumbent_vote_reg_share), ]
cat("Panel A (treatment non-NA + outcome non-NA):", nrow(panelA), "\n")

# Treatment coding: recode treatment_local and treatment_comp using treatment variable
panelA$trt_local <- as.integer(panelA$treatment %in% c(2, 4))
panelA$trt_comp  <- as.integer(panelA$treatment %in% c(3, 5))
cat("trt_local=1:", sum(panelA$trt_local), "  trt_comp=1:", sum(panelA$trt_comp), "\n")

# Find variables with mean ~0.21 and sd ~0.17 in the Panel A sample
cat("\n=== Searching for variable with mean~0.21, sd~0.17 ===\n")
for (v in names(panelA)) {
  x <- panelA[[v]]
  if (is.numeric(x)) {
    m <- mean(x, na.rm=TRUE)
    s <- sd(x, na.rm=TRUE)
    if (!is.na(m) && !is.na(s) && m > 0.15 && m < 0.27 && s > 0.12 && s < 0.22) {
      cat(sprintf("%-40s  mean=%.4f  sd=%.4f  n=%d\n", v, m, s, sum(!is.na(x))))
    }
  }
}

# Also find variables with mean ~0.09 and sd ~0.04
cat("\n=== Searching for variable with mean~0.09, sd~0.04 ===\n")
for (v in names(panelA)) {
  x <- panelA[[v]]
  if (is.numeric(x)) {
    m <- mean(x, na.rm=TRUE)
    s <- sd(x, na.rm=TRUE)
    if (!is.na(m) && !is.na(s) && m > 0.06 && m < 0.12 && s > 0.02 && s < 0.06) {
      cat(sprintf("%-40s  mean=%.4f  sd=%.4f  n=%d\n", v, m, s, sum(!is.na(x))))
    }
  }
}

# Construct weight: share_received_local for local, share_received_comp for comp
# and share_received for control (canvassing contact rate)
# First check share_received for each treatment group
cat("\n=== share_received by treatment number ===\n")
for (t in 1:7) {
  sub <- panelA[panelA$treatment==t, ]
  cat(sprintf("Treatment %d: share_received mean=%.3f  share_received_local=%.3f  share_received_comp=%.3f\n",
              t,
              mean(sub$share_received, na.rm=TRUE),
              mean(sub$share_received_local, na.rm=TRUE),
              mean(sub$share_received_comp, na.rm=TRUE)))
}
