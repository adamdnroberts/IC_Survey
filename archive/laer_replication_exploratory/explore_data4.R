library(haven)
d <- read_dta("C:/Users/adamd/Downloads/ANALYSISedited.dta")

# Check share_received by treatment group
cat("=== share_received by treatment group ===\n")
cat("Control (both=0): mean=", mean(d$share_received[d$treatment_local==0 & d$treatment_comp==0], na.rm=TRUE), "\n")
cat("Local (local=1): mean=", mean(d$share_received[d$treatment_local==1], na.rm=TRUE), "\n")
cat("Comp (comp=1): mean=", mean(d$share_received[d$treatment_comp==1], na.rm=TRUE), "\n")

# Check treatment_any variable
cat("\n=== treatment_any ===\n")
print(table(d$treatment_any, useNA="always"))

# Check Ti Tc variables
cat("\n=== Ti and Tc ===\n")
print(table(d$Ti, useNA="always"))
print(table(d$Tc, useNA="always"))

# Check if Ti + Tc matches treatment_local + treatment_comp
cat("Ti vs treatment_local agreement:", mean(d$Ti == d$treatment_local, na.rm=TRUE), "\n")
cat("Tc vs treatment_comp agreement:", mean(d$Tc == d$treatment_comp, na.rm=TRUE), "\n")

# Full crosstab: treatment vs treatment_local/comp
cat("\n=== treatment (1-7) vs treatment_local ===\n")
print(table(d$treatment, d$treatment_local, useNA="always"))

# How many obs have non-NA treatment but NA treatment_local?
cat("\nNon-NA treatment but NA treatment_local:",
    sum(!is.na(d$treatment) & is.na(d$treatment_local)), "\n")

# What are the treatment values for NA treatment_local obs?
cat("Treatment levels for NA treatment_local obs:\n")
print(table(d$treatment[is.na(d$treatment_local)], useNA="always"))

# Check if treatment==1 might be control
cat("\n=== Means of outcome by treatment level ===\n")
for (t in 1:7) {
  sub <- d[!is.na(d$treatment) & d$treatment==t,]
  cat(sprintf("Treatment=%d: n=%d, outcome mean=%.3f, treatment_local mean=%.3f, treatment_comp mean=%.3f\n",
              t, nrow(sub),
              mean(sub$incumbent_vote_reg_share, na.rm=TRUE),
              mean(sub$treatment_local, na.rm=TRUE),
              mean(sub$treatment_comp, na.rm=TRUE)))
}
