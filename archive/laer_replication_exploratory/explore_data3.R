library(haven)
d <- read_dta("C:/Users/adamd/Downloads/ANALYSISedited.dta")

# Check sample sizes for estimation
cat("=== Sample construction ===\n")
cat("Total obs:", nrow(d), "\n")
cat("treatment_local=1:", sum(d$treatment_local==1, na.rm=TRUE), "\n")
cat("treatment_comp=1:", sum(d$treatment_comp==1, na.rm=TRUE), "\n")
cat("Both 0 (control):", sum(d$treatment_local==0 & d$treatment_comp==0, na.rm=TRUE), "\n")
cat("Any NA:", sum(is.na(d$treatment_local) | is.na(d$treatment_comp)), "\n")

# Check Panel A sample (control + local + comp)
panelA <- d[!is.na(d$treatment_local) & !is.na(d$treatment_comp) &
             !is.na(d$incumbent_vote_reg_share), ]
cat("\nPanel A (non-missing treatment+outcome):", nrow(panelA), "\n")

# Check cluster variable
cat("\n=== Cluster variable (cluster2) ===\n")
cat("Unique clusters:", length(unique(d$cluster2)), "\n")
cat("First few:", head(sort(unique(d$cluster2))), "\n")

# Check weight variables distribution
cat("\n=== Share received by treatment group ===\n")
cat("Control (both=0) - share_received_local mean:",
    mean(d$share_received_local[d$treatment_local==0 & d$treatment_comp==0], na.rm=TRUE), "\n")
cat("Local - share_received_local mean:",
    mean(d$share_received_local[d$treatment_local==1], na.rm=TRUE), "\n")
cat("Comp - share_received_comp mean:",
    mean(d$share_received_comp[d$treatment_comp==1], na.rm=TRUE), "\n")

# Check means of moderator variables on Panel A sample
cat("\n=== Moderator variable means (Panel A, n=675 target) ===\n")
# Col 2 moderators
cat("share_not_poor mean:", mean(panelA$share_not_poor, na.rm=TRUE), "sd:", sd(panelA$share_not_poor, na.rm=TRUE), "\n")
cat("share_corruption mean:", mean(panelA$share_corruption, na.rm=TRUE), "sd:", sd(panelA$share_corruption, na.rm=TRUE), "\n")
cat("share_not_poor + share_corruption mean:", mean(panelA$share_not_poor + panelA$share_corruption, na.rm=TRUE), "\n")
cat("share_not_poor_others mean:", mean(panelA$share_not_poor_others, na.rm=TRUE), "sd:", sd(panelA$share_not_poor_others, na.rm=TRUE), "\n")
cat("share_corruption_others mean:", mean(panelA$share_corruption_others, na.rm=TRUE), "sd:", sd(panelA$share_corruption_others, na.rm=TRUE), "\n")

# Col 3 moderators
cat("newchange_i3 mean:", mean(panelA$newchange_i3, na.rm=TRUE), "sd:", sd(panelA$newchange_i3, na.rm=TRUE), "\n")
cat("newchange_i4 mean:", mean(panelA$newchange_i4, na.rm=TRUE), "sd:", sd(panelA$newchange_i4, na.rm=TRUE), "\n")
cat("newchangeCA mean:", mean(panelA$newchangeCA, na.rm=TRUE), "sd:", sd(panelA$newchangeCA, na.rm=TRUE), "\n")
cat("newchangeCB mean:", mean(panelA$newchangeCB, na.rm=TRUE), "sd:", sd(panelA$newchangeCB, na.rm=TRUE), "\n")
cat("newchangeCC mean:", mean(panelA$newchangeCC, na.rm=TRUE), "sd:", sd(panelA$newchangeCC, na.rm=TRUE), "\n")

# Check which col 3 variable has 651 non-missing obs (vs 675 in col 1/2)
cat("\n=== Missing obs for col 3 (target n=651) ===\n")
for (v in c("newchange_i3", "newchange_i4", "newchangeCA", "newchangeCB", "newchangeCC", "prior_i3", "prior_i4")) {
  n <- sum(!is.na(panelA[[v]]))
  cat(v, "non-missing in Panel A:", n, "\n")
}
