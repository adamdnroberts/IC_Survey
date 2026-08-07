library(haven)
d <- read_dta("C:/Users/adamd/Downloads/ANALYSISedited.dta")

# Treatment variables
cat("=== Treatment variables ===\n")
print(table(d$treatment, useNA="always"))
print(table(d$treatment_local, d$treatment_comp, useNA="always"))

cat("\n=== Key variable means (for identifying interaction variables) ===\n")
# Potential malfeasance variables (col 2 interactions: mean~0.21/0.17, ~0.09/0.04)
vars <- c("share_not_poor", "share_corruption", "share_not_poor_others", "share_corruption_others",
          "not_poor", "corruption",
          "bad_i3", "bad_i4", "bad_CA", "bad_CB", "bad_CC",
          "good_i3", "good_i4", "good_news", "bad_news",
          "newchange_i3", "newchange_i4", "newchangeCA", "newchangeCB", "newchangeCC")
for (v in vars) {
  if (v %in% names(d)) {
    x <- d[[v]]
    cat(sprintf("%-35s  mean=%.3f  sd=%.3f  n=%d\n", v, mean(x, na.rm=TRUE), sd(x, na.rm=TRUE), sum(!is.na(x))))
  }
}

cat("\n=== Dependent variable ===\n")
cat(sprintf("incumbent_vote_reg_share: mean=%.3f sd=%.3f\n",
            mean(d$incumbent_vote_reg_share, na.rm=TRUE),
            sd(d$incumbent_vote_reg_share, na.rm=TRUE)))

cat("\n=== Weight variable candidates ===\n")
wvars <- c("share_received", "share_received_local", "share_received_comp",
           "share_received_nonsocial", "share_received_social")
for (v in wvars) {
  if (v %in% names(d)) {
    x <- d[[v]]
    cat(sprintf("%-35s  mean=%.3f  sd=%.3f\n", v, mean(x, na.rm=TRUE), sd(x, na.rm=TRUE)))
  }
}

cat("\n=== id_block sample ===\n")
cat("N blocks:", length(unique(d$id_block)), "\n")
