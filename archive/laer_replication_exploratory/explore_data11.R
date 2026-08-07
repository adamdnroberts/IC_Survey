library(haven)
library(fixest)
library(dplyr)

d <- read_dta("C:/Users/adamd/Downloads/ANALYSISedited.dta")

panelA <- d %>%
  filter(!is.na(treatment), !is.na(incumbent_vote_reg_share)) %>%
  mutate(
    trt_local = as.integer(Ti),
    trt_comp  = as.integer(Tc),
    inc_mal   = intensity_own,
    chl_mal   = intensity_others,
    upd_inc   = newchange_i3,
    upd_chl   = newchangeCC
  )

panelB <- panelA %>% filter(trt_local==1 | trt_comp==1)
cat("Panel A n:", nrow(panelA),
    " Panel B n:", nrow(panelB),
    " Panel A col3 n:", sum(!is.na(panelA$upd_inc)),
    " Panel B col3 n:", sum(!is.na(panelB$upd_inc)), "\n")

# Panel A all 3 columns
m_a1 <- feols(incumbent_vote_reg_share ~ trt_local + trt_comp | id_block,
              data = panelA, weights = ~share_received, cluster = ~cluster2)
m_a2 <- feols(incumbent_vote_reg_share ~
                trt_local + trt_comp +
                trt_local:inc_mal + trt_comp:inc_mal +
                trt_local:chl_mal + trt_comp:chl_mal | id_block,
              data = panelA, weights = ~share_received, cluster = ~cluster2)
m_a3 <- feols(incumbent_vote_reg_share ~
                trt_local + trt_comp +
                trt_local:upd_inc + trt_comp:upd_inc +
                trt_local:upd_chl + trt_comp:upd_chl | id_block,
              data = panelA, weights = ~share_received, cluster = ~cluster2)

cat("\n===== PANEL A: Results vs Published Table =====\n")
cat("                   Col1                    Col2                    Col3\n")
cat("                 Mine  Pub              Mine  Pub              Mine  Pub\n")
coefs_a <- list(m_a1, m_a2, m_a3)
for (v in c("trt_local", "trt_comp",
            "trt_local:inc_mal", "trt_comp:inc_mal",
            "trt_local:chl_mal", "trt_comp:chl_mal",
            "trt_local:upd_inc", "trt_comp:upd_inc",
            "trt_local:upd_chl", "trt_comp:upd_chl")) {
  row <- sprintf("%-25s", v)
  for (m in coefs_a) {
    if (v %in% names(coef(m))) {
      row <- paste0(row, sprintf(" %7.4f (%6.4f)", coef(m)[v], se(m)[v]))
    } else {
      row <- paste0(row, "         ---        ")
    }
  }
  cat(row, "\n")
}
cat("Observations:", nobs(m_a1), nobs(m_a2), nobs(m_a3), "\n")

# Panel B all 3 columns
m_b1 <- feols(incumbent_vote_reg_share ~ trt_comp | id_block,
              data = panelB, weights = ~share_received, cluster = ~cluster2)
m_b2 <- feols(incumbent_vote_reg_share ~
                trt_comp + trt_comp:inc_mal + trt_comp:chl_mal | id_block,
              data = panelB, weights = ~share_received, cluster = ~cluster2)
m_b3 <- feols(incumbent_vote_reg_share ~
                trt_comp + trt_comp:upd_inc + trt_comp:upd_chl | id_block,
              data = panelB, weights = ~share_received, cluster = ~cluster2)

cat("\n===== PANEL B: Results vs Published Table =====\n")
for (v in c("trt_comp", "trt_comp:inc_mal", "trt_comp:chl_mal",
            "trt_comp:upd_inc", "trt_comp:upd_chl")) {
  row <- sprintf("%-25s", v)
  for (m in list(m_b1, m_b2, m_b3)) {
    if (v %in% names(coef(m))) {
      row <- paste0(row, sprintf(" %7.4f (%6.4f)", coef(m)[v], se(m)[v]))
    } else {
      row <- paste0(row, "                   ")
    }
  }
  cat(row, "\n")
}
cat("Observations:", nobs(m_b1), nobs(m_b2), nobs(m_b3), "\n")
