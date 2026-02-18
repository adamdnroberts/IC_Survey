library(dplyr)
library(readr)

ps <- read_csv(
  "data/public_security_funds_mun.csv",
  col_names = c("cve_ent", "estado", "cve_mun", "municipio", "y2021", "y2022"),
  skip = 1,
  col_types = cols(.default = "c")
)

# Remove country row (cve_ent == 0) and state subtotal rows (cve_mun == 0)
ps_mun <- ps %>%
  mutate(cve_ent = as.integer(cve_ent), cve_mun = as.integer(cve_mun)) %>%
  filter(cve_ent != 0, cve_mun != 0)

# Parse year columns: "-" → NA, remove thousands separators, convert to numeric
parse_pesos <- function(x) {
  x <- trimws(x)
  x[x == "-"] <- NA
  as.numeric(gsub("\\s", "", x))
}

ps_mun <- ps_mun %>%
  mutate(across(c(y2021, y2022), parse_pesos))

n <- nrow(ps_mun)

# Count and percent of NAs per year column
cat("=== NAs ===\n")
ps_mun %>%
  summarise(across(
    c(y2021, y2022),
    list(
      n   = ~ sum(is.na(.)),
      pct = ~ round(mean(is.na(.)) * 100, 1)
    )
  )) %>%
  print()

cat("\n=== Zeros only ===\n")
ps_mun %>%
  summarise(across(
    c(y2021, y2022),
    list(
      n   = ~ sum(. == 0, na.rm = TRUE),
      pct = ~ round(mean(. == 0, na.rm = TRUE) * 100, 1)
    )
  )) %>%
  print()

cat("\n=== Total municipality rows ===\n")
cat(n, "\n")
