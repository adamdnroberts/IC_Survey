library(readxl)
INEGI_censo_sexo_estado <- read_excel("data/raw/INEGI_censo_sexo_estado.xlsx")

test <- filter(
  INEGI_censo_sexo_estado,
  ...1 != "10" & ...1 != "09" & ...1 != "20" & ...1 != "30" & !is.na(...1)
)

test$total <- as.numeric(gsub(",", "", test$Total))
test$hombres <- as.numeric(gsub(",", "", test$Hombres))
test$mujeres <- as.numeric(gsub(",", "", test$Mujeres))

sum(test$hombres) / sum(test$total)
sum(test$mujeres) / sum(test$total)

excluded_inegi <- c("09", "10", "20", "30")

region_quotas <- test %>%
  mutate(
    prop = total / sum(total),
    target = as.integer(round(prop * 2180)),
    nq_code = case_when(
      ...1 == "01" ~ "20",
      ...1 == "02" ~ "21",
      ...1 == "03" ~ "22",
      ...1 == "04" ~ "23",
      ...1 == "05" ~ "24",
      ...1 == "06" ~ "25",
      ...1 == "07" ~ "26",
      ...1 == "08" ~ "27",
      ...1 == "11" ~ "30",
      ...1 == "12" ~ "31",
      ...1 == "13" ~ "32",
      ...1 == "14" ~ "33",
      ...1 == "15" ~ "34",
      ...1 == "16" ~ "35",
      ...1 == "17" ~ "36",
      ...1 == "18" ~ "37",
      ...1 == "19" ~ "38",
      ...1 == "21" ~ "40",
      ...1 == "22" ~ "41",
      ...1 == "23" ~ "42",
      ...1 == "24" ~ "43",
      ...1 == "25" ~ "44",
      ...1 == "26" ~ "45",
      ...1 == "27" ~ "46",
      ...1 == "28" ~ "47",
      ...1 == "29" ~ "48",
      ...1 == "31" ~ "50",
      ...1 == "32" ~ "51"
    )
  ) %>%
  select(inegi = ...1, state = ...2, total, prop, target, nq_code)

print(region_quotas, n = 50)
cat(sprintf("Total target: %d\n", sum(region_quotas$target)))

saveRDS(
  setNames(region_quotas$target, region_quotas$nq_code),
  "data/region_quotas_wave1.rds"
)
