library(dplyr)
library(ggplot2)

magar_incumbents <- read.csv("~/IC_Survey/data/magar_incumbents.csv")

# Map state abbreviations to full names
state_names <- c(
  "ags" = "Aguascalientes",
  "bc" = "Baja California",
  "bcs" = "Baja California Sur",
  "cam" = "Campeche",
  "coa" = "Coahuila",
  "col" = "Colima",
  "chi" = "Chiapas",
  "chs" = "Chihuahua",
  "cin" = "Ciudad de México",
  "cdmx" = "Ciudad de México",
  "df" = "Ciudad de México",
  "dur" = "Durango",
  "gua" = "Guanajuato",
  "gue" = "Guerrero",
  "hid" = "Hidalgo",
  "jal" = "Jalisco",
  "mex" = "Estado de México",
  "mic" = "Michoacán",
  "mor" = "Morelos",
  "nay" = "Nayarit",
  "nl" = "Nuevo León",
  "oax" = "Oaxaca",
  "pue" = "Puebla",
  "que" = "Querétaro",
  "qui" = "Quintana Roo",
  "san" = "San Luis Potosí",
  "sin" = "Sinaloa",
  "son" = "Sonora",
  "tab" = "Tabasco",
  "tam" = "Tamaulipas",
  "tla" = "Tlaxcala",
  "ver" = "Veracruz",
  "yuc" = "Yucatán",
  "zac" = "Zacatecas"
)

magar_incumbents <- magar_incumbents %>%
  mutate(
    state_abbr = sub("-.*", "", emm),
    estado = state_names[state_abbr]
  )

magar_2024 <- filter(magar_incumbents, yr == 2024 & !grepl("^oax", emm))

# Group any coalition containing morena into the MORENA coalition (pvem-pt-morena)
magar_2024 <- magar_2024 %>%
  mutate(
    coalition = case_when(
      grepl("morena|pvem|pt", part) ~ "pvem-pt-morena",
      grepl("pan|pri|prd", part) ~ "pan-pri-prd",
      grepl("mc", part) ~ "mc",
      TRUE ~ part
    )
  )

# Check for cross-coalition cases (morena + pan/pri/prd in same coalition)
cross_coalition <- magar_2024 %>%
  filter(grepl("morena", part) & grepl("pan|pri|prd", part))
cat(
  "Cross-coalition cases (morena with pan/pri/prd):",
  nrow(cross_coalition),
  "\n"
)
if (nrow(cross_coalition) > 0) {
  print(cross_coalition %>% select(emm, mun, yr, part))
}

# Bar graph: frequency of each coalition
coalition_counts <- magar_2024 %>%
  count(estado, coalition, sort = TRUE)

ggplot(coalition_counts, aes(x = reorder(coalition, n), y = n)) +
  geom_col(fill = "#0072B2") +
  coord_flip() +
  labs(
    title = "Frequency of coalitions in municipal elections (2024)",
    x = "Coalition",
    y = "Count"
  ) +
  facet_wrap(~estado) +
  theme_minimal()
