library(dplyr)

# Load population data (AGEEML reference file)
ageeml <- data.table::fread(
  "references/AGEEML_202512121054579_utf.csv",
  encoding = "Latin-1"
)
pop_data <- ageeml %>%
  mutate(
    Cve..Municipio = as.numeric(CVEGEO),
    POB_TOTAL = as.numeric(ifelse(POB_TOTAL == "-", NA, POB_TOTAL))
  ) %>%
  select(Cve..Municipio, POB_TOTAL)

# Load crime data
incidencia_delictiva <- data.table::fread(
  "data/raw/IDM_NM_dic25.csv",
  encoding = "Latin-1"
)
names(incidencia_delictiva) <- make.names(names(incidencia_delictiva))

# Define robbery subtypes
robo_subtypes <- c(
  "Robo a casa habitación",
  "Robo de vehículo automotor",
  "Robo de autopartes",
  "Robo a transportista",
  "Robo a transeúnte en vía pública",
  "Robo a transeúnte en espacio abierto al público",
  "Robo en transporte público individual",
  "Robo en transporte público colectivo",
  "Robo en transporte individual",
  "Robo a institución bancaria",
  "Robo a negocio",
  "Robo de ganado",
  "Robo de maquinaria",
  "Otros robos"
)

# Filter to 2024-2025 and robbery subtypes
delitos_robo <- incidencia_delictiva %>%
  filter(Año %in% c(2024, 2025), Subtipo.de.delito %in% robo_subtypes)

# Aggregate monthly data (Jan-Nov) by municipality and year
robo_by_year <- delitos_robo %>%
  group_by(Cve..Municipio, Año) %>%
  summarize(
    robos = sum(
      Enero +
        Febrero +
        Marzo +
        Abril +
        Mayo +
        Junio +
        Julio +
        Agosto +
        Septiembre +
        Octubre +
        Noviembre +
        Diciembre
    ),
    .groups = "drop"
  )

# Get 2024 and 2025 data separately
robo_2024 <- robo_by_year %>%
  filter(Año == 2024) %>%
  select(Cve..Municipio, robos_2024 = robos)

robo_2025 <- robo_by_year %>%
  filter(Año == 2025) %>%
  select(Cve..Municipio, robos = robos)

# Join and calculate change
robo_data <- robo_2025 %>%
  left_join(robo_2024, by = "Cve..Municipio") %>%
  mutate(
    change = robos - robos_2024
  )

# Calculate z-score of the change
mean_change <- mean(robo_data$change, na.rm = TRUE)
sd_change <- sd(robo_data$change, na.rm = TRUE)
#symmetric percent change (0 if both years are 0)
robo_data$pct_change <- ifelse(
  robo_data$robos == 0 & robo_data$robos_2024 == 0,
  0,
  robo_data$change / ((robo_data$robos + robo_data$robos_2024) / 2) * 100
)

robo_data <- robo_data %>%
  mutate(
    z_change = (change - mean_change) / sd_change
  )

# Join population and compute robbery rate per 100,000 residents
robo_data <- robo_data %>%
  left_join(pop_data, by = "Cve..Municipio") %>%
  mutate(
    rate_per_100k = ifelse(
      is.na(POB_TOTAL) | POB_TOTAL == 0,
      NA_real_,
      robos / POB_TOTAL * 100000
    )
  )

summary(robo_data$rate_per_100k)
hist(
  log1p(robo_data$rate_per_100k),
  main = "Log robbery rate per 100k, all municipalities"
)

hist(
  robo_data$rate_per_100k,
  main = "Robbery rate per 100k, all municipalities",
  breaks = 100
)

ggplot(robo_data, aes(x = rate_per_100k, weight = POB_TOTAL)) +
  geom_histogram(bins = 60, fill = "steelblue", color = "white") +
  labs(
    title = "Robbery rate per 100k, municipalities weighted by population",
    x = "Robbery rate per 100k",
    y = "Total population"
  ) +
  theme_minimal()

# Save as RDS for use in app.R
saveRDS(robo_data, "data/robo_2025.rds")
