library(dplyr)

# Load crime data
incidencia_delictiva <- read.csv("data/crime_data_all_years.csv")

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
  filter(Ano %in% c(2024, 2025), Subtipo.de.delito %in% robo_subtypes)

# Aggregate monthly data (Jan-Nov) by municipality and year
robo_by_year <- delitos_robo %>%
  group_by(Cve..Municipio, Ano) %>%
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
        Noviembre
    ),
    .groups = "drop"
  )

# Get 2024 and 2025 data separately
robo_2024 <- robo_by_year %>%
  filter(Ano == 2024) %>%
  select(Cve..Municipio, robos_2024 = robos)

robo_2025 <- robo_by_year %>%
  filter(Ano == 2025) %>%
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

# Save as RDS for use in app.R
saveRDS(robo_data, "data/robo_2025.rds")
