rsconnect::deployApp(
  appDir = ".",
  appFiles = c(
    "app_wave2.R",
    ".Renviron",
    "data/00mun_simplified.geojson",
    "data/robo_2025.rds",
    "data/magar2024_coalitions.Rdata",
    "data/precip_data.rds",
    "www/Morena_logo.png",
    "www/PAN_logo.png",
    "www/PRI_logo.png",
    "www/PRD_logo.png",
    "www/Movimiento_Ciudadano_logo.png",
    "www/PT_logo.png",
    "www/PVEM_logo.png"
  ),
  appPrimaryDoc = "app_wave2.R",
  appName = "IC_Survey_Wave2"
)
