rsconnect::deployApp(
  appDir = ".",
  appFiles = c(
    "app_wave1.R",
    ".Renviron",
    "data/00mun_simplified.geojson",
    "data/robo_2025.rds",
    "data/magar2024_coalitions.Rdata",
    "data/nearest10.rds",
    "data/mp_mc_data.rds",
    "data/precip_data.rds",
    "data/region_quotas_wave1.rds",
    "www/Information_Sheet_Informational_Comparisons_Spanish.pdf",
    "www/Morena_logo.png",
    "www/PAN_logo.png",
    "www/PRI_logo.png",
    "www/PRD_logo.png",
    "www/Movimiento_Ciudadano_logo.png",
    "www/PT_logo.png",
    "www/PVEM_logo.png"
  ),
  appPrimaryDoc = "app_wave1.R",
  appName = "IC_Survey_Wave1"
)
