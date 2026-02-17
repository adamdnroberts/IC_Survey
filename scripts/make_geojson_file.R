library(dplyr)
library(sf)

shapefile_path <- "data/00mun.shp"
d_geo <- st_read(shapefile_path, quiet = TRUE)

# Fix invalid geometries (very important)
d_geo <- st_make_valid(d_geo)
d_geo <- st_transform(d_geo, 4326)
d_geo_fast <- st_simplify(d_geo, dTolerance = 100, preserveTopology = TRUE)

muns <- read.csv(
  "references/MUNICIPIOS_202408.csv"
)
muns$mun_id <- sprintf("%05d", as.numeric(muns$mun_id))

ageeml <- read.csv("references/AGEEML_202512121054579_utf.csv")
state_ids <- distinct(ageeml, CVE_ENT, NOM_ENT)

# Population data by municipality
pop_data <- ageeml %>%
  mutate(
    CVEGEO = as.character(CVEGEO),
    POB_TOTAL = as.numeric(ifelse(POB_TOTAL == "-", NA, POB_TOTAL))
  ) %>%
  select(CVEGEO, POB_TOTAL)

# State capital CVEGEOs (32 Mexican state capitals)
capital_cvegeos <- c(
  "01001", # Aguascalientes
  "02002", # Mexicali
  "03003", # La Paz
  "04002", # San Francisco de Campeche
  "05030", # Saltillo
  "06002", # Colima
  "07101", # Tuxtla Gutiérrez
  "08019", # Chihuahua
  "09000", # Ciudad de México (special case — use full DF)
  "10005", # Victoria de Durango
  "11020", # Guanajuato
  "12029", # Chilpancingo de los Bravo
  "13048", # Pachuca de Soto
  "14039", # Guadalajara
  "15106", # Toluca
  "16053", # Morelia
  "17007", # Cuernavaca
  "18017", # Tepic
  "19039", # Monterrey
  "20067", # Oaxaca de Juárez
  "21114", # Puebla
  "22014", # Santiago de Querétaro
  "23005", # Chetumal (Othón P. Blanco)
  "24028", # San Luis Potosí
  "25006", # Culiacán
  "26030", # Hermosillo
  "27004", # Villahermosa (Centro)
  "28041", # Ciudad Victoria
  "29033", # Tlaxcala
  "30087", # Xalapa
  "31050", # Mérida
  "32056"  # Zacatecas
)

test <- muns %>% left_join(state_ids, join_by(EFE_KEY == CVE_ENT))

test2 <- d_geo_fast %>% left_join(test, join_by(CVEGEO == mun_id))

test2 <- test2 %>%
  left_join(pop_data, by = "CVEGEO") %>%
  mutate(is_capital = CVEGEO %in% capital_cvegeos)

test2$mun_state <- paste0(test2$NOMGEO, ", ", test2$NOM_ENT)

st_write(test2, "data/00mun_simplified.geojson", delete_dsn = TRUE)
