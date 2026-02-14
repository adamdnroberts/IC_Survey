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

state_ids <- read.csv("references/AGEEML_202512121054579_utf.csv")
state_ids <- distinct(state_ids, CVE_ENT, NOM_ENT)

test <- muns %>% left_join(state_ids, join_by(EFE_KEY == CVE_ENT))

test2 <- d_geo_fast %>% left_join(test, join_by(CVEGEO == mun_id))

test2$mun_state <- paste0(test2$NOMGEO, ", ", test2$NOM_ENT)

st_write(test2, "data/00mun_simplified.geojson", delete_dsn = TRUE)
