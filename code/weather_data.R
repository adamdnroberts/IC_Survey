library(geodata)
library(terra)
library(sf)
library(dplyr)

if (!file.exists("data/00mun_simplified.geojson")) {
  stop("Missing data/00mun_simplified.geojson. Run: source('code/make_geojson_file.R')")
}

d_geo <- st_read("data/00mun_simplified.geojson", quiet = TRUE)

# Download WorldClim v2.1 bioclimatic variables for Mexico (cached after first run)
# bio12 = Annual Precipitation (mm), resolution 2.5 arcmin (~5 km)
cat("Downloading WorldClim data (cached after first run)...\n")
bio <- worldclim_country(
  country = "MEX",
  var     = "bio",
  res     = 2.5,
  path    = "data/worldclim_cache"
)
precip_raster <- bio[[12]]

# Extract mean annual precipitation per municipality polygon
cat("Extracting precipitation per municipality...\n")
extracted <- terra::extract(precip_raster, vect(d_geo), fun = mean, na.rm = TRUE)

precip_data <- data.frame(
  muni_id  = d_geo$CVEGEO,
  precip_mm = round(extracted[[2]], 1)
)

saveRDS(precip_data, "data/precip_data.rds")
cat("Saved data/precip_data.rds:", nrow(precip_data), "municipalities\n")
cat("  Range:", min(precip_data$precip_mm, na.rm = TRUE), "–",
    max(precip_data$precip_mm, na.rm = TRUE), "mm/year\n")
