# Builds the four derived data files that the analysis pipeline depends on.
# Run from the project root. Slow: make_geojson_file.R simplifies the full INEGI
# municipality shapefile and weather_data.R downloads WorldClim rasters on first
# run, so master_script.R only calls this when the outputs are missing.
#
# Inputs (all gitignored, must be present):
#   data/00mun.shp                              INEGI municipality shapefile
#   data/crime_data_all_years.csv               SESNSP robbery counts
#   references/AGEEML_202512121054579_utf.csv   population
#   references/MUNICIPIOS_202408.csv            municipality names
#
# Outputs:
#   data/robo_2025.rds
#   data/00mun_simplified.geojson
#   data/precip_data.rds
#   data/nearest10.rds

# Robbery statistics. Independent of the geodata below.
source("code/crime_data.R")

# Simplified municipality geometry. Must run before the two scripts that follow:
# both read data/00mun_simplified.geojson.
source("code/make_geojson_file.R")

# WorldClim annual precipitation, used as the placebo treatment.
source("code/weather_data.R")

# Ten nearest municipalities per municipality, used by the benchmark panel.
source("code/precompute_nearest10.R")
