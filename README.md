# IC Survey

An R Shiny web application for academic research on how crime data influences voting behavior in Mexican municipalities. The app implements a multi-page survey experiment that collects demographics, political preferences, and location data, delivers a treatment (comparative robbery statistics), and measures post-treatment changes in voting intentions.

**Principal Investigator:** Adam Roberts, University of Rochester, Department of Political Science

## Requirements

- R (>= 4.0)
- Required packages:

```r
install.packages(c(
  "shiny", "shinyjs", "sortable", "sf", "dplyr",
  "ggplot2", "httr", "jsonlite", "purrr", "readxl", "tidyr", "spdep"
))
```

## Setup

Before running the application, two data files must be generated (they are gitignored):

1. **Municipality boundaries** (`data/00mun_simplified.geojson`):
   - Requires INEGI shapefiles in `references/`
   - Generate with: `source("scripts/make_geojson_file.R")`

2. **Robbery data** (`data/robo_2025.rds`):
   - Requires `data/crime_data_all_years.csv`
   - Generate with: `source("scripts/crime_data.R")`

## Running the Application

```r
shiny::runApp("app.R")
```

The app is also configured for deployment to [shinyapps.io](https://www.shinyapps.io/) via the `rsconnect/` directory.

## Survey Flow

The survey consists of 10 pages (0--9):

| Page | Content |
|------|---------|
| 0 | Information sheet / consent |
| 1 | Home municipality selection (searchable dropdown) |
| 2 | Municipality confirmation |
| 3 | Political views: party preference, last election vote, governance grid |
| 4 | Pre-treatment: municipality crime ranking (bucket list), incumbent rating, turnout likelihood, vote intention, issue importance ranking |
| 5 | Demographics: age, gender, indigenous identity, attention check |
| 6 | Treatment delivery (robbery statistics with comparison graphs) |
| 7 | Post-treatment: turnout likelihood, incumbent rating, crime ranking, party allegiance update, vote intention |
| 8 | Reference municipality selection |
| 9 | Thank you / confirmation |

## Treatment Groups

The experiment has four treatment conditions:

1. **Control** -- Placebo text about agave cultivation
2. **Plain Information** -- Robbery change statistics for the respondent's home municipality
3. **Same-Party Comparison** -- Robbery statistics compared to municipalities governed by the same political coalition
4. **Opposite-Party Comparison** -- Robbery statistics compared to municipalities governed by the opposing coalition

Coalitions are defined as:
- Coalition A: MORENA, PT, PVEM
- Coalition B: PAN, PRI, PRD, MC

## Project Structure

```
IC_Survey/
├── app.R                        # Shiny application (UI + server)
├── scripts/
│   ├── crime_data.R             # Aggregate robbery data -> data/robo_2025.rds
│   ├── make_geojson_file.R      # Simplify INEGI shapefiles -> GeoJSON
│   ├── power_analysis_simulation.R          # Power analysis (7 groups, N=6000)
│   ├── power_analysis_simulation_less_treatments.R  # Power analysis (5 groups, variable N)
│   └── create_municipality_graph.R          # Sample comparison bar charts
├── data/                        # Data files (gitignored)
├── docs/                        # Research protocols & documentation
├── www/                         # Static assets (party logos)
├── references/                  # Reference data & INEGI scripts (gitignored)
├── latex/                       # LaTeX documents
└── rsconnect/                   # Shinyapps.io deployment config
```

## Data Pipeline

```
INEGI shapefiles (.shp)
  └─> scripts/make_geojson_file.R
        └─> data/00mun_simplified.geojson  (loaded by app.R at startup)

crime_data_all_years.csv
  └─> scripts/crime_data.R
        └─> data/robo_2025.rds  (loaded by app.R at startup)

Survey responses
  └─> data/survey_responses.csv  (appended on each submission)
```

## Data Output

Survey responses are saved to `data/survey_responses.csv`. Each row contains:

- Home municipality and respondent-selected reference municipalities
- Demographics (age, gender, indigenous identity)
- Political views (party preference, party strength, last election vote)
- Governance beliefs (which party governs each municipality)
- Issue importance rankings
- Pre-treatment measures (crime ranking, incumbent rating, turnout likelihood, vote intention)
- Post-treatment measures (crime ranking, incumbent rating, turnout likelihood, party allegiance, vote intention)
- Timestamp

## License

This project is part of academic research at the University of Rochester. Contact [arober48@ur.rochester.edu](mailto:arober48@ur.rochester.edu) for inquiries.
