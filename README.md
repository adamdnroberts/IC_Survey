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

The survey consists of 11 pages (0--10):

| Page | Content |
|------|---------|
| 0 | Information sheet / consent |
| 1 | Home municipality selection (address geocoding via Nominatim/Photon + dropdown fallback + confirmation) |
| 2 | Practice drag-and-drop ranking (news sources — attention check) |
| 3 | Last election vote, governance grid |
| 4 | Pre-treatment: crime explainer, home municipality crime handling slider, coalition handling sliders, robbery estimate, municipality crime ranking, issue importance ranking |
| 5 | Political views: left-right scale, party identification, attention check |
| 6 | Treatment delivery (robbery % change statistics with comparison bar charts) |
| 7 | Post-treatment: crime explainer, home municipality crime handling slider, coalition handling sliders, municipality crime ranking |
| 8 | Post-treatment: turnout likelihood, vote intention |
| 9 | Reference municipality selection |
| 10 | Thank you / confirmation |

## Treatment Groups

The experiment has five treatment conditions:

1. **Control** -- Placebo text about agave cultivation
2. **T1 — Plain Information** -- Robbery % change for the respondent's home municipality only
3. **T2 — Non-partisan Comparison** -- Robbery change + bar chart vs. randomly sampled same-state municipalities
4. **T3 — Opposite-coalition Comparison** -- Robbery change + bar chart vs. opposite-coalition same-state municipalities (with party labels)
5. **T4 — Same-coalition Comparison** -- Robbery change + bar chart vs. same-coalition same-state municipalities (with party labels)

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

- Home municipality (CVEGEO) and respondent-selected reference municipalities
- Political views (left-right scale, party identification, last election vote)
- Governance beliefs (governance grid ratings per municipality)
- Issue importance rankings
- Pre-treatment measures (crime handling sliders, robbery estimate, municipality crime ranking)
- Treatment group assignment and post-treatment measures (crime handling sliders, municipality crime ranking, turnout likelihood, vote intention)
- Timestamp

## License

This project is part of academic research at the University of Rochester. Contact [arober48@ur.rochester.edu](mailto:arober48@ur.rochester.edu) for inquiries.
