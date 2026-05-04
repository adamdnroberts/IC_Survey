library(shiny)
library(shinyjs)
library(sortable)
library(sf)
library(leaflet)
library(dplyr)
library(ggplot2)
library(httr)
library(jsonlite)
library(paws.storage)

# Read municipalities from GeoJSON file
if (!file.exists("data/00mun_simplified.geojson")) {
  stop(
    "Missing data/00mun_simplified.geojson. Run: source('scripts/make_geojson_file.R')"
  )
}
d_geo <- st_read("data/00mun_simplified.geojson", quiet = TRUE)
municipality_list <- sort(unique(d_geo$mun_state))
d_geo$muni_id <- d_geo$CVEGEO
muni_choices <- setNames(d_geo$muni_id, d_geo$mun_state)
muni_choices <- muni_choices[order(names(muni_choices))]

# Load robbery data for treatment graph
if (!file.exists("data/robo_2025.rds")) {
  stop("Missing data/robo_2025.rds. Run: source('scripts/crime_data.R')")
}
robo_data <- readRDS("data/robo_2025.rds")

# Load pre-computed municipality precipitation data
if (!file.exists("data/precip_data.rds")) {
  stop("Missing data/precip_data.rds. Run: source('code/weather_data.R')")
}
precip_data <- readRDS("data/precip_data.rds")

# Load pre-computed nearest-10 municipality lookup
if (!file.exists("data/nearest10.rds")) {
  stop(
    "Missing data/nearest10.rds. Run: source('scripts/precompute_nearest10.R')"
  )
}
nearest10 <- readRDS("data/nearest10.rds")

# Top 20 municipalities by population (for benchmark selection)
top20_munis <- d_geo %>%
  st_drop_geometry() %>%
  filter(!is.na(POB_TOTAL)) %>%
  arrange(desc(POB_TOTAL)) %>%
  slice_head(n = 20) %>%
  pull(muni_id)

# Filter to large cities (200k+ population) and state capitals
# Load governing party data from Magar 2024 coalition dataset (all states exc. Oaxaca, CDMX, Durango, Veracruz)
load("data/magar2024_coalitions.Rdata") # loads magar2024
all_parties <- magar2024 %>%
  mutate(
    CVEGEO = sprintf("%05d", inegi),
    governing_party = case_when(
      grepl("morena|pvem|pt", l01) ~ "MORENA",
      grepl("pan", l01) ~ "PAN",
      grepl("pri|prd", l01) ~ "PRI",
      grepl("mc", l01) ~ "MC",
      TRUE ~ NA_character_
    ),
    coalition_label = case_when(
      grepl("morena|pvem|pt", l01) ~ "MORENA/PVEM/PT",
      grepl("pan|pri|prd", l01) ~ "PAN/PRI/PRD",
      grepl("mc", l01) ~ "MC",
      TRUE ~ NA_character_
    )
  ) %>%
  select(CVEGEO, governing_party, coalition_label)

d_geo <- d_geo %>%
  left_join(all_parties, by = c("muni_id" = "CVEGEO"))

# Add area (km²) and centroid coordinates for Mahalanobis matching
d_geo <- d_geo %>%
  mutate(area_km2 = as.numeric(sf::st_area(.)) / 1e6)
muni_centroid_coords <- sf::st_coordinates(sf::st_centroid(d_geo))
d_geo$centroid_lon <- muni_centroid_coords[, 1]
d_geo$centroid_lat <- muni_centroid_coords[, 2]

# Coalition definitions for treatment groups
coalition_a <- c("MORENA", "PVEM", "PT")
coalition_b <- c("PAN", "PRI", "PRD")

get_same_coalition_parties <- function(party) {
  if (party %in% coalition_a) {
    coalition_a
  } else if (party %in% coalition_b) {
    coalition_b
  } else {
    c("MC")
  }
}

get_opposite_parties <- function(party) {
  if (party %in% coalition_a) coalition_b else coalition_a
}

get_coalition_label <- function(parties) {
  paste(parties, collapse = "/")
}

# Mahalanobis matching on log(population), log(area_km2), log(distance_km).
# Returns the n closest candidates to home_id by Mahalanobis distance.
# candidates must contain columns: muni_id, POB_TOTAL, area_km2, centroid_lon, centroid_lat.
mahal_match_munis <- function(home_id, candidates, n = 4) {
  if (nrow(candidates) == 0) {
    return(candidates)
  }
  if (nrow(candidates) <= n) {
    return(candidates)
  }

  home_row <- d_geo %>%
    st_drop_geometry() %>%
    filter(muni_id == home_id)

  if (nrow(home_row) == 0) {
    return(candidates[seq_len(n), ])
  }

  # Great-circle distance from home centroid to each candidate centroid (km)
  home_sf <- sf::st_sfc(
    sf::st_point(c(home_row$centroid_lon[1], home_row$centroid_lat[1])),
    crs = 4326
  )
  cand_sf <- sf::st_as_sf(
    candidates,
    coords = c("centroid_lon", "centroid_lat"),
    crs = 4326,
    remove = FALSE
  )
  dist_km <- as.numeric(sf::st_distance(home_sf, cand_sf)) / 1000

  # Covariate matrix (log-transformed)
  X <- cbind(
    log_pop = log(pmax(candidates$POB_TOTAL, 1)),
    log_area = log(pmax(candidates$area_km2, 0.01)),
    log_dist = log(pmax(dist_km, 0.1))
  )

  # Target: home municipality values; distance target = 0.1 km (prefer nearby)
  x0 <- c(
    log_pop = log(max(home_row$POB_TOTAL[1], 1)),
    log_area = log(max(home_row$area_km2[1], 0.01)),
    log_dist = log(0.1)
  )

  cov_X <- cov(X)
  mahal_dist <- tryCatch(
    mahalanobis(X, x0, cov_X),
    error = function(e) rowSums(scale(X)^2)
  )

  candidates[order(mahal_dist)[seq_len(n)], ]
}

# Map color palette for governing coalition
coalition_map_colors <- c(
  "MORENA/PVEM/PT" = "#8B0000",
  "PAN/PRI/PRD" = "#00308F",
  "MC" = "#FF5722"
)
coalition_pal <- colorFactor(
  palette = unname(coalition_map_colors),
  levels = names(coalition_map_colors),
  na.color = "#D3D3D3"
)
pop_pal <- colorQuantile(
  palette = "YlOrRd",
  domain = d_geo$POB_TOTAL,
  n = 6,
  na.color = "#D3D3D3"
)

party_radio_choice <- function(
  value,
  label,
  img_src = NULL,
  input_name = "party_preference"
) {
  tags$label(
    class = "party-radio-label",
    tags$input(
      type = "radio",
      name = input_name,
      value = value,
      class = "party-radio-input"
    ),
    if (!is.null(img_src)) tags$img(src = img_src, class = "party-logo"),
    tags$span(label)
  )
}

party_checkbox_choice <- function(
  value,
  label,
  img_src = NULL,
  group = "last_election_vote"
) {
  tags$label(
    class = "party-radio-label",
    tags$input(
      type = "checkbox",
      value = value,
      class = "party-checkbox-input",
      `data-group` = group
    ),
    if (!is.null(img_src)) tags$img(src = img_src, class = "party-logo"),
    tags$span(label)
  )
}

slot_ranker_ui <- function(
  input_id,
  items,
  source_label = "Available items:",
  slots_label = "Your ranking:"
) {
  n <- length(items)
  source_id <- paste0(input_id, "_sr_source")
  container_id <- paste0(input_id, "_sr_container")
  item_divs <- lapply(items, function(item) {
    tags$div(
      class = "slot-rank-item",
      `data-value` = item,
      `data-source-id` = source_id,
      `data-container-id` = container_id,
      `data-input-id` = input_id,
      `data-n` = as.character(n),
      item,
      tags$button(
        class = "remove-item",
        onclick = "removeFromSlot(this);",
        title = "Quitar",
        "\u00d7"
      )
    )
  })
  slot_rows <- lapply(seq_len(n), function(i) {
    tags$div(
      class = "slot-rank-row",
      tags$span(class = "slot-rank-label", paste0(i, ".")),
      tags$div(id = paste0(container_id, "_slot_", i), class = "slot-rank-drop")
    )
  })
  tagList(
    div(
      class = "slot-ranker",
      div(
        class = "slot-ranker-source",
        tags$p(tags$strong(source_label)),
        tags$div(id = source_id, class = "slot-rank-pool", tagList(item_divs))
      ),
      div(
        class = "slot-ranker-slots",
        tags$p(tags$strong(slots_label)),
        tagList(slot_rows)
      )
    ),
    tags$script(HTML(sprintf(
      "setTimeout(function(){initSlotRanker('%s','%s','%s',%d);},100);",
      source_id,
      container_id,
      input_id,
      n
    )))
  )
}

ui <- fluidPage(
  useShinyjs(),
  tags$head(
    tags$script(
      src = "https://cdn.jsdelivr.net/npm/sortablejs@1.15.2/Sortable.min.js"
    ),
    tags$style(HTML(
      "
      /* Mobile-only elements */
      .mobile-only { display: none; }
      @media (max-width: 768px) { .mobile-only { display: block; } }

      /* Full-width map breakout on mobile */
      @media (max-width: 768px) {
        body, html { overflow-x: hidden; }
        .well { overflow: visible !important; }
        .map-fullwidth {
          width: 100vw;
          position: relative;
          left: 50%;
          transform: translateX(-50%);
        }
      }

      /* Whitespace between survey elements */
      .shiny-input-container {
        margin-bottom: 24px;
      }
      .form-group {
        margin-bottom: 24px;
      }
      .well p, .well hr {
        margin-bottom: 16px;
      }
      .party-radio-group {
        display: grid;
        grid-template-columns: 1fr 1fr;
        gap: 0;
      }
      .party-radio-group .party-radio-label {
        display: flex;
        align-items: center;
        padding: 6px 4px;
        cursor: pointer;
        border-radius: 4px;
        margin-bottom: 4px;
      }
      .party-radio-group .party-radio-label:hover {
        background-color: #f0f0f0;
      }
      .party-radio-group .party-radio-input {
        margin-right: 10px;
        flex-shrink: 0;
      }
      .party-radio-group .party-logo {
        height: 30px;
        width: 30px;
        object-fit: contain;
        margin-right: 10px;
        flex-shrink: 0;
      }
      .rank-list-container {
        counter-reset: rank-number;
      }
      .rank-list-container .rank-list-item {
        counter-increment: rank-number;
      }
      .rank-list-container .rank-list-item::before {
        content: counter(rank-number) '.';
        font-weight: bold;
        margin-right: 8px;
        min-width: 20px;
      }
      /* Source bins: no numbering, no color coding */
      .source-bin .rank-list-item::before {
        content: none !important;
      }
      .source-bin .rank-list-item,
      .source-bin .rank-list-item:nth-child(1),
      .source-bin .rank-list-item:nth-child(2),
      .source-bin .rank-list-item:nth-child(3),
      .source-bin .rank-list-item:nth-child(4),
      .source-bin .rank-list-item:nth-child(5) {
        background-color: #ffffff !important;
        color: #333333 !important;
      }
      .rank-list-container .rank-list-item:nth-child(1) {
        background-color: #D4AF37;
        color: #333333;
      }
      .rank-list-container .rank-list-item:nth-child(2) {
        background-color: #C08B2A;
        color: #333333;
      }
      .rank-list-container .rank-list-item:nth-child(3) {
        background-color: #9B6B3A;
        color: #ffffff;
      }
      .rank-list-container .rank-list-item:nth-child(4) {
        background-color: #7A4E2D;
        color: #ffffff;
      }
      .rank-list-container .rank-list-item:nth-child(5) {
        background-color: #5C3317;
        color: #ffffff;
      }
      /* Page 9 & 11 sliders: grayed out until touched */
      #page9 .shiny-input-container.slider-untouched .irs-line,
      #page9 .shiny-input-container.slider-untouched .irs-bar,
      #page9 .shiny-input-container.slider-untouched .irs-bar-edge,
      #page9 .shiny-input-container.slider-untouched .irs-handle,
      #page9 .shiny-input-container.slider-untouched .irs-single,
      #page9 .shiny-input-container.slider-untouched .irs-min,
      #page9 .shiny-input-container.slider-untouched .irs-max {
        opacity: 0.35;
      }
      /* Governance grid table */
      .governance-grid {
        overflow-x: auto;
      }
      .governance-grid table {
        width: 100%;
        border-collapse: collapse;
        font-size: 0.9em;
      }
      .governance-grid th {
        text-align: center;
        padding: 6px 4px;
        background-color: #f8f9fa;
        border-bottom: 2px solid #dee2e6;
        font-weight: 600;
        font-size: 0.85em;
        white-space: nowrap;
      }
      .governance-grid th:first-child {
        text-align: left;
        min-width: 160px;
      }
      .governance-grid td {
        text-align: center;
        padding: 8px 4px;
        border-bottom: 1px solid #eee;
        vertical-align: middle;
      }
      .governance-grid td:first-child {
        text-align: left;
        font-weight: 500;
      }
      .governance-grid tr:hover {
        background-color: #f8f9fa;
      }
      .governance-grid input[type='radio'] {
        margin: 0;
        cursor: pointer;
        width: 16px;
        height: 16px;
      }
      .governance-grid tr.home-row td:first-child {
        color: #0072B2;
        font-weight: 600;
      }

      /* Slot ranker widget */
      .slot-ranker {
        display: flex;
        gap: 24px;
        align-items: flex-start;
        margin: 10px 0;
      }
      .slot-ranker-source, .slot-ranker-slots { flex: 1; }
      .slot-rank-pool {
        border: 1px solid #ddd;
        border-radius: 6px;
        padding: 8px;
        background: #f8f9fa;
        min-height: 52px;
      }
      .slot-rank-item {
        background: #fff;
        border: 1px solid #ccc;
        border-radius: 6px;
        padding: 10px 14px;
        margin-bottom: 6px;
        cursor: grab;
        user-select: none;
        font-size: 0.95em;
        touch-action: none;
      }
      .slot-rank-item:last-child { margin-bottom: 0; }
      .slot-rank-item:active { cursor: grabbing; }
      .slot-rank-row {
        display: flex;
        align-items: center;
        gap: 10px;
        margin-bottom: 8px;
      }
      .slot-rank-label {
        font-weight: bold;
        min-width: 28px;
        text-align: right;
        color: #555;
        flex-shrink: 0;
      }
      .slot-rank-drop {
        flex: 1;
        min-height: 46px;
        border: 2px dashed #ccc;
        border-radius: 6px;
        padding: 4px;
        background: #fafafa;
        transition: border-color 0.15s, background 0.15s;
      }
      .slot-rank-drop .slot-rank-item {
        margin-bottom: 0;
        background: #eaf6ee;
        border-color: #27AE60;
        cursor: grab;
        display: flex;
        align-items: center;
        justify-content: space-between;
      }
      .slot-rank-item .remove-item { display: none; }
      .slot-rank-drop .slot-rank-item .remove-item {
        display: inline-flex;
        align-items: center;
        background: none;
        border: none;
        color: #999;
        font-size: 1.1em;
        cursor: pointer;
        padding: 0 0 0 8px;
        line-height: 1;
        flex-shrink: 0;
      }
      .slot-rank-drop .slot-rank-item .remove-item:hover { color: #c0392b; }
      @media (max-width: 540px) {
        .slot-ranker { flex-direction: column; }
      }
    "
    )),
    tags$script(HTML(
      "
      $(document).on('change', 'input[name=\"party_preference\"]', function() {
        Shiny.setInputValue('party_preference', $(this).val());
      });
      $(document).on('keypress', '#address', function(e) {
        if (e.which == 13) {
          e.preventDefault();
          $('#geocode_btn').click();
        }
      });
      $(document).on('change', 'input.party-checkbox-input', function() {
        var group = $(this).data('group');
        var val = $(this).val();
        var exclusive = ['did_not_vote', 'dont_remember'];
        var partyCoalition = {
          'morena': 'A', 'pvem': 'A', 'pt': 'A',
          'pan': 'B', 'pri': 'B', 'prd': 'B',
          'mc': 'C'
        };
        if ($(this).is(':checked')) {
          if (exclusive.indexOf(val) !== -1) {
            // Exclusive option: uncheck everything else in the group
            $('input.party-checkbox-input[data-group=\"' + group + '\"]').not(this).prop('checked', false);
          } else {
            // Party or 'other' selected: uncheck exclusive options
            exclusive.forEach(function(ex) {
              $('input.party-checkbox-input[data-group=\"' + group + '\"][value=\"' + ex + '\"]').prop('checked', false);
            });
            // Uncheck any currently checked parties from a different coalition
            // ('other' is always allowed alongside any coalition, so skip it)
            var newCoalition = partyCoalition[val];
            if (newCoalition !== undefined) {
              $('input.party-checkbox-input[data-group=\"' + group + '\"]:checked').not(this).each(function() {
                var checkedVal = $(this).val();
                if (checkedVal !== 'other' && partyCoalition[checkedVal] !== newCoalition) {
                  $(this).prop('checked', false);
                }
              });
            }
          }
        }
        var checked = [];
        $('input.party-checkbox-input[data-group=\"' + group + '\"]:checked').each(function() {
          checked.push($(this).val());
        });
        Shiny.setInputValue(group, checked.length > 0 ? checked : null);
      });
      $(document).ready(function() {
        Shiny.setInputValue('is_mobile', window.innerWidth <= 768);
      });
      /* Track user interaction with sliders that require explicit interaction */
      var sliderTouched = {};
      var page9ListenActive = false;
      var page9SliderIds = ['home_crime_handling_pre', 'morena_crime_rating',
                            'coalition_pan_pri_prd_crime_rating', 'mc_crime_rating'];

      Shiny.addCustomMessageHandler('initPage9Sliders', function(msg) {
        page9ListenActive = false;
        setTimeout(function() {
          $('#page9 .shiny-input-container:has(.js-range-slider)').each(function() {
            var id = $(this).find('.js-range-slider').attr('id');
            if (id && !sliderTouched[id]) $(this).addClass('slider-untouched');
          });
          page9ListenActive = true;
        }, 300);
      });

      $(document).on('shiny:inputchanged', function(e) {
        if (page9ListenActive && page9SliderIds.indexOf(e.name) !== -1) {
          sliderTouched[e.name] = true;
          $('#' + e.name).closest('.shiny-input-container').removeClass('slider-untouched');
          Shiny.setInputValue('slider_touched_' + e.name, true);
        }
      });

      function setMapMode(btn, mode) {
        $(btn).closest('.btn-group').find('.btn').removeClass('active');
        $(btn).addClass('active');
        Shiny.setInputValue('map_view_mode', mode, {priority: 'event'});
      }

      /* Slot ranker: one-item-per-slot drag-and-drop ranking widget */
      function initSlotRanker(sourceId, containerId, inputId, n) {
        var source = document.getElementById(sourceId);
        if (!source) return;
        Sortable.create(source, {
          group: { name: inputId, pull: true, put: true },
          animation: 150,
          sort: false
        });
        for (var i = 1; i <= n; i++) {
          (function(slotNum) {
            var slot = document.getElementById(containerId + '_slot_' + slotNum);
            if (!slot) return;
            Sortable.create(slot, {
              group: { name: inputId, pull: true, put: true },
              animation: 150,
              sort: false,
              onAdd: function() {
                while (slot.children.length > 1) {
                  source.appendChild(slot.children[0]);
                }
                reportSlotRanking(containerId, inputId, n);
              },
              onRemove: function() {
                reportSlotRanking(containerId, inputId, n);
              }
            });
          })(i);
        }
      }

      function removeFromSlot(btn) {
        var item = btn.parentElement;
        var sourceId    = item.getAttribute('data-source-id');
        var containerId = item.getAttribute('data-container-id');
        var inputId     = item.getAttribute('data-input-id');
        var n           = parseInt(item.getAttribute('data-n'));
        document.getElementById(sourceId).appendChild(item);
        reportSlotRanking(containerId, inputId, n);
      }

      function reportSlotRanking(containerId, inputId, n) {
        var ranking = [];
        for (var i = 1; i <= n; i++) {
          var slot = document.getElementById(containerId + '_slot_' + i);
          if (slot && slot.children.length > 0) {
            ranking.push(slot.children[0].getAttribute('data-value'));
          }
        }
        Shiny.setInputValue(inputId, ranking, {priority: 'event'});
      }
    "
    ))
  ),
  fluidRow(
    column(
      8,
      offset = 2,
      wellPanel(
        # Page 0: Information Sheet
        hidden(
          div(
            id = "page0",
            #p(em("Principal Investigator: Adam Roberts")),
            # p(
            #   "This form describes a research study that is being conducted by Adam Roberts from the ",
            #   "University of Rochester's Department of Political Science. The purpose of this study is to ",
            #   "understand how voters use information when deciding which candidate or political party to vote for."
            # ),
            p(strong(
              "Por favor, lea esta hoja informativa antes de continuar."
            )),
            tags$iframe(
              src = "Information_Sheet_Informational_Comparisons_Spanish.pdf",
              width = "100%",
              height = "500px",
              style = "border: 1px solid #ccc; border-radius: 4px;"
            ),
            p(
              style = "margin-top: 0.5em; font-size: 0.9em;",
              "Si no puede ver el documento, ",
              tags$a(
                href = "https://adamdnroberts.github.io/assets/pdf/Information_Sheet_Informational_Comparisons_Spanish.pdf",
                target = "_blank",
                "haga clic aqu\u00ed para abrirlo."
              )
            ),
            hr(),
            fluidRow(
              column(
                12,
                align = "right",
                div(
                  class = "mobile-only",
                  p(strong(
                    "Para una mejor experiencia, le recomendamos completar esta encuesta en modo horizontal."
                  ))
                ),
                actionButton(
                  "page0_next",
                  "Entiendo, Continuar \u2192",
                  class = "btn-primary btn-lg"
                )
              )
            )
          )
        ),

        # Page 1: Find Municipality
        hidden(
          div(
            id = "page1",

            p(strong(
              "Ingrese su dirección para encontrar su municipio de origen:"
            )),
            fluidRow(
              column(
                9,
                textInput(
                  "address",
                  NULL,
                  placeholder = "p. ej., Av. Yucatán 147, Roma Nte., Cuauhtémoc, CDMX"
                )
              ),
              column(
                3,
                actionButton(
                  "geocode_btn",
                  "Buscar",
                  class = "btn-info",
                  style = "margin-top: 0px; width: 100%;"
                )
              )
            ),
            hidden(
              div(
                id = "loading_msg",
                style = paste0(
                  "color: #0066cc; margin-top: 10px; padding: 10px; ",
                  "background-color: #e7f3ff; border-radius: 4px;"
                ),
                icon("spinner", class = "fa-spin"),
                tags$strong(" Buscando su dirección..."),
                tags$br(),
                tags$small("Esto puede tomar algunos segundos.")
              )
            ),
            uiOutput("geocode_result"),

            # Confirmation section (shown after municipality is found)
            hidden(
              div(
                id = "home_confirmation_section",
                hr(),
                p(
                  "Por favor, confirme que la siguiente información es correcta antes de continuar:"
                ),
                uiOutput("home_municipality_summary"),
                actionButton(
                  "clear_selection_btn",
                  "Limpiar selección",
                  icon = icon("refresh"),
                  class = "btn-secondary"
                )
              )
            ),

            # Dropdown fallback (shown after any geocode attempt)
            hidden(
              div(
                id = "dropdown_fallback_section",
                hr(),
                selectInput(
                  "dropdown_municipality",
                  "¿No encontró su municipio? También puede seleccionarlo de la lista:",
                  choices = c(
                    "-- Seleccione un municipio --" = "",
                    muni_choices
                  ),
                  selected = "",
                  selectize = TRUE,
                  width = "100%"
                )
              )
            ),

            hr(),
            fluidRow(
              column(
                12,
                align = "right",
                actionButton(
                  "page1_next",
                  "Siguiente \u2192",
                  class = "btn-primary btn-lg"
                )
              )
            )
          )
        ),

        # Page 4: Benchmark Municipality Selection
        hidden(
          div(
            id = "page4",
            uiOutput("benchmark_instructions_text"),
            hr(),
            uiOutput("benchmark_list"),
            hr(),
            uiOutput("additional_munis_instructions_text"),
            selectizeInput(
              "additional_munis_dropdown",
              label = NULL,
              choices = NULL,
              multiple = TRUE,
              options = list(
                placeholder = "Escriba para buscar municipios..."
              )
            ),
            hr(),
            uiOutput("page4_warning"),
            fluidRow(
              column(
                12,
                align = "right",
                actionButton(
                  "page4_next",
                  "Siguiente \u2192",
                  class = "btn-primary btn-lg"
                )
              )
            )
          )
        ),

        # Page 2: Screen-out (home municipality not governed by a main party)
        hidden(
          div(
            id = "page2",
            br(),
            br(),
            br(),
            div(
              style = "text-align: center; padding: 50px;",
              p(
                icon("info-circle"),
                tags$strong(" Gracias por su interés."),
                style = "font-size: 1.2em;"
              ),
              br(),
              p(
                "Lamentablemente, no cumple con los criterios para este estudio, por lo que no es elegible para participar."
              ),
              br(),
              p(style = "color: #6c757d;", "Ahora puede cerrar esta ventana.")
            )
          )
        ),

        # Page 3: Attention Check
        hidden(
          div(
            id = "page3",
            p(tags$strong("Por favor, ordene esta lista de fuentes de noticias en la siguiente manera:")),
            tags$ol(
              tags$li("Radio"),
              tags$li("Per\u00eddicos impresos"),
              tags$li("Redes sociales")
            ),
            tags$hr(),
            slot_ranker_ui(
              "practice_ranking",
              items = c(
                "Redes sociales",
                "Radio",
                "Periódicos impresos"
              ),
              source_label = "Elementos disponibles:",
              slots_label = "Lista ordenada:"
            ),
            tags$hr(),
            uiOutput("practice_warning"),
            fluidRow(
              column(
                12,
                align = "right",
                actionButton(
                  "page3_next",
                  "Siguiente \u2192",
                  class = "btn-primary btn-lg"
                )
              )
            )
          )
        ),

        # Page 6: Issue Importance Ranking
        hidden(
          div(
            id = "page6",

            # Importance of Issues, language loosely based on Mitofsky
            # public opinion survey (see Google Drive reference)
            h5(strong(
              "\u00bfQu\u00e9 tan importantes son los siguientes temas para usted? Ord\u00e9nelos de m\u00e1s a menos importante."
            )),
            div(
              id = "issue_importance_wrapper",
              uiOutput("issue_importance_ui")
            ),

            hr(),

            tags$div(
              class = "form-group",
              tags$label(
                "Si tuviera que votar, \u00bfpor cu\u00e1l partido o partidos votar\u00eda en las elecciones municipales de 2027?"
              ),
              tags$div(
                class = "party-radio-group",
                party_checkbox_choice(
                  "pan",
                  "PAN (Partido Acci\u00f3n Nacional)",
                  "PAN_logo.png",
                  "vote_intention_pre"
                ),
                party_checkbox_choice(
                  "pri",
                  "PRI (Partido Revolucionario Institucional)",
                  "PRI_logo.png",
                  "vote_intention_pre"
                ),
                party_checkbox_choice(
                  "prd",
                  "PRD (Partido de la Revoluci\u00f3n Democr\u00e1tica)",
                  "PRD_logo.png",
                  "vote_intention_pre"
                ),
                party_checkbox_choice(
                  "pvem",
                  "PVEM (Partido Verde Ecologista de M\u00e9xico)",
                  "PVEM_logo.png",
                  "vote_intention_pre"
                ),
                party_checkbox_choice(
                  "pt",
                  "PT (Partido del Trabajo)",
                  "PT_logo.png",
                  "vote_intention_pre"
                ),
                party_checkbox_choice(
                  "mc",
                  "MC (Movimiento Ciudadano)",
                  "Movimiento_Ciudadano_logo.png",
                  "vote_intention_pre"
                ),
                party_checkbox_choice(
                  "morena",
                  "MORENA",
                  "Morena_logo.png",
                  "vote_intention_pre"
                ),
                party_checkbox_choice(
                  "other",
                  "Otro",
                  group = "vote_intention_pre"
                )
              ),
              tags$p(
                style = "color: #6c757d; font-size: 0.85em; margin-top: 8px;",
                "Puede seleccionar varios partidos de la misma coalici\u00f3n. Si selecciona un partido de una coalici\u00f3n diferente, se eliminar\u00e1n las selecciones anteriores."
              )
            ),
            hidden(
              textInput(
                "vote_intention_pre_other",
                "Por favor, especifique el partido:",
                placeholder = "Ingrese el nombre del partido..."
              )
            ),

            hr(),
            uiOutput("issue_importance_warning"),
            fluidRow(
              column(
                12,
                align = "right",
                actionButton(
                  "page6_next",
                  "Siguiente \u2192",
                  class = "btn-primary btn-lg"
                )
              )
            )
          )
        ),

        # Page 9: Your Municipality's Performance
        hidden(
          div(
            id = "page9",

            p(
              "En 2025,",
              strong(
                "la mitad de todos los municipios en M\u00e9xico tuvo menos de 79 robos por cada 100,000 personas, y la otra mitad tuvo m\u00e1s."
              )
            ),
            uiOutput("robbery_estimate_ui"),

            hr(),

            p(strong(
              "Las siguientes preguntas le pedir\u00e1n que eval\u00fae c\u00f3mo ciertos municipios y partidos \u201cmanejan\u201d la delincuencia no violenta, como los robos. \u201cManejar la delincuencia\u201d aqu\u00ed se refiere a los esfuerzos del gobierno para prevenir la delincuencia, hacer cumplir la ley y garantizar la seguridad p\u00fablica."
            )),

            uiOutput("home_crime_handling_pre_ui"),

            p(strong(
              "En promedio, \u00bfqu\u00e9 tan bien cree que los gobiernos municipales de los siguientes partidos y coaliciones manejan la delincuencia?"
            )),
            fluidRow(
              column(
                6,
                p(
                  "0 = Maneja la delincuencia extremadamente mal",
                  style = "color: #6c757d; font-size: 0.85em;"
                )
              ),
              column(
                6,
                p(
                  "100 = Maneja la delincuencia extremadamente bien",
                  style = "color: #6c757d; font-size: 0.85em; text-align: right;"
                )
              )
            ),
            sliderInput(
              "morena_crime_rating",
              "Sigamos Haciendo Historia (MORENA/PT/PVEM)",
              min = 0,
              max = 100,
              value = 50
            ),
            sliderInput(
              "coalition_pan_pri_prd_crime_rating",
              "Fuerza y Coraz\u00f3n por M\u00e9xico (PAN/PRI/PRD)",
              min = 0,
              max = 100,
              value = 50
            ),
            sliderInput(
              "mc_crime_rating",
              "MC (Movimiento Ciudadano)",
              min = 0,
              max = 100,
              value = 50
            ),

            hr(),
            uiOutput("page9_warning"),
            fluidRow(
              column(
                12,
                align = "right",
                actionButton(
                  "page9_submit",
                  "Enviar encuesta",
                  class = "btn-success btn-lg",
                  icon = icon("check")
                )
              )
            )
          )
        ),

        # Page 2: Select Reference Municipalities (commented out)
        if (FALSE) {
          hidden(
            div(
              id = "page2",

              uiOutput("reference_instructions_text"),
              p(em(
                "(You can select multiple municipalities by clicking on them. Selected municipalities will be highlighted in ",
                tags$span(
                  style = "color: #9B59B6; font-weight: bold;",
                  "purple"
                ),
                ". Click again to deselect. Your home municipality is outlined in green.)"
              )),
              p(
                class = "mobile-only",
                style = "color: #856404; background-color: #fff3cd; border: 1px solid #ffc107; border-radius: 4px; padding: 8px 12px;",
                icon("rotate"),
                " ",
                "For the best experience, we strongly recommend rotating your phone to landscape mode for this question."
              ),

              fluidRow(
                column(
                  6,
                  selectizeInput(
                    "muni_search",
                    "Search for a municipality by name:",
                    choices = NULL,
                    options = list(
                      placeholder = "Type to search municipalities..."
                    )
                  )
                ),
                column(
                  3,
                  style = "margin-top: 25px;",
                  actionButton(
                    "zoom_search",
                    "Zoom",
                    icon = icon("crosshairs"),
                    class = "btn-info",
                    style = "width: 100%;"
                  )
                ),
                column(
                  3,
                  style = "margin-top: 25px;",
                  actionButton(
                    "zoom_home",
                    "Zoom to Home",
                    icon = icon("home"),
                    class = "btn-primary",
                    style = "width: 100%;"
                  )
                )
              ),

              p(
                style = "color: #856404; background-color: #fff3cd; border: 1px solid #ffc107; border-radius: 4px; padding: 8px 12px; font-size: 0.9em;",
                icon("info-circle"),
                " ",
                tags$strong("Note for researchers:"),
                " Respondents will only see one map view. The toggle below is for preview purposes only."
              ),

              div(
                style = "margin-bottom: 8px;",
                tags$label(style = "margin-right: 10px;", "Map view:"),
                div(
                  class = "btn-group",
                  role = "group",
                  tags$button(
                    class = "btn btn-sm btn-default",
                    onclick = "setMapMode(this, 'borders')",
                    "Borders only"
                  ),
                  tags$button(
                    class = "btn btn-sm btn-default active",
                    onclick = "setMapMode(this, 'parties')",
                    "Parties"
                  ),
                  tags$button(
                    class = "btn btn-sm btn-default",
                    onclick = "setMapMode(this, 'population')",
                    "Population"
                  )
                )
              ),

              div(
                class = "map-fullwidth",
                leafletOutput("map_page2", height = 450),
                p(em(
                  style = "color: #6c757d; font-size: 0.9em;",
                  "Note: May take a few seconds for map to load."
                ))
              ),

              uiOutput("selected_munis_display"),

              hr(),
              fluidRow(
                column(
                  12,
                  align = "right",
                  actionButton(
                    "goto_page3_from_2",
                    "Siguiente \u2192",
                    class = "btn-primary btn-lg"
                  )
                )
              )
            )
          )
        }, # end if (FALSE)

        # Page 12: Thank you screen
        hidden(
          div(
            id = "page12",
            br(),
            br(),
            br(),
            div(
              style = "text-align: center; padding: 50px;",
              p(
                icon("check-circle"),
                tags$strong(" ¡Gracias! Su respuesta ha sido enviada."),
                style = "font-size: 1.2em; color: #28a745;"
              ),
              br(),
              p(style = "color: #6c757d;", "Ahora puede cerrar esta ventana.")
            )
          )
        ),
      )
    )
  )
)

# ── Marginal quota targets ─────────────────────────────────────────────────
# Sources:
#   Sex:    INEGI, Censo de Población y Vivienda 2020, population 18+
#           Male 48.3%, Female 51.7%
#   Age:    INEGI, Censo de Población y Vivienda 2020, population 18+
#           (Tabulados del Cuestionario Ampliado, edad quinquenal)
#   SEL:    AMAI, Regla AMAI NSE 8x7, actualización 2018
#           AB=7.6%, C+=13.9%, C=16.7%, C-=17.3%, D+=20.5%, D=17.5%, E=6.5%
#   Region: INEGI, Censo de Población y Vivienda 2020, state totals
#           Proportional within eligible states (excl. CDMX, Durango, Oaxaca,
#           Veracruz).
# Total N = 2,180. Marginal (per-variable) quotas only.

QUOTA_N <- 2180L

# Sex (INEGI 2020 18+ pop., eligible states)
QUOTA_SEX <- c("1" = 1067L, "2" = 1113L)

# Age bracket helper — maps raw Netquest age value to bracket label
age_bracket <- function(age_val) {
  age <- suppressWarnings(as.integer(age_val))
  if (is.na(age) || length(age) == 0) {
    return(NA_character_)
  }
  if (age >= 18 && age <= 24) {
    "18-24"
  } else if (age >= 25 && age <= 34) {
    "25-34"
  } else if (age >= 35 && age <= 44) {
    "35-44"
  } else if (age >= 45 && age <= 54) {
    "45-54"
  } else if (age >= 55 && age <= 64) {
    "55-64"
  } else if (age >= 65) {
    "65+"
  } else {
    NA_character_
  }
}

# Age targets (INEGI 2020 18+ distribution, eligible states only)
# Denominator = sum of 18+ age groups = 70,678,795 (excludes under-18)
QUOTA_AGE <- c(
  "18-24" = 377L,
  "25-34" = 491L,
  "35-44" = 441L,
  "45-54" = 371L,
  "55-64" = 255L,
  "65+" = 245L
)

# SEL targets (from Netquest; codes: 1=AB, 2=C+, 3=C, 4=C-, 5=D+/D/E merged)
# D+ (5), D (6), and E (7) are merged: codes 6 and 7 are remapped to 5 at session start
QUOTA_SEL <- c(
  "1" = 189L,
  "2" = 346L,
  "3" = 415L,
  "4" = 430L,
  "5" = 800L
)

# Region targets: loaded from pre-computed RDS (run code/census_quotas.R to generate)
# Source: INEGI Censo de Población y Vivienda 2020, state totals,
#         proportional within eligible states (excl. CDMX, Durango, Oaxaca, Veracruz)
QUOTA_REGION <- readRDS("data/region_quotas_wave1.rds")

# ── S3 quota counter helpers ────────────────────────────────────────────────
QUOTA_S3_KEY <- "quota_counts/wave1_quota_counts.json"

.quota_empty_counts <- function() {
  list(
    sex = as.list(setNames(rep(0L, length(QUOTA_SEX)), names(QUOTA_SEX))),
    age = as.list(setNames(rep(0L, length(QUOTA_AGE)), names(QUOTA_AGE))),
    sel = as.list(setNames(rep(0L, length(QUOTA_SEL)), names(QUOTA_SEL))),
    region = as.list(setNames(
      rep(0L, length(QUOTA_REGION)),
      names(QUOTA_REGION)
    ))
  )
}

read_quota_counts <- function(bucket) {
  tryCatch(
    {
      client <- paws.storage::s3()
      res <- client$get_object(Bucket = bucket, Key = QUOTA_S3_KEY)
      counts <- jsonlite::fromJSON(rawToChar(res$Body), simplifyVector = FALSE)
      # Ensure each dimension is a named list with integer values
      for (dim in c("sex", "age", "sel", "region")) {
        counts[[dim]] <- lapply(counts[[dim]], as.integer)
      }
      # Migrate any pre-merge SEL D (6) and E (7) counts into the combined D+/D/E cell (5)
      if (!is.null(counts$sel[["6"]])) {
        counts$sel[["5"]] <- (if (is.null(counts$sel[["5"]])) 0L else counts$sel[["5"]]) + counts$sel[["6"]]
        counts$sel[["6"]] <- NULL
      }
      if (!is.null(counts$sel[["7"]])) {
        counts$sel[["5"]] <- (if (is.null(counts$sel[["5"]])) 0L else counts$sel[["5"]]) + counts$sel[["7"]]
        counts$sel[["7"]] <- NULL
      }
      counts
    },
    error = function(e) .quota_empty_counts()
  )
}

write_quota_counts <- function(bucket, counts) {
  tryCatch(
    {
      client <- paws.storage::s3()
      client$put_object(
        Bucket = bucket,
        Key = QUOTA_S3_KEY,
        Body = charToRaw(jsonlite::toJSON(counts, auto_unbox = TRUE))
      )
    },
    error = function(e) warning("Quota count write failed: ", e$message)
  )
}

# ── Screenout recorder ──────────────────────────────────────────────────────
# Saves a minimal CSV row to S3 (wave1_screenouts/) or locally for each
# participant who does not reach the survey proper.
save_screenout <- function(
  respondent_id,
  reason,
  muni_id,
  netquest_pid,
  nq_age,
  nq_sex,
  nq_region,
  nq_sel,
  s3_bucket
) {
  df <- data.frame(
    Respondent_ID = respondent_id,
    Timestamp = as.character(Sys.time()),
    Screenout_Reason = reason,
    Found_Municipality_ID = if (is.null(muni_id)) NA_character_ else muni_id,
    Netquest_PID = if (is.null(netquest_pid)) NA_character_ else netquest_pid,
    NQ_Age = if (is.null(nq_age)) NA_character_ else nq_age,
    NQ_Sex = if (is.null(nq_sex)) NA_character_ else nq_sex,
    NQ_Region = if (is.null(nq_region)) NA_character_ else nq_region,
    NQ_SEL = if (is.null(nq_sel)) NA_character_ else nq_sel,
    stringsAsFactors = FALSE
  )
  tc <- textConnection("csv_out", "w", local = TRUE)
  write.csv(df, tc, row.names = FALSE)
  close(tc)
  csv_content <- paste(csv_out, collapse = "\n")
  if (nchar(s3_bucket) > 0) {
    tryCatch(
      {
        s3_client <- paws.storage::s3()
        s3_client$put_object(
          Bucket = s3_bucket,
          Key = paste0("wave1_screenouts/", respondent_id, ".csv"),
          Body = charToRaw(csv_content)
        )
      },
      error = function(e) warning("Screenout S3 upload failed: ", e$message)
    )
  } else {
    tryCatch(
      write(csv_content, file = "data/screenout_responses.csv", append = TRUE),
      error = function(e) warning("Screenout local write failed: ", e$message)
    )
  }
}

server <- function(input, output, session) {
  # Reactive values
  selected_map_munis <- reactiveVal(character())
  found_municipality <- reactiveVal(NULL)
  found_address_coords <- reactiveVal(NULL)
  geocode_attempted <- reactiveVal(FALSE)
  current_page <- reactiveVal(0)
  page_enter_time <- reactiveVal(Sys.time())
  page_durations <- reactiveVal(list())
  prev_page_rv <- reactiveVal(0)

  observeEvent(
    current_page(),
    {
      now <- Sys.time()
      prev_pg <- as.character(prev_page_rv())
      durs <- page_durations()
      elapsed <- as.numeric(difftime(now, page_enter_time(), units = "secs"))
      durs[[prev_pg]] <- (if (is.null(durs[[prev_pg]])) {
        0
      } else {
        durs[[prev_pg]]
      }) +
        elapsed
      page_durations(durs)
      page_enter_time(now)
      prev_page_rv(current_page())
    },
    ignoreInit = TRUE
  )
  benchmark_candidates <- reactiveVal(NULL) # 15 candidate munis shown on page 4
  nearest3_rv <- reactiveVal(NULL) # 3 geographically nearest municipalities (Wave 1 governance prior)
  selected_benchmarks <- reactiveVal(character()) # IDs selected by respondent

  # Populate benchmark candidates when home municipality is confirmed
  observeEvent(found_municipality(), {
    req(found_municipality())
    home_id <- found_municipality()
    all_ids <- d_geo %>% st_drop_geometry() %>% pull(muni_id)

    # Pool 1: 5 from nearest 10
    nn_ids <- nearest10 %>%
      filter(muni_id == home_id) %>%
      pull(neighbor_id)
    pool1 <- if (length(nn_ids) >= 5) sample(nn_ids, 5) else nn_ids

    # Pool 2: 5 from top 20 largest (deduplicate against pool1 and home)
    top20_cands <- setdiff(top20_munis, c(home_id, pool1))
    pool2 <- if (length(top20_cands) >= 5) {
      sample(top20_cands, 5)
    } else {
      top20_cands
    }

    # Pool 3: 5 random from remainder (excluding home, pool1, pool2)
    remaining <- setdiff(all_ids, c(home_id, pool1, pool2))
    pool3 <- if (length(remaining) >= 5) sample(remaining, 5) else remaining

    randomized_ids <- sample(c(pool1, pool2, pool3))
    candidates <- d_geo %>%
      st_drop_geometry() %>%
      filter(muni_id %in% randomized_ids) %>%
      select(muni_id, NOMGEO, NOM_ENT) %>%
      arrange(match(muni_id, randomized_ids))

    benchmark_candidates(candidates)
    selected_benchmarks(character())

    # Nearest 3 by geographic distance for Wave 1 governance prior question
    nn3 <- nearest10 %>%
      filter(muni_id == home_id, rank <= 3) %>%
      arrange(rank)
    nearest3_rv(nn3)
  })

  # Generate unique respondent ID at session start
  respondent_id <- paste0(
    format(Sys.time(), "%Y%m%d_%H%M%S"),
    "_",
    paste(sample(c(letters, 0:9), 8, replace = TRUE), collapse = "")
  )

  # Capture Netquest URL parameters at session start.
  # Netquest appends these automatically when sending panelists to the survey URL.
  # Confirm exact parameter names with Netquest before deployment.
  nq_params <- parseQueryString(isolate(session$clientData$url_search))
  netquest_pid <- nq_params[["pid"]] # 83-character panelist identifier (required for redirects)
  nq_age <- nq_params[["age"]] # age from panel profile
  nq_sex <- nq_params[["sex"]] # sex from panel profile (confirm coding with Netquest)
  nq_region <- nq_params[["region"]] # state/region code from panel profile
  nq_sel <- nq_params[["sel"]] # socio-economic level code from panel profile
  if (!is.null(nq_sel) && nq_sel %in% c("6", "7")) nq_sel <- "5" # D+/D/E merged

  # Quota full: set SURVEY_QUOTA_FULL=true in .Renviron to redirect all new visitors
  observe({
    if (Sys.getenv("SURVEY_QUOTA_FULL") == "true") {
      ticket <- if (!is.null(netquest_pid) && nchar(netquest_pid) > 0) {
        netquest_pid
      } else {
        ""
      }
      save_screenout(respondent_id, "survey_quota_full", NULL,
                     netquest_pid, nq_age, nq_sex, nq_region, nq_sel,
                     Sys.getenv("S3_BUCKET"))
      shinyjs::runjs(sprintf(
        'window.location.href = "https://transit.nicequest.com/transit/participation?tp=qf_1&c=ok&ticket=%s"',
        ticket
      ))
    }
  })

  # ── Per-cell quota check at session start ──────────────────────────────────
  # Reads S3 counts once per session; redirects to quota-full if any marginal
  # cell for this respondent is at or above its target + QUOTA_BUFFER.
  # QUOTA_BUFFER = 5 absorbs concurrent sessions that haven't yet incremented.
  QUOTA_BUFFER <- 5L

  observe({
    s3_bucket <- Sys.getenv("S3_BUCKET")
    if (nchar(s3_bucket) == 0) {
      return()
    }
    pid <- if (!is.null(netquest_pid) && nchar(netquest_pid) > 0) {
      netquest_pid
    } else {
      ""
    }

    counts <- read_quota_counts(s3_bucket)

    sex_cell <- if (!is.null(nq_sex) && nq_sex %in% names(QUOTA_SEX)) {
      nq_sex
    } else {
      NULL
    }
    age_cell <- if (!is.null(nq_age)) age_bracket(nq_age) else NULL
    sel_cell <- if (!is.null(nq_sel) && nq_sel %in% names(QUOTA_SEL)) {
      nq_sel
    } else {
      NULL
    }
    region_cell <- if (
      !is.null(nq_region) && nq_region %in% names(QUOTA_REGION)
    ) {
      nq_region
    } else {
      NULL
    }

    # Safe count lookup: returns 0 if key is missing or NULL
    get_n <- function(vec, key) {
      v <- vec[[key]]
      if (is.null(v) || is.na(v)) 0L else as.integer(v)
    }

    over_quota <-
      (!is.null(sex_cell) &&
        !is.na(sex_cell) &&
        get_n(counts$sex, sex_cell) >= QUOTA_SEX[sex_cell] + QUOTA_BUFFER) ||
      (!is.null(age_cell) &&
        !is.na(age_cell) &&
        get_n(counts$age, age_cell) >= QUOTA_AGE[age_cell] + QUOTA_BUFFER) ||
      (!is.null(sel_cell) &&
        !is.na(sel_cell) &&
        get_n(counts$sel, sel_cell) >= QUOTA_SEL[sel_cell] + QUOTA_BUFFER) ||
      (!is.null(region_cell) &&
        !is.na(region_cell) &&
        get_n(counts$region, region_cell) >=
          QUOTA_REGION[region_cell] + QUOTA_BUFFER)

    if (isTRUE(over_quota)) {
      save_screenout(respondent_id, "over_quota", NULL,
                     netquest_pid, nq_age, nq_sex, nq_region, nq_sel,
                     Sys.getenv("S3_BUCKET"))
      shinyjs::runjs(sprintf(
        'window.location.href = "https://transit.nicequest.com/transit/participation?tp=qf_1&c=ok&ticket=%s"',
        pid
      ))
    }
  })

  # Define the file path for saving responses (local fallback)
  responses_file <- "data/survey_responses.csv"

  # Populate municipality search choices for page 8 map
  updateSelectizeInput(
    session,
    "muni_search",
    choices = c(" " = "", setNames(d_geo$muni_id, d_geo$mun_state)),
    server = TRUE
  )

  # Populate all-Mexico dropdown for page 5
  updateSelectizeInput(
    session,
    "additional_munis_dropdown",
    choices = muni_choices,
    server = TRUE
  )

  # Geocode address and find municipality
  observeEvent(input$geocode_btn, {
    req(input$address)

    geocode_attempted(TRUE)
    disable("geocode_btn")
    updateActionButton(session, "geocode_btn", label = "Buscando...")
    shinyjs::show("loading_msg")
    output$geocode_result <- renderUI({})

    tryCatch(
      {
        # Method 1: Nominatim with Mexico bias
        url1 <- paste0(
          "https://nominatim.openstreetmap.org/search?",
          "q=",
          URLencode(paste(input$address, "Mexico")),
          "&format=json&limit=3&countrycodes=mx"
        )

        response1 <- GET(url1, user_agent("ShinyApp/1.0 (Survey Research)"))
        Sys.sleep(1)
        result1 <- fromJSON(content(response1, "text", encoding = "UTF-8"))

        # Method 2: Photon geocoder
        url2 <- paste0(
          "https://photon.komoot.io/api/?",
          "q=",
          URLencode(input$address),
          "&limit=3"
        )

        response2 <- GET(url2)
        result2 <- fromJSON(content(response2, "text", encoding = "UTF-8"))

        lat <- lon <- NULL

        if (length(result1) > 0 && nrow(result1) > 0) {
          lat <- as.numeric(result1$lat[1])
          lon <- as.numeric(result1$lon[1])
        } else if (
          !is.null(result2$features) && length(result2$features$geometry) > 0
        ) {
          coords <- result2$features$geometry$coordinates[[1]]
          lon <- coords[1]
          lat <- coords[2]
        }

        if (!is.null(lat) && !is.null(lon)) {
          point <- st_sfc(st_point(c(lon, lat)), crs = 4326)

          if (!is.na(st_crs(d_geo))) {
            point <- st_transform(point, st_crs(d_geo))
          }

          intersection <- st_intersects(point, d_geo, sparse = FALSE)

          if (any(intersection)) {
            muni_id <- d_geo$muni_id[which(intersection)[1]]
            muni_name <- d_geo$NOMGEO[which(intersection)[1]]
            muni_state <- d_geo$NOM_ENT[which(intersection)[1]]

            found_municipality(muni_id)
            found_address_coords(list(lon = lon, lat = lat))

            shinyjs::hide("loading_msg")
            enable("geocode_btn")
            updateActionButton(
              session,
              "geocode_btn",
              label = "Buscar",
              icon = NULL
            )

            output$geocode_result <- renderUI({
              tags$div(
                style = paste0(
                  "color: #0072B2; margin-top: 10px; padding: 10px; ",
                  "background-color: #d4e9f7; border-radius: 4px;"
                ),
                icon("check-circle"),
                tags$strong(paste0(
                  "Encontrado: ",
                  muni_name,
                  ", ",
                  muni_state
                )),
                tags$br()
              )
            })
          } else {
            shinyjs::hide("loading_msg")
            enable("geocode_btn")
            updateActionButton(
              session,
              "geocode_btn",
              label = "Buscar",
              icon = NULL
            )

            output$geocode_result <- renderUI({
              tags$div(
                style = paste0(
                  "color: #856404; margin-top: 10px; padding: 10px; ",
                  "background-color: #fff3cd; border-radius: 4px;"
                ),
                icon("exclamation-triangle"),
                " Dirección encontrada, pero no está dentro de nuestra base de datos de municipios."
              )
            })
          }
        } else {
          shinyjs::hide("loading_msg")
          enable("geocode_btn")
          updateActionButton(
            session,
            "geocode_btn",
            label = "Buscar",
            icon = NULL
          )

          output$geocode_result <- renderUI({
            tags$div(
              style = "color: #721c24; margin-top: 10px; padding: 10px; background-color: #f8d7da; border-radius: 4px;",
              icon("times-circle"),
              " No se pudo encontrar esa dirección.",
              tags$br(),
              tags$small(
                "Por favor, intente simplificar su dirección o verificar la ortografía."
              )
            )
          })
        }
      },
      error = function(e) {
        shinyjs::hide("loading_msg")
        enable("geocode_btn")
        updateActionButton(
          session,
          "geocode_btn",
          label = "Buscar",
          icon = NULL
        )

        output$geocode_result <- renderUI({
          tags$div(
            style = "color: #721c24; margin-top: 10px; padding: 10px; background-color: #f8d7da; border-radius: 4px;",
            icon("times-circle"),
            " Error al buscar la dirección."
          )
        })
      }
    )
  })

  # Reference municipalities dropdown → sets selected_map_munis (commented out with page 2)
  # observeEvent(input$reference_munis_dropdown, {
  #   selected_map_munis(input$reference_munis_dropdown)
  # }, ignoreNULL = FALSE)

  # Show appropriate page and scroll to top
  observe({
    pages <- c(paste0("page", 0:12))
    lapply(pages, hide)
    shinyjs::show(paste0("page", current_page()))
    runjs("window.scrollTo(0, 0);")
  })

  # Show/hide confirmation and dropdown sections based on geocode state
  observe({
    if (!is.null(found_municipality())) {
      shinyjs::show("home_confirmation_section")
      shinyjs::show("dropdown_fallback_section")
      updateSelectInput(
        session,
        "dropdown_municipality",
        selected = found_municipality()
      )
    } else if (geocode_attempted()) {
      shinyjs::hide("home_confirmation_section")
      shinyjs::show("dropdown_fallback_section")
    } else {
      shinyjs::hide("home_confirmation_section")
      shinyjs::hide("dropdown_fallback_section")
    }
  })

  observeEvent(
    input$vote_intention_pre,
    {
      toggleElement(
        "vote_intention_pre_other",
        condition = "other" %in% input$vote_intention_pre
      )
    },
    ignoreNULL = FALSE
  )

  # Enable/disable Next button on page 1 based on home municipality found
  observe({
    has_home <- !is.null(found_municipality())

    if (has_home) {
      shinyjs::delay(1000, enable("page1_next"))
    } else {
      disable("page1_next")
    }
  })

  # Page navigation
  # Page 0 → Page 1 (Information Sheet → Find Municipality)
  observeEvent(input$page0_next, {
    current_page(1)
  })

  # Page 1 → Page 3 (Attention check), or Page 2 (screen-out) if home muni is in an
  # excluded state (CDMX=09, Durango=10, Oaxaca=20, Veracruz=30) or has no main-party
  # government, or if the municipality's state doesn't match the Netquest region code.
  excluded_state_codes <- c("09", "10", "20", "30")

  # Netquest region code → INEGI 2-digit state code (for region consistency check)
  nq_to_inegi_state <- c(
    "20" = "01",
    "21" = "02",
    "22" = "03",
    "23" = "04",
    "24" = "05",
    "25" = "06",
    "26" = "07",
    "27" = "08",
    "30" = "11",
    "31" = "12",
    "32" = "13",
    "33" = "14",
    "34" = "15",
    "35" = "16",
    "36" = "17",
    "37" = "18",
    "38" = "19",
    "40" = "21",
    "41" = "22",
    "42" = "23",
    "43" = "24",
    "44" = "25",
    "45" = "26",
    "46" = "27",
    "47" = "28",
    "48" = "29",
    "50" = "31",
    "51" = "32"
  )

  observeEvent(input$page1_next, {
    home_id <- found_municipality()
    state_code <- substr(home_id, 1, 2)
    home_party <- d_geo %>%
      st_drop_geometry() %>%
      filter(muni_id == home_id) %>%
      pull(governing_party) %>%
      `[`(1)

    # Check Netquest region matches selected municipality's state (skip if no NQ param)
    region_mismatch <- !is.null(nq_region) &&
      nq_region %in% names(nq_to_inegi_state) &&
      nq_to_inegi_state[nq_region] != state_code

    if (region_mismatch) {
      # Quality/attention check failure — redirect directly to Netquest dr_0
      pid <- if (!is.null(netquest_pid) && nchar(netquest_pid) > 0) {
        netquest_pid
      } else {
        ""
      }
      save_screenout(
        respondent_id,
        "region_mismatch",
        home_id,
        netquest_pid,
        nq_age,
        nq_sex,
        nq_region,
        nq_sel,
        Sys.getenv("S3_BUCKET")
      )
      shinyjs::runjs(sprintf(
        'setTimeout(function(){ window.location.href = "https://transit.nicequest.com/transit/participation?tp=dr_0&c=ok&ticket=%s"; }, 1500)',
        pid
      ))
    } else if (state_code %in% excluded_state_codes || is.na(home_party)) {
      save_screenout(
        respondent_id,
        if (state_code %in% excluded_state_codes) {
          "excluded_state"
        } else {
          "non_main_party"
        },
        home_id,
        netquest_pid,
        nq_age,
        nq_sex,
        nq_region,
        nq_sel,
        Sys.getenv("S3_BUCKET")
      )
      current_page(2)
    } else {
      current_page(3)
    }
  })

  # Screen-out redirect to Netquest
  observeEvent(current_page(), {
    if (
      current_page() == 2 && !is.null(netquest_pid) && nchar(netquest_pid) > 0
    ) {
      shinyjs::runjs(sprintf(
        'setTimeout(function(){ window.location.href = "https://transit.nicequest.com/transit/participation?tp=dr_0&c=ok&ticket=%s"; }, 1500)',
        netquest_pid
      ))
    }
    if (current_page() == 9) {
      session$sendCustomMessage("initPage9Sliders", list())
    }
  })

  # Page 4 → Page 6 (benchmark + additional dropdown → issue importance ranking)
  page4_warning_msg <- reactiveVal(NULL)
  output$page4_warning <- renderUI({
    msg <- page4_warning_msg()
    if (!is.null(msg)) p(style = "color: #c0392b; font-weight: bold;", msg)
  })
  observeEvent(input$page4_next, {
    if (
      is.null(input$benchmark_selected) || length(input$benchmark_selected) == 0
    ) {
      page4_warning_msg(
        "Por favor, seleccione al menos un municipio antes de continuar."
      )
      return()
    }
    additional <- input$additional_munis_dropdown
    if (!is.null(additional) && length(additional) > 0) {
      selected_benchmarks(unique(c(selected_benchmarks(), additional)))
    }
    page4_warning_msg(NULL)
    current_page(6)
  })

  # Page 2 → Page 1 (commented out with page 2)
  # observeEvent(input$goto_page1_from_3, { current_page(1) })

  # Page 3 → Page 4 (attention check → benchmark selection)
  practice_warning_msg <- reactiveVal(NULL)
  output$practice_warning <- renderUI({
    msg <- practice_warning_msg()
    if (!is.null(msg)) p(style = "color: #c0392b; font-weight: bold;", msg)
  })
  observeEvent(input$page3_next, {
    ranking <- input$practice_ranking
    if (is.null(ranking) || length(ranking) < 3) {
      practice_warning_msg(
        "Por favor, clasifique todos los elementos antes de continuar."
      )
      return()
    }
    if (ranking[1] != "Radio" || ranking[3] != "Redes sociales") {
      ticket <- if (!is.null(netquest_pid) && nchar(netquest_pid) > 0) {
        netquest_pid
      } else {
        ""
      }
      save_screenout(
        respondent_id,
        "attention_check_fail",
        found_municipality(),
        netquest_pid,
        nq_age,
        nq_sex,
        nq_region,
        nq_sel,
        Sys.getenv("S3_BUCKET")
      )
      shinyjs::runjs(sprintf(
        'window.location.href = "https://transit.nicequest.com/transit/participation?tp=fo_0&c=ok&ticket=%s"',
        ticket
      ))
      return()
    }
    practice_warning_msg(NULL)
    current_page(4)
  })

  # Page 2 → Page 3 (commented out with page 2)
  # observeEvent(input$goto_page3_from_2, { current_page(3) })

  # Page 6: Issue Importance Ranking + Vote Intention → Page 9
  issue_importance_warning_msg <- reactiveVal(NULL)
  output$issue_importance_warning <- renderUI({
    msg <- issue_importance_warning_msg()
    if (!is.null(msg)) p(style = "color: #c0392b; font-weight: bold;", msg)
  })
  observeEvent(input$page6_next, {
    ranking <- input$issue_importance_ranking
    if (is.null(ranking) || length(ranking) < 5) {
      issue_importance_warning_msg(
        "Por favor, ordene todos los temas de importancia antes de continuar."
      )
      return()
    }
    if (
      is.null(input$vote_intention_pre) || length(input$vote_intention_pre) == 0
    ) {
      issue_importance_warning_msg("Por favor, indique su intención de voto.")
      return()
    }
    if (
      "other" %in%
        input$vote_intention_pre &&
        (is.null(input$vote_intention_pre_other) ||
          trimws(input$vote_intention_pre_other) == "")
    ) {
      issue_importance_warning_msg(
        "Por favor, especifique el partido para la opción 'Otro' en la pregunta de intención de voto."
      )
      return()
    }
    issue_importance_warning_msg(NULL)
    current_page(9)
  })

  # Page 9 → Wave 1 submit
  page9_warning_msg <- reactiveVal(NULL)
  output$page9_warning <- renderUI({
    msg <- page9_warning_msg()
    if (!is.null(msg)) p(style = "color: #c0392b; font-weight: bold;", msg)
  })
  observeEvent(input$page9_submit, {
    disable("page9_submit")
    est <- input$robbery_estimate
    if (is.null(est) || is.na(est)) {
      enable("page9_submit")
      page9_warning_msg("Por favor, ingrese su estimación antes de continuar.")
      return()
    }
    untouched <- c()
    if (!isTRUE(input$slider_touched_home_crime_handling_pre)) {
      untouched <- c(untouched, "el manejo de la delincuencia en su municipio")
    }
    if (!isTRUE(input$slider_touched_morena_crime_rating)) {
      untouched <- c(
        untouched,
        "el manejo de la delincuencia por Sigamos Haciendo Historia"
      )
    }
    if (!isTRUE(input$slider_touched_coalition_pan_pri_prd_crime_rating)) {
      untouched <- c(
        untouched,
        "el manejo de la delincuencia por Fuerza y Coraz\u00f3n por M\u00e9xico"
      )
    }
    if (!isTRUE(input$slider_touched_mc_crime_rating)) {
      untouched <- c(untouched, "el manejo de la delincuencia por MC")
    }
    if (length(untouched) > 0) {
      enable("page9_submit")
      page9_warning_msg(
        "Por favor, responda todos los controles deslizantes antes de continuar."
      )
      return()
    }
    page9_warning_msg(NULL)
    found_muni_name <- if (!is.null(found_municipality())) {
      d_geo %>%
        st_drop_geometry() %>%
        filter(muni_id == found_municipality()) %>%
        mutate(full_name = paste0(NOMGEO, ", ", NOM_ENT)) %>%
        pull(full_name)
    } else {
      NA_character_
    }

    response_df <- data.frame(
      Respondent_ID = respondent_id,
      Netquest_PID = if (!is.null(netquest_pid)) {
        netquest_pid
      } else {
        NA_character_
      },
      NQ_Age = if (!is.null(nq_age)) nq_age else NA_character_,
      NQ_Sex = if (!is.null(nq_sex)) nq_sex else NA_character_,
      NQ_Region = if (!is.null(nq_region)) nq_region else NA_character_,
      NQ_SEL = if (!is.null(nq_sel)) nq_sel else NA_character_,
      Found_Municipality = found_muni_name,
      Found_Municipality_ID = found_municipality(),
      Benchmark_Candidate_Municipalities = {
        cands <- benchmark_candidates()
        if (is.null(cands) || nrow(cands) == 0) {
          NA_character_
        } else {
          paste(cands$muni_id, collapse = ";")
        }
      },
      Benchmark_Selected_Municipalities = if (
        length(selected_benchmarks()) == 0
      ) {
        NA_character_
      } else {
        paste(selected_benchmarks(), collapse = ";")
      },
      Nearest3_Muni_1_ID = if (
        !is.null(nearest3_rv()) && nrow(nearest3_rv()) >= 1
      ) {
        nearest3_rv()$neighbor_id[1]
      } else {
        NA_character_
      },
      Nearest3_Muni_2_ID = if (
        !is.null(nearest3_rv()) && nrow(nearest3_rv()) >= 2
      ) {
        nearest3_rv()$neighbor_id[2]
      } else {
        NA_character_
      },
      Nearest3_Muni_3_ID = if (
        !is.null(nearest3_rv()) && nrow(nearest3_rv()) >= 3
      ) {
        nearest3_rv()$neighbor_id[3]
      } else {
        NA_character_
      },
      Importance_Crime = {
        ranking <- input$issue_importance_ranking
        match("Seguridad / Delincuencia", ranking, nomatch = NA_integer_)
      },
      Importance_Economy = {
        ranking <- input$issue_importance_ranking
        match("Econom\u00eda / Inflaci\u00f3n", ranking, nomatch = NA_integer_)
      },
      Importance_Employment_Poverty = {
        ranking <- input$issue_importance_ranking
        match("Desempleo / Bajos salarios", ranking, nomatch = NA_integer_)
      },
      Importance_Corruption = {
        ranking <- input$issue_importance_ranking
        match("Corrupci\u00f3n", ranking, nomatch = NA_integer_)
      },
      Importance_Education_Health = {
        ranking <- input$issue_importance_ranking
        match(
          "Educaci\u00f3n y servicios de salud",
          ranking,
          nomatch = NA_integer_
        )
      },
      Robbery_Estimate = ifelse(
        is.null(input$robbery_estimate),
        NA_integer_,
        input$robbery_estimate
      ),
      Home_Crime_Handling_Pre = input$home_crime_handling_pre,
      MORENA_Crime_Rating_Pre = input$morena_crime_rating,
      Coalition_PAN_PRI_PRD_Crime_Rating_Pre = input$coalition_pan_pri_prd_crime_rating,
      MC_Crime_Rating_Pre = input$mc_crime_rating,
      Vote_Intention_Pre = if (
        is.null(input$vote_intention_pre) ||
          length(input$vote_intention_pre) == 0
      ) {
        NA_character_
      } else {
        paste(input$vote_intention_pre, collapse = ";")
      },
      Vote_Intention_Pre_Other = ifelse(
        is.null(input$vote_intention_pre_other) ||
          input$vote_intention_pre_other == "",
        NA_character_,
        input$vote_intention_pre_other
      ),
      Timestamp = as.character(Sys.time()),
      Time_Total_Seconds = {
        durs <- page_durations()
        elapsed <- as.numeric(difftime(
          Sys.time(),
          page_enter_time(),
          units = "secs"
        ))
        pg <- as.character(current_page())
        durs[[pg]] <- (if (is.null(durs[[pg]])) 0 else durs[[pg]]) + elapsed
        round(sum(unlist(durs)), 1)
      },
      Time_Page_0_Sec = round(
        if (is.null(page_durations()[["0"]])) {
          NA_real_
        } else {
          page_durations()[["0"]]
        },
        1
      ),
      Time_Page_1_Sec = round(
        if (is.null(page_durations()[["1"]])) {
          NA_real_
        } else {
          page_durations()[["1"]]
        },
        1
      ),
      Time_Page_3_Sec = round(
        if (is.null(page_durations()[["3"]])) {
          NA_real_
        } else {
          page_durations()[["3"]]
        },
        1
      ),
      Time_Page_4_Sec = round(
        if (is.null(page_durations()[["4"]])) {
          NA_real_
        } else {
          page_durations()[["4"]]
        },
        1
      ),
      Time_Page_6_Sec = round(
        if (is.null(page_durations()[["6"]])) {
          NA_real_
        } else {
          page_durations()[["6"]]
        },
        1
      ),
      Time_Page_9_Sec = round(
        {
          elapsed <- as.numeric(difftime(
            Sys.time(),
            page_enter_time(),
            units = "secs"
          ))
          accumulated <- if (is.null(page_durations()[["9"]])) {
            0
          } else {
            page_durations()[["9"]]
          }
          accumulated + elapsed
        },
        1
      ),
      Is_Mobile = isTRUE(input$is_mobile),
      stringsAsFactors = FALSE
    )

    tc <- textConnection("csv_out", "w", local = TRUE)
    write.csv(response_df, tc, row.names = FALSE)
    close(tc)
    csv_content <- paste(csv_out, collapse = "\n")

    s3_bucket <- Sys.getenv("S3_BUCKET")
    save_success <- if (nchar(s3_bucket) > 0) {
      tryCatch(
        {
          s3_client <- paws.storage::s3()
          s3_client$put_object(
            Bucket = s3_bucket,
            Key = paste0("wave1/", respondent_id, ".csv"),
            Body = charToRaw(csv_content)
          )
          TRUE
        },
        error = function(e) {
          warning("S3 upload failed: ", e$message)
          FALSE
        }
      )
    } else {
      tryCatch(
        {
          responses_file <- "data/survey_responses_wave1.csv"
          if (file.exists(responses_file)) {
            write.table(
              response_df,
              responses_file,
              append = TRUE,
              sep = ",",
              row.names = FALSE,
              col.names = FALSE
            )
          } else {
            write.csv(response_df, responses_file, row.names = FALSE)
          }
          TRUE
        },
        error = function(e) {
          warning("Local CSV write failed: ", e$message)
          FALSE
        }
      )
    }

    # Save Wave 1 lookup for Wave 2 (keyed by respondent_id)
    if (nchar(s3_bucket) > 0) {
      tryCatch(
        {
          lookup <- list(
            Found_Municipality_ID = found_municipality(),
            Netquest_PID = netquest_pid
          )
          s3_client <- paws.storage::s3()
          con <- rawConnection(raw(0), "wb")
          saveRDS(lookup, con)
          rds_bytes <- rawConnectionValue(con)
          close(con)
          s3_client$put_object(
            Bucket = s3_bucket,
            Key = paste0("wave1_lookup/", respondent_id, ".rds"),
            Body = rds_bytes
          )
        },
        error = function(e) {
          warning("S3 lookup write failed: ", e$message)
        }
      )
    }

    # ── Increment marginal quota counts after successful submission ────────────
    if (nchar(s3_bucket) > 0) {
      tryCatch(
        {
          counts <- read_quota_counts(s3_bucket)

          sex_cell <- if (!is.null(nq_sex) && nq_sex %in% names(QUOTA_SEX)) {
            nq_sex
          } else {
            NULL
          }
          age_cell <- if (!is.null(nq_age)) age_bracket(nq_age) else NULL
          sel_cell <- if (!is.null(nq_sel) && nq_sel %in% names(QUOTA_SEL)) {
            nq_sel
          } else {
            NULL
          }
          region_cell <- if (
            !is.null(nq_region) && nq_region %in% names(QUOTA_REGION)
          ) {
            nq_region
          } else {
            NULL
          }

          bump <- function(vec, key) {
            v <- vec[[key]]
            vec[[key]] <- if (is.null(v) || is.na(v)) 1L else as.integer(v) + 1L
            vec
          }

          if (!is.null(sex_cell) && !is.na(sex_cell)) {
            counts$sex <- bump(counts$sex, sex_cell)
          }
          if (!is.null(age_cell) && !is.na(age_cell)) {
            counts$age <- bump(counts$age, age_cell)
          }
          if (!is.null(sel_cell) && !is.na(sel_cell)) {
            counts$sel <- bump(counts$sel, sel_cell)
          }
          if (!is.null(region_cell) && !is.na(region_cell)) {
            counts$region <- bump(counts$region, region_cell)
          }

          write_quota_counts(s3_bucket, counts)
        },
        error = function(e) warning("Quota count update failed: ", e$message)
      )
    }

    if (!save_success) {
      showNotification(
        "Error: Su respuesta no pudo ser guardada. Por favor, intente de nuevo o contacte al investigador.",
        type = "error",
        duration = NULL
      )
    }

    current_page(12)

    if (!is.null(netquest_pid) && nchar(netquest_pid) > 0) {
      shinyjs::runjs(sprintf(
        'setTimeout(function(){ window.location.href = "https://transit.nicequest.com/transit/participation?tp=co_0&c=ok&ticket=%s"; }, 2000)',
        netquest_pid
      ))
    }
  })

  # Clear selection button - resets the found municipality
  # Dropdown municipality selection
  observeEvent(
    input$dropdown_municipality,
    {
      req(input$dropdown_municipality != "")
      found_municipality(input$dropdown_municipality)
      found_address_coords(NULL)
      output$geocode_result <- renderUI({})
    },
    ignoreInit = TRUE
  )

  observeEvent(input$clear_selection_btn, {
    found_municipality(NULL)
    found_address_coords(NULL)
    geocode_attempted(FALSE)
    updateTextInput(session, "address", value = "")
    updateSelectInput(session, "dropdown_municipality", selected = "")
    output$geocode_result <- renderUI({})
  })

  # Display home municipality summary on page 1 confirmation section
  output$home_municipality_summary <- renderUI({
    home_muni <- if (!is.null(found_municipality())) {
      d_geo %>%
        st_drop_geometry() %>%
        filter(muni_id == found_municipality()) %>%
        mutate(full_name = paste0(NOMGEO, ", ", NOM_ENT)) %>%
        pull(full_name)
    } else {
      "None"
    }

    div(
      style = "background-color: #f8f9fa; padding: 15px; border-radius: 4px; margin-bottom: 20px;",
      h5(icon("info-circle"), " Su municipio:"),
      tags$p(
        tags$span(
          home_muni,
          style = "color: #0072B2; font-size: 1.2em; font-weight: bold;"
        )
      )
    )
  })

  # Display verification info on page 2

  # Issue importance ranking with randomized order (per session)
  issue_labels_randomized <- sample(c(
    "Seguridad / Delincuencia",
    "Economía / Inflación",
    "Desempleo / Bajos salarios",
    "Corrupción",
    "Educación y servicios de salud"
  ))
  output$issue_importance_ui <- renderUI({
    slot_ranker_ui(
      "issue_importance_ranking",
      items = issue_labels_randomized,
      source_label = "Problemas disponibles:",
      slots_label = "Su clasificación (1 = más importante, 5 = menos importante):"
    )
  })

  # Helper: get municipality display name for dynamic questions
  home_muni_name <- reactive({
    home_id <- found_municipality()
    if (!is.null(home_id)) {
      d_geo %>%
        st_drop_geometry() %>%
        filter(muni_id == home_id) %>%
        mutate(full_name = paste0(NOMGEO, ", ", NOM_ENT)) %>%
        pull(full_name)
    } else {
      "su municipio"
    }
  })

  # Pre-treatment: home municipality crime handling slider

  output$home_crime_handling_pre_ui <- renderUI({
    muni_name <- home_muni_name()
    tagList(
      sliderInput(
        "home_crime_handling_pre",
        paste0(
          "¿Qué tan bien cree que el gobierno de ",
          muni_name,
          " maneja la delincuencia?"
        ),
        min = 0,
        max = 100,
        value = 50
      ),
      fluidRow(
        column(
          6,
          p(
            "Maneja la delincuencia extremadamente mal",
            style = "color: #6c757d; font-size: 0.85em; margin-top: -15px;"
          )
        ),
        column(
          6,
          p(
            "Maneja la delincuencia extremadamente bien",
            style = "color: #6c757d; font-size: 0.85em; margin-top: -15px; text-align: right;"
          )
        )
      )
    )
  })

  # Robbery estimate question with dynamic municipality name
  output$robbery_estimate_ui <- renderUI({
    home_id <- found_municipality()
    muni_name <- if (!is.null(home_id)) {
      d_geo %>%
        st_drop_geometry() %>%
        filter(muni_id == home_id) %>%
        mutate(full_name = paste0(NOMGEO, ", ", NOM_ENT)) %>%
        pull(full_name)
    } else {
      "su municipio"
    }
    numericInput(
      "robbery_estimate",
      paste0(
        "¿Cuántos robos por cada 100,000 personas cree que fueron reportados en ",
        muni_name,
        " en 2025?"
      ),
      value = NULL,
      min = 0,
      step = 1
    )
  })

  # Benchmark instructions text with dynamic municipality name (page 4)
  output$benchmark_instructions_text <- renderUI({
    req(!is.null(found_municipality()))
    muni_data <- d_geo %>%
      st_drop_geometry() %>%
      filter(muni_id == found_municipality())
    muni_name <- paste0(muni_data$NOMGEO, ", ", muni_data$NOM_ENT)
    tagList(
      p(strong(
        "Los gobiernos municipales suelen evaluarse comparándolos con otros. ",
        paste0(
          "¿Con cuáles de los siguientes municipios compararía a ",
          muni_name,
          "? (Elija al menos uno.)"
        )
      ))
    )
  })

  # Additional municipality instructions text
  output$additional_munis_instructions_text <- renderUI({
    p("Agregue otros municipios de comparaci\u00f3n (opcional):")
  })

  # Benchmark municipality checklist (page 4)
  output$benchmark_list <- renderUI({
    cands <- benchmark_candidates()
    req(!is.null(cands) && nrow(cands) > 0)
    checkboxGroupInput(
      "benchmark_selected",
      label = NULL,
      choiceNames = lapply(seq_len(nrow(cands)), function(i) {
        paste0(cands$NOMGEO[i], ", ", cands$NOM_ENT[i])
      }),
      choiceValues = as.list(cands$muni_id),
      selected = character(0)
    )
  })

  observeEvent(input$benchmark_selected, {
    val <- input$benchmark_selected
    selected_benchmarks(if (is.null(val)) character(0) else val)
  })

  # Reference instructions text (commented out with page 2)
  # output$reference_instructions_text <- renderUI({ ... })

  # Display selected municipality names as removable chips (commented out with page 2)
  if (FALSE) {
    output$selected_munis_display <- renderUI({
      ids <- selected_map_munis()
      if (length(ids) == 0) {
        return(p(
          style = "color: #6c757d; font-style: italic;",
          "No municipalities selected yet."
        ))
      }
      names_df <- d_geo %>%
        st_drop_geometry() %>%
        filter(muni_id %in% ids) %>%
        mutate(full_name = paste0(NOMGEO, ", ", NOM_ENT))
      tagList(
        p(strong("Selected municipalities:")),
        div(
          style = "display: flex; flex-wrap: wrap; gap: 8px; margin-bottom: 12px;",
          lapply(seq_len(nrow(names_df)), function(i) {
            muni_id_val <- names_df$muni_id[i]
            tags$span(
              style = paste0(
                "display: inline-flex; align-items: center; background-color: #9B59B6; ",
                "color: white; border-radius: 16px; padding: 4px 12px; font-size: 0.9em;"
              ),
              names_df$full_name[i],
              tags$button(
                "\u00d7",
                style = paste0(
                  "background: none; border: none; color: white; font-size: 1.1em; ",
                  "cursor: pointer; margin-left: 8px; padding: 0; line-height: 1;"
                ),
                onclick = sprintf(
                  "Shiny.setInputValue('remove_muni', '%s', {priority: 'event'});",
                  muni_id_val
                )
              )
            )
          })
        )
      )
    })
  }

  # Page 2 map observers (commented out with page 2)
  if (FALSE) {
    observeEvent(input$remove_muni, {
      id <- input$remove_muni
      selected_map_munis(setdiff(selected_map_munis(), id))
      leafletProxy("map_page2") %>%
        addPolygons(
          data = d_geo[d_geo$muni_id == id, ],
          layerId = id,
          fillColor = ~ coalition_pal(coalition_label),
          fillOpacity = 0.55,
          color = "#555555",
          weight = 0.4,
          label = ~ paste0(
            NOMGEO,
            ", ",
            NOM_ENT,
            ifelse(
              !is.na(coalition_label),
              paste0(" \u2014 ", coalition_label),
              ""
            )
          ),
          highlightOptions = highlightOptions(
            weight = 2,
            color = "#222222",
            fillOpacity = 0.75,
            bringToFront = TRUE
          )
        )
    })
  }

  if (FALSE) {
    # Map view mode toggle (borders / parties / population)
    observeEvent(
      input$map_view_mode,
      {
        mode <- input$map_view_mode
        home_id <- found_municipality()
        selected_ids <- selected_map_munis()
        exclude_ids <- c(home_id, selected_ids)
        base_data <- d_geo[!d_geo$muni_id %in% exclude_ids, ]
        proxy <- leafletProxy("map_page2")

        if (mode == "borders") {
          proxy %>%
            clearControls() %>%
            addPolygons(
              data = base_data,
              layerId = ~muni_id,
              fillColor = "#DDDDDD",
              fillOpacity = 0.5,
              color = "#555555",
              weight = 0.4,
              label = ~ paste0(NOMGEO, ", ", NOM_ENT),
              highlightOptions = highlightOptions(
                weight = 2,
                color = "#222222",
                fillOpacity = 0.7,
                bringToFront = TRUE
              )
            ) %>%
            addLegend(
              position = "bottomright",
              colors = c("#9B59B6", "#27AE60"),
              labels = c("Selected", "Your municipality"),
              opacity = 0.8
            )
        } else if (mode == "parties") {
          proxy %>%
            clearControls() %>%
            addPolygons(
              data = base_data,
              layerId = ~muni_id,
              fillColor = ~ coalition_pal(coalition_label),
              fillOpacity = 0.55,
              color = "#555555",
              weight = 0.4,
              label = ~ paste0(
                NOMGEO,
                ", ",
                NOM_ENT,
                ifelse(
                  !is.na(coalition_label),
                  paste0(" — ", coalition_label),
                  ""
                )
              ),
              highlightOptions = highlightOptions(
                weight = 2,
                color = "#222222",
                fillOpacity = 0.75,
                bringToFront = TRUE
              )
            ) %>%
            addLegend(
              position = "bottomright",
              colors = c(
                unname(coalition_map_colors),
                "#D3D3D3",
                "#9B59B6",
                "#27AE60"
              ),
              labels = c(
                names(coalition_map_colors),
                "Other",
                "Selected",
                "Your municipality"
              ),
              title = "Governing coalition",
              opacity = 0.8
            )
        } else if (mode == "population") {
          proxy %>%
            clearControls() %>%
            addPolygons(
              data = base_data,
              layerId = ~muni_id,
              fillColor = ~ pop_pal(POB_TOTAL),
              fillOpacity = 0.7,
              color = "#555555",
              weight = 0.4,
              label = ~ paste0(
                NOMGEO,
                ", ",
                NOM_ENT,
                ifelse(
                  !is.na(POB_TOTAL),
                  paste0(" — Pop: ", format(POB_TOTAL, big.mark = ",")),
                  ""
                )
              ),
              highlightOptions = highlightOptions(
                weight = 2,
                color = "#222222",
                fillOpacity = 0.85,
                bringToFront = TRUE
              )
            ) %>%
            addLegend(
              position = "bottomright",
              pal = pop_pal,
              values = d_geo$POB_TOTAL,
              title = "Population",
              opacity = 0.8,
              labFormat = labelFormat(big.mark = ",")
            ) %>%
            addLegend(
              position = "bottomright",
              colors = c("#9B59B6", "#27AE60"),
              labels = c("Selected", "Your municipality"),
              opacity = 0.8
            )
        }

        # Re-draw selected municipalities on top
        if (length(selected_ids) > 0) {
          proxy %>%
            addPolygons(
              data = d_geo[d_geo$muni_id %in% selected_ids, ],
              group = "selected",
              layerId = ~muni_id,
              fillColor = "#9B59B6",
              fillOpacity = 0.6,
              color = "#6C3483",
              weight = 1.2,
              label = ~ paste0(NOMGEO, ", ", NOM_ENT),
              highlightOptions = highlightOptions(
                weight = 2,
                color = "#4A235A",
                fillOpacity = 0.75,
                bringToFront = TRUE
              )
            )
        }

        # Re-draw home municipality on top
        if (!is.null(home_id)) {
          proxy %>%
            clearGroup("found_muni") %>%
            addPolygons(
              data = d_geo[d_geo$muni_id == home_id, ],
              group = "found_muni",
              fillColor = "#27AE60",
              fillOpacity = 0.4,
              color = "#27AE60",
              weight = 3,
              label = ~ paste0(NOMGEO, ", ", NOM_ENT, " (Your municipality)")
            )
        }
      },
      ignoreNULL = TRUE
    )
  }

  # Page 2 Map - Interactive map for reference municipality selection (commented out with page 2)
  if (FALSE) {
    output$map_page2 <- renderLeaflet({
      home_id <- found_municipality()

      map <- leaflet(d_geo) %>%
        addTiles() %>%
        addPolygons(
          layerId = ~muni_id,
          fillColor = ~ coalition_pal(coalition_label),
          fillOpacity = 0.55,
          color = "#555555",
          weight = 0.4,
          label = ~ paste0(
            NOMGEO,
            ", ",
            NOM_ENT,
            ifelse(!is.na(coalition_label), paste0(" — ", coalition_label), "")
          ),
          highlightOptions = highlightOptions(
            weight = 2,
            color = "#222222",
            fillOpacity = 0.75,
            bringToFront = TRUE
          )
        ) %>%
        addLegend(
          position = "bottomright",
          colors = c(
            unname(coalition_map_colors),
            "#D3D3D3",
            "#9B59B6",
            "#27AE60"
          ),
          labels = c(
            names(coalition_map_colors),
            "Other",
            "Selected",
            "Your municipality"
          ),
          title = "Governing coalition",
          opacity = 0.8
        ) %>%
        fitBounds(lng1 = -115.0, lat1 = 16.0, lng2 = -88.0, lat2 = 30.5)

      if (!is.null(home_id)) {
        home_data <- d_geo[d_geo$muni_id == home_id, ]
        home_coords <- found_address_coords()

        map <- map %>%
          addPolygons(
            data = home_data,
            group = "found_muni",
            fillColor = "#27AE60",
            fillOpacity = 0.4,
            color = "#27AE60",
            weight = 3,
            label = ~ paste0(NOMGEO, ", ", NOM_ENT, " (Your municipality)")
          )

        if (!is.null(home_coords)) {
          map <- map %>%
            addMarkers(
              lng = home_coords$lon,
              lat = home_coords$lat,
              popup = "Your address",
              group = "address_marker"
            )
        }
      }

      map
    })
  }

  if (FALSE) {
    # Click-to-toggle municipality selection on page 2 map
    observeEvent(input$map_page2_shape_click, {
      id <- input$map_page2_shape_click$id
      req(id)
      req(!is.null(found_municipality()))

      # Clicking home municipality just zooms to it
      if (id == found_municipality()) {
        muni_data <- d_geo %>% filter(muni_id == found_municipality())
        centroid <- st_centroid(st_geometry(muni_data))
        coords <- st_coordinates(centroid)

        leafletProxy("map_page2") %>%
          setView(lng = coords[1], lat = coords[2], zoom = 8) %>%
          clearGroup("found_muni") %>%
          addPolygons(
            data = muni_data,
            group = "found_muni",
            fillColor = "#27AE60",
            fillOpacity = 0.4,
            color = "#27AE60",
            weight = 3,
            label = ~ paste0(NOMGEO, ", ", NOM_ENT, " (Your municipality)")
          )

        if (!is.null(found_address_coords())) {
          address_coords <- found_address_coords()
          leafletProxy("map_page2") %>%
            clearGroup("address_marker") %>%
            addMarkers(
              lng = address_coords$lon,
              lat = address_coords$lat,
              popup = "Your address",
              group = "address_marker"
            )
        }
        return()
      }

      current <- selected_map_munis()
      is_deselecting <- id %in% current

      if (is_deselecting) {
        selected_map_munis(setdiff(current, id))
        leafletProxy("map_page2") %>%
          addPolygons(
            data = d_geo[d_geo$muni_id == id, ],
            layerId = id,
            fillColor = ~ coalition_pal(coalition_label),
            fillOpacity = 0.55,
            color = "#555555",
            weight = 0.4,
            label = ~ paste0(
              NOMGEO,
              ", ",
              NOM_ENT,
              ifelse(
                !is.na(coalition_label),
                paste0(" — ", coalition_label),
                ""
              )
            ),
            highlightOptions = highlightOptions(
              weight = 2,
              color = "#222222",
              fillOpacity = 0.75,
              bringToFront = TRUE
            )
          )
      } else {
        selected_map_munis(c(current, id))
        leafletProxy("map_page2") %>%
          addPolygons(
            data = d_geo[d_geo$muni_id == id, ],
            group = "selected",
            layerId = id,
            fillColor = "#9B59B6",
            fillOpacity = 0.6,
            color = "#6C3483",
            weight = 1.2,
            label = ~ paste0(NOMGEO, ", ", NOM_ENT),
            highlightOptions = highlightOptions(
              weight = 2,
              color = "#4A235A",
              fillOpacity = 0.75,
              bringToFront = TRUE
            )
          )
      }

      # Keep home municipality highlighted on top
      if (!is.null(found_municipality())) {
        leafletProxy("map_page2") %>%
          clearGroup("found_muni") %>%
          addPolygons(
            data = d_geo[d_geo$muni_id == found_municipality(), ],
            group = "found_muni",
            fillColor = "#27AE60",
            fillOpacity = 0.4,
            color = "#27AE60",
            weight = 3,
            label = ~ paste0(NOMGEO, ", ", NOM_ENT, " (Your municipality)")
          )
      }

      if (!is.null(found_address_coords())) {
        address_coords <- found_address_coords()
        leafletProxy("map_page2") %>%
          addMarkers(
            lng = address_coords$lon,
            lat = address_coords$lat,
            popup = "Your address",
            group = "address_marker"
          )
      }
    })
  }

  if (FALSE) {
    # Search box: auto-select and highlight searched municipality
    observeEvent(input$muni_search, {
      req(input$muni_search != "")
      muni_id <- input$muni_search
      muni_data <- d_geo %>% filter(muni_id == !!muni_id)
      req(nrow(muni_data) > 0)

      if (!is.null(found_municipality()) && muni_id != found_municipality()) {
        current <- selected_map_munis()
        if (!(muni_id %in% current)) {
          selected_map_munis(c(current, muni_id))

          leafletProxy("map_page2") %>%
            clearGroup("selected") %>%
            clearGroup("found_muni") %>%
            addPolygons(
              data = d_geo[d_geo$muni_id %in% selected_map_munis(), ],
              group = "selected",
              layerId = ~muni_id,
              fillColor = "#9B59B6",
              fillOpacity = 0.6,
              color = "#6C3483",
              weight = 1.2,
              label = ~ paste0(NOMGEO, ", ", NOM_ENT),
              highlightOptions = highlightOptions(
                weight = 2,
                color = "#4A235A",
                fillOpacity = 0.75,
                bringToFront = TRUE
              )
            ) %>%
            addPolygons(
              data = d_geo[d_geo$muni_id == found_municipality(), ],
              group = "found_muni",
              fillColor = "#27AE60",
              fillOpacity = 0.6,
              color = "#1E8449",
              weight = 2,
              label = ~ paste0(NOMGEO, ", ", NOM_ENT, " (Your municipality)")
            )

          if (!is.null(found_address_coords())) {
            address_coords <- found_address_coords()
            leafletProxy("map_page2") %>%
              addMarkers(
                lng = address_coords$lon,
                lat = address_coords$lat,
                popup = "Your address",
                group = "address_marker"
              )
          }
        }
      }
    })
  }

  if (FALSE) {
    # Zoom to searched municipality
    observeEvent(input$zoom_search, {
      req(input$muni_search != "")
      muni_id <- input$muni_search
      muni_data <- d_geo %>% filter(muni_id == !!muni_id)
      req(nrow(muni_data) > 0)

      centroid <- st_centroid(st_geometry(muni_data))
      coords <- st_coordinates(centroid)
      leafletProxy("map_page2") %>%
        setView(lng = coords[1], lat = coords[2], zoom = 8)
    })
  }

  if (FALSE) {
    # Clear search and reset map view
    observeEvent(input$clear_search, {
      updateSelectizeInput(session, "muni_search", selected = character(0))
      leafletProxy("map_page2") %>%
        fitBounds(lng1 = -115.0, lat1 = 16.0, lng2 = -88.0, lat2 = 30.5)
    })
  }

  if (FALSE) {
    # Zoom to home municipality
    observeEvent(input$zoom_home, {
      home_id <- found_municipality()
      req(home_id)
      home_data <- d_geo[d_geo$muni_id == home_id, ]
      centroid <- st_centroid(st_geometry(home_data))
      coords <- st_coordinates(centroid)
      leafletProxy("map_page2") %>%
        setView(lng = coords[1], lat = coords[2], zoom = 8)
    })
  }
}

shinyApp(ui = ui, server = server)
