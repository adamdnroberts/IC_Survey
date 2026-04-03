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

# Load pre-computed incumbent multiple-choice data
if (!file.exists("data/mp_mc_data.rds")) {
  stop("Missing data/mp_mc_data.rds. Run: source('code/party_data.R')")
}
mp_mc_data <- readRDS("data/mp_mc_data.rds")

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
      /* Hide minor tick marks on ideology slider */
      #left_right_scale .irs-grid-pol.small { display: none; }

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
      /* Issue importance ranking: grayed out until first interaction */
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
      .governance-grid input[type='checkbox'] {
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
      $(document).on('change', '#governance_grid input.governance-checkbox', function() {
        var inputName = $(this).data('input-name');
        var checked = [];
        $('#governance_grid input.governance-checkbox[data-input-name=\"' + inputName + '\"]:checked').each(function() {
          checked.push($(this).val());
        });
        Shiny.setInputValue(inputName, checked.length > 0 ? checked : null);
      });
      $(document).on('change', 'input.party-checkbox-input', function() {
        var group = $(this).data('group');
        var val = $(this).val();
        var exclusive = ['did_not_vote', 'dont_remember'];
        if ($(this).is(':checked')) {
          if (exclusive.indexOf(val) !== -1) {
            // Exclusive option selected: uncheck everything else in the group
            $('input.party-checkbox-input[data-group=\"' + group + '\"]').not(this).prop('checked', false);
          } else {
            // Regular option selected: uncheck exclusive options
            exclusive.forEach(function(ex) {
              $('input.party-checkbox-input[data-group=\"' + group + '\"][value=\"' + ex + '\"]').prop('checked', false);
            });
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
      $(document).on('shiny:connected', function() {
        var slider = $('#left_right_scale').data('ionRangeSlider');
        if (slider) slider.update({grid_num: 10});
      });

      function startTreatmentTimer(seconds) {
        var btn = $('#goto_page12_from_9');
        var msg = $('#treatment_wait_msg');
        btn.prop('disabled', true);
        msg.show();
        setTimeout(function() {
          btn.prop('disabled', false);
          msg.hide();
          $('#treatment_intro_msg').hide();
        }, seconds * 1000);
      }

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
                  "goto_page1_from_0",
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
                selectInput(
                  "dropdown_municipality",
                  "¿No es correcto? Busque su dirección nuevamente o seleccione su municipio de la lista:",
                  choices = c(
                    "-- Seleccione un municipio --" = "",
                    muni_choices
                  ),
                  selected = "",
                  selectize = TRUE,
                  width = "100%"
                ),
                actionButton(
                  "clear_selection_btn",
                  "Limpiar selección",
                  icon = icon("refresh"),
                  class = "btn-secondary"
                )
              )
            ),

            hr(),
            fluidRow(
              column(
                12,
                align = "right",
                actionButton(
                  "goto_page2_from_1",
                  "Siguiente \u2192",
                  class = "btn-primary btn-lg"
                )
              )
            )
          )
        ),

        # Page 15: Benchmark Municipality Selection
        hidden(
          div(
            id = "page15",
            uiOutput("benchmark_instructions_text"),
            hr(),
            uiOutput("benchmark_list"),
            hr(),
            fluidRow(
              column(
                12,
                align = "right",
                actionButton(
                  "goto_page2_from_15",
                  "Siguiente \u2192",
                  class = "btn-primary btn-lg"
                )
              )
            )
          )
        ),

        # Page 18: Additional municipality selection (all-Mexico dropdown)
        hidden(
          div(
            id = "page18",
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
            fluidRow(
              column(
                12,
                align = "right",
                actionButton(
                  "goto_page4_from_18",
                  "Siguiente \u2192",
                  class = "btn-primary btn-lg"
                )
              )
            )
          )
        ),

        # Page 16: Screen-out (home municipality not governed by a main party)
        hidden(
          div(
            id = "page16",
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

        # Page 3: Practice Drag-and-Drop
        hidden(
          div(
            id = "page3",
            p(
              "La siguiente es una lista de fuentes de noticias que puede reorganizar arrastrando y soltando cada elemento."
            ),
            p(
              "Para asegurarnos de que está leyendo con atención, por favor clasifíquelas en este orden: ",
              "Radio primero, Sitios de noticias en línea segundo, Televisión tercero, Periódicos impresos cuarto, y Redes sociales último."
            ),
            tags$hr(),
            slot_ranker_ui(
              "practice_ranking",
              items = c(
                "Televisión",
                "Redes sociales",
                "Sitios de noticias en línea",
                "Radio",
                "Periódicos impresos"
              ),
              source_label = "Elementos disponibles:",
              slots_label = "Su clasificación:"
            ),
            tags$hr(),
            uiOutput("practice_warning"),
            fluidRow(
              column(
                12,
                align = "right",
                actionButton(
                  "goto_page4_from_3",
                  "Siguiente \u2192",
                  class = "btn-primary btn-lg"
                )
              )
            )
          )
        ),

        # Page 4: Political Views & Municipal Governance
        hidden(
          div(
            id = "page4",

            tags$div(
              class = "form-group",
              tags$label(
                "¿Por cuál partido o partidos votó en la última elección municipal? (seleccione todos los que correspondan)"
              ),
              tags$div(
                class = "party-radio-group",
                party_checkbox_choice(
                  "pan",
                  "PAN (Partido Acción Nacional)",
                  "PAN_logo.png"
                ),
                party_checkbox_choice(
                  "pri",
                  "PRI (Partido Revolucionario Institucional)",
                  "PRI_logo.png"
                ),
                party_checkbox_choice(
                  "prd",
                  "PRD (Partido de la Revolución Democrática)",
                  "PRD_logo.png"
                ),
                party_checkbox_choice(
                  "pvem",
                  "PVEM (Partido Verde Ecologista de México)",
                  "PVEM_logo.png"
                ),
                party_checkbox_choice(
                  "pt",
                  "PT (Partido del Trabajo)",
                  "PT_logo.png"
                ),
                party_checkbox_choice(
                  "mc",
                  "MC (Movimiento Ciudadano)",
                  "Movimiento_Ciudadano_logo.png"
                ),
                party_checkbox_choice("morena", "MORENA", "Morena_logo.png"),
                party_checkbox_choice("did_not_vote", "No voté"),
                party_checkbox_choice("dont_remember", "No recuerdo"),
                party_checkbox_choice("other", "Otro")
              )
            ),
            hidden(
              textInput(
                "last_election_vote_other",
                "Por favor, especifique el partido:",
                placeholder = "Ingrese el nombre del partido..."
              )
            ),

            hr(),

            sliderInput(
              "turnout_likelihood_pre",
              "¿Qué tan probable es que vote en las elecciones locales de 2027 en una escala de 0 a 100, donde 0 significa 'definitivamente no votaré' y 100 significa 'ciertamente votaré'?",
              min = 0,
              max = 100,
              value = 50
            ),
            fluidRow(
              column(
                6,
                p(
                  "Definitivamente no votaré",
                  style = "color: #6c757d; font-size: 0.85em; margin-top: -15px;"
                )
              ),
              column(
                6,
                p(
                  "Ciertamente votaré",
                  style = "color: #6c757d; font-size: 0.85em; margin-top: -15px; text-align: right;"
                )
              )
            ),

            br(),

            tags$div(
              class = "form-group",
              tags$label(
                "¿Por cuál partido o partidos tiene intención de votar en la próxima elección municipal? (seleccione todos los que correspondan)"
              ),
              tags$div(
                class = "party-radio-group",
                party_checkbox_choice(
                  "pan",
                  "PAN (Partido Acción Nacional)",
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
                  "PRD (Partido de la Revolución Democrática)",
                  "PRD_logo.png",
                  "vote_intention_pre"
                ),
                party_checkbox_choice(
                  "pvem",
                  "PVEM (Partido Verde Ecologista de México)",
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
                  "undecided",
                  "Indeciso",
                  group = "vote_intention_pre"
                ),
                party_checkbox_choice(
                  "will_not_vote",
                  "No votaré",
                  group = "vote_intention_pre"
                ),
                party_checkbox_choice(
                  "other",
                  "Otro",
                  group = "vote_intention_pre"
                )
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

            # Importance of Issues, language loosely based on Mitofsky
            # public opinion survey (see Google Drive reference)
            h5(strong(
              "\u00bfQu\u00e9 tan importantes son los siguientes temas para usted? Ord\u00e9nelos de m\u00e1s a menos importante."
            )),
            div(
              id = "issue_importance_wrapper",
              uiOutput("issue_importance_ui")
            ),
            uiOutput("mp_name_question_ui"),

            hr(),
            uiOutput("issue_importance_warning"),
            fluidRow(
              column(
                12,
                align = "right",
                actionButton(
                  "goto_page5_from_4",
                  "Siguiente →",
                  class = "btn-primary btn-lg"
                )
              )
            )
          )
        ),

        # Page 5: Your Municipality's Performance
        hidden(
          div(
            id = "page5",

            p(strong(
              "Las siguientes preguntas le pedir\u00e1n que eval\u00fae c\u00f3mo ciertos municipios y partidos \u201cmanejan\u201d la delincuencia no violenta, como los robos. \u201cManejar la delincuencia\u201d aqu\u00ed se refiere a los esfuerzos del gobierno para prevenir el crimen, hacer cumplir la ley y garantizar la seguridad p\u00fablica."
            )),

            uiOutput("home_crime_handling_pre_ui"),

            sliderInput(
              "morena_crime_rating",
              "En promedio, ¿qué tan bien cree que los municipios gobernados por MORENA, PT o PVEM manejan el crimen?",
              min = 0,
              max = 100,
              value = 50
            ),
            fluidRow(
              column(
                6,
                p(
                  "Maneja el crimen extremadamente mal",
                  style = "color: #6c757d; font-size: 0.85em; margin-top: -15px;"
                )
              ),
              column(
                6,
                p(
                  "Maneja el crimen extremadamente bien",
                  style = "color: #6c757d; font-size: 0.85em; margin-top: -15px; text-align: right;"
                )
              )
            ),

            sliderInput(
              "coalition_pan_pri_prd_crime_rating",
              "En promedio, ¿qué tan bien cree que los municipios gobernados por PAN, PRI o PRD manejan el crimen?",
              min = 0,
              max = 100,
              value = 50
            ),
            fluidRow(
              column(
                6,
                p(
                  "Maneja el crimen extremadamente mal",
                  style = "color: #6c757d; font-size: 0.85em; margin-top: -15px;"
                )
              ),
              column(
                6,
                p(
                  "Maneja el crimen extremadamente bien",
                  style = "color: #6c757d; font-size: 0.85em; margin-top: -15px; text-align: right;"
                )
              )
            ),

            sliderInput(
              "mc_crime_rating",
              "En promedio, ¿qué tan bien cree que los municipios gobernados por MC manejan el crimen?",
              min = 0,
              max = 100,
              value = 50
            ),
            fluidRow(
              column(
                6,
                p(
                  "Maneja el crimen extremadamente mal",
                  style = "color: #6c757d; font-size: 0.85em; margin-top: -15px;"
                )
              ),
              column(
                6,
                p(
                  "Maneja el crimen extremadamente bien",
                  style = "color: #6c757d; font-size: 0.85em; margin-top: -15px; text-align: right;"
                )
              )
            ),

            hr(),

            uiOutput("nearest5_governance_ui"),

            hr(),
            fluidRow(
              column(
                12,
                align = "right",
                actionButton(
                  "goto_page6_from_5",
                  "Siguiente \u2192",
                  class = "btn-primary btn-lg"
                )
              )
            )
          )
        ),

        # Page 17: Robbery estimate
        hidden(
          div(
            id = "page17",
            p(
              "Las tasas de robo se miden por cada 100,000 habitantes para que los municipios de diferentes tamaños puedan compararse de manera justa.",
              "Por ejemplo, un municipio con 1,000 robos y 2 millones de habitantes tiene una tasa de criminalidad más baja que uno con 500 robos",
              "y solo 100,000 habitantes, aunque haya registrado más robos en términos absolutos.",
              "En 2025,",
              strong(
                "la mitad de todos los municipios tuvo menos de 79 robos por cada 100,000 personas, y la otra mitad tuvo más."
              )
            ),
            uiOutput("robbery_estimate_ui"),
            hr(),
            fluidRow(
              column(
                12,
                align = "right",
                actionButton(
                  "goto_page6_from_17",
                  "Siguiente \u2192",
                  class = "btn-primary btn-lg"
                )
              )
            )
          )
        ),

        # Page 6: About You
        hidden(
          div(
            id = "page6",

            h5(strong("Opiniones Políticas")),

            sliderInput(
              "left_right_scale",
              "En política, a veces se habla de ser 'izquierda' o 'derecha' en una escala ideológica, donde más a la derecha significa más conservador.
              ¿Dónde se ubicaría usted en esta escala?",
              min = 0,
              max = 10,
              value = 5,
              step = 1
            ),
            fluidRow(
              column(
                6,
                p(
                  "Izquierda",
                  style = "color: #6c757d; font-size: 0.85em; margin-top: -15px;"
                )
              ),
              column(
                6,
                p(
                  "Derecha",
                  style = "color: #6c757d; font-size: 0.85em; margin-top: -15px; text-align: right;"
                )
              )
            ),

            hr(),
            fluidRow(
              column(
                12,
                align = "right",
                actionButton(
                  "goto_page11_from_6",
                  "Siguiente \u2192",
                  class = "btn-primary btn-lg"
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

        # Page 10: Thank you screen
        hidden(
          div(
            id = "page10",
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

        # Page 11: Wave 1 / Wave 2 boundary marker (development placeholder)
        hidden(
          div(
            id = "page11",
            div(
              style = paste0(
                "background: #fff3cd; border: 2px dashed #ffc107; border-radius: 8px; ",
                "padding: 24px; margin-bottom: 24px;"
              ),
              h4(
                icon("flag"),
                " Fin de la Fase 1",
                style = "color: #856404; margin-top: 0;"
              ),
              p(
                strong("Nota para investigadores:"),
                " En el estudio en vivo, la encuesta termina aquí y los participantes ",
                "serán recontactados para la próxima fase."
              )
            ),
            hr(),
            fluidRow(
              column(
                12,
                align = "right",
                actionButton(
                  "goto_page7_from_11",
                  "Continuar a la Fase 2 \u2192",
                  class = "btn-warning btn-lg"
                )
              )
            )
          )
        ),

        # Page 14: Wave 2 — Attention check
        hidden(
          div(
            id = "page14",
            radioButtons(
              "attention_check",
              paste(
                "La agricultura sostenible es un tema importante para muchos mexicanos.",
                "Queremos asegurarnos de que está leyendo cada pregunta con atención.",
                "Para confirmar que está leyendo con atención, por favor seleccione 'De acuerdo en parte' a continuación."
              ),
              choices = c(
                "Totalmente en desacuerdo" = "strongly_disagree",
                "En desacuerdo en parte" = "somewhat_disagree",
                "Ni de acuerdo ni en desacuerdo" = "neutral",
                "De acuerdo en parte" = "somewhat_agree",
                "Totalmente de acuerdo" = "strongly_agree"
              ),
              selected = character(0)
            ),
            hr(),
            uiOutput("attention_warning"),
            fluidRow(
              column(
                12,
                align = "right",
                actionButton(
                  "goto_page7_from_14",
                  "Siguiente \u2192",
                  class = "btn-primary btn-lg"
                )
              )
            )
          )
        ),

        # Page 7: Wave 2 — Governance Grid (pre-treatment)
        hidden(
          div(
            id = "page7",
            uiOutput("governance_grid_ui"),
            hr(),
            fluidRow(
              column(
                12,
                align = "right",
                actionButton(
                  "goto_page8_from_7",
                  "Siguiente \u2192",
                  class = "btn-primary btn-lg"
                )
              )
            )
          )
        ),

        # Page 8: Wave 2 — Pre-treatment municipality crime ranking
        hidden(
          div(
            id = "page8",
            uiOutput("municipality_ranking_ui"),
            hr(),
            fluidRow(
              column(
                12,
                align = "right",
                actionButton(
                  "goto_page9_from_8",
                  "Siguiente \u2192",
                  class = "btn-primary btn-lg"
                )
              )
            )
          )
        ),

        # Page 9: Wave 2 — Treatment delivery
        hidden(
          div(
            id = "page9",
            p(
              id = "treatment_intro_msg",
              style = "font-style: italic; color: #555;",
              "Por favor, lea la información a continuación. Podrá continuar en breve."
            ),
            uiOutput("treatment_content_ui"),
            hr(),
            p(
              id = "treatment_wait_msg",
              style = "display: none; color: #555; font-style: italic;",
              "Por favor, no continúe hasta que haya leído la información anterior. Podrá proceder en breve."
            ),
            fluidRow(
              column(
                12,
                align = "right",
                actionButton(
                  "goto_page12_from_9",
                  "Siguiente \u2192",
                  class = "btn-primary btn-lg"
                )
              )
            )
          )
        ),

        # Page 12: Wave 2 — Post-treatment crime questions
        hidden(
          div(
            id = "page12",
            p(strong(
              "Las siguientes preguntas le pedir\u00e1n que eval\u00fae c\u00f3mo ciertos municipios y partidos \u201cmanejan\u201d la delincuencia no violenta, como los robos. \u201cManejar la delincuencia\u201d aqu\u00ed se refiere a los esfuerzos del gobierno para prevenir el crimen, hacer cumplir la ley y garantizar la seguridad p\u00fablica."
            )),
            uiOutput("home_crime_handling_post_ui"),
            sliderInput(
              "morena_crime_rating_post",
              "En promedio, ¿qué tan bien cree que los municipios gobernados por MORENA, PT o PVEM manejan el crimen?",
              min = 0,
              max = 100,
              value = 50
            ),
            fluidRow(
              column(
                6,
                p(
                  "Maneja el crimen extremadamente mal",
                  style = "color: #6c757d; font-size: 0.85em; margin-top: -15px;"
                )
              ),
              column(
                6,
                p(
                  "Maneja el crimen extremadamente bien",
                  style = "color: #6c757d; font-size: 0.85em; margin-top: -15px; text-align: right;"
                )
              )
            ),
            sliderInput(
              "coalition_pan_pri_prd_crime_rating_post",
              "En promedio, ¿qué tan bien cree que los municipios gobernados por PAN, PRI o PRD manejan el crimen?",
              min = 0,
              max = 100,
              value = 50
            ),
            fluidRow(
              column(
                6,
                p(
                  "Maneja el crimen extremadamente mal",
                  style = "color: #6c757d; font-size: 0.85em; margin-top: -15px;"
                )
              ),
              column(
                6,
                p(
                  "Maneja el crimen extremadamente bien",
                  style = "color: #6c757d; font-size: 0.85em; margin-top: -15px; text-align: right;"
                )
              )
            ),

            sliderInput(
              "mc_crime_rating_post",
              "En promedio, ¿qué tan bien cree que los municipios gobernados por MC manejan el crimen?",
              min = 0,
              max = 100,
              value = 50
            ),
            fluidRow(
              column(
                6,
                p(
                  "Maneja el crimen extremadamente mal",
                  style = "color: #6c757d; font-size: 0.85em; margin-top: -15px;"
                )
              ),
              column(
                6,
                p(
                  "Maneja el crimen extremadamente bien",
                  style = "color: #6c757d; font-size: 0.85em; margin-top: -15px; text-align: right;"
                )
              )
            ),
            hr(),
            uiOutput("municipality_ranking_post_ui"),
            hr(),
            fluidRow(
              column(
                12,
                align = "right",
                actionButton(
                  "goto_page13_from_12",
                  "Siguiente \u2192",
                  class = "btn-primary btn-lg"
                )
              )
            )
          )
        ),

        # Page 13: Wave 2 — Post-treatment turnout + vote intention
        hidden(
          div(
            id = "page13",
            sliderInput(
              "turnout_likelihood",
              "¿Qué tan probable es que vote en las elecciones locales de 2027 en una escala de 0 a 100, donde 0 significa 'definitivamente no votaré' y 100 significa 'ciertamente votaré'?",
              min = 0,
              max = 100,
              value = 50
            ),
            fluidRow(
              column(
                6,
                p(
                  "Definitivamente no votaré",
                  style = "color: #6c757d; font-size: 0.85em; margin-top: -15px;"
                )
              ),
              column(
                6,
                p(
                  "Ciertamente votaré",
                  style = "color: #6c757d; font-size: 0.85em; margin-top: -15px; text-align: right;"
                )
              )
            ),
            br(),
            tags$div(
              class = "form-group",
              tags$label(
                "¿Por cuál partido o partidos tiene intención de votar en la próxima elección municipal? (seleccione todos los que correspondan)"
              ),
              tags$div(
                class = "party-radio-group",
                party_checkbox_choice(
                  "pan",
                  "PAN (Partido Acci\u00f3n Nacional)",
                  "PAN_logo.png",
                  "vote_intention_2027"
                ),
                party_checkbox_choice(
                  "pri",
                  "PRI (Partido Revolucionario Institucional)",
                  "PRI_logo.png",
                  "vote_intention_2027"
                ),
                party_checkbox_choice(
                  "prd",
                  "PRD (Partido de la Revoluci\u00f3n Democr\u00e1tica)",
                  "PRD_logo.png",
                  "vote_intention_2027"
                ),
                party_checkbox_choice(
                  "pvem",
                  "PVEM (Partido Verde Ecologista de M\u00e9xico)",
                  "PVEM_logo.png",
                  "vote_intention_2027"
                ),
                party_checkbox_choice(
                  "pt",
                  "PT (Partido del Trabajo)",
                  "PT_logo.png",
                  "vote_intention_2027"
                ),
                party_checkbox_choice(
                  "mc",
                  "MC (Movimiento Ciudadano)",
                  "Movimiento_Ciudadano_logo.png",
                  "vote_intention_2027"
                ),
                party_checkbox_choice(
                  "morena",
                  "MORENA",
                  "Morena_logo.png",
                  "vote_intention_2027"
                ),
                party_checkbox_choice(
                  "undecided",
                  "Indeciso",
                  group = "vote_intention_2027"
                ),
                party_checkbox_choice(
                  "will_not_vote",
                  "No votaré",
                  group = "vote_intention_2027"
                ),
                party_checkbox_choice(
                  "other",
                  "Otro",
                  group = "vote_intention_2027"
                )
              )
            ),
            hidden(
              textInput(
                "vote_intention_2027_other",
                "Por favor, especifique el partido:",
                placeholder = "Ingrese el nombre del partido..."
              )
            ),
            hr(),
            fluidRow(
              column(
                12,
                align = "right",
                actionButton(
                  "submit",
                  "Enviar encuesta",
                  class = "btn-success btn-lg",
                  icon = icon("check")
                )
              )
            )
          )
        )
      )
    )
  )
)

server <- function(input, output, session) {
  # Helper function to find municipality's robbery category from bucket inputs
  # Returns "more_than_double", "more", "same", "fewer", "less_than_half", or NA
  make_crime_ranking_grid <- function(home_name, comp_labels, prefix) {
    col_labels <- c(
      "more_than_double" = "Más del doble",
      "more" = "Más pero menos del doble",
      "same" = "Aproximadamente igual",
      "fewer" = "Menos pero más de la mitad",
      "less_than_half" = "Menos de la mitad"
    )
    header_cells <- tagList(
      tags$th(
        "Municipio",
        style = "text-align: left; padding: 6px 10px; min-width: 160px;"
      ),
      lapply(names(col_labels), function(val) {
        tags$th(
          col_labels[[val]],
          style = "text-align: center; padding: 6px 8px; font-size: 0.85em; vertical-align: bottom;"
        )
      })
    )
    body_rows <- lapply(seq_along(comp_labels), function(i) {
      input_id <- paste0(prefix, i)
      row_style <- if (i %% 2 == 0) "background-color: #f9f9f9;" else ""
      tags$tr(
        style = row_style,
        tags$td(comp_labels[i], style = "padding: 6px 10px; font-weight: 500;"),
        lapply(names(col_labels), function(val) {
          tags$td(
            style = "text-align: center; padding: 6px 8px;",
            tags$input(
              type = "radio",
              name = input_id,
              value = val,
              onclick = sprintf(
                "Shiny.setInputValue('%s', this.value, {priority: 'event'});",
                input_id
              )
            )
          )
        })
      )
    })
    tagList(
      p(
        "Comparado con ",
        strong(home_name),
        ", ¿cómo cree que es el número de robos en estos municipios?"
      ),
      div(
        class = "mobile-only",
        p(
          em(
            "\u21d0 Puede que necesite desplácese hacia los lados para ver todas las opciones \u21d2"
          ),
          style = "text-align: center; color: #555; margin-bottom: 4px;"
        )
      ),
      tags$div(
        style = "overflow-x: auto; margin-top: 10px; border: 1px solid #ccc; border-radius: 4px; padding: 4px;",
        tags$table(
          style = "border-collapse: collapse; width: 100%;",
          tags$thead(
            style = "border-bottom: 2px solid #ddd;",
            tags$tr(header_cells)
          ),
          tags$tbody(body_rows)
        )
      )
    )
  }

  # Reactive values
  selected_map_munis <- reactiveVal(character())
  found_municipality <- reactiveVal(NULL)
  found_address_coords <- reactiveVal(NULL)
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
  comparison_municipalities <- reactiveVal(NULL) # Stores comparison muni data for ranking & graph
  comp_munis_nonpartisan_rv <- reactiveVal(NULL) # Stores non-partisan comparison munis
  comp_munis_same_coalition_rv <- reactiveVal(NULL) # Stores same-coalition comparison munis
  mc_opp_coalition_rv <- reactiveVal(NULL) # Randomly chosen opposition coalition for MC home municipalities
  benchmark_candidates <- reactiveVal(NULL) # 15 candidate munis shown on page 15
  nearest5_rv <- reactiveVal(NULL) # 5 geographically nearest municipalities (Wave 1 governance prior)
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

    candidates <- d_geo %>%
      st_drop_geometry() %>%
      filter(muni_id %in% sample(c(pool1, pool2, pool3))) %>%
      select(muni_id, NOMGEO, NOM_ENT)

    benchmark_candidates(candidates)
    selected_benchmarks(character())

    # Nearest 5 by geographic distance for Wave 1 governance prior question
    nn5 <- nearest10 %>%
      filter(muni_id == home_id, rank <= 5) %>%
      arrange(rank)
    nearest5_rv(nn5)
  })

  # Randomly assign treatment group for this respondent at session start
  treatment_group <- reactiveVal(
    sample(c("control", "T1", "T2", "T3", "T4", "control2"), 1)
  )

  # For control/T1, randomly assign which type of comparison municipalities
  # are shown in the governance grid and crime ranking (independent of treatment)
  control_t1_comp_type <- reactiveVal(
    sample(c("nonpartisan", "opposite", "same"), 1)
  )
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

  # Quota full: set SURVEY_QUOTA_FULL=true in .Renviron to redirect all new visitors
  observe({
    if (Sys.getenv("SURVEY_QUOTA_FULL") == "true") {
      ticket <- if (!is.null(netquest_pid) && nchar(netquest_pid) > 0) {
        netquest_pid
      } else {
        ""
      }
      shinyjs::runjs(sprintf(
        'window.location.href = "https://transit.nicequest.com/transit/participation?tp=qf_1&c=ok&ticket=%s"',
        ticket
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

  # Populate all-Mexico dropdown for page 18
  updateSelectizeInput(
    session,
    "additional_munis_dropdown",
    choices = muni_choices,
    server = TRUE
  )

  # Geocode address and find municipality
  observeEvent(input$geocode_btn, {
    req(input$address)

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
                tags$strong(paste(
                  " Encontrado: ",
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

  # Set comparison municipalities (T3: opposite-coalition) when home municipality is found
  observeEvent(found_municipality(), {
    home_id <- found_municipality()
    req(!is.null(home_id))

    home_party <- d_geo %>%
      st_drop_geometry() %>%
      filter(muni_id == home_id) %>%
      pull(governing_party) %>%
      `[`(1)

    if (!is.na(home_party) && home_party == "MC") {
      opp <- sample(list(coalition_a, coalition_b), 1)[[1]]
      mc_opp_coalition_rv(opp)
      opposite_coalition <- opp
    } else {
      mc_opp_coalition_rv(NULL)
      opposite_coalition <- get_opposite_parties(home_party)
    }

    candidates <- d_geo %>%
      st_drop_geometry() %>%
      filter(
        governing_party %in% opposite_coalition,
        muni_id != home_id,
        !is.na(POB_TOTAL),
        !is.na(area_km2)
      ) %>%
      select(
        muni_id,
        NOMGEO,
        NOM_ENT,
        POB_TOTAL,
        area_km2,
        centroid_lon,
        centroid_lat
      )

    if (nrow(candidates) == 0) {
      candidates <- d_geo %>%
        st_drop_geometry() %>%
        filter(
          muni_id != home_id,
          !is.na(POB_TOTAL),
          !is.na(area_km2)
        ) %>%
        select(
          muni_id,
          NOMGEO,
          NOM_ENT,
          POB_TOTAL,
          area_km2,
          centroid_lon,
          centroid_lat
        )
    }

    comp_munis <- mahal_match_munis(home_id, candidates) %>%
      select(muni_id, NOMGEO, NOM_ENT)
    comparison_municipalities(comp_munis)
  })

  # Set non-partisan comparison municipalities (T2) when home municipality is found
  observeEvent(found_municipality(), {
    home_id <- found_municipality()
    req(!is.null(home_id))

    candidates <- d_geo %>%
      st_drop_geometry() %>%
      filter(
        muni_id != home_id,
        !is.na(POB_TOTAL),
        !is.na(area_km2)
      ) %>%
      select(
        muni_id,
        NOMGEO,
        NOM_ENT,
        POB_TOTAL,
        area_km2,
        centroid_lon,
        centroid_lat
      )

    comp_munis_np <- mahal_match_munis(home_id, candidates) %>%
      select(muni_id, NOMGEO, NOM_ENT)
    comp_munis_nonpartisan_rv(comp_munis_np)
  })

  # Set same-coalition comparison municipalities (T4) when home municipality is found
  observeEvent(found_municipality(), {
    home_id <- found_municipality()
    req(!is.null(home_id))

    home_party <- d_geo %>%
      st_drop_geometry() %>%
      filter(muni_id == home_id) %>%
      pull(governing_party) %>%
      `[`(1)

    same_coalition <- get_same_coalition_parties(home_party)

    candidates <- d_geo %>%
      st_drop_geometry() %>%
      filter(
        governing_party %in% same_coalition,
        muni_id != home_id,
        !is.na(POB_TOTAL),
        !is.na(area_km2)
      ) %>%
      select(
        muni_id,
        NOMGEO,
        NOM_ENT,
        POB_TOTAL,
        area_km2,
        centroid_lon,
        centroid_lat
      )

    if (nrow(candidates) == 0) {
      candidates <- d_geo %>%
        st_drop_geometry() %>%
        filter(
          muni_id != home_id,
          !is.na(POB_TOTAL),
          !is.na(area_km2)
        ) %>%
        select(
          muni_id,
          NOMGEO,
          NOM_ENT,
          POB_TOTAL,
          area_km2,
          centroid_lon,
          centroid_lat
        )
    }

    comp_munis_sc <- mahal_match_munis(home_id, candidates) %>%
      select(muni_id, NOMGEO, NOM_ENT)
    comp_munis_same_coalition_rv(comp_munis_sc)
  })

  # Return the comparison municipalities appropriate for the assigned treatment group.
  active_comp_munis <- reactive({
    tg <- treatment_group()
    if (tg %in% c("T2", "control2")) {
      comp_munis_nonpartisan_rv()
    } else if (tg == "T3") {
      comparison_municipalities()
    } else if (tg == "T4") {
      comp_munis_same_coalition_rv()
    } else {
      switch(
        control_t1_comp_type(),
        "nonpartisan" = comp_munis_nonpartisan_rv(),
        "opposite" = comparison_municipalities(),
        "same" = comp_munis_same_coalition_rv()
      )
    }
  })

  # Show appropriate page and scroll to top
  observe({
    pages <- c(paste0("page", 0:18))
    lapply(pages, hide)
    shinyjs::show(paste0("page", current_page()))
    runjs("window.scrollTo(0, 0);")
  })

  # Show/hide home confirmation section based on whether municipality is found
  observe({
    if (!is.null(found_municipality())) {
      shinyjs::show("home_confirmation_section")
      updateSelectInput(
        session,
        "dropdown_municipality",
        selected = found_municipality()
      )
    } else {
      shinyjs::hide("home_confirmation_section")
    }
  })

  output$mp_name_question_ui <- renderUI({
    mc <- mp_mc_data[[found_municipality()]]
    if (is.null(mc)) {
      return(NULL)
    }
    fixed <- c(mc$correct, mc$runner_up[!is.na(mc$runner_up)], mc$pool)
    choices <- c(sample(fixed), "No sé")
    radioButtons(
      "municipal_president_mc",
      label = "¿Cuál de las siguientes personas es el/la presidente/a municipal de su municipio?",
      choices = choices,
      selected = character(0)
    )
  })

  observeEvent(
    input$last_election_vote,
    {
      toggleElement(
        "last_election_vote_other",
        condition = "other" %in% input$last_election_vote
      )
    },
    ignoreNULL = FALSE
  )

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
      shinyjs::delay(3000, enable("goto_page2_from_1"))
    } else {
      disable("goto_page2_from_1")
    }
  })

  # Page navigation
  # Page 0 → Page 1 (Information Sheet → Find Municipality)
  observeEvent(input$goto_page1_from_0, {
    current_page(1)
  })

  # Page 1 → Page 3 (Practice ranking), or Page 16 (screen-out) if home muni is in an
  # excluded state (CDMX=09, Durango=10, Oaxaca=20, Veracruz=30) or has no main-party government
  excluded_state_codes <- c("09", "10", "20", "30")

  observeEvent(input$goto_page2_from_1, {
    home_id <- found_municipality()
    state_code <- substr(home_id, 1, 2)
    home_party <- d_geo %>%
      st_drop_geometry() %>%
      filter(muni_id == home_id) %>%
      pull(governing_party) %>%
      `[`(1)
    if (state_code %in% excluded_state_codes || is.na(home_party)) {
      current_page(16)
    } else {
      current_page(3)
    }
  })

  # Screen-out redirect to Netquest
  observeEvent(current_page(), {
    if (
      current_page() == 16 && !is.null(netquest_pid) && nchar(netquest_pid) > 0
    ) {
      shinyjs::runjs(sprintf(
        'setTimeout(function(){ window.location.href = "https://transit.nicequest.com/transit/participation?tp=fo_0&c=ok&ticket=%s"; }, 1500)',
        netquest_pid
      ))
    }
  })

  # Page 15 → Page 18 (benchmark → additional municipality dropdown)
  observeEvent(input$goto_page2_from_15, {
    current_page(18)
  })

  # Page 18 → Page 4 (additional municipality dropdown → issue importance ranking)
  observeEvent(input$goto_page4_from_18, {
    additional <- input$additional_munis_dropdown
    if (!is.null(additional) && length(additional) > 0) {
      selected_benchmarks(unique(c(selected_benchmarks(), additional)))
    }
    current_page(4)
  })

  # Page 2 → Page 1 (commented out with page 2)
  # observeEvent(input$goto_page1_from_3, { current_page(1) })

  # Page 3 → Page 15 (practice ranking → benchmark selection)
  practice_warning_msg <- reactiveVal(NULL)
  output$practice_warning <- renderUI({
    msg <- practice_warning_msg()
    if (!is.null(msg)) p(style = "color: #c0392b; font-weight: bold;", msg)
  })
  observeEvent(input$goto_page4_from_3, {
    ranking <- input$practice_ranking
    if (is.null(ranking) || length(ranking) < 5) {
      practice_warning_msg(
        "Por favor, clasifique todos los elementos antes de continuar."
      )
      return()
    }
    if (ranking[1] != "Radio" || ranking[5] != "Redes sociales") {
      ticket <- if (!is.null(netquest_pid) && nchar(netquest_pid) > 0) {
        netquest_pid
      } else {
        ""
      }
      shinyjs::runjs(sprintf(
        'window.location.href = "https://transit.nicequest.com/transit/participation?tp=fo_0&c=ok&ticket=%s"',
        ticket
      ))
      return()
    }
    practice_warning_msg(NULL)
    current_page(15)
  })

  # Page 2 → Page 3 (commented out with page 2)
  # observeEvent(input$goto_page3_from_2, { current_page(3) })

  # Page 3 → Page 4
  issue_importance_warning_msg <- reactiveVal(NULL)
  output$issue_importance_warning <- renderUI({
    msg <- issue_importance_warning_msg()
    if (!is.null(msg)) p(style = "color: #c0392b; font-weight: bold;", msg)
  })
  observeEvent(input$goto_page5_from_4, {
    current_page(5)
  })

  # Page 4 → Page 3
  observeEvent(input$goto_page4_from_5, {
    current_page(4)
  })

  # Page 5 → Page 17 (robbery estimate)
  observeEvent(input$goto_page6_from_5, {
    current_page(17)
  })

  # Page 17 → Page 6
  observeEvent(input$goto_page6_from_17, {
    current_page(6)
  })

  # Page 6 → Page 11 (Wave 1/Wave 2 boundary marker)
  observeEvent(input$goto_page11_from_6, {
    current_page(11)
  })

  # Page 5 → Page 4
  observeEvent(input$goto_page5_from_6, {
    current_page(5)
  })

  # Page 11 → Page 14 (Wave 2 start: attention check)
  observeEvent(input$goto_page7_from_11, {
    current_page(14)
  })

  # Page 14 → Page 7 (attention check → governance grid)
  attention_warning_msg <- reactiveVal(NULL)
  output$attention_warning <- renderUI({
    msg <- attention_warning_msg()
    if (!is.null(msg)) p(style = "color: #c0392b; font-weight: bold;", msg)
  })
  observeEvent(input$goto_page7_from_14, {
    if (is.null(input$attention_check) || length(input$attention_check) == 0) {
      attention_warning_msg(
        "Por favor, seleccione una respuesta antes de continuar."
      )
      return()
    }
    if (input$attention_check != "somewhat_agree") {
      ticket <- if (!is.null(netquest_pid) && nchar(netquest_pid) > 0) {
        netquest_pid
      } else {
        ""
      }
      shinyjs::runjs(sprintf(
        'window.location.href = "https://transit.nicequest.com/transit/participation?tp=fo_0&c=ok&ticket=%s"',
        ticket
      ))
      return()
    }
    attention_warning_msg(NULL)
    current_page(7)
  })

  # Page 7 → Page 8 (governance grid → pre-treatment ranking)
  observeEvent(input$goto_page8_from_7, {
    current_page(8)
  })

  # Page 8 → Page 9 (pre-treatment ranking → treatment, with timer)
  observeEvent(input$goto_page9_from_8, {
    current_page(9)
    duration <- switch(
      treatment_group(),
      "control" = 30,
      "T1" = 30,
      "T2" = 45,
      "T3" = 45,
      "T4" = 45,
      "control2" = 45
    )
    runjs(paste0("startTreatmentTimer(", duration, ");"))
  })

  # Page 9 → Page 12 (treatment → post-treatment)
  observeEvent(input$goto_page12_from_9, {
    current_page(12)
  })

  # Page 12 → Page 13 (post-treatment crime → turnout/vote)
  observeEvent(input$goto_page13_from_12, {
    current_page(13)
  })

  # vote_intention_2027 "other" toggle
  observeEvent(
    input$vote_intention_2027,
    {
      toggleElement(
        "vote_intention_2027_other",
        condition = "other" %in% input$vote_intention_2027
      )
    },
    ignoreNULL = FALSE
  )

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
    updateTextInput(session, "address", value = "")
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
      "your municipality"
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
          " maneja el crimen?"
        ),
        min = 0,
        max = 100,
        value = 50
      ),
      fluidRow(
        column(
          6,
          p(
            "Maneja el crimen extremadamente mal",
            style = "color: #6c757d; font-size: 0.85em; margin-top: -15px;"
          )
        ),
        column(
          6,
          p(
            "Maneja el crimen extremadamente bien",
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
      "your municipality"
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

  # Benchmark instructions text with dynamic municipality name (page 15)
  output$benchmark_instructions_text <- renderUI({
    req(!is.null(found_municipality()))
    muni_data <- d_geo %>%
      st_drop_geometry() %>%
      filter(muni_id == found_municipality())
    muni_name <- paste0(muni_data$NOMGEO, ", ", muni_data$NOM_ENT)
    tagList(
      p(strong(
        "Los gobiernos a menudo son evaluados comparándolos con otros. ",
        "Por ejemplo, se puede evaluar si un gobierno está haciendo un buen trabajo comparándolo con lo que logran otros municipios. ",
        paste0(
          "Si pudiera elegir con qué municipios comparar a ",
          muni_name,
          ", ¿cuál elegiría de esta lista?"
        )
      )),
      p(
        "Si tiene en mente algún municipio que no figura en esta lista, tendrá la oportunidad de elegir otros municipios en la página siguiente."
      )
    )
  })

  # Additional municipality instructions text (page 18)
  output$additional_munis_instructions_text <- renderUI({
    req(!is.null(found_municipality()))
    muni_data <- d_geo %>%
      st_drop_geometry() %>%
      filter(muni_id == found_municipality())
    muni_name <- paste0(muni_data$NOMGEO, ", ", muni_data$NOM_ENT)
    tagList(
      p(strong(paste0(
        "Si tiene en mente algún otro municipio con el que le gustaría comparar a ",
        muni_name,
        ", puede buscarlo y seleccionarlo a continuación. Usted puede elegir varios."
      ))),
      p(
        "Si ya eligió todos los municipios que deseaba en la página anterior, puede continuar."
      )
    )
  })

  # Benchmark municipality checklist (page 15)
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

  # Submit button - now includes page 3 survey responses
  observeEvent(input$submit, {
    map_munis <- selected_map_munis()
    map_munis_names <- if (length(map_munis) > 0) {
      d_geo %>%
        st_drop_geometry() %>%
        filter(muni_id %in% map_munis) %>%
        mutate(full_name = paste0(NOMGEO, ", ", NOM_ENT)) %>%
        pull(full_name) %>%
        paste(collapse = "; ")
    } else {
      ""
    }

    # Get found municipality name
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
      Map_Selected_Municipalities = map_munis_names,
      # Demographics
      Age = ifelse(
        is.null(input$age),
        NA_integer_,
        input$age
      ),
      Gender = ifelse(
        is.null(input$gender) || input$gender == "",
        NA_character_,
        input$gender
      ),
      Indigenous = ifelse(
        is.null(input$indigenous) || input$indigenous == "",
        NA_character_,
        input$indigenous
      ),
      Attention_Check = ifelse(
        is.null(input$attention_check) || length(input$attention_check) == 0,
        NA_character_,
        input$attention_check
      ),
      Municipal_President_Name = ifelse(
        is.null(input$municipal_president_mc) ||
          length(input$municipal_president_mc) == 0,
        NA_character_,
        input$municipal_president_mc
      ),
      Left_Right_Scale = input$left_right_scale,
      Municipal_President_Correct = {
        mc <- mp_mc_data[[found_municipality()]]
        if (
          !is.null(mc) &&
            !is.null(input$municipal_president_mc) &&
            length(input$municipal_president_mc) > 0
        ) {
          input$municipal_president_mc == mc$correct
        } else {
          NA
        }
      },
      Last_Election_Vote = if (
        is.null(input$last_election_vote) ||
          length(input$last_election_vote) == 0
      ) {
        NA_character_
      } else {
        paste(input$last_election_vote, collapse = ";")
      },
      Last_Election_Vote_Other = ifelse(
        is.null(input$last_election_vote_other) ||
          input$last_election_vote_other == "",
        NA_character_,
        input$last_election_vote_other
      ),
      Home_Governing_Party_Belief = {
        val <- input$home_governing_party_belief
        if (is.null(val) || length(val) == 0) {
          NA_character_
        } else {
          paste(val, collapse = ";")
        }
      },
      Comp_Governing_Party_Belief_1 = {
        val <- input$comp_governing_party_1
        if (is.null(val) || length(val) == 0) {
          NA_character_
        } else {
          paste(val, collapse = ";")
        }
      },
      Comp_Governing_Party_Belief_2 = {
        val <- input$comp_governing_party_2
        if (is.null(val) || length(val) == 0) {
          NA_character_
        } else {
          paste(val, collapse = ";")
        }
      },
      Comp_Governing_Party_Belief_3 = {
        val <- input$comp_governing_party_3
        if (is.null(val) || length(val) == 0) {
          NA_character_
        } else {
          paste(val, collapse = ";")
        }
      },
      Comp_Governing_Party_Belief_4 = {
        val <- input$comp_governing_party_4
        if (is.null(val) || length(val) == 0) {
          NA_character_
        } else {
          paste(val, collapse = ";")
        }
      },
      # Nearest-5 governance priors (Wave 1, page 5)
      Nearest5_Muni_1_ID = if (
        !is.null(nearest5_rv()) && nrow(nearest5_rv()) >= 1
      ) {
        nearest5_rv()$neighbor_id[1]
      } else {
        NA_character_
      },
      Nearest5_Muni_2_ID = if (
        !is.null(nearest5_rv()) && nrow(nearest5_rv()) >= 2
      ) {
        nearest5_rv()$neighbor_id[2]
      } else {
        NA_character_
      },
      Nearest5_Muni_3_ID = if (
        !is.null(nearest5_rv()) && nrow(nearest5_rv()) >= 3
      ) {
        nearest5_rv()$neighbor_id[3]
      } else {
        NA_character_
      },
      Nearest5_Muni_4_ID = if (
        !is.null(nearest5_rv()) && nrow(nearest5_rv()) >= 4
      ) {
        nearest5_rv()$neighbor_id[4]
      } else {
        NA_character_
      },
      Nearest5_Muni_5_ID = if (
        !is.null(nearest5_rv()) && nrow(nearest5_rv()) >= 5
      ) {
        nearest5_rv()$neighbor_id[5]
      } else {
        NA_character_
      },
      Nearest5_Governing_Party_Belief_1 = {
        val <- input$nearest5_governing_party_1
        if (is.null(val) || length(val) == 0) {
          NA_character_
        } else {
          paste(val, collapse = ";")
        }
      },
      Nearest5_Governing_Party_Belief_2 = {
        val <- input$nearest5_governing_party_2
        if (is.null(val) || length(val) == 0) {
          NA_character_
        } else {
          paste(val, collapse = ";")
        }
      },
      Nearest5_Governing_Party_Belief_3 = {
        val <- input$nearest5_governing_party_3
        if (is.null(val) || length(val) == 0) {
          NA_character_
        } else {
          paste(val, collapse = ";")
        }
      },
      Nearest5_Governing_Party_Belief_4 = {
        val <- input$nearest5_governing_party_4
        if (is.null(val) || length(val) == 0) {
          NA_character_
        } else {
          paste(val, collapse = ";")
        }
      },
      Nearest5_Governing_Party_Belief_5 = {
        val <- input$nearest5_governing_party_5
        if (is.null(val) || length(val) == 0) {
          NA_character_
        } else {
          paste(val, collapse = ";")
        }
      },
      # Issue importance rankings (from drag-and-drop rank_list)
      # Convert ordered list to numeric ranks (1 = most important)
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
      Turnout_Likelihood_Pre = input$turnout_likelihood_pre,
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
      # Treatment assignment
      Treatment_Group = treatment_group(),
      Control_T1_Comp_Type = ifelse(
        treatment_group() %in% c("control", "T1"),
        control_t1_comp_type(),
        NA_character_
      ),
      # Comparison municipalities
      Comparison_Muni_1 = {
        comp <- active_comp_munis()
        if (is.null(comp) || nrow(comp) < 1) {
          NA_character_
        } else {
          paste0(comp$NOMGEO[1], ", ", comp$NOM_ENT[1])
        }
      },
      Comparison_Muni_2 = {
        comp <- active_comp_munis()
        if (is.null(comp) || nrow(comp) < 2) {
          NA_character_
        } else {
          paste0(comp$NOMGEO[2], ", ", comp$NOM_ENT[2])
        }
      },
      Comparison_Muni_3 = {
        comp <- active_comp_munis()
        if (is.null(comp) || nrow(comp) < 3) {
          NA_character_
        } else {
          paste0(comp$NOMGEO[3], ", ", comp$NOM_ENT[3])
        }
      },
      Comparison_Muni_4 = {
        comp <- active_comp_munis()
        if (is.null(comp) || nrow(comp) < 4) {
          NA_character_
        } else {
          paste0(comp$NOMGEO[4], ", ", comp$NOM_ENT[4])
        }
      },
      Comparison_Muni_1_ID = {
        comp <- active_comp_munis()
        if (is.null(comp) || nrow(comp) < 1) NA_character_ else comp$muni_id[1]
      },
      Comparison_Muni_2_ID = {
        comp <- active_comp_munis()
        if (is.null(comp) || nrow(comp) < 2) NA_character_ else comp$muni_id[2]
      },
      Comparison_Muni_3_ID = {
        comp <- active_comp_munis()
        if (is.null(comp) || nrow(comp) < 3) NA_character_ else comp$muni_id[3]
      },
      Comparison_Muni_4_ID = {
        comp <- active_comp_munis()
        if (is.null(comp) || nrow(comp) < 4) NA_character_ else comp$muni_id[4]
      },
      # Pre-treatment comparison municipality crime ranking
      Crime_Rank_Comp_1 = ifelse(
        is.null(input$muni_rank_comp_1),
        NA_character_,
        input$muni_rank_comp_1
      ),
      Crime_Rank_Comp_2 = ifelse(
        is.null(input$muni_rank_comp_2),
        NA_character_,
        input$muni_rank_comp_2
      ),
      Crime_Rank_Comp_3 = ifelse(
        is.null(input$muni_rank_comp_3),
        NA_character_,
        input$muni_rank_comp_3
      ),
      Crime_Rank_Comp_4 = ifelse(
        is.null(input$muni_rank_comp_4),
        NA_character_,
        input$muni_rank_comp_4
      ),
      # Post-treatment outcomes
      Home_Crime_Handling_Post = input$home_crime_handling_post,
      MORENA_Crime_Rating_Post = input$morena_crime_rating_post,
      Coalition_PAN_PRI_PRD_Crime_Rating_Post = input$coalition_pan_pri_prd_crime_rating_post,
      MC_Crime_Rating_Post = input$mc_crime_rating_post,
      Crime_Rank_Comp_1_Post = ifelse(
        is.null(input$muni_rank_post_comp_1),
        NA_character_,
        input$muni_rank_post_comp_1
      ),
      Crime_Rank_Comp_2_Post = ifelse(
        is.null(input$muni_rank_post_comp_2),
        NA_character_,
        input$muni_rank_post_comp_2
      ),
      Crime_Rank_Comp_3_Post = ifelse(
        is.null(input$muni_rank_post_comp_3),
        NA_character_,
        input$muni_rank_post_comp_3
      ),
      Crime_Rank_Comp_4_Post = ifelse(
        is.null(input$muni_rank_post_comp_4),
        NA_character_,
        input$muni_rank_post_comp_4
      ),
      Turnout_Likelihood_Post = input$turnout_likelihood,
      Vote_Intention_Post = if (
        is.null(input$vote_intention_2027) ||
          length(input$vote_intention_2027) == 0
      ) {
        NA_character_
      } else {
        paste(input$vote_intention_2027, collapse = ";")
      },
      Vote_Intention_Post_Other = ifelse(
        is.null(input$vote_intention_2027_other) ||
          input$vote_intention_2027_other == "",
        NA_character_,
        input$vote_intention_2027_other
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
      Time_Page_5_Sec = round(
        if (is.null(page_durations()[["5"]])) {
          NA_real_
        } else {
          page_durations()[["5"]]
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
      Time_Page_7_Sec = round(
        if (is.null(page_durations()[["7"]])) {
          NA_real_
        } else {
          page_durations()[["7"]]
        },
        1
      ),
      Time_Page_8_Sec = round(
        if (is.null(page_durations()[["8"]])) {
          NA_real_
        } else {
          page_durations()[["8"]]
        },
        1
      ),
      Time_Page_9_Sec = round(
        if (is.null(page_durations()[["9"]])) {
          NA_real_
        } else {
          page_durations()[["9"]]
        },
        1
      ),
      Time_Page_12_Sec = round(
        if (is.null(page_durations()[["12"]])) {
          NA_real_
        } else {
          page_durations()[["12"]]
        },
        1
      ),
      Time_Page_13_Sec = round(
        if (is.null(page_durations()[["13"]])) {
          NA_real_
        } else {
          page_durations()[["13"]]
        },
        1
      ),
      Is_Mobile = isTRUE(input$is_mobile),
      stringsAsFactors = FALSE
    )

    # Serialize response to CSV text
    tc <- textConnection("csv_out", "w", local = TRUE)
    write.csv(response_df, tc, row.names = FALSE)
    close(tc)
    csv_content <- paste(csv_out, collapse = "\n")

    s3_bucket <- Sys.getenv("S3_BUCKET")
    save_success <- if (nchar(s3_bucket) > 0) {
      # Production: upload to S3 (one file per respondent, no race conditions)
      tryCatch(
        {
          s3_client <- paws.storage::s3()
          s3_client$put_object(
            Bucket = s3_bucket,
            Key = paste0("responses/", respondent_id, ".csv"),
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
      # Local development fallback: append to local CSV
      tryCatch(
        {
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

    # Save Wave 1 lookup for Wave 2 (fast per-respondent S3 key lookup)
    if (nchar(s3_bucket) > 0) {
      tryCatch(
        {
          lookup <- list(Found_Municipality_ID = found_municipality())
          s3_client <- paws.storage::s3()
          s3_client$put_object(
            Bucket = s3_bucket,
            Key = paste0("wave1_lookup/", respondent_id, ".rds"),
            Body = serialize(lookup, NULL)
          )
        },
        error = function(e) {
          warning("S3 lookup write failed: ", e$message)
        }
      )
    }

    if (!save_success) {
      showNotification(
        "Error: Su respuesta no pudo ser guardada. Por favor, intente de nuevo o contacte al investigador.",
        type = "error",
        duration = NULL
      )
    }

    # Go to thank you page instead of resetting
    current_page(10)

    # Redirect to Netquest complete URL after a brief delay (so thank-you page is visible)
    if (!is.null(netquest_pid) && nchar(netquest_pid) > 0) {
      shinyjs::runjs(sprintf(
        'setTimeout(function(){ window.location.href = "https://transit.nicequest.com/transit/participation?tp=co_0&c=ok&ticket=%s"; }, 2000)',
        netquest_pid
      ))
    }
  })

  # --- Wave 2 server outputs ---

  # Pre-treatment: home municipality crime handling slider (post)
  output$home_crime_handling_post_ui <- renderUI({
    muni_name <- home_muni_name()
    tagList(
      sliderInput(
        "home_crime_handling_post",
        paste0(
          "¿Qué tan bien cree que el gobierno de ",
          muni_name,
          " maneja el crimen?"
        ),
        min = 0,
        max = 100,
        value = 50
      ),
      fluidRow(
        column(
          6,
          p(
            "Maneja el crimen extremadamente mal",
            style = "color: #6c757d; font-size: 0.85em; margin-top: -15px;"
          )
        ),
        column(
          6,
          p(
            "Maneja el crimen extremadamente bien",
            style = "color: #6c757d; font-size: 0.85em; margin-top: -15px; text-align: right;"
          )
        )
      )
    )
  })

  # Pre-treatment municipality crime ranking
  output$municipality_ranking_ui <- renderUI({
    home_id <- found_municipality()
    comp_munis <- active_comp_munis()
    if (is.null(home_id) || is.null(comp_munis)) {
      return(p(em(
        "Por favor, encuentre primero su municipio de origen para ver las opciones de clasificación."
      )))
    }
    home_info <- d_geo %>% st_drop_geometry() %>% filter(muni_id == home_id)
    comp_labels <- paste0(comp_munis$NOMGEO, ", ", comp_munis$NOM_ENT)
    make_crime_ranking_grid(home_info$NOMGEO, comp_labels, "muni_rank_comp_")
  })

  # Post-treatment municipality crime ranking
  output$municipality_ranking_post_ui <- renderUI({
    home_id <- found_municipality()
    comp_munis <- active_comp_munis()
    if (is.null(home_id) || is.null(comp_munis)) {
      return(p(em(
        "Por favor, encuentre primero su municipio de origen para ver las opciones de clasificación."
      )))
    }
    home_info <- d_geo %>% st_drop_geometry() %>% filter(muni_id == home_id)
    comp_labels <- paste0(comp_munis$NOMGEO, ", ", comp_munis$NOM_ENT)
    make_crime_ranking_grid(
      home_info$NOMGEO,
      comp_labels,
      "muni_rank_post_comp_"
    )
  })

  # Governance grid (pre-treatment)
  output$governance_grid_ui <- renderUI({
    home_id <- found_municipality()
    comp_munis <- active_comp_munis()

    if (is.null(home_id)) {
      return(p(em("Por favor, encuentre primero su municipio de origen.")))
    }

    home_info <- d_geo %>% st_drop_geometry() %>% filter(muni_id == home_id)
    home_name <- paste0(home_info$NOMGEO[1], ", ", home_info$NOM_ENT[1])

    muni_rows <- list(list(
      name = home_name,
      input_name = "home_governing_party_belief",
      is_home = TRUE
    ))
    if (!is.null(comp_munis) && nrow(comp_munis) > 0) {
      for (i in seq_len(nrow(comp_munis))) {
        muni_rows <- c(
          muni_rows,
          list(list(
            name = paste0(comp_munis$NOMGEO[i], ", ", comp_munis$NOM_ENT[i]),
            input_name = paste0("comp_governing_party_", i),
            is_home = FALSE
          ))
        )
      }
    }

    party_labels <- c(
      "PAN",
      "PRI",
      "PRD",
      "PVEM",
      "PT",
      "MC",
      "MORENA",
      "Otro",
      "No\nsé"
    )
    party_values <- c(
      "pan",
      "pri",
      "prd",
      "pvem",
      "pt",
      "mc",
      "morena",
      "other",
      "dont_know"
    )

    header_cells <- c(
      list(tags$th("Municipio")),
      lapply(party_labels, function(lbl) tags$th(HTML(gsub("\n", "<br>", lbl))))
    )

    body_rows <- lapply(muni_rows, function(row) {
      cells <- c(
        list(tags$td(row$name)),
        lapply(seq_along(party_values), function(j) {
          tags$td(tags$input(
            type = "checkbox",
            class = "governance-checkbox",
            `data-input-name` = row$input_name,
            value = party_values[j]
          ))
        })
      )
      row_class <- if (row$is_home) "home-row" else NULL
      do.call(tags$tr, c(cells, list(class = row_class)))
    })

    tagList(
      p(
        "¿Qué partido o partidos cree que actualmente gobiernan cada uno de los siguientes municipios? (seleccione todos los que correspondan)"
      ),
      div(
        class = "mobile-only",
        p(
          em(
            "\u21d0 Desplácese hacia los lados para ver todas las opciones \u21d2"
          ),
          style = "text-align: center; color: #555; margin-bottom: 4px;"
        )
      ),
      div(
        id = "governance_grid",
        class = "governance-grid",
        style = "border: 1px solid #ccc; border-radius: 4px; padding: 4px;",
        tags$table(
          tags$thead(do.call(tags$tr, header_cells)),
          do.call(tags$tbody, body_rows)
        )
      )
    )
  })

  # Nearest-5 governance prior grid (Wave 1, page 5)
  output$nearest5_governance_ui <- renderUI({
    nn5 <- nearest5_rv()
    if (is.null(nn5) || nrow(nn5) == 0) {
      return(p(em("Por favor, encuentre primero su municipio de origen.")))
    }

    party_labels <- c(
      "PAN",
      "PRI",
      "PRD",
      "PVEM",
      "PT",
      "MC",
      "MORENA",
      "Otro",
      "No\nsé"
    )
    party_values <- c(
      "pan",
      "pri",
      "prd",
      "pvem",
      "pt",
      "mc",
      "morena",
      "other",
      "dont_know"
    )

    header_cells <- c(
      list(tags$th("Municipio")),
      lapply(party_labels, function(lbl) tags$th(HTML(gsub("\n", "<br>", lbl))))
    )

    body_rows <- lapply(seq_len(nrow(nn5)), function(i) {
      muni_label <- paste0(nn5$neighbor_name[i], ", ", nn5$neighbor_state[i])
      input_name <- paste0("nearest5_governing_party_", i)
      cells <- c(
        list(tags$td(muni_label)),
        lapply(seq_along(party_values), function(j) {
          tags$td(tags$input(
            type = "checkbox",
            class = "governance-checkbox",
            `data-input-name` = input_name,
            value = party_values[j]
          ))
        })
      )
      do.call(tags$tr, cells)
    })

    tagList(
      p(
        "¿Qué partido o partidos cree que actualmente gobiernan cada uno de los siguientes municipios, que están geográficamente cerca de su municipio de origen? (seleccione todos los que correspondan)"
      ),
      div(
        class = "mobile-only",
        p(
          em(
            "\u21d0 Desplácese hacia los lados para ver todas las opciones \u21d2"
          ),
          style = "text-align: center; color: #555; margin-bottom: 4px;"
        )
      ),
      div(
        id = "nearest5_governance_grid",
        class = "governance-grid",
        style = "border: 1px solid #ccc; border-radius: 4px; padding: 4px;",
        tags$table(
          tags$thead(do.call(tags$tr, header_cells)),
          do.call(tags$tbody, body_rows)
        )
      )
    )
  })

  # Treatment helpers
  home_robbery_change_text <- reactive({
    home_id <- found_municipality()
    req(!is.null(home_id))
    home_name <- d_geo %>%
      st_drop_geometry() %>%
      filter(muni_id == home_id) %>%
      pull(NOMGEO) %>%
      `[`(1)
    home_rate <- robo_data %>%
      filter(Cve..Municipio == as.numeric(home_id)) %>%
      pull(rate_per_100k)
    home_rate <- ifelse(
      length(home_rate) == 0 || is.na(home_rate),
      0,
      home_rate
    )
    paste0(
      "Teniendo esto en cuenta, se reportaron ",
      format(round(home_rate, 1), nsmall = 1),
      " robos por cada 100,000 personas en ",
      home_name,
      " en 2025."
    )
  })

  plain_info_text <- paste0(
    "Las fuerzas de la policía municipal pueden ayudar a reducir el crimen respondiendo a incidentes delictivos, patrullando las calles, ",
    "y proporcionando información valiosa a operaciones de seguridad pública de niveles superiores. ",
    "Las decisiones sobre el financiamiento y la estructura de las fuerzas de la policía municipal están en gran medida en manos de los presidentes municipales."
  )
  plain_info_text_last <- paste0(
    "Por lo tanto, los gobiernos municipales tienen cierta capacidad para controlar el crimen, aunque muchos factores que conducen al crimen ",
    "están fuera del control del gobierno."
  )

  output$treatment_content_ui <- renderUI({
    tg <- treatment_group()
    switch(
      tg,
      "control" = tagList(
        uiOutput("treatment_control_ui"),
        p(
          em("Fuente: WorldClim v2.1 (Fick & Hijmans, 2017)."),
          style = "font-size: 0.85em; color: #555; margin-top: 4px;"
        )
      ),
      "T1" = uiOutput("treatment_plain_info_ui"),
      "T2" = tagList(
        uiOutput("treatment_nonpartisan_ui"),
        plotOutput("treatment_histogram_nonpartisan", height = "350px"),
        p(
          em(
            "Source: Secretariado Ejecutivo del Sistema Nacional de Seguridad P\u00fablica (SESNSP)."
          ),
          style = "font-size: 0.85em; color: #555; margin-top: 4px;"
        )
      ),
      "T3" = tagList(
        uiOutput("treatment_partisan_ui"),
        plotOutput("treatment_histogram_partisan", height = "350px"),
        p(
          em(
            "Fuente: Secretariado Ejecutivo del Sistema Nacional de Seguridad P\u00fablica (SESNSP)."
          ),
          style = "font-size: 0.85em; color: #555; margin-top: 4px;"
        )
      ),
      "T4" = tagList(
        uiOutput("treatment_same_coalition_ui"),
        plotOutput("treatment_histogram_same_coalition", height = "350px"),
        p(
          em(
            "Source: Secretariado Ejecutivo del Sistema Nacional de Seguridad P\u00fablica (SESNSP)."
          ),
          style = "font-size: 0.85em; color: #555; margin-top: 4px;"
        )
      ),
      "control2" = tagList(
        uiOutput("treatment_weather_ui"),
        plotOutput("treatment_histogram_weather", height = "350px"),
        p(
          em("Source: WorldClim v2.1 (Fick & Hijmans, 2017)."),
          style = "font-size: 0.85em; color: #555; margin-top: 4px;"
        )
      )
    )
  })

  output$treatment_control_ui <- renderUI({
    home_id <- found_municipality()
    req(!is.null(home_id))
    home_info <- d_geo %>% st_drop_geometry() %>% filter(muni_id == home_id)
    home_name <- home_info$NOMGEO[1]
    home_precip <- precip_data %>%
      filter(muni_id == home_id) %>%
      pull(precip_mm)
    precip_text <- if (length(home_precip) > 0 && !is.na(home_precip[1])) {
      paste0(
        "En 2025, ",
        home_name,
        " tuvo una precipitación anual promedio de ",
        round(home_precip[1]),
        " mm."
      )
    } else {
      NULL
    }
    tagList(
      p(
        "Los niveles de precipitación varían significativamente entre los municipios mexicanos y están determinados por ",
        "la geografía, la altitud y los sistemas climáticos regionales. Los patrones climáticos estacionales, las corrientes ",
        "oceánicas y las tendencias climáticas a largo plazo desempeñan un papel en la cantidad de lluvia que recibe una zona."
      ),
      p(strong(
        "Por lo tanto, la precipitación en cualquier municipio está determinada principalmente por factores naturales ",
        "y geográficos."
      )),
      if (!is.null(precip_text)) p(strong(precip_text))
    )
  })

  output$treatment_plain_info_ui <- renderUI({
    change_text <- home_robbery_change_text()
    tagList(
      p(plain_info_text),
      p(strong(plain_info_text_last)),
      p(strong(change_text))
    )
  })

  output$treatment_nonpartisan_ui <- renderUI({
    home_id <- found_municipality()
    req(!is.null(home_id))
    home_info <- d_geo %>% st_drop_geometry() %>% filter(muni_id == home_id)
    home_name <- home_info$NOMGEO[1]
    change_text <- home_robbery_change_text()
    tagList(
      p(plain_info_text),
      p(strong(plain_info_text_last)),
      p(strong(change_text)),
      p(paste0(
        "El siguiente gráfico muestra la tasa de robos por cada 100,000 personas en 2025 para ",
        home_name,
        " y una muestra de municipios similares."
      ))
    )
  })

  output$treatment_partisan_ui <- renderUI({
    home_id <- found_municipality()
    req(!is.null(home_id))
    home_info <- d_geo %>% st_drop_geometry() %>% filter(muni_id == home_id)
    home_name <- home_info$NOMGEO[1]
    home_party <- home_info$governing_party[1]
    if (home_party == "MC") {
      opposite_coalition <- mc_opp_coalition_rv()
      req(!is.null(opposite_coalition))
    } else {
      opposite_coalition <- get_opposite_parties(home_party)
    }
    opposite_label <- get_coalition_label(opposite_coalition)
    change_text <- home_robbery_change_text()
    tagList(
      p(plain_info_text),
      p(strong(plain_info_text_last)),
      p(strong(change_text)),
      p(paste0(
        "El siguiente gráfico muestra la tasa de robos por cada 100,000 personas en 2025 para ",
        home_name,
        " y una muestra de municipios similares gobernados por ",
        opposite_label,
        "."
      ))
    )
  })

  output$treatment_same_coalition_ui <- renderUI({
    home_id <- found_municipality()
    req(!is.null(home_id))
    home_info <- d_geo %>% st_drop_geometry() %>% filter(muni_id == home_id)
    home_name <- home_info$NOMGEO[1]
    home_party <- home_info$governing_party[1]
    same_coalition <- get_same_coalition_parties(home_party)
    same_label <- get_coalition_label(same_coalition)
    change_text <- home_robbery_change_text()
    tagList(
      p(plain_info_text),
      p(strong(plain_info_text_last)),
      p(strong(change_text)),
      p(paste0(
        "El siguiente gráfico muestra la tasa de robos por cada 100,000 personas en 2025 para ",
        home_name,
        " y una muestra de municipios similares gobernados por ",
        same_label,
        "."
      ))
    )
  })

  build_treatment_plot <- function(plot_df, show_party = FALSE) {
    if (show_party) {
      fill_values <- c(
        "Su municipio" = "#666666",
        "MORENA/PVEM/PT" = "#8B0000",
        "PAN/PRI/PRD" = "#00308F",
        "MC" = "#FF5722",
        "Other" = "#D3D3D3"
      )
      fill_values <- fill_values[
        names(fill_values) %in% unique(plot_df$fill_group)
      ]
    } else {
      fill_values <- c(
        "Su municipio" = "#666666",
        "Comparación" = "#E69F00"
      )
    }
    ggplot(
      plot_df,
      aes(x = municipality, y = rate_per_100k, fill = fill_group)
    ) +
      geom_col(color = "black", linewidth = 0.5) +
      scale_fill_manual(values = fill_values, name = NULL) +
      labs(x = NULL, y = "Robos por 100,000 personas en 2025") +
      coord_cartesian(clip = "off") +
      theme_minimal() +
      theme(
        axis.text.x = element_text(angle = 45, hjust = 1, size = 11),
        axis.title.y = element_text(size = 12),
        panel.grid.major.x = element_blank(),
        legend.position = "bottom",
        legend.box = "horizontal",
        plot.margin = margin(t = 5, r = 5, b = 5, l = 40)
      )
  }

  build_plot_df <- function(home_id, comp_munis, show_party = FALSE) {
    all_munis <- bind_rows(
      d_geo %>%
        st_drop_geometry() %>%
        filter(muni_id == home_id) %>%
        select(muni_id, NOMGEO, governing_party, coalition_label),
      comp_munis %>%
        left_join(
          d_geo %>%
            st_drop_geometry() %>%
            select(muni_id, governing_party, coalition_label),
          by = "muni_id"
        )
    )
    muni_info <- all_munis %>%
      mutate(Cve..Municipio = as.numeric(muni_id)) %>%
      left_join(robo_data, by = "Cve..Municipio") %>%
      mutate(
        rate_per_100k = ifelse(is.na(rate_per_100k), 0, rate_per_100k),
        fill_group = case_when(
          muni_id == home_id ~ "Su municipio",
          show_party & !is.na(coalition_label) ~ coalition_label,
          show_party ~ "Other",
          TRUE ~ "Comparación"
        )
      ) %>%
      arrange(rate_per_100k)
    data.frame(
      municipality = factor(
        stringr::str_wrap(muni_info$NOMGEO, width = 15),
        levels = stringr::str_wrap(muni_info$NOMGEO, width = 15)
      ),
      rate_per_100k = muni_info$rate_per_100k,
      fill_group = muni_info$fill_group
    )
  }

  output$treatment_histogram_partisan <- renderPlot({
    home_id <- found_municipality()
    comp_munis <- comparison_municipalities()
    req(!is.null(home_id), !is.null(comp_munis))
    plot_df <- build_plot_df(home_id, comp_munis, show_party = TRUE)
    build_treatment_plot(plot_df, show_party = TRUE)
  })
  outputOptions(
    output,
    "treatment_histogram_partisan",
    suspendWhenHidden = FALSE
  )

  output$treatment_histogram_nonpartisan <- renderPlot({
    home_id <- found_municipality()
    comp_munis <- comp_munis_nonpartisan_rv()
    req(!is.null(home_id), !is.null(comp_munis))
    plot_df <- build_plot_df(home_id, comp_munis, show_party = FALSE)
    build_treatment_plot(plot_df, show_party = FALSE)
  })
  outputOptions(
    output,
    "treatment_histogram_nonpartisan",
    suspendWhenHidden = FALSE
  )

  output$treatment_histogram_same_coalition <- renderPlot({
    home_id <- found_municipality()
    comp_munis <- comp_munis_same_coalition_rv()
    req(!is.null(home_id), !is.null(comp_munis))
    plot_df <- build_plot_df(home_id, comp_munis, show_party = TRUE)
    build_treatment_plot(plot_df, show_party = TRUE)
  })
  outputOptions(
    output,
    "treatment_histogram_same_coalition",
    suspendWhenHidden = FALSE
  )

  output$treatment_weather_ui <- renderUI({
    home_id <- found_municipality()
    req(!is.null(home_id))
    home_name <- d_geo %>%
      st_drop_geometry() %>%
      filter(muni_id == home_id) %>%
      pull(NOMGEO)
    tagList(
      p(
        "Los niveles de precipitación varían significativamente entre los municipios mexicanos y están determinados por ",
        "la geografía, la altitud y los sistemas climáticos regionales. Los patrones climáticos estacionales, las corrientes ",
        "oceánicas y las tendencias climáticas a largo plazo desempeñan un papel en la cantidad de lluvia que recibe una zona."
      ),
      p(strong(
        "Por lo tanto, la precipitación en cualquier municipio está determinada principalmente por factores naturales ",
        "y geográficos."
      )),
      p(paste0(
        "El siguiente gráfico muestra la precipitación anual promedio en ",
        home_name[1],
        " y una muestra de municipios similares."
      ))
    )
  })

  build_weather_plot_df <- function(home_id, comp_munis) {
    muni_info <- bind_rows(
      d_geo %>%
        st_drop_geometry() %>%
        filter(muni_id == home_id) %>%
        select(muni_id, NOMGEO),
      comp_munis %>% select(muni_id, NOMGEO)
    ) %>%
      left_join(precip_data, by = "muni_id") %>%
      mutate(
        precip_mm = ifelse(is.na(precip_mm), 0, precip_mm),
        fill_group = ifelse(
          muni_id == home_id,
          "Su municipio",
          "Comparación"
        )
      ) %>%
      arrange(precip_mm)
    data.frame(
      municipality = factor(
        stringr::str_wrap(muni_info$NOMGEO, width = 15),
        levels = stringr::str_wrap(muni_info$NOMGEO, width = 15)
      ),
      precip_mm = muni_info$precip_mm,
      fill_group = muni_info$fill_group
    )
  }

  build_weather_plot <- function(plot_df) {
    fill_values <- c("Su municipio" = "#0072B2", "Comparación" = "#E69F00")
    ggplot(plot_df, aes(x = municipality, y = precip_mm, fill = fill_group)) +
      geom_col(color = "black", linewidth = 0.5) +
      scale_fill_manual(values = fill_values, name = NULL) +
      labs(x = NULL, y = "Precipitaci\u00f3n anual promedio (mm)") +
      coord_cartesian(clip = "off") +
      theme_minimal() +
      theme(
        axis.text.x = element_text(angle = 45, hjust = 1, size = 11),
        axis.title.y = element_text(size = 12),
        panel.grid.major.x = element_blank(),
        legend.position = "bottom",
        legend.box = "horizontal",
        plot.margin = margin(t = 5, r = 5, b = 5, l = 40)
      )
  }

  output$treatment_histogram_weather <- renderPlot({
    home_id <- found_municipality()
    comp_munis <- comp_munis_nonpartisan_rv()
    req(!is.null(home_id), !is.null(comp_munis))
    plot_df <- build_weather_plot_df(home_id, comp_munis)
    build_weather_plot(plot_df)
  })
  outputOptions(
    output,
    "treatment_histogram_weather",
    suspendWhenHidden = FALSE
  )
}

shinyApp(ui = ui, server = server)
