library(shiny)
library(shinyjs)
library(sortable)
library(sf)
library(leaflet)
library(dplyr)
library(ggplot2)
library(httr)
library(jsonlite)

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

# Filter to large cities (200k+ population) and state capitals
large_munis <- d_geo %>%
  st_drop_geometry() %>%
  filter(!is.na(POB_TOTAL) & (POB_TOTAL >= 200000 | is_capital)) %>%
  pull(muni_id)

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

# Coalition definitions for treatment groups
coalition_a <- c("MORENA", "PT", "PVEM")
coalition_b <- c("PAN", "PRI", "PRD", "MC")

get_same_coalition_parties <- function(party) {
  if (party %in% coalition_a) coalition_a else coalition_b
}

get_opposite_parties <- function(party) {
  if (party %in% coalition_a) coalition_b else coalition_a
}

get_coalition_label <- function(parties) {
  paste(parties, collapse = "/")
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

ui <- fluidPage(
  useShinyjs(),
  tags$head(
    tags$style(HTML(
      "
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
      .issue-grayed {
        opacity: 0.4;
        transition: opacity 0.4s ease;
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
            h4("Information Sheet"),
            p(em("Principal Investigator: Adam Roberts")),
            p(
              "This form describes a research study that is being conducted by Adam Roberts from the ",
              "University of Rochester's Department of Political Science. The purpose of this study is to ",
              "understand how voters use information when deciding which candidate or political party to vote for."
            ),
            p(
              "If you decide to take part in this study, you will be asked to complete a survey that will take ",
              "about 10\u201315 minutes to complete. The surveys will ask questions ",
              "about the municipality you live in, ",
              "demographics, and political topics, including your political preferences, party affiliation, and ",
              "vote choice in the most recent municipal elections."
            ),
            p(
              "You will be asked to provide your location to identify which municipality you live in, but we ",
              "will not save this information. Some of the survey questions may be upsetting or make you feel ",
              "uncomfortable, but not any more uncomfortable than you might feel reading the news. You may ",
              "withdraw at any time. This study will not collect any direct identifiers, like your name or ",
              "government ID number. However, we will collect some indirect identifiers, namely the municipality ",
              "in which you live, political affiliation, and demographic information. All information we collect ",
              "will be stored in a secure manner and only the principal investigator will have access to it. There ",
              "are no other expected risks. There are also no expected benefits."
            ),
            p(
              "Your participation in this study is completely voluntary. You are free not to participate or to ",
              "withdraw at any time, for whatever reason."
            ),
            p(
              "For more information or questions about this research you may contact Adam Roberts at ",
              tags$a(
                href = "mailto:arober48@ur.rochester.edu",
                "arober48@ur.rochester.edu"
              ),
              ". Please contact the University of Rochester Research Subjects Review Board at 265 Crittenden Blvd., ",
              "CU 420628, Rochester, NY 14642, ",
              "Telephone +1 (585) 276-0005 or +1 (877) 449-4441 for the following reasons:"
            ),
            tags$ul(
              tags$li(
                "You wish to talk to someone other than the research staff about your rights as a research subject;"
              ),
              tags$li("To voice concerns about the research;"),
              tags$li("To provide input concerning the research process;"),
              tags$li("In the event the study staff could not be reached.")
            ),
            hr(),
            fluidRow(
              column(
                12,
                align = "right",
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
            h4("Find Your Municipality"),

            p(strong("Enter your address to find your home municipality:")),
            fluidRow(
              column(
                9,
                textInput(
                  "address",
                  NULL,
                  placeholder = "e.g., Av. Yucatán 147, Roma Nte., Cuauhtémoc, CDMX"
                )
              ),
              column(
                3,
                actionButton(
                  "geocode_btn",
                  "Search",
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
                tags$strong(" Searching for your address..."),
                tags$br(),
                tags$small("This may take a few seconds.")
              )
            ),
            uiOutput("geocode_result"),

            # Confirmation section (shown after municipality is found)
            hidden(
              div(
                id = "home_confirmation_section",
                hr(),
                p(
                  "Please confirm that the following information is correct before continuing:"
                ),
                uiOutput("home_municipality_summary"),
                selectInput(
                  "dropdown_municipality",
                  "Not right? Search your address again or select your municipality from the list:",
                  choices = c("-- Select a municipality --" = "", muni_choices),
                  selected = "",
                  selectize = TRUE,
                  width = "100%"
                ),
                actionButton(
                  "clear_selection_btn",
                  "Clear Selection",
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
                disabled(
                  actionButton(
                    "goto_page2_from_1",
                    "Next \u2192",
                    class = "btn-primary btn-lg"
                  )
                )
              )
            )
          )
        ),

        # Page 2: Practice Drag-and-Drop
        hidden(
          div(
            id = "page2",
            p(
              "This survey includes questions where you rank items by dragging and dropping them into order."
            ),
            p(
              "The following is a list of news sources that you can re-arrange by dragging and dropping each individual item.",
              "To make sure that you are paying attention to the question wording, please put ",
              "Radio in position 1 (top) and Social media is in position 5 (bottom)."
            ),
            tags$hr(),
            bucket_list(
              header = NULL,
              group_name = "practice_bucket",
              orientation = "horizontal",
              add_rank_list(
                text = "Available items — drag into slots:",
                labels = c(
                  "Television",
                  "Social media",
                  "Online news websites",
                  "Radio",
                  "Print newspapers"
                ),
                input_id = "practice_source",
                class = "default-sortable source-bin"
              ),
              add_rank_list(
                text = "Your ranking (1 = top, 5 = bottom):",
                labels = NULL,
                input_id = "practice_ranking",
                class = "default-sortable"
              )
            ),
            tags$hr(),
            uiOutput("practice_warning"),
            fluidRow(
              column(
                6,
                actionButton(
                  "goto_page1_from_2",
                  "\u2190 Back",
                  class = "btn-default btn-lg"
                )
              ),
              column(
                6,
                align = "right",
                actionButton(
                  "goto_page3_from_2",
                  "Next \u2192",
                  class = "btn-primary btn-lg"
                )
              )
            )
          )
        ),

        # Page 3: Political Views & Municipal Governance
        hidden(
          div(
            id = "page3",
            h4("Your Municipality"),

            tags$div(
              class = "form-group",
              tags$label(
                "Which party or parties did you vote for in the last municipal election? (select all that apply)"
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
                party_checkbox_choice("did_not_vote", "Did not vote"),
                party_checkbox_choice("dont_remember", "Don't remember"),
                party_checkbox_choice("other", "Other")
              )
            ),
            hidden(
              textInput(
                "last_election_vote_other",
                "Please specify the party:",
                placeholder = "Enter party name..."
              )
            ),

            hr(),

            h5(strong("Municipal Governance")),

            uiOutput("governance_grid_ui"),

            hr(),

            sliderInput(
              "turnout_likelihood_pre",
              "How likely are you to vote in the 2027 local elections on a scale of 0 to 100, where 0 means 'definitely won't vote' and 100 means 'certainly will vote'?",
              min = 0,
              max = 100,
              value = 50
            ),
            fluidRow(
              column(
                6,
                p(
                  "Definitely won't vote",
                  style = "color: #6c757d; font-size: 0.85em; margin-top: -15px;"
                )
              ),
              column(
                6,
                p(
                  "Certainly will vote",
                  style = "color: #6c757d; font-size: 0.85em; margin-top: -15px; text-align: right;"
                )
              )
            ),

            br(),

            tags$div(
              class = "form-group",
              tags$label(
                "Which party or parties do you intend to vote for in the next municipal election? (select all that apply)"
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
                  "Undecided",
                  group = "vote_intention_pre"
                ),
                party_checkbox_choice(
                  "will_not_vote",
                  "Will not vote",
                  group = "vote_intention_pre"
                ),
                party_checkbox_choice(
                  "other",
                  "Other",
                  group = "vote_intention_pre"
                )
              )
            ),
            hidden(
              textInput(
                "vote_intention_pre_other",
                "Please specify the party:",
                placeholder = "Enter party name..."
              )
            ),

            hr(),

            # Importance of Issues, language loosely based on Mitofsky
            # public opinion survey (see Google Drive reference)
            h5(strong("Issue Importance")),
            div(
              id = "issue_importance_wrapper",
              class = "issue-grayed",
              uiOutput("issue_importance_ui")
            ),

            hr(),
            uiOutput("issue_importance_warning"),
            fluidRow(
              column(
                6,
                actionButton(
                  "goto_page1_from_3",
                  "← Back",
                  class = "btn-secondary btn-lg"
                )
              ),
              column(
                6,
                align = "right",
                actionButton(
                  "goto_page4_from_3",
                  "Next →",
                  class = "btn-primary btn-lg"
                )
              )
            )
          )
        ),

        # Page 4: Your Municipality's Performance
        hidden(
          div(
            id = "page4",
            h4("Your Municipality's Performance"),

            p(strong(
              "Las siguientes preguntas le pedir\u00e1n que eval\u00fae c\u00f3mo ciertos municipios y partidos \u201cmanejan\u201d la delincuencia no violenta, como los robos. \u201cManejar la delincuencia\u201d aqu\u00ed se refiere a los esfuerzos del gobierno para prevenir el crimen, hacer cumplir la ley y garantizar la seguridad p\u00fablica."
            )),

            uiOutput("home_crime_handling_pre_ui"),

            sliderInput(
              "morena_crime_rating",
              "On average, how well do you think municipalities governed by MORENA, PT, or PVEM handle crime?",
              min = 0,
              max = 100,
              value = 50
            ),
            fluidRow(
              column(
                6,
                p(
                  "Handles crime extremely poorly",
                  style = "color: #6c757d; font-size: 0.85em; margin-top: -15px;"
                )
              ),
              column(
                6,
                p(
                  "Handles crime extremely well",
                  style = "color: #6c757d; font-size: 0.85em; margin-top: -15px; text-align: right;"
                )
              )
            ),

            sliderInput(
              "coalition_pan_pri_prd_crime_rating",
              "On average, how well do you think municipalities governed by PAN, PRI, or PRD handle crime?",
              min = 0,
              max = 100,
              value = 50
            ),
            fluidRow(
              column(
                6,
                p(
                  "Handles crime extremely poorly",
                  style = "color: #6c757d; font-size: 0.85em; margin-top: -15px;"
                )
              ),
              column(
                6,
                p(
                  "Handles crime extremely well",
                  style = "color: #6c757d; font-size: 0.85em; margin-top: -15px; text-align: right;"
                )
              )
            ),

            sliderInput(
              "coalition_mc_crime_rating",
              "On average, how well do you think municipalities governed by MC handle crime?",
              min = 0,
              max = 100,
              value = 50
            ),
            fluidRow(
              column(
                6,
                p(
                  "Handles crime extremely poorly",
                  style = "color: #6c757d; font-size: 0.85em; margin-top: -15px;"
                )
              ),
              column(
                6,
                p(
                  "Handles crime extremely well",
                  style = "color: #6c757d; font-size: 0.85em; margin-top: -15px; text-align: right;"
                )
              )
            ),

            hr(),

            uiOutput("robbery_estimate_ui"),

            hr(),

            uiOutput("municipality_ranking_ui"),

            hr(),
            fluidRow(
              column(
                6,
                actionButton(
                  "goto_page3_from_4",
                  "← Back",
                  class = "btn-secondary btn-lg"
                )
              ),
              column(
                6,
                align = "right",
                actionButton(
                  "goto_page5_from_4",
                  "Next →",
                  class = "btn-primary btn-lg"
                )
              )
            )
          )
        ),

        # Page 5: About You
        hidden(
          div(
            id = "page5",
            h4("About You"),

            h5(strong("Political Views")),

            sliderInput(
              "left_right_scale",
              "In politics, people sometimes talk being 'left' or 'right' on an ideological scale, where further right means more conservative.
              Where would you place yourself on this scale?",
              min = 0,
              max = 10,
              value = 5,
              step = 1
            ),
            fluidRow(
              column(
                6,
                p(
                  "Left",
                  style = "color: #6c757d; font-size: 0.85em; margin-top: -15px;"
                )
              ),
              column(
                6,
                p(
                  "Right",
                  style = "color: #6c757d; font-size: 0.85em; margin-top: -15px; text-align: right;"
                )
              )
            ),

            tags$div(
              class = "form-group",
              tags$label("Which political party do you most identify with?"),
              tags$div(
                id = "party_preference_group",
                class = "party-radio-group",
                party_radio_choice(
                  "pan",
                  "PAN (Partido Acción Nacional)",
                  "PAN_logo.png"
                ),
                party_radio_choice(
                  "pri",
                  "PRI (Partido Revolucionario Institucional)",
                  "PRI_logo.png"
                ),
                party_radio_choice(
                  "prd",
                  "PRD (Partido de la Revolución Democrática)",
                  "PRD_logo.png"
                ),
                party_radio_choice(
                  "pvem",
                  "PVEM (Partido Verde Ecologista de México)",
                  "PVEM_logo.png"
                ),
                party_radio_choice(
                  "pt",
                  "PT (Partido del Trabajo)",
                  "PT_logo.png"
                ),
                party_radio_choice(
                  "mc",
                  "MC (Movimiento Ciudadano)",
                  "Movimiento_Ciudadano_logo.png"
                ),
                party_radio_choice("morena", "MORENA", "Morena_logo.png"),
                party_radio_choice("none", "None / Independent"),
                party_radio_choice("other", "Other")
              )
            ),
            hidden(
              textInput(
                "party_preference_other",
                "Please specify the party:",
                placeholder = "Enter party name..."
              )
            ),

            selectInput(
              "party_preference_2nd",
              "Which political party do you feel second closest to? (optional)",
              choices = c(
                "Select a party..." = "",
                "PAN (Partido Acción Nacional)" = "pan",
                "PRI (Partido Revolucionario Institucional)" = "pri",
                "PRD (Partido de la Revolución Democrática)" = "prd",
                "PVEM (Partido Verde Ecologista de México)" = "pvem",
                "PT (Partido del Trabajo)" = "pt",
                "MC (Movimiento Ciudadano)" = "mc",
                "MORENA" = "morena",
                "None" = "none"
              ),
              selected = ""
            ),

            selectInput(
              "party_preference_3rd",
              "Which political party do you feel third closest to? (optional)",
              choices = c(
                "Select a party..." = "",
                "PAN (Partido Acción Nacional)" = "pan",
                "PRI (Partido Revolucionario Institucional)" = "pri",
                "PRD (Partido de la Revolución Democrática)" = "prd",
                "PVEM (Partido Verde Ecologista de México)" = "pvem",
                "PT (Partido del Trabajo)" = "pt",
                "MC (Movimiento Ciudadano)" = "mc",
                "MORENA" = "morena",
                "None" = "none"
              ),
              selected = ""
            ),

            hr(),

            # Attention check
            radioButtons(
              "attention_check",
              paste(
                "Sustainable farming is an important issue for many Mexicans.",
                "We want to make sure you are reading each question carefully.",
                "To ensure you are reading each question carefully, please select 'Somewhat agree' below."
              ),
              choices = c(
                "Strongly disagree" = "strongly_disagree",
                "Somewhat disagree" = "somewhat_disagree",
                "Neither agree nor disagree" = "neutral",
                "Somewhat agree" = "somewhat_agree",
                "Strongly agree" = "strongly_agree"
              ),
              selected = character(0)
            ),

            hr(),
            fluidRow(
              column(
                6,
                actionButton(
                  "goto_page4_from_5",
                  "← Back",
                  class = "btn-secondary btn-lg"
                )
              ),
              column(
                6,
                align = "right",
                actionButton(
                  "goto_page6_from_5",
                  "Next →",
                  class = "btn-primary btn-lg"
                )
              )
            )
          )
        ),

        # Page 6: Treatment
        hidden(
          div(
            id = "page6",

            uiOutput("treatment_content_ui"),

            hr(),
            fluidRow(
              column(
                6,
                actionButton(
                  "goto_page5_from_6",
                  "← Back",
                  class = "btn-secondary btn-lg"
                )
              ),
              column(
                6,
                align = "right",
                actionButton(
                  "goto_page7_from_6",
                  "Next →",
                  class = "btn-primary btn-lg"
                )
              )
            )
          )
        ),

        # Page 7: Post-Treatment Questions
        hidden(
          div(
            id = "page7",
            h4("Post-Treatment Survey"),

            # 1. Party handling of crime (coalition ratings)
            p(strong(
              "Las siguientes preguntas le pedir\u00e1n que eval\u00fae c\u00f3mo ciertos municipios y partidos \u201cmanejan\u201d la delincuencia no violenta, como los robos. \u201cManejar la delincuencia\u201d aqu\u00ed se refiere a los esfuerzos del gobierno para prevenir el crimen, hacer cumplir la ley y garantizar la seguridad p\u00fablica."
            )),

            uiOutput("home_crime_handling_post_ui"),

            sliderInput(
              "morena_crime_rating_post",
              "On average, how well do you think municipalities governed by MORENA, PT, or PVEM handle crime?",
              min = 0,
              max = 100,
              value = 50
            ),
            fluidRow(
              column(
                6,
                p(
                  "Handles crime extremely poorly",
                  style = "color: #6c757d; font-size: 0.85em; margin-top: -15px;"
                )
              ),
              column(
                6,
                p(
                  "Handles crime extremely well",
                  style = "color: #6c757d; font-size: 0.85em; margin-top: -15px; text-align: right;"
                )
              )
            ),

            sliderInput(
              "coalition_pan_pri_prd_crime_rating_post",
              "On average, how well do you think municipalities governed by PAN, PRI, or PRD handle crime?",
              min = 0,
              max = 100,
              value = 50
            ),
            fluidRow(
              column(
                6,
                p(
                  "Handles crime extremely poorly",
                  style = "color: #6c757d; font-size: 0.85em; margin-top: -15px;"
                )
              ),
              column(
                6,
                p(
                  "Handles crime extremely well",
                  style = "color: #6c757d; font-size: 0.85em; margin-top: -15px; text-align: right;"
                )
              )
            ),

            sliderInput(
              "coalition_mc_crime_rating_post",
              "On average, how well do you think municipalities governed by MC handle crime?",
              min = 0,
              max = 100,
              value = 50
            ),
            fluidRow(
              column(
                6,
                p(
                  "Handles crime extremely poorly",
                  style = "color: #6c757d; font-size: 0.85em; margin-top: -15px;"
                )
              ),
              column(
                6,
                p(
                  "Handles crime extremely well",
                  style = "color: #6c757d; font-size: 0.85em; margin-top: -15px; text-align: right;"
                )
              )
            ),

            hr(),

            # 2. Municipality performance (ranking)
            h5(strong("Your Municipality's Performance")),

            uiOutput("municipality_ranking_post_ui"),

            hr(),
            fluidRow(
              column(
                6,
                actionButton(
                  "goto_page6_from_7",
                  "← Back",
                  class = "btn-secondary btn-lg"
                )
              ),
              column(
                6,
                align = "right",
                actionButton(
                  "goto_page8_from_7",
                  "Next \u2192",
                  class = "btn-primary btn-lg"
                )
              )
            )
          )
        ),

        # Page 8: Turnout Likelihood + Vote Intention
        hidden(
          div(
            id = "page8",

            # Vote likelihood (turnout)
            sliderInput(
              "turnout_likelihood",
              "How likely are you to vote in the 2027 local elections on a scale of 0 to 100, where 0 means 'definitely won't vote' and 100 means 'certainly will vote'?",
              min = 0,
              max = 100,
              value = 50
            ),
            fluidRow(
              column(
                6,
                p(
                  "Definitely won't vote",
                  style = "color: #6c757d; font-size: 0.85em; margin-top: -15px;"
                )
              ),
              column(
                6,
                p(
                  "Certainly will vote",
                  style = "color: #6c757d; font-size: 0.85em; margin-top: -15px; text-align: right;"
                )
              )
            ),

            br(),

            # Vote choice (vote intention)
            tags$div(
              class = "form-group",
              tags$label(
                "Which party or parties do you intend to vote for in the next municipal election? (select all that apply)"
              ),
              tags$div(
                class = "party-radio-group",
                party_checkbox_choice(
                  "pan",
                  "PAN (Partido Acción Nacional)",
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
                  "PRD (Partido de la Revolución Democrática)",
                  "PRD_logo.png",
                  "vote_intention_2027"
                ),
                party_checkbox_choice(
                  "pvem",
                  "PVEM (Partido Verde Ecologista de México)",
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
                  "Undecided",
                  group = "vote_intention_2027"
                ),
                party_checkbox_choice(
                  "will_not_vote",
                  "Will not vote",
                  group = "vote_intention_2027"
                ),
                party_checkbox_choice(
                  "other",
                  "Other",
                  group = "vote_intention_2027"
                )
              )
            ),
            hidden(
              textInput(
                "vote_intention_2027_other",
                "Please specify the party:",
                placeholder = "Enter party name..."
              )
            ),

            hr(),
            fluidRow(
              column(
                6,
                actionButton(
                  "goto_page7_from_8",
                  "\u2190 Back",
                  class = "btn-secondary btn-lg"
                )
              ),
              column(
                6,
                align = "right",
                actionButton(
                  "goto_page9_from_8",
                  "Next \u2192",
                  class = "btn-primary btn-lg"
                )
              )
            )
          )
        ),

        # Page 9: Select Reference Municipalities
        hidden(
          div(
            id = "page9",
            h4("Select Reference Municipalities"),

            uiOutput("reference_instructions_text"),
            p(em(
              "(You can select multiple municipalities by clicking on them. Selected municipalities will be highlighted in ",
              tags$span(style = "color: #9B59B6; font-weight: bold;", "purple"),
              ". Click again to deselect. Your home municipality is outlined in blue.)"
            )),

            fluidRow(
              column(
                12,
                align = "right",
                actionButton(
                  "zoom_home",
                  "Zoom to Home",
                  icon = icon("home"),
                  class = "btn-primary"
                )
              )
            ),

            leafletOutput("map_page9", height = 450),
            p(em(
              style = "color: #6c757d; font-size: 0.9em;",
              "Note: May take a few seconds for map to load."
            )),

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
                  "clear_search",
                  "Clear",
                  class = "btn-secondary",
                  style = "width: 100%;"
                )
              )
            ),

            verbatimTextOutput("selected_munis_display"),

            hr(),
            fluidRow(
              column(
                6,
                actionButton(
                  "goto_page8_from_9",
                  "\u2190 Back",
                  class = "btn-secondary btn-lg"
                )
              ),
              column(
                6,
                align = "right",
                actionButton(
                  "submit",
                  "Submit Survey",
                  class = "btn-success btn-lg",
                  icon = icon("check")
                )
              )
            )
          )
        ),

        # Page 10: Thank you screen
        hidden(
          div(
            id = "page10",
            br(),
            br(),
            br(),
            div(
              style = "text-align: center; padding: 50px;",
              h3(icon("check-circle"), " Thank you!"),
              p("Your survey has been submitted successfully."),
              br(),
              p(style = "color: #6c757d;", "You may now close this window.")
            )
          )
        )
      )
    )
  )
)

server <- function(input, output, session) {
  # Helper function to find municipality's robbery category from bucket inputs
  # Returns "more_than_double", "more", "same", "fewer", "less_than_half", "dont_know", or NA
  make_crime_ranking_grid <- function(home_name, comp_labels, prefix) {
    col_labels <- c(
      "more_than_double" = "More than double",
      "more" = "More, less than double",
      "same" = "About the same",
      "fewer" = "Fewer, more than half",
      "less_than_half" = "Fewer, less than half",
      "dont_know" = "Don't know"
    )
    header_cells <- tagList(
      tags$th(
        "Municipality",
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
        "Compared to ",
        strong(home_name),
        ", how do you think the number of robberies in these municipalities compares?"
      ),
      tags$div(
        style = "overflow-x: auto; margin-top: 10px;",
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
  comparison_municipalities <- reactiveVal(NULL) # Stores comparison muni data for ranking & graph
  comp_munis_nonpartisan_rv <- reactiveVal(NULL) # Stores non-partisan comparison munis
  comp_munis_same_coalition_rv <- reactiveVal(NULL) # Stores same-coalition comparison munis

  # Randomly assign treatment group for this respondent at session start
  treatment_group <- reactiveVal(
    sample(c("control", "T1", "T2", "T3", "T4"), 1)
  )

  # Define the file path for saving responses
  responses_file <- "data/survey_responses.csv"

  # Populate municipality search choices for page 8 map
  updateSelectizeInput(
    session,
    "muni_search",
    choices = c(" " = "", setNames(d_geo$muni_id, d_geo$mun_state)),
    server = TRUE
  )

  # Geocode address and find municipality
  observeEvent(input$geocode_btn, {
    req(input$address)

    disable("geocode_btn")
    updateActionButton(session, "geocode_btn", label = "Searching...")
    show("loading_msg")
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

            hide("loading_msg")
            enable("geocode_btn")
            updateActionButton(
              session,
              "geocode_btn",
              label = "Search",
              icon = NULL
            )

            output$geocode_result <- renderUI({
              tags$div(
                style = paste0(
                  "color: #0072B2; margin-top: 10px; padding: 10px; ",
                  "background-color: #d4e9f7; border-radius: 4px;"
                ),
                icon("check-circle"),
                tags$strong(paste(" Found: ", muni_name, ", ", muni_state)),
                tags$br()
              )
            })
          } else {
            hide("loading_msg")
            enable("geocode_btn")
            updateActionButton(
              session,
              "geocode_btn",
              label = "Search",
              icon = NULL
            )

            output$geocode_result <- renderUI({
              tags$div(
                style = paste0(
                  "color: #856404; margin-top: 10px; padding: 10px; ",
                  "background-color: #fff3cd; border-radius: 4px;"
                ),
                icon("exclamation-triangle"),
                " Address found, but not within our municipality database."
              )
            })
          }
        } else {
          hide("loading_msg")
          enable("geocode_btn")
          updateActionButton(
            session,
            "geocode_btn",
            label = "Search",
            icon = NULL
          )

          output$geocode_result <- renderUI({
            tags$div(
              style = "color: #721c24; margin-top: 10px; padding: 10px; background-color: #f8d7da; border-radius: 4px;",
              icon("times-circle"),
              " Could not find that address.",
              tags$br(),
              tags$small(
                "Please try simplifying your address or checking spelling."
              )
            )
          })
        }
      },
      error = function(e) {
        hide("loading_msg")
        enable("geocode_btn")
        updateActionButton(
          session,
          "geocode_btn",
          label = "Search",
          icon = NULL
        )

        output$geocode_result <- renderUI({
          tags$div(
            style = "color: #721c24; margin-top: 10px; padding: 10px; background-color: #f8d7da; border-radius: 4px;",
            icon("times-circle"),
            " Error searching for address."
          )
        })
      }
    )
  })

  # Reference municipalities dropdown → sets selected_map_munis
  observeEvent(
    input$reference_munis_dropdown,
    {
      selected_map_munis(input$reference_munis_dropdown)
    },
    ignoreNULL = FALSE
  )

  # Set comparison municipalities when home municipality is found
  observeEvent(found_municipality(), {
    home_id <- found_municipality()
    req(!is.null(home_id))

    home_info <- d_geo %>%
      st_drop_geometry() %>%
      filter(muni_id == home_id)

    home_state <- home_info$NOM_ENT[1]
    home_party <- home_info$governing_party[1]
    opposite_coalition <- get_opposite_parties(home_party)

    # Get comparison municipalities (large cities, opposite coalition, excluding home)
    comp_munis <- d_geo %>%
      st_drop_geometry() %>%
      filter(
        governing_party %in% opposite_coalition,
        muni_id != home_id,
        muni_id %in% large_munis
      ) %>%
      select(muni_id, NOMGEO, NOM_ENT)

    # Fallback: if no opposite-coalition large municipalities, use any large city
    if (nrow(comp_munis) == 0) {
      comp_munis <- d_geo %>%
        st_drop_geometry() %>%
        filter(
          muni_id != home_id,
          muni_id %in% large_munis
        ) %>%
        select(muni_id, NOMGEO, NOM_ENT)
    }

    # Randomly select up to 4 comparison municipalities
    if (nrow(comp_munis) > 4) {
      comp_munis <- comp_munis %>% slice_sample(n = 4)
    }

    comparison_municipalities(comp_munis)
  })

  # Set non-partisan comparison municipalities when home municipality is found
  observeEvent(found_municipality(), {
    home_id <- found_municipality()
    req(!is.null(home_id))

    home_info <- d_geo %>%
      st_drop_geometry() %>%
      filter(muni_id == home_id)

    home_state <- home_info$NOM_ENT[1]

    # Get comparison municipalities (large cities, any party, excluding home)
    comp_munis_np <- d_geo %>%
      st_drop_geometry() %>%
      filter(
        muni_id != home_id,
        muni_id %in% large_munis
      ) %>%
      select(muni_id, NOMGEO, NOM_ENT)

    # Randomly select up to 4 comparison municipalities
    if (nrow(comp_munis_np) > 4) {
      comp_munis_np <- comp_munis_np %>% slice_sample(n = 4)
    }

    comp_munis_nonpartisan_rv(comp_munis_np)
  })

  # Set same-coalition comparison municipalities when home municipality is found
  observeEvent(found_municipality(), {
    home_id <- found_municipality()
    req(!is.null(home_id))

    home_info <- d_geo %>%
      st_drop_geometry() %>%
      filter(muni_id == home_id)

    home_party <- home_info$governing_party[1]
    same_coalition <- get_same_coalition_parties(home_party)

    comp_munis_sc <- d_geo %>%
      st_drop_geometry() %>%
      filter(
        governing_party %in% same_coalition,
        muni_id != home_id,
        muni_id %in% large_munis
      ) %>%
      select(muni_id, NOMGEO, NOM_ENT)

    # Fallback: any large city
    if (nrow(comp_munis_sc) == 0) {
      comp_munis_sc <- d_geo %>%
        st_drop_geometry() %>%
        filter(muni_id != home_id, muni_id %in% large_munis) %>%
        select(muni_id, NOMGEO, NOM_ENT)
    }

    if (nrow(comp_munis_sc) > 4) {
      comp_munis_sc <- comp_munis_sc %>% slice_sample(n = 4)
    }

    comp_munis_same_coalition_rv(comp_munis_sc)
  })

  # Return the comparison municipalities appropriate for the assigned treatment group
  active_comp_munis <- reactive({
    tg <- treatment_group()
    if (tg %in% c("control", "T1", "T2")) {
      comp_munis_nonpartisan_rv()
    } else if (tg == "T3") {
      comparison_municipalities()
    } else {
      comp_munis_same_coalition_rv()
    }
  })

  # Show appropriate page
  observe({
    pages <- paste0("page", 0:10)
    lapply(pages, hide)
    show(paste0("page", current_page()))
  })

  # Show/hide home confirmation section based on whether municipality is found
  observe({
    if (!is.null(found_municipality())) {
      show("home_confirmation_section")
      updateSelectInput(
        session,
        "dropdown_municipality",
        selected = found_municipality()
      )
    } else {
      hide("home_confirmation_section")
    }
  })

  # Show/hide "Other" text inputs for party questions
  observeEvent(input$party_preference, {
    toggleElement(
      "party_preference_other",
      condition = input$party_preference == "other"
    )
  })

  # Update 2nd/3rd party dropdowns to exclude already-selected parties
  all_party_choices <- c(
    "Select a party..." = "",
    "PAN (Partido Acción Nacional)" = "pan",
    "PRI (Partido Revolucionario Institucional)" = "pri",
    "PRD (Partido de la Revolución Democrática)" = "prd",
    "PVEM (Partido Verde Ecologista de México)" = "pvem",
    "PT (Partido del Trabajo)" = "pt",
    "MC (Movimiento Ciudadano)" = "mc",
    "MORENA" = "morena",
    "None" = "none"
  )

  observeEvent(c(input$party_preference, input$party_preference_2nd), {
    exclude_1st <- if (
      !is.null(input$party_preference) &&
        input$party_preference != "" &&
        input$party_preference != "none" &&
        input$party_preference != "other"
    ) {
      input$party_preference
    } else {
      NULL
    }

    # Update 2nd choice: exclude 1st
    choices_2nd <- all_party_choices[!(all_party_choices %in% exclude_1st)]
    current_2nd <- input$party_preference_2nd
    if (!is.null(current_2nd) && !(current_2nd %in% choices_2nd)) {
      current_2nd <- ""
    }
    updateSelectInput(
      session,
      "party_preference_2nd",
      choices = choices_2nd,
      selected = current_2nd
    )

    # Update 3rd choice: exclude 1st and 2nd
    exclude_2nd <- if (
      !is.null(current_2nd) && current_2nd != "" && current_2nd != "none"
    ) {
      current_2nd
    } else {
      NULL
    }
    choices_3rd <- all_party_choices[
      !(all_party_choices %in% c(exclude_1st, exclude_2nd))
    ]
    current_3rd <- input$party_preference_3rd
    if (!is.null(current_3rd) && !(current_3rd %in% choices_3rd)) {
      current_3rd <- ""
    }
    updateSelectInput(
      session,
      "party_preference_3rd",
      choices = choices_3rd,
      selected = current_3rd
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
    input$vote_intention_2027,
    {
      toggleElement(
        "vote_intention_2027_other",
        condition = "other" %in% input$vote_intention_2027
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
      enable("goto_page2_from_1")
    } else {
      disable("goto_page2_from_1")
    }
  })

  # Page navigation
  # Page 0 → Page 1 (Information Sheet → Find Municipality)
  observeEvent(input$goto_page1_from_0, {
    current_page(1)
  })

  # Page 1 → Page 2 (Practice)
  observeEvent(input$goto_page2_from_1, {
    current_page(2)
  })

  # Page 2 → Page 1
  observeEvent(input$goto_page1_from_2, {
    current_page(1)
  })

  # Page 2 → Page 3 (attention check: Radio #1, Social media #5 — validation TODO)
  practice_warning_msg <- reactiveVal(NULL)
  output$practice_warning <- renderUI({
    msg <- practice_warning_msg()
    if (!is.null(msg)) p(style = "color: #c0392b; font-weight: bold;", msg)
  })
  observeEvent(input$goto_page3_from_2, {
    if (length(input$practice_ranking) < 5) {
      practice_warning_msg(
        "Please drag all items into the ranking slots before continuing."
      )
    } else {
      practice_warning_msg(NULL)
      current_page(3)
    }
  })

  # Page 3 → Page 1
  observeEvent(input$goto_page1_from_3, {
    current_page(1)
  })

  # Page 7 → Page 8 (Post-treatment → Turnout/Vote Intention)
  observeEvent(input$goto_page8_from_7, {
    current_page(8)
  })

  # Page 8 → Page 7
  observeEvent(input$goto_page7_from_8, {
    current_page(7)
  })

  # Page 8 → Page 9 (Turnout/Vote Intention → Reference Municipalities)
  observeEvent(input$goto_page9_from_8, {
    current_page(9)
  })

  # Page 9 → Page 8
  observeEvent(input$goto_page8_from_9, {
    current_page(8)
  })

  # Page 3 → Page 4
  issue_importance_warning_msg <- reactiveVal(NULL)
  output$issue_importance_warning <- renderUI({
    msg <- issue_importance_warning_msg()
    if (!is.null(msg)) p(style = "color: #c0392b; font-weight: bold;", msg)
  })
  observeEvent(input$goto_page4_from_3, {
    if (length(input$issue_importance_ranking) < 5) {
      issue_importance_warning_msg(
        "Please drag all issues into the ranking slots before continuing."
      )
    } else {
      issue_importance_warning_msg(NULL)
      current_page(4)
    }
  })

  # Page 4 → Page 3
  observeEvent(input$goto_page3_from_4, {
    current_page(3)
  })

  # Page 4 → Page 5
  observeEvent(input$goto_page5_from_4, {
    current_page(5)
  })

  # Page 5 → Page 4
  observeEvent(input$goto_page4_from_5, {
    current_page(4)
  })

  # Page 5 → Page 6
  observeEvent(input$goto_page6_from_5, {
    current_page(6)
  })

  # Page 6 → Page 5
  observeEvent(input$goto_page5_from_6, {
    current_page(5)
  })

  # Page 6 → Page 7
  observeEvent(input$goto_page7_from_6, {
    current_page(7)
  })

  # Page 7 → Page 6
  observeEvent(input$goto_page6_from_7, {
    current_page(6)
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
      h5(icon("info-circle"), " Your Home Municipality:"),
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
    bucket_list(
      header = NULL,
      group_name = "issue_bucket",
      orientation = "horizontal",
      add_rank_list(
        text = "Available issues — drag into slots:",
        labels = issue_labels_randomized,
        input_id = "issue_importance_source",
        class = "default-sortable source-bin"
      ),
      add_rank_list(
        text = "Your ranking (1 = most important, 5 = least important):",
        labels = NULL,
        input_id = "issue_importance_ranking",
        class = "default-sortable"
      )
    )
  })
  observeEvent(
    input$issue_importance_ranking,
    {
      removeClass("issue_importance_wrapper", "issue-grayed")
    },
    ignoreInit = TRUE,
    once = TRUE
  )

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
          "How well do you think the government of ",
          muni_name,
          " handles crime?"
        ),
        min = 0,
        max = 100,
        value = 50
      ),
      fluidRow(
        column(
          6,
          p(
            "Handles crime extremely poorly",
            style = "color: #6c757d; font-size: 0.85em; margin-top: -15px;"
          )
        ),
        column(
          6,
          p(
            "Handles crime extremely well",
            style = "color: #6c757d; font-size: 0.85em; margin-top: -15px; text-align: right;"
          )
        )
      )
    )
  })

  # Post-treatment: home municipality crime handling slider
  output$home_crime_handling_post_ui <- renderUI({
    muni_name <- home_muni_name()
    tagList(
      sliderInput(
        "home_crime_handling_post",
        paste0(
          "How well do you think the government of ",
          muni_name,
          " handles crime?"
        ),
        min = 0,
        max = 100,
        value = 50
      ),
      fluidRow(
        column(
          6,
          p(
            "Handles crime extremely poorly",
            style = "color: #6c757d; font-size: 0.85em; margin-top: -15px;"
          )
        ),
        column(
          6,
          p(
            "Handles crime extremely well",
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
        "How many robberies do you think were reported in ",
        muni_name,
        " in 2025?"
      ),
      value = NULL,
      min = 0,
      step = 1
    )
  })

  # Dynamic bucket list for municipality crime ranking (relative to home)
  output$municipality_ranking_ui <- renderUI({
    home_id <- found_municipality()
    comp_munis <- active_comp_munis()
    if (is.null(home_id) || is.null(comp_munis)) {
      return(p(em(
        "Please find your home municipality first to see the ranking options."
      )))
    }
    home_info <- d_geo %>% st_drop_geometry() %>% filter(muni_id == home_id)
    comp_labels <- paste0(comp_munis$NOMGEO, ", ", comp_munis$NOM_ENT)
    make_crime_ranking_grid(home_info$NOMGEO, comp_labels, "muni_rank_comp_")
  })

  # Dynamic bucket list for post-treatment municipality crime ranking (relative to home)
  output$municipality_ranking_post_ui <- renderUI({
    home_id <- found_municipality()
    comp_munis <- active_comp_munis()

    # Show placeholder if home not yet found
    if (is.null(home_id) || is.null(comp_munis)) {
      return(p(em(
        "Please find your home municipality first to see the ranking options."
      )))
    }

    # Get home municipality name and state
    home_info <- d_geo %>%
      st_drop_geometry() %>%
      filter(muni_id == home_id)
    # Create labels for comparison municipalities only
    comp_labels <- paste0(comp_munis$NOMGEO, ", ", comp_munis$NOM_ENT)

    make_crime_ranking_grid(
      home_info$NOMGEO,
      comp_labels,
      "muni_rank_post_comp_"
    )
  })

  # Helper: get robbery change text for home municipality
  home_robbery_change_text <- reactive({
    home_id <- found_municipality()
    req(!is.null(home_id))

    home_name <- d_geo %>%
      st_drop_geometry() %>%
      filter(muni_id == home_id) %>%
      pull(NOMGEO) %>%
      `[`(1)

    home_robos <- robo_data %>%
      filter(Cve..Municipio == as.numeric(home_id)) %>%
      pull(robos)
    home_robos <- ifelse(
      length(home_robos) == 0 || is.na(home_robos),
      0,
      home_robos
    )

    paste0(
      "Keeping this in mind, there were ",
      format(home_robos, big.mark = ","),
      " robberies reported in ",
      home_name,
      " in 2025."
    )
  })

  # Plain information paragraph (shared by groups 2, 3, and 4)
  plain_info_text <- paste0(
    "Municipal police forces in Mexico can do a lot to help reduce crime. ",
    "Specifically, municipal forces can respond to criminal incidents, patrol the streets, ",
    "and supply valuable information to higher-level operations. ",
    "Decisions about funding and structure of municipal police forces are largely ",
    "in the hands of municipal presidents. ",
    "Therefore, municipal governments have some ability to control crime, although many factors that lead to crime ",
    "are out of the government\u2019s hands."
  )

  # Show only the assigned treatment condition on page 6
  output$treatment_content_ui <- renderUI({
    tg <- treatment_group()
    switch(
      tg,
      "control" = uiOutput("treatment_control_ui"),
      "T1" = uiOutput("treatment_plain_info_ui"),
      "T2" = tagList(
        uiOutput("treatment_nonpartisan_ui"),
        plotOutput("treatment_histogram_nonpartisan", height = "350px")
      ),
      "T3" = tagList(
        uiOutput("treatment_partisan_ui"),
        plotOutput("treatment_histogram_partisan", height = "350px")
      ),
      "T4" = tagList(
        uiOutput("treatment_same_coalition_ui"),
        plotOutput("treatment_histogram_same_coalition", height = "350px")
      )
    )
  })

  # Control (Placebo)
  output$treatment_control_ui <- renderUI({
    p(
      "Agave cultivation is a major industry in Mexico, and specialized farming techniques can do a lot ",
      "to help improve plant health. Specifically, farmers can monitor for invasive pests, implement ",
      "sustainable harvest cycles, and ensure proper soil drainage to protect the heart of the plant. ",
      "Decisions about the timing and methods of these agricultural practices are largely in the hands ",
      "of independent field managers. Therefore, plantation owners have some ability to protect their ",
      "crops, although many weather patterns that affect agave growth are out of the owners\u2019 hands."
    )
  })

  # T1: Plain Information
  output$treatment_plain_info_ui <- renderUI({
    change_text <- home_robbery_change_text()
    tagList(
      p(plain_info_text),
      p(strong(change_text))
    )
  })

  # T2: Non-Partisan Comparison
  output$treatment_nonpartisan_ui <- renderUI({
    home_id <- found_municipality()
    req(!is.null(home_id))

    home_info <- d_geo %>%
      st_drop_geometry() %>%
      filter(muni_id == home_id)

    home_name <- home_info$NOMGEO[1]
    home_state <- home_info$NOM_ENT[1]
    change_text <- home_robbery_change_text()

    tagList(
      p(plain_info_text),
      p(strong(change_text)),
      p(
        paste0(
          "The following graph shows the number of robberies reported in 2025 for ",
          home_name,
          " and a sample of other municipalities, as recorded by the Secretariado ",
          "Ejecutivo del Sistema Nacional de Seguridad P\u00fablica (SESNSP)."
        )
      )
    )
  })

  # T3: Partisan Comparison
  output$treatment_partisan_ui <- renderUI({
    home_id <- found_municipality()
    req(!is.null(home_id))

    home_info <- d_geo %>%
      st_drop_geometry() %>%
      filter(muni_id == home_id)

    home_name <- home_info$NOMGEO[1]
    home_state <- home_info$NOM_ENT[1]
    home_party <- home_info$governing_party[1]
    opposite_coalition <- get_opposite_parties(home_party)
    opposite_label <- get_coalition_label(opposite_coalition)
    change_text <- home_robbery_change_text()

    tagList(
      p(plain_info_text),
      p(strong(change_text)),
      p(
        paste0(
          "The following graph shows the number of robberies reported in 2025 for ",
          home_name,
          " and a sample of other municipalities that are governed by ",
          opposite_label,
          ", as recorded by the Secretariado Ejecutivo del Sistema Nacional de Seguridad P\u00fablica (SESNSP)."
        )
      )
    )
  })

  # T4: Same-Coalition Comparison
  output$treatment_same_coalition_ui <- renderUI({
    home_id <- found_municipality()
    req(!is.null(home_id))

    home_info <- d_geo %>%
      st_drop_geometry() %>%
      filter(muni_id == home_id)

    home_name <- home_info$NOMGEO[1]
    home_party <- home_info$governing_party[1]
    same_coalition <- get_same_coalition_parties(home_party)
    same_label <- get_coalition_label(same_coalition)
    change_text <- home_robbery_change_text()

    tagList(
      p(plain_info_text),
      p(strong(change_text)),
      p(
        paste0(
          "The following graph shows the number of robberies reported in 2025 for ",
          home_name,
          " and a sample of other municipalities that are governed by ",
          same_label,
          ", as recorded by the Secretariado Ejecutivo del Sistema Nacional de Seguridad P\u00fablica (SESNSP)."
        )
      )
    )
  })

  # T4: Same-coalition histogram
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

  # Combined governance grid for home + comparison municipalities
  output$governance_grid_ui <- renderUI({
    home_id <- found_municipality()
    comp_munis <- active_comp_munis()

    if (is.null(home_id)) {
      return(p(em("Please find your home municipality first.")))
    }

    home_info <- d_geo %>%
      st_drop_geometry() %>%
      filter(muni_id == home_id)
    home_name <- paste0(home_info$NOMGEO[1], ", ", home_info$NOM_ENT[1])

    # Build list of municipality rows: home first, then comparisons
    muni_rows <- list(
      list(
        name = home_name,
        input_name = "home_governing_party_belief",
        is_home = TRUE
      )
    )
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
      "Other",
      "Don't\nknow"
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

    # Table header
    header_cells <- c(
      list(tags$th("Municipality")),
      lapply(party_labels, function(lbl) tags$th(HTML(gsub("\n", "<br>", lbl))))
    )

    # Table body rows
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
        "Which party or parties do you believe currently govern each of the following municipalities? (select all that apply)"
      ),
      div(
        id = "governance_grid",
        class = "governance-grid",
        tags$table(
          tags$thead(do.call(tags$tr, header_cells)),
          do.call(tags$tbody, body_rows)
        )
      )
    )
  })

  # Dynamic incumbent crime rating question (pre-treatment)

  build_treatment_plot <- function(plot_df, show_party = FALSE) {
    if (show_party) {
      fill_values <- c(
        "Your municipality" = "#0072B2",
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
        "Your municipality" = "#0072B2",
        "Comparison" = "#E69F00"
      )
    }

    ggplot(plot_df, aes(x = municipality, y = robos, fill = fill_group)) +
      geom_col(color = "black", linewidth = 0.5) +
      scale_fill_manual(values = fill_values, name = NULL) +
      labs(x = NULL, y = "Robos en 2025") +
      theme_minimal() +
      theme(
        axis.text.x = element_text(angle = 45, hjust = 1, size = 11),
        axis.title.y = element_text(size = 12),
        panel.grid.major.x = element_blank(),
        legend.position = "bottom",
        legend.box = "horizontal"
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
        robos = ifelse(is.na(robos), 0, robos),
        fill_group = case_when(
          muni_id == home_id ~ "Your municipality",
          show_party & !is.na(coalition_label) ~ coalition_label,
          show_party ~ "Other",
          TRUE ~ "Comparison"
        )
      ) %>%
      arrange(robos)

    data.frame(
      municipality = factor(muni_info$NOMGEO, levels = muni_info$NOMGEO),
      robos = muni_info$robos,
      fill_group = muni_info$fill_group
    )
  }

  # T3: Partisan comparison histogram (with party colors)
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

  # T2: Non-partisan comparison histogram (no party colors)
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

  # Reference instructions text with dynamic municipality name
  output$reference_instructions_text <- renderUI({
    req(!is.null(found_municipality()))
    muni_data <- d_geo %>%
      st_drop_geometry() %>%
      filter(muni_id == found_municipality())
    muni_name <- paste0(muni_data$NOMGEO, ", ", muni_data$NOM_ENT)

    p(strong(
      paste0(
        "If you could compare robbery rates in ",
        muni_name,
        " to other municipalities, which ones would you find most informative? ",
        "Use the search bar below to select them."
      )
    ))
  })

  # Display selected municipality names
  output$selected_munis_display <- renderPrint({
    ids <- selected_map_munis()
    if (length(ids) == 0) {
      return("No additional municipalities selected yet.")
    }
    d_geo %>%
      st_drop_geometry() %>%
      filter(muni_id %in% ids) %>%
      mutate(full_name = paste0(NOMGEO, ", ", NOM_ENT)) %>%
      pull(full_name)
  })

  # Page 8 Map - Interactive map for reference municipality selection
  output$map_page9 <- renderLeaflet({
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
        colors = c(unname(coalition_map_colors), "#D3D3D3", "#9B59B6"),
        labels = c(names(coalition_map_colors), "Other", "Selected"),
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
          fillColor = "#0072B2",
          fillOpacity = 0,
          color = "#0072B2",
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

  # Click-to-toggle municipality selection on page 8 map
  observeEvent(input$map_page9_shape_click, {
    id <- input$map_page9_shape_click$id
    req(id)
    req(!is.null(found_municipality()))

    # Clicking home municipality just zooms to it
    if (id == found_municipality()) {
      muni_data <- d_geo %>% filter(muni_id == found_municipality())
      centroid <- st_centroid(st_geometry(muni_data))
      coords <- st_coordinates(centroid)

      leafletProxy("map_page9") %>%
        setView(lng = coords[1], lat = coords[2], zoom = 8) %>%
        clearGroup("found_muni") %>%
        addPolygons(
          data = muni_data,
          group = "found_muni",
          fillColor = "#0072B2",
          fillOpacity = 0,
          color = "#0072B2",
          weight = 3,
          label = ~ paste0(NOMGEO, ", ", NOM_ENT, " (Your municipality)")
        )

      if (!is.null(found_address_coords())) {
        address_coords <- found_address_coords()
        leafletProxy("map_page9") %>%
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
      leafletProxy("map_page9") %>%
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
            ifelse(!is.na(coalition_label), paste0(" — ", coalition_label), "")
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
      leafletProxy("map_page9") %>%
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
      leafletProxy("map_page9") %>%
        clearGroup("found_muni") %>%
        addPolygons(
          data = d_geo[d_geo$muni_id == found_municipality(), ],
          group = "found_muni",
          fillColor = "#0072B2",
          fillOpacity = 0,
          color = "#0072B2",
          weight = 3,
          label = ~ paste0(NOMGEO, ", ", NOM_ENT, " (Your municipality)")
        )
    }

    if (!is.null(found_address_coords())) {
      address_coords <- found_address_coords()
      leafletProxy("map_page9") %>%
        addMarkers(
          lng = address_coords$lon,
          lat = address_coords$lat,
          popup = "Your address",
          group = "address_marker"
        )
    }
  })

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

        leafletProxy("map_page9") %>%
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
            fillColor = "#0072B2",
            fillOpacity = 0.6,
            color = "#005080",
            weight = 2,
            label = ~ paste0(NOMGEO, ", ", NOM_ENT, " (Your municipality)")
          )

        if (!is.null(found_address_coords())) {
          address_coords <- found_address_coords()
          leafletProxy("map_page9") %>%
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

  # Zoom to searched municipality
  observeEvent(input$zoom_search, {
    req(input$muni_search != "")
    muni_id <- input$muni_search
    muni_data <- d_geo %>% filter(muni_id == !!muni_id)
    req(nrow(muni_data) > 0)

    centroid <- st_centroid(st_geometry(muni_data))
    coords <- st_coordinates(centroid)
    leafletProxy("map_page9") %>%
      setView(lng = coords[1], lat = coords[2], zoom = 8)
  })

  # Clear search and reset map view
  observeEvent(input$clear_search, {
    updateSelectizeInput(session, "muni_search", selected = character(0))
    leafletProxy("map_page9") %>%
      fitBounds(lng1 = -115.0, lat1 = 16.0, lng2 = -88.0, lat2 = 30.5)
  })

  # Zoom to home municipality
  observeEvent(input$zoom_home, {
    home_id <- found_municipality()
    req(home_id)
    home_data <- d_geo[d_geo$muni_id == home_id, ]
    centroid <- st_centroid(st_geometry(home_data))
    coords <- st_coordinates(centroid)
    leafletProxy("map_page9") %>%
      setView(lng = coords[1], lat = coords[2], zoom = 8)
  })

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
      Found_Municipality = found_muni_name,
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
      # Political views
      Party_Preference = ifelse(
        is.null(input$party_preference) || input$party_preference == "",
        NA_character_,
        input$party_preference
      ),
      Party_Preference_Other = ifelse(
        is.null(input$party_preference_other) ||
          input$party_preference_other == "",
        NA_character_,
        input$party_preference_other
      ),
      Party_Preference_2nd = ifelse(
        is.null(input$party_preference_2nd) || input$party_preference_2nd == "",
        NA_character_,
        input$party_preference_2nd
      ),
      Party_Preference_3rd = ifelse(
        is.null(input$party_preference_3rd) || input$party_preference_3rd == "",
        NA_character_,
        input$party_preference_3rd
      ),
      Left_Right_Scale = input$left_right_scale,
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
      # Issue importance rankings (from drag-and-drop rank_list)
      # Convert ordered list to numeric ranks (1 = most important)
      Importance_Crime = {
        ranking <- input$issue_importance_ranking
        match("Seguridad / Delincuencia", ranking, nomatch = NA_integer_)
      },
      Importance_Economy = {
        ranking <- input$issue_importance_ranking
        match("Economía / Inflación", ranking, nomatch = NA_integer_)
      },
      Importance_Employment_Poverty = {
        ranking <- input$issue_importance_ranking
        match("Desempleo / Bajos salarios", ranking, nomatch = NA_integer_)
      },
      Importance_Corruption = {
        ranking <- input$issue_importance_ranking
        match("Corrupción", ranking, nomatch = NA_integer_)
      },
      Importance_Education_Health = {
        ranking <- input$issue_importance_ranking
        match("Educación y servicios de salud", ranking, nomatch = NA_integer_)
      },
      # Priors - municipality crime ranking relative to home
      # (home is the fixed reference point)
      # Comparison municipality names (for reference)
      Treatment_Group = treatment_group(),
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
      # Comparison municipality crime categories (relative to home)
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
      Robbery_Estimate = ifelse(
        is.null(input$robbery_estimate),
        NA_integer_,
        input$robbery_estimate
      ),
      Home_Crime_Handling_Pre = input$home_crime_handling_pre,
      MORENA_Crime_Rating = input$morena_crime_rating,
      Coalition_PAN_PRI_PRD_Crime_Rating = input$coalition_pan_pri_prd_crime_rating,
      Coalition_MC_Crime_Rating = input$coalition_mc_crime_rating,
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
      # Treatment outcomes
      Home_Crime_Handling_Post = input$home_crime_handling_post,
      Turnout_Likelihood = input$turnout_likelihood,
      MORENA_Crime_Rating_Post = input$morena_crime_rating_post,
      Coalition_PAN_PRI_PRD_Crime_Rating_Post = input$coalition_pan_pri_prd_crime_rating_post,
      Coalition_MC_Crime_Rating_Post = input$coalition_mc_crime_rating_post,
      # Post-treatment municipality crime categories (relative to home)
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
      Vote_Intention_2027 = if (
        is.null(input$vote_intention_2027) ||
          length(input$vote_intention_2027) == 0
      ) {
        NA_character_
      } else {
        paste(input$vote_intention_2027, collapse = ";")
      },
      Vote_Intention_2027_Other = ifelse(
        is.null(input$vote_intention_2027_other) ||
          input$vote_intention_2027_other == "",
        NA_character_,
        input$vote_intention_2027_other
      ),
      Timestamp = as.character(Sys.time()),
      stringsAsFactors = FALSE
    )

    # Save to local CSV
    save_success <- tryCatch(
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

    if (!save_success) {
      showNotification(
        "Error: Your response could not be saved. Please try again or contact the researcher.",
        type = "error",
        duration = NULL
      )
    }

    # Go to thank you page instead of resetting
    current_page(10)
  })
}

shinyApp(ui = ui, server = server)
