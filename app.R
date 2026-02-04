library(shiny)
library(shinyjs)
library(sortable)
library(sf)
library(leaflet)
library(dplyr)
library(httr)
library(jsonlite)
library(ggplot2)

# Read municipalities from GeoJSON file
d_geo <- st_read("00mun_simplified.geojson", quiet = TRUE)
municipality_list <- sort(unique(d_geo$mun_state))
d_geo$muni_id <- d_geo$CVEGEO

# Load robbery data for treatment graph
robo_data <- readRDS("robo_2025.rds")

# Randomly assign governing parties to municipalities
# TODO: Replace with actual party data when available
set.seed(42) # For reproducibility during testing
parties <- c("MORENA", "PAN", "PRI", "PRD", "PVEM", "PT", "MC")
d_geo$governing_party <- sample(parties, nrow(d_geo), replace = TRUE)

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

ui <- fluidPage(
  useShinyjs(),
  tags$head(
    tags$style(HTML(
      "
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
      .rank-list-container .rank-list-item:nth-child(1) {
        background-color: #e74c3c;
        color: #ffffff;
      }
      .rank-list-container .rank-list-item:nth-child(2) {
        background-color: #f1948a;
      }
      .rank-list-container .rank-list-item:nth-child(3) {
        background-color: #f5c6cb;
      }
      .rank-list-container .rank-list-item:nth-child(4) {
        background-color: #fae5e8;
      }
      .rank-list-container .rank-list-item:nth-child(5) {
        background-color: #ffffff;
      }
    "
    )),
    tags$script(HTML(
      "
      $(document).on('change', 'input[name=\"party_preference\"]', function() {
        Shiny.setInputValue('party_preference', $(this).val());
      });
      $(document).on('change', 'input[name=\"vote_intention_2027\"]', function() {
        Shiny.setInputValue('vote_intention_2027', $(this).val());
      });
      $(document).on('change', 'input[name=\"last_election_vote\"]', function() {
        Shiny.setInputValue('last_election_vote', $(this).val());
      });
      $(document).on('keypress', '#address', function(e) {
        if (e.which == 13) {
          e.preventDefault();
          $('#geocode_btn').click();
        }
      });
    "
    ))
  ),
  titlePanel("Municipality Survey"),

  fluidRow(
    column(
      8,
      offset = 2,
      wellPanel(
        # Page 1: Find Municipality
        hidden(
          div(
            id = "page1",
            h4("Step 1: Find Your Municipality"),

            # Address search
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
                style = "color: #0066cc; margin-top: 10px; padding: 10px; background-color: #e7f3ff; border-radius: 4px;",
                icon("spinner", class = "fa-spin"),
                tags$strong(" Searching for your address..."),
                tags$br(),
                tags$small("This may take a few seconds.")
              )
            ),
            uiOutput("geocode_result"),

            # Home municipality info (shown after geocoding)
            uiOutput("home_muni_info"),

            leafletOutput("map_page1", height = 450),
            p(em(
              style = "color: #6c757d; font-size: 0.9em;",
              "Note: The map may take a few seconds to load."
            )),

            hr(),
            fluidRow(
              column(
                12,
                align = "right",
                disabled(
                  actionButton(
                    "goto_page2",
                    "Next →",
                    class = "btn-primary btn-lg"
                  )
                )
              )
            )
          )
        ),

        # Page 2: Confirm Home Municipality
        hidden(
          div(
            id = "page2",
            h4("Step 2: Confirm Your Municipality"),

            uiOutput("home_municipality_summary"),

            h5("Is this correct?"),
            p("If not, go back to the map to update your selection."),
            actionButton(
              "goto_page1_from_2_edit",
              "← Back to Map",
              icon = icon("map"),
              class = "btn-secondary"
            ),

            hr(),
            fluidRow(
              column(
                6,
                actionButton(
                  "goto_page1_from_2",
                  "← Back",
                  class = "btn-secondary btn-lg"
                )
              ),
              column(
                6,
                align = "right",
                actionButton(
                  "goto_page3_from_2",
                  "Next →",
                  class = "btn-primary btn-lg"
                )
              )
            )
          )
        ),

        # Page 3: Political Views & Your Municipality
        hidden(
          div(
            id = "page3",
            h4("Step 3: Your Municipality"),

            # Political Party Allegiance
            h5(strong("Political Views")),

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

            radioButtons(
              "party_strength",
              "How strongly do you identify with this party?",
              choices = c(
                "Not strongly at all" = "not_strongly",
                "Not very strongly" = "not_very_strongly",
                "Somewhat strongly" = "somewhat_strongly",
                "Very strongly" = "very_strongly"
              ),
              selected = character(0)
            ),

            hr(),

            tags$div(
              class = "form-group",
              tags$label(
                "Which party did you vote for in the last municipal election?"
              ),
              tags$div(
                id = "last_election_vote_group",
                class = "party-radio-group",
                party_radio_choice(
                  "pan",
                  "PAN (Partido Acción Nacional)",
                  "PAN_logo.png",
                  input_name = "last_election_vote"
                ),
                party_radio_choice(
                  "pri",
                  "PRI (Partido Revolucionario Institucional)",
                  "PRI_logo.png",
                  input_name = "last_election_vote"
                ),
                party_radio_choice(
                  "prd",
                  "PRD (Partido de la Revolución Democrática)",
                  "PRD_logo.png",
                  input_name = "last_election_vote"
                ),
                party_radio_choice(
                  "pvem",
                  "PVEM (Partido Verde Ecologista de México)",
                  "PVEM_logo.png",
                  input_name = "last_election_vote"
                ),
                party_radio_choice(
                  "pt",
                  "PT (Partido del Trabajo)",
                  "PT_logo.png",
                  input_name = "last_election_vote"
                ),
                party_radio_choice(
                  "mc",
                  "MC (Movimiento Ciudadano)",
                  "Movimiento_Ciudadano_logo.png",
                  input_name = "last_election_vote"
                ),
                party_radio_choice(
                  "morena",
                  "MORENA",
                  "Morena_logo.png",
                  input_name = "last_election_vote"
                ),
                party_radio_choice(
                  "did_not_vote",
                  "Did not vote",
                  input_name = "last_election_vote"
                ),
                party_radio_choice(
                  "dont_remember",
                  "Don't remember",
                  input_name = "last_election_vote"
                ),
                party_radio_choice(
                  "other",
                  "Other",
                  input_name = "last_election_vote"
                )
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

            h5(strong("Your Municipality's Performance")),

            p(
              "Drag and drop the following municipalities to rank them on crime, from worst (top) to best (bottom):"
            ),
            uiOutput("municipality_ranking_ui"),

            uiOutput("incumbent_crime_rating_ui"),

            br(),

            sliderInput(
              "turnout_likelihood_pre",
              "How likely are you to vote in the 2027 local elections?",
              min = 0,
              max = 100,
              value = 50
            ),

            hr(),

            # Importance of Issues, language loosely based on Mitofsky public opinion survey https://drive.google.com/file/d/1GPiJS0CDUoho_NRnb3M3T5rHq5dtiqLb/view
            h5(strong("Issue Importance")),
            rank_list(
              text = "Drag to rank the following issues in order of importance (top = most important):",
              labels = c(
                "Delincuencia",
                "Economia / Inflacion",
                "Corrupción",
                "Desempleo / Bajos salarios",
                "Educacion / Servicios de salud"
              ),
              input_id = "issue_importance_ranking"
            ),

            hr(),
            fluidRow(
              column(
                6,
                actionButton(
                  "goto_page2_from_3",
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

        # Page 4: About You
        hidden(
          div(
            id = "page4",
            h4("Step 4: About You"),

            # Demographics
            h5(strong("Demographics")),

            fluidRow(
              column(
                6,
                numericInput(
                  "age",
                  "What is your age?",
                  value = NULL,
                  min = 18,
                  max = 120
                )
              ),
              column(
                6,
                selectInput(
                  "gender",
                  "What is your gender?",
                  choices = c(
                    "Select..." = "",
                    "Male" = "male",
                    "Female" = "female",
                    "Non-binary" = "non_binary",
                    "Other" = "other",
                    "Prefer not to say" = "prefer_not_to_say"
                  )
                )
              )
            ),

            selectInput(
              "indigenous",
              "Do you identify as indigenous?",
              choices = c(
                "Select..." = "",
                "Yes" = "yes",
                "No" = "no",
                "Prefer not to say" = "prefer_not_to_say"
              )
            ),

            hr(),

            # Attention check
            radioButtons(
              "attention_check",
              "To ensure you are reading each question carefully, please select 'Somewhat agree' below.",
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
                  "goto_page3_from_4",
                  "← Back",
                  class = "btn-secondary btn-lg"
                )
              ),
              column(
                6,
                align = "right",
                actionButton(
                  "goto_page5",
                  "Next →",
                  class = "btn-primary btn-lg"
                )
              )
            )
          )
        ),

        # Page 5: Treatment
        hidden(
          div(
            id = "page5",
            h4("Step 5: Review & Survey"),

            uiOutput("treatment_description"),

            plotOutput("treatment_histogram", height = "350px"),

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

        # Page 6: Post-Treatment Questions
        hidden(
          div(
            id = "page6",
            h4("Step 6: Post-Treatment Survey"),

            sliderInput(
              "turnout_likelihood",
              "How likely are you to vote in the 2027 local elections?",
              min = 0,
              max = 100,
              value = 50
            ),

            br(),

            uiOutput("vote_likelihood_ui"),

            br(),

            uiOutput("incumbent_crime_rating_post_ui"),

            br(),

            h5(strong("Your Municipality's Performance (Post-Treatment)")),

            p(
              "After seeing the crime data, drag and drop the following municipalities to rank them on crime, from worst (top) to best (bottom):"
            ),
            uiOutput("municipality_ranking_post_ui"),

            br(),

            uiOutput("party_allegiance_update_ui"),

            br(),

            tags$div(
              class = "form-group",
              tags$label(
                "Which party do you intend to vote for in the 2027 municipal elections?"
              ),
              tags$div(
                id = "vote_intention_2027_group",
                class = "party-radio-group",
                party_radio_choice(
                  "pan",
                  "PAN (Partido Acción Nacional)",
                  "PAN_logo.png",
                  input_name = "vote_intention_2027"
                ),
                party_radio_choice(
                  "pri",
                  "PRI (Partido Revolucionario Institucional)",
                  "PRI_logo.png",
                  input_name = "vote_intention_2027"
                ),
                party_radio_choice(
                  "prd",
                  "PRD (Partido de la Revolución Democrática)",
                  "PRD_logo.png",
                  input_name = "vote_intention_2027"
                ),
                party_radio_choice(
                  "pvem",
                  "PVEM (Partido Verde Ecologista de México)",
                  "PVEM_logo.png",
                  input_name = "vote_intention_2027"
                ),
                party_radio_choice(
                  "pt",
                  "PT (Partido del Trabajo)",
                  "PT_logo.png",
                  input_name = "vote_intention_2027"
                ),
                party_radio_choice(
                  "mc",
                  "MC (Movimiento Ciudadano)",
                  "Movimiento_Ciudadano_logo.png",
                  input_name = "vote_intention_2027"
                ),
                party_radio_choice(
                  "morena",
                  "MORENA",
                  "Morena_logo.png",
                  input_name = "vote_intention_2027"
                ),
                party_radio_choice(
                  "undecided",
                  "Undecided",
                  input_name = "vote_intention_2027"
                ),
                party_radio_choice(
                  "will_not_vote",
                  "Will not vote",
                  input_name = "vote_intention_2027"
                ),
                party_radio_choice(
                  "other",
                  "Other",
                  input_name = "vote_intention_2027"
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

        # Page 7: Select Reference Municipalities
        hidden(
          div(
            id = "page7",
            h4("Step 7: Select Reference Municipalities"),

            p("Now we would like you to select some reference municipalities for comparison."),
            uiOutput("reference_instructions_text"),
            p(em(
              "(You can select multiple municipalities by clicking on them. Click again to deselect.)"
            )),

            leafletOutput("map_page7", height = 450),
            p(em(
              style = "color: #6c757d; font-size: 0.9em;",
              "Note: Your home municipality is shown in blue."
            )),

            # Municipality search box
            fluidRow(
              column(
                7,
                selectizeInput(
                  "muni_search",
                  "Search for a municipality by name:",
                  choices = NULL,
                  options = list(
                    placeholder = 'Type to search municipalities...'
                  )
                )
              ),
              column(
                2,
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
                  "goto_page6_from_7",
                  "← Back",
                  class = "btn-secondary btn-lg"
                )
              ),
              column(
                6,
                align = "right",
                disabled(
                  actionButton(
                    "goto_page8_from_7",
                    "Next →",
                    class = "btn-primary btn-lg"
                  )
                )
              )
            )
          )
        ),

        # Page 8: Feedback
        hidden(
          div(
            id = "page8",
            h4("Step 8: Feedback"),

            textAreaInput(
              "additional_comments",
              "Do you have any comments about this survey? Anything I could improve? (Optional)",
              placeholder = "Enter any additional information here...",
              rows = 3
            ),

            hr(),
            fluidRow(
              column(
                6,
                actionButton(
                  "goto_page7_from_8",
                  "← Back",
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

        # Page 9: Thank you screen
        hidden(
          div(
            id = "page9",
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
  # Reactive values
  selected_map_munis <- reactiveVal(character())
  found_municipality <- reactiveVal(NULL)
  found_address_coords <- reactiveVal(NULL)
  current_page <- reactiveVal(1)
  comparison_municipalities <- reactiveVal(NULL) # Stores comparison muni data for ranking & graph

  # Define the file path for saving responses
  responses_file <- "survey_responses.csv"

  # Update municipality search choices on server side
  updateSelectizeInput(
    session,
    "muni_search",
    choices = c(" " = "", setNames(d_geo$muni_id, d_geo$mun_state)),
    server = TRUE
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

    # Get comparison municipalities (same state, same party, excluding home)
    comp_munis <- d_geo %>%
      st_drop_geometry() %>%
      filter(
        NOM_ENT == home_state,
        governing_party == home_party,
        muni_id != home_id
      ) %>%
      select(muni_id, NOMGEO, NOM_ENT)

    # Randomly select up to 4 comparison municipalities
    if (nrow(comp_munis) > 4) {
      comp_munis <- comp_munis %>% slice_sample(n = 4)
    }

    comparison_municipalities(comp_munis)
  })

  # Show appropriate page
  observe({
    pages <- paste0("page", 1:9)
    lapply(pages, hide)
    show(paste0("page", current_page()))
  })

  # Show/hide "Other" text inputs for party questions
  observeEvent(input$party_preference, {
    toggleElement(
      "party_preference_other",
      condition = input$party_preference == "other"
    )
  })

  observeEvent(input$last_election_vote, {
    toggleElement(
      "last_election_vote_other",
      condition = input$last_election_vote == "other"
    )
  })

  observeEvent(input$vote_intention_2027, {
    toggleElement(
      "vote_intention_2027_other",
      condition = input$vote_intention_2027 == "other"
    )
  })

  # Enable/disable Next button on page 1 based on home municipality found
  observe({
    has_home <- !is.null(found_municipality())

    if (has_home) {
      enable("goto_page2")
    } else {
      disable("goto_page2")
    }
  })

  # Page navigation
  # Page 1 → Page 2
  observeEvent(input$goto_page2, {
    current_page(2)
  })

  # Page 2 → Page 1
  observeEvent(input$goto_page1_from_2, {
    current_page(1)
  })

  observeEvent(input$goto_page1_from_2_edit, {
    current_page(1)
  })

  # Page 2 → Page 3
  observeEvent(input$goto_page3_from_2, {
    current_page(3)
  })

  # Page 3 → Page 2
  observeEvent(input$goto_page2_from_3, {
    current_page(2)
  })

  # Page 3 → Page 4
  observeEvent(input$goto_page4_from_3, {
    current_page(4)
  })

  # Page 4 → Page 3
  observeEvent(input$goto_page3_from_4, {
    current_page(3)
  })

  # Page 4 → Page 5
  observeEvent(input$goto_page5, {
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

  # Page 6 → Page 7 (Reference Selection)
  observeEvent(input$goto_page7_from_6, {
    current_page(7)
  })

  # Page 7 → Page 6
  observeEvent(input$goto_page6_from_7, {
    current_page(6)
  })

  # Page 7 → Page 8 (Feedback)
  observeEvent(input$goto_page8_from_7, {
    current_page(8)
  })

  # Page 8 → Page 7
  observeEvent(input$goto_page7_from_8, {
    current_page(7)
  })

  # Display home municipality summary on page 2
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
        tags$span(home_muni, style = "color: #0072B2; font-size: 1.2em; font-weight: bold;")
      )
    )
  })

  # Display selection summary (used elsewhere if needed)
  output$selection_summary <- renderUI({
    home_muni <- if (!is.null(found_municipality())) {
      d_geo %>%
        st_drop_geometry() %>%
        filter(muni_id == found_municipality()) %>%
        mutate(full_name = paste0(NOMGEO, ", ", NOM_ENT)) %>%
        pull(full_name)
    } else {
      "None"
    }

    selected_munis <- if (length(selected_map_munis()) > 0) {
      d_geo %>%
        st_drop_geometry() %>%
        filter(muni_id %in% selected_map_munis()) %>%
        mutate(full_name = paste0(NOMGEO, ", ", NOM_ENT)) %>%
        pull(full_name) %>%
        paste(collapse = ", ")
    } else {
      "None selected"
    }

    div(
      style = "background-color: #f8f9fa; padding: 15px; border-radius: 4px; margin-bottom: 20px;",
      h5(icon("info-circle"), " Your Selections:"),
      tags$p(
        tags$strong("Home Municipality: "),
        tags$span(home_muni, style = "color: #0072B2;")
      ),
      tags$p(
        tags$strong("Additional Selected Municipalities: "),
        tags$span(selected_munis, style = "color: #E69F00;")
      )
    )
  })

  # Dynamic rank list for municipality crime ranking
  output$municipality_ranking_ui <- renderUI({
    home_id <- found_municipality()
    comp_munis <- comparison_municipalities()

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
    home_label <- paste0(home_info$NOMGEO, ", ", home_info$NOM_ENT)

    # Create labels: home municipality + comparison municipalities (with state)
    comp_labels <- paste0(comp_munis$NOMGEO, ", ", comp_munis$NOM_ENT)
    labels <- c(home_label, comp_labels)

    rank_list(
      text = NULL,
      labels = labels,
      input_id = "municipality_crime_ranking"
    )
  })

  # Dynamic rank list for post-treatment municipality crime ranking
  output$municipality_ranking_post_ui <- renderUI({
    home_id <- found_municipality()
    comp_munis <- comparison_municipalities()

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
    home_label <- paste0(home_info$NOMGEO, ", ", home_info$NOM_ENT)

    # Create labels: home municipality + comparison municipalities (with state)
    comp_labels <- paste0(comp_munis$NOMGEO, ", ", comp_munis$NOM_ENT)
    labels <- c(home_label, comp_labels)

    rank_list(
      text = NULL,
      labels = labels,
      input_id = "municipality_crime_ranking_post"
    )
  })

  # Treatment description with dynamic municipality/state/party names
  output$treatment_description <- renderUI({
    home_id <- found_municipality()
    req(!is.null(home_id))

    home_info <- d_geo %>%
      st_drop_geometry() %>%
      filter(muni_id == home_id)

    home_name <- home_info$NOMGEO[1]
    home_state <- home_info$NOM_ENT[1]
    home_party <- home_info$governing_party[1]

    tagList(
      h5(
        paste0(
          "The following graph shows the change in robbery rates from 2024 to 2025 (January-November) for ",
          home_name,
          " and a sample of other municipalities in ",
          home_state,
          " that are governed by ",
          home_party,
          ", as recorded by the Secretariado Ejecutivo del Sistema Nacional de Seguridad Pública (SESNSP)."
        )
      ),
      p(
        em("The values shown are z-scores, which indicate how many standard deviations each municipality's change is from the national average. Positive values indicate increases in robbery, negative values indicate decreases.")
      )
    )
  })

  # Dynamic party allegiance question with respondent's party
  output$party_allegiance_update_ui <- renderUI({
    party_code <- input$party_preference

    # Map party codes to display names
    party_names <- c(
      "pan" = "PAN",
      "pri" = "PRI",
      "prd" = "PRD",
      "pvem" = "PVEM",
      "pt" = "PT",
      "mc" = "MC",
      "morena" = "MORENA",
      "none" = "your party",
      "other" = "your party"
    )

    party_display <- if (is.null(party_code) || party_code == "") {
      "your party"
    } else {
      party_names[party_code]
    }

    radioButtons(
      "party_allegiance_update",
      paste0(
        "Has this information changed how strongly you identify with ",
        party_display,
        "?"
      ),
      choices = c(
        "Much less strongly" = "much_less",
        "Somewhat less strongly" = "somewhat_less",
        "No change" = "no_change",
        "Somewhat more strongly" = "somewhat_more",
        "Much more strongly" = "much_more"
      ),
      selected = character(0)
    )
  })

  # Dynamic incumbent crime rating question (pre-treatment)
  output$incumbent_crime_rating_ui <- renderUI({
    home_id <- found_municipality()

    incumbent_party <- if (is.null(home_id)) {
      "the incumbent party"
    } else {
      d_geo %>%
        st_drop_geometry() %>%
        filter(muni_id == home_id) %>%
        pull(governing_party)
    }

    radioButtons(
      "incumbent_crime_rating",
      paste0(
        "How well do you think the ",
        incumbent_party,
        " government in your municipality handles crime?"
      ),
      choices = c(
        "Very poorly" = "very_poorly",
        "Poorly" = "poorly",
        "Neither well nor poorly" = "neutral",
        "Well" = "well",
        "Very well" = "very_well"
      ),
      selected = character(0)
    )
  })

  # Dynamic incumbent crime rating question (post-treatment)
  output$incumbent_crime_rating_post_ui <- renderUI({
    home_id <- found_municipality()

    incumbent_party <- if (is.null(home_id)) {
      "the incumbent party"
    } else {
      d_geo %>%
        st_drop_geometry() %>%
        filter(muni_id == home_id) %>%
        pull(governing_party)
    }

    radioButtons(
      "incumbent_crime_rating_post",
      paste0(
        "After seeing this information, how well do you think the ",
        incumbent_party,
        " in general handles crime?"
      ),
      choices = c(
        "Very poorly" = "very_poorly",
        "Poorly" = "poorly",
        "Neither well nor poorly" = "neutral",
        "Well" = "well",
        "Very well" = "very_well"
      ),
      selected = character(0)
    )
  })

  # Dynamic vote likelihood question with incumbent party
  output$vote_likelihood_ui <- renderUI({
    home_id <- found_municipality()

    incumbent_party <- if (is.null(home_id)) {
      "the incumbent party"
    } else {
      d_geo %>%
        st_drop_geometry() %>%
        filter(muni_id == home_id) %>%
        pull(governing_party)
    }

    sliderInput(
      "vote_likelihood",
      paste0(
        "How likely would you be to vote for a ",
        incumbent_party,
        " candidate in the upcoming municipal elections?"
      ),
      min = 0,
      max = 100,
      value = 50
    )
  })

  # Treatment histogram
  output$treatment_histogram <- renderPlot({
    home_id <- found_municipality()
    comp_munis <- comparison_municipalities()
    req(!is.null(home_id), !is.null(comp_munis))

    # Combine home municipality with comparison municipalities
    all_munis <- bind_rows(
      d_geo %>%
        st_drop_geometry() %>%
        filter(muni_id == home_id) %>%
        select(muni_id, NOMGEO),
      comp_munis
    )

    # Look up z-score change from data
    muni_info <- all_munis %>%
      mutate(Cve..Municipio = as.numeric(muni_id)) %>%
      left_join(robo_data, by = "Cve..Municipio") %>%
      mutate(
        z_score = ifelse(is.na(z_change), 0, z_change),
        is_home = ifelse(muni_id == home_id, "home", "comparison")
      )

    # Sort by z-score
    muni_info <- muni_info %>%
      arrange(z_score)

    plot_df <- data.frame(
      municipality = factor(muni_info$NOMGEO, levels = muni_info$NOMGEO),
      z_score = muni_info$z_score,
      is_home = muni_info$is_home,
      z_sign = ifelse(muni_info$z_score < 0, "negative", "positive")
    )

    # Colorblind-friendly palette:
    # Fill: green (#009E73) for negative z-score (crime decreased), vermillion (#D55E00) for positive (crime increased)
    # Outline: blue (#0072B2) for home, orange (#E69F00) for comparison
    ggplot(plot_df, aes(x = municipality, y = z_score, fill = z_sign, color = is_home)) +
      geom_col(linewidth = 1.5) +
      geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
      scale_fill_manual(
        values = c("negative" = "#009E73", "positive" = "#D55E00"),
        labels = c("negative" = "Decreased", "positive" = "Increased"),
        name = "Crime Change"
      ) +
      scale_color_manual(
        values = c("home" = "#0072B2", "comparison" = "#E69F00"),
        labels = c("home" = "Your Municipality", "comparison" = "Comparison"),
        name = "Municipality"
      ) +
      labs(x = NULL, y = "Cambio en Robos (z-score)") +
      theme_minimal() +
      theme(
        axis.text.x = element_text(angle = 45, hjust = 1, size = 11),
        axis.title.y = element_text(size = 12),
        panel.grid.major.x = element_blank(),
        legend.position = "bottom",
        legend.box = "horizontal"
      ) +
      guides(
        fill = guide_legend(order = 1),
        color = guide_legend(order = 2, override.aes = list(fill = NA, linewidth = 2))
      )
  })

  outputOptions(output, "treatment_histogram", suspendWhenHidden = FALSE)

  # Display zoom to home button
  output$home_muni_info <- renderUI({
    if (!is.null(found_municipality())) {
      div(
        style = "margin-bottom: 10px;",
        actionButton(
          "zoom_home",
          "Zoom to Home",
          icon = icon("crosshairs"),
          class = "btn-primary btn-sm"
        )
      )
    }
  })

  # Reference instructions text with dynamic municipality name
  output$reference_instructions_text <- renderUI({
    req(!is.null(found_municipality()))
    muni_data <- d_geo %>%
      st_drop_geometry() %>%
      filter(muni_id == found_municipality())
    muni_name <- paste0(muni_data$NOMGEO, ", ", muni_data$NOM_ENT)

    p(strong(
      paste0(
        "We are interested in which municipalities you think are the most useful comparisons for your municipality. Click on the map or use the search bar below to select these reference municipalities for your home municipality of ",
        muni_name,
        ":"
      )
    ))
  })

  # Zoom to home municipality button
  observeEvent(input$zoom_home, {
    req(!is.null(found_municipality()))

    muni_data <- d_geo %>% filter(muni_id == found_municipality())
    centroid <- st_centroid(st_geometry(muni_data))
    coords <- st_coordinates(centroid)

    leafletProxy("map_page1") %>%
      setView(lng = coords[1], lat = coords[2], zoom = 8)
  })

  # Page 1 Map - Interactive map for both geocoding and selection
  output$map_page1 <- renderLeaflet({
    leaflet(d_geo) %>%
      addTiles() %>%
      addPolygons(
        layerId = ~muni_id,
        fillColor = "#9ecae1",
        fillOpacity = 0.35,
        color = "#3182bd",
        weight = 0.4,
        label = ~ paste0(NOMGEO, ", ", NOM_ENT),
        highlightOptions = highlightOptions(
          weight = 2,
          color = "#08519c",
          fillOpacity = 0.55,
          bringToFront = TRUE
        )
      ) %>%
      fitBounds(lng1 = -115.0, lat1 = 16.0, lng2 = -88.0, lat2 = 30.5)
  })

  # Page 7 Map - Interactive map for reference selection
  output$map_page7 <- renderLeaflet({
    home_id <- found_municipality()

    map <- leaflet(d_geo) %>%
      addTiles() %>%
      addPolygons(
        layerId = ~muni_id,
        fillColor = "#9ecae1",
        fillOpacity = 0.35,
        color = "#3182bd",
        weight = 0.4,
        label = ~ paste0(NOMGEO, ", ", NOM_ENT),
        highlightOptions = highlightOptions(
          weight = 2,
          color = "#08519c",
          fillOpacity = 0.55,
          bringToFront = TRUE
        )
      ) %>%
      fitBounds(lng1 = -115.0, lat1 = 16.0, lng2 = -88.0, lat2 = 30.5)

    # Add home municipality highlight if found
    if (!is.null(home_id)) {
      home_data <- d_geo[d_geo$muni_id == home_id, ]
      home_coords <- found_address_coords()

      map <- map %>%
        addPolygons(
          data = home_data,
          group = "found_muni",
          fillColor = "#0072B2",
          fillOpacity = 0.6,
          color = "#005080",
          weight = 2,
          label = ~ paste0(NOMGEO, ", ", NOM_ENT, " (Your municipality)")
        )

      # Add marker if we have coordinates
      if (!is.null(home_coords)) {
        map <- map %>%
          addMarkers(
            lng = home_coords$lon,
            lat = home_coords$lat,
            popup = "Your address",
            group = "address_marker"
          )
      }

      # Zoom to home municipality
      centroid <- st_centroid(st_geometry(home_data))
      coords <- st_coordinates(centroid)
      map <- map %>%
        setView(lng = coords[1], lat = coords[2], zoom = 8)
    }

    map
  })

  # Enable/disable Next button on page 7 based on reference selections
  observe({
    has_reference <- length(selected_map_munis()) > 0

    if (has_reference) {
      enable("goto_page8_from_7")
    } else {
      disable("goto_page8_from_7")
    }
  })

  # Municipality search - auto-select municipality on page 7
  observeEvent(input$muni_search, {
    req(input$muni_search != "")

    muni_id <- input$muni_search
    muni_data <- d_geo %>% filter(muni_id == !!muni_id)

    if (nrow(muni_data) > 0) {
      # Auto-select the municipality if it's not the home municipality
      if (!is.null(found_municipality()) && muni_id != found_municipality()) {
        current <- selected_map_munis()
        if (!(muni_id %in% current)) {
          selected_map_munis(c(current, muni_id))

          # Update map highlighting
          leafletProxy("map_page7") %>%
            clearGroup("selected") %>%
            clearGroup("found_muni")

          # Add selected municipalities
          leafletProxy("map_page7") %>%
            addPolygons(
              data = d_geo[d_geo$muni_id %in% selected_map_munis(), ],
              group = "selected",
              layerId = ~muni_id,
              fillColor = "#E69F00",
              fillOpacity = 0.6,
              color = "#CC8800",
              weight = 1.2,
              label = ~ paste0(NOMGEO, ", ", NOM_ENT),
              highlightOptions = highlightOptions(
                weight = 2,
                color = "#08519c",
                fillOpacity = 0.55,
                bringToFront = TRUE
              )
            )

          # Re-add found municipality on top
          leafletProxy("map_page7") %>%
            addPolygons(
              data = d_geo[d_geo$muni_id == found_municipality(), ],
              group = "found_muni",
              fillColor = "#0072B2",
              fillOpacity = 0.6,
              color = "#005080",
              weight = 2,
              label = ~ paste0(NOMGEO, ", ", NOM_ENT, " (Your municipality)")
            )

          # Re-add address marker
          if (!is.null(found_address_coords())) {
            address_coords <- found_address_coords()
            leafletProxy("map_page7") %>%
              addMarkers(
                lng = address_coords$lon,
                lat = address_coords$lat,
                popup = "Your address",
                group = "address_marker"
              )
          }
        }
      }
    }
  })

  # Clear search button (page 7)
  observeEvent(input$clear_search, {
    updateSelectizeInput(session, "muni_search", selected = character(0))

    leafletProxy("map_page7") %>%
      fitBounds(lng1 = -115.0, lat1 = 16.0, lng2 = -88.0, lat2 = 30.5)
  })

  # Zoom to searched municipality button (page 7)
  observeEvent(input$zoom_search, {
    req(input$muni_search != "")

    muni_id <- input$muni_search
    muni_data <- d_geo %>% filter(muni_id == !!muni_id)

    if (nrow(muni_data) > 0) {
      centroid <- st_centroid(st_geometry(muni_data))
      coords <- st_coordinates(centroid)

      leafletProxy("map_page7") %>%
        setView(lng = coords[1], lat = coords[2], zoom = 10)
    }
  })

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

            # Highlight the found municipality on page 1 map
            leafletProxy("map_page1") %>%
              clearGroup("found_muni") %>%
              clearGroup("address_marker") %>%
              addPolygons(
                data = d_geo[d_geo$muni_id == muni_id, ],
                group = "found_muni",
                fillColor = "#0072B2",
                fillOpacity = 0.6,
                color = "#005080",
                weight = 2,
                label = ~ paste0(NOMGEO, ", ", NOM_ENT)
              ) %>%
              addMarkers(
                lng = lon,
                lat = lat,
                popup = paste0(
                  "<b>Your Address</b><br>",
                  muni_name,
                  ", ",
                  muni_state
                ),
                group = "address_marker"
              )

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
                style = "color: #0072B2; margin-top: 10px; padding: 10px; background-color: #d4e9f7; border-radius: 4px;",
                icon("check-circle"),
                tags$strong(paste(" Found: ", muni_name, ", ", muni_state)),
                tags$br()
              )
            })
          } else {
            leafletProxy("map_page1") %>%
              clearGroup("address_marker") %>%
              setView(lng = lon, lat = lat, zoom = 8) %>%
              addMarkers(
                lng = lon,
                lat = lat,
                popup = "Your searched address",
                group = "address_marker"
              )

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
                style = "color: #856404; margin-top: 10px; padding: 10px; background-color: #fff3cd; border-radius: 4px;",
                icon("exclamation-triangle"),
                " Address found on map, but not within our municipality database."
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

  # Click-to-toggle municipality selection on map (page 7)
  observeEvent(input$map_page7_shape_click, {
    id <- input$map_page7_shape_click$id
    req(id)

    # Only allow selection after home municipality is found
    req(!is.null(found_municipality()))

    # If clicking the found municipality, zoom to it and show marker
    if (id == found_municipality()) {
      muni_data <- d_geo %>% filter(muni_id == found_municipality())
      centroid <- st_centroid(st_geometry(muni_data))
      coords <- st_coordinates(centroid)

      leafletProxy("map_page7") %>%
        setView(lng = coords[1], lat = coords[2], zoom = 10)

      # Ensure blue highlight and address marker are visible
      leafletProxy("map_page7") %>%
        clearGroup("found_muni") %>%
        addPolygons(
          data = muni_data,
          group = "found_muni",
          fillColor = "#0072B2",
          fillOpacity = 0.6,
          color = "#005080",
          weight = 2,
          label = ~ paste0(NOMGEO, ", ", NOM_ENT, " (Your municipality)")
        )

      if (!is.null(found_address_coords())) {
        address_coords <- found_address_coords()
        leafletProxy("map_page7") %>%
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
    } else {
      selected_map_munis(c(current, id))
    }

    # Update map based on whether selecting or deselecting
    if (is_deselecting) {
      # Re-add the deselected municipality as a clickable base polygon
      leafletProxy("map_page7") %>%
        addPolygons(
          data = d_geo[d_geo$muni_id == id, ],
          layerId = id,
          fillColor = "#9ecae1",
          fillOpacity = 0.35,
          color = "#3182bd",
          weight = 0.4,
          label = ~ paste0(NOMGEO, ", ", NOM_ENT),
          highlightOptions = highlightOptions(
            weight = 2,
            color = "#08519c",
            fillOpacity = 0.55,
            bringToFront = TRUE
          )
        )
    } else {
      # Add the newly selected municipality with orange highlight
      leafletProxy("map_page7") %>%
        addPolygons(
          data = d_geo[d_geo$muni_id == id, ],
          group = "selected",
          layerId = id,
          fillColor = "#E69F00",
          fillOpacity = 0.6,
          color = "#CC8800",
          weight = 1.2,
          label = ~ paste0(NOMGEO, ", ", NOM_ENT),
          highlightOptions = highlightOptions(
            weight = 2,
            color = "#08519c",
            fillOpacity = 0.55,
            bringToFront = TRUE
          )
        )
    }

    # Re-add found municipality on top to ensure it stays visible
    if (!is.null(found_municipality())) {
      leafletProxy("map_page7") %>%
        clearGroup("found_muni") %>%
        addPolygons(
          data = d_geo[d_geo$muni_id == found_municipality(), ],
          group = "found_muni",
          fillColor = "#0072B2",
          fillOpacity = 0.6,
          color = "#005080",
          weight = 2,
          label = ~ paste0(NOMGEO, ", ", NOM_ENT, " (Your municipality)")
        )
    }

    # Re-add address marker
    if (!is.null(found_address_coords())) {
      address_coords <- found_address_coords()
      leafletProxy("map_page7") %>%
        addMarkers(
          lng = address_coords$lon,
          lat = address_coords$lat,
          popup = "Your address from Step 1",
          group = "address_marker"
        )
    }
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
      Additional_Comments = ifelse(
        is.null(input$additional_comments) || input$additional_comments == "",
        NA_character_,
        input$additional_comments
      ),
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
      Party_Strength = ifelse(
        is.null(input$party_strength) || length(input$party_strength) == 0,
        NA_character_,
        input$party_strength
      ),
      Last_Election_Vote = ifelse(
        is.null(input$last_election_vote) || input$last_election_vote == "",
        NA_character_,
        input$last_election_vote
      ),
      Last_Election_Vote_Other = ifelse(
        is.null(input$last_election_vote_other) ||
          input$last_election_vote_other == "",
        NA_character_,
        input$last_election_vote_other
      ),
      # Issue importance rankings (from drag-and-drop rank_list)
      # Convert ordered list to numeric ranks (1 = most important)
      Importance_Crime = {
        ranking <- input$issue_importance_ranking
        match("Delincuencia", ranking, nomatch = NA_integer_)
      },
      Importance_Economy = {
        ranking <- input$issue_importance_ranking
        match("Economia / Inflacion", ranking, nomatch = NA_integer_)
      },
      Importance_Corruption = {
        ranking <- input$issue_importance_ranking
        match("Corrupción", ranking, nomatch = NA_integer_)
      },
      Importance_Employment = {
        ranking <- input$issue_importance_ranking
        match("Desempleo / Bajos salarios", ranking, nomatch = NA_integer_)
      },
      Importance_Education_Health = {
        ranking <- input$issue_importance_ranking
        match("Educacion / Servicios de salud", ranking, nomatch = NA_integer_)
      },
      # Priors - municipality crime ranking (1 = worst, 5 = best)
      # Home municipality rank
      Crime_Rank_Home = {
        ranking <- input$municipality_crime_ranking
        home_info <- d_geo %>%
          st_drop_geometry() %>%
          filter(muni_id == found_municipality())
        if (is.null(ranking) || length(ranking) == 0 || nrow(home_info) == 0) {
          NA_integer_
        } else {
          home_label <- paste0(home_info$NOMGEO, ", ", home_info$NOM_ENT)
          match(home_label, ranking, nomatch = NA_integer_)
        }
      },
      # Comparison municipality names (for reference)
      Comparison_Muni_1 = {
        comp <- comparison_municipalities()
        if (is.null(comp) || nrow(comp) < 1) {
          NA_character_
        } else {
          paste0(comp$NOMGEO[1], ", ", comp$NOM_ENT[1])
        }
      },
      Comparison_Muni_2 = {
        comp <- comparison_municipalities()
        if (is.null(comp) || nrow(comp) < 2) {
          NA_character_
        } else {
          paste0(comp$NOMGEO[2], ", ", comp$NOM_ENT[2])
        }
      },
      Comparison_Muni_3 = {
        comp <- comparison_municipalities()
        if (is.null(comp) || nrow(comp) < 3) {
          NA_character_
        } else {
          paste0(comp$NOMGEO[3], ", ", comp$NOM_ENT[3])
        }
      },
      Comparison_Muni_4 = {
        comp <- comparison_municipalities()
        if (is.null(comp) || nrow(comp) < 4) {
          NA_character_
        } else {
          paste0(comp$NOMGEO[4], ", ", comp$NOM_ENT[4])
        }
      },
      # Comparison municipality ranks
      Crime_Rank_Comp_1 = {
        ranking <- input$municipality_crime_ranking
        comp <- comparison_municipalities()
        if (
          is.null(ranking) ||
            length(ranking) == 0 ||
            is.null(comp) ||
            nrow(comp) < 1
        ) {
          NA_integer_
        } else {
          comp_label <- paste0(comp$NOMGEO[1], ", ", comp$NOM_ENT[1])
          match(comp_label, ranking, nomatch = NA_integer_)
        }
      },
      Crime_Rank_Comp_2 = {
        ranking <- input$municipality_crime_ranking
        comp <- comparison_municipalities()
        if (
          is.null(ranking) ||
            length(ranking) == 0 ||
            is.null(comp) ||
            nrow(comp) < 2
        ) {
          NA_integer_
        } else {
          comp_label <- paste0(comp$NOMGEO[2], ", ", comp$NOM_ENT[2])
          match(comp_label, ranking, nomatch = NA_integer_)
        }
      },
      Crime_Rank_Comp_3 = {
        ranking <- input$municipality_crime_ranking
        comp <- comparison_municipalities()
        if (
          is.null(ranking) ||
            length(ranking) == 0 ||
            is.null(comp) ||
            nrow(comp) < 3
        ) {
          NA_integer_
        } else {
          comp_label <- paste0(comp$NOMGEO[3], ", ", comp$NOM_ENT[3])
          match(comp_label, ranking, nomatch = NA_integer_)
        }
      },
      Crime_Rank_Comp_4 = {
        ranking <- input$municipality_crime_ranking
        comp <- comparison_municipalities()
        if (
          is.null(ranking) ||
            length(ranking) == 0 ||
            is.null(comp) ||
            nrow(comp) < 4
        ) {
          NA_integer_
        } else {
          comp_label <- paste0(comp$NOMGEO[4], ", ", comp$NOM_ENT[4])
          match(comp_label, ranking, nomatch = NA_integer_)
        }
      },
      Incumbent_Crime_Rating = ifelse(
        is.null(input$incumbent_crime_rating) ||
          length(input$incumbent_crime_rating) == 0,
        NA_character_,
        input$incumbent_crime_rating
      ),
      Turnout_Likelihood_Pre = input$turnout_likelihood_pre,
      # Treatment outcomes
      Vote_Likelihood = input$vote_likelihood,
      Turnout_Likelihood = input$turnout_likelihood,
      Incumbent_Crime_Rating_Post = ifelse(
        is.null(input$incumbent_crime_rating_post) ||
          length(input$incumbent_crime_rating_post) == 0,
        NA_character_,
        input$incumbent_crime_rating_post
      ),
      # Post-treatment municipality crime ranking
      Crime_Rank_Home_Post = {
        ranking <- input$municipality_crime_ranking_post
        home_info <- d_geo %>%
          st_drop_geometry() %>%
          filter(muni_id == found_municipality())
        if (is.null(ranking) || length(ranking) == 0 || nrow(home_info) == 0) {
          NA_integer_
        } else {
          home_label <- paste0(home_info$NOMGEO, ", ", home_info$NOM_ENT)
          match(home_label, ranking, nomatch = NA_integer_)
        }
      },
      Crime_Rank_Comp_1_Post = {
        ranking <- input$municipality_crime_ranking_post
        comp <- comparison_municipalities()
        if (
          is.null(ranking) ||
            length(ranking) == 0 ||
            is.null(comp) ||
            nrow(comp) < 1
        ) {
          NA_integer_
        } else {
          comp_label <- paste0(comp$NOMGEO[1], ", ", comp$NOM_ENT[1])
          match(comp_label, ranking, nomatch = NA_integer_)
        }
      },
      Crime_Rank_Comp_2_Post = {
        ranking <- input$municipality_crime_ranking_post
        comp <- comparison_municipalities()
        if (
          is.null(ranking) ||
            length(ranking) == 0 ||
            is.null(comp) ||
            nrow(comp) < 2
        ) {
          NA_integer_
        } else {
          comp_label <- paste0(comp$NOMGEO[2], ", ", comp$NOM_ENT[2])
          match(comp_label, ranking, nomatch = NA_integer_)
        }
      },
      Crime_Rank_Comp_3_Post = {
        ranking <- input$municipality_crime_ranking_post
        comp <- comparison_municipalities()
        if (
          is.null(ranking) ||
            length(ranking) == 0 ||
            is.null(comp) ||
            nrow(comp) < 3
        ) {
          NA_integer_
        } else {
          comp_label <- paste0(comp$NOMGEO[3], ", ", comp$NOM_ENT[3])
          match(comp_label, ranking, nomatch = NA_integer_)
        }
      },
      Crime_Rank_Comp_4_Post = {
        ranking <- input$municipality_crime_ranking_post
        comp <- comparison_municipalities()
        if (
          is.null(ranking) ||
            length(ranking) == 0 ||
            is.null(comp) ||
            nrow(comp) < 4
        ) {
          NA_integer_
        } else {
          comp_label <- paste0(comp$NOMGEO[4], ", ", comp$NOM_ENT[4])
          match(comp_label, ranking, nomatch = NA_integer_)
        }
      },
      Party_Allegiance_Update = ifelse(
        is.null(input$party_allegiance_update) ||
          length(input$party_allegiance_update) == 0,
        NA_character_,
        input$party_allegiance_update
      ),
      Vote_Intention_2027 = ifelse(
        is.null(input$vote_intention_2027) || input$vote_intention_2027 == "",
        NA_character_,
        input$vote_intention_2027
      ),
      Vote_Intention_2027_Other = ifelse(
        is.null(input$vote_intention_2027_other) ||
          input$vote_intention_2027_other == "",
        NA_character_,
        input$vote_intention_2027_other
      ),
      Timestamp = as.character(Sys.time()),
      stringsAsFactors = FALSE
    )

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

    # Go to thank you page instead of resetting
    current_page(9)
  })
}

shinyApp(ui = ui, server = server)
