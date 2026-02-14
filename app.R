library(shiny)
library(shinyjs)
library(sortable)
library(sf)
library(dplyr)
library(ggplot2)

# Read municipalities from GeoJSON file
if (!file.exists("data/00mun_simplified.geojson")) {
  stop("Missing data/00mun_simplified.geojson. Run: source('scripts/make_geojson_file.R')")
}
d_geo <- st_read("data/00mun_simplified.geojson", quiet = TRUE)
municipality_list <- sort(unique(d_geo$mun_state))
d_geo$muni_id <- d_geo$CVEGEO

# Load robbery data for treatment graph
if (!file.exists("data/robo_2025.rds")) {
  stop("Missing data/robo_2025.rds. Run: source('scripts/crime_data.R')")
}
robo_data <- readRDS("data/robo_2025.rds")

# WARNING: Governing parties are randomly assigned — replace with actual data before deployment
warning("Party assignments are FAKE (randomized). Replace with real data before collecting responses.")
set.seed(42)
parties <- c("MORENA", "PAN", "PRI", "PRD", "PVEM", "PT", "MC")
d_geo$governing_party <- sample(parties, nrow(d_geo), replace = TRUE)

# Coalition definitions for treatment groups
coalition_a <- c("MORENA", "PT", "PVEM")
coalition_b <- c("PAN", "PRI", "PRD", "MC")

get_opposite_parties <- function(party) {
  if (party %in% coalition_a) coalition_b else coalition_a
}

get_same_coalition_parties <- function(party) {
  if (party %in% coalition_a) coalition_a else coalition_b
}

get_coalition_label <- function(parties) {
  paste(parties, collapse = "/")
}

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
      /* Remove numbering for municipality bucket lists */
      .bucket-list .rank-list-container .rank-list-item::before {
        content: none;
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
      $(document).on('change', 'input[name=\"vote_intention_pre\"]', function() {
        Shiny.setInputValue('vote_intention_pre', $(this).val());
      });
      $(document).on('change', '#governance_grid input[type=\"radio\"]', function() {
        Shiny.setInputValue($(this).attr('name'), $(this).val());
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
        # Page 0: Information Sheet
        hidden(
          div(
            id = "page0",
            h4("Information Sheet"),
            h5(strong(
              "Evaluating the Efficacy of Sub-National Informational Comparisons"
            )),
            p(em("Principal Investigator: Adam Roberts")),
            p(
              "This form describes a research study that is being conducted by Adam Roberts from the ",
              "University of Rochester's Department of Political Science. The purpose of this study is to ",
              "understand how voters use information when deciding which candidate or political party to vote for."
            ),
            p(
              "If you decide to take part in this study, you will be asked to complete a survey that will take ",
              "about 15\u201320 minutes to complete. The surveys will ask questions ",
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
                  "I have read the information sheet. Continue \u2192",
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

            p(strong("Select your home municipality from the list below:")),
            selectizeInput(
              "home_muni_dropdown",
              NULL,
              choices = NULL,
              options = list(
                placeholder = "Type to search municipalities..."
              )
            ),

            # Confirmation section (shown after municipality is selected)
            hidden(
              div(
                id = "home_confirmation_section",
                hr(),
                uiOutput("home_municipality_summary"),
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
                    "goto_page2",
                    "Next →",
                    class = "btn-primary btn-lg"
                  )
                )
              )
            )
          )
        ),

        # Page 2: Verify Municipality
        hidden(
          div(
            id = "page2",
            h4("Confirm Your Municipality"),

            p(
              "Please confirm that the following information is correct before continuing:"
            ),

            uiOutput("verify_municipality_info"),

            p("If this is not correct, click Back to search again."),

            hr(),
            fluidRow(
              column(
                6,
                actionButton(
                  "goto_page1_from_2",
                  "\u2190 Back",
                  class = "btn-secondary btn-lg"
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

            sliderInput(
              "party_strength",
              "How strongly do you identify with this party?",
              min = 0,
              max = 100,
              value = 50
            ),
            fluidRow(
              column(
                6,
                p(
                  "Not at all",
                  style = "color: #6c757d; font-size: 0.85em; margin-top: -15px;"
                )
              ),
              column(
                6,
                p(
                  "Very strongly",
                  style = "color: #6c757d; font-size: 0.85em; margin-top: -15px; text-align: right;"
                )
              )
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

            h5(strong("Municipal Governance")),

            uiOutput("governance_grid_ui"),

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

        # Page 4: Your Municipality's Performance
        hidden(
          div(
            id = "page4",
            h4("Your Municipality's Performance"),

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

            br(),

            tags$div(
              class = "form-group",
              tags$label(
                "Which party do you intend to vote for in the next municipal election?"
              ),
              tags$div(
                id = "vote_intention_pre_group",
                class = "party-radio-group",
                party_radio_choice(
                  "pan",
                  "PAN (Partido Acción Nacional)",
                  "PAN_logo.png",
                  input_name = "vote_intention_pre"
                ),
                party_radio_choice(
                  "pri",
                  "PRI (Partido Revolucionario Institucional)",
                  "PRI_logo.png",
                  input_name = "vote_intention_pre"
                ),
                party_radio_choice(
                  "prd",
                  "PRD (Partido de la Revolución Democrática)",
                  "PRD_logo.png",
                  input_name = "vote_intention_pre"
                ),
                party_radio_choice(
                  "pvem",
                  "PVEM (Partido Verde Ecologista de México)",
                  "PVEM_logo.png",
                  input_name = "vote_intention_pre"
                ),
                party_radio_choice(
                  "pt",
                  "PT (Partido del Trabajo)",
                  "PT_logo.png",
                  input_name = "vote_intention_pre"
                ),
                party_radio_choice(
                  "mc",
                  "MC (Movimiento Ciudadano)",
                  "Movimiento_Ciudadano_logo.png",
                  input_name = "vote_intention_pre"
                ),
                party_radio_choice(
                  "morena",
                  "MORENA",
                  "Morena_logo.png",
                  input_name = "vote_intention_pre"
                ),
                party_radio_choice(
                  "undecided",
                  "Undecided",
                  input_name = "vote_intention_pre"
                ),
                party_radio_choice(
                  "will_not_vote",
                  "Will not vote",
                  input_name = "vote_intention_pre"
                ),
                party_radio_choice(
                  "other",
                  "Other",
                  input_name = "vote_intention_pre"
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
            h4("Treatment Groups (Preview)"),
            p(em(
              "All four treatment conditions are shown below for preview purposes. ",
              "In the final survey, respondents will be randomly assigned to one group."
            )),

            # Group 1: Control (Agave)
            wellPanel(
              h5(strong("Group 1: Control")),
              uiOutput("treatment_control_ui")
            ),

            # Group 2: Plain Information
            wellPanel(
              h5(strong("Group 2: Plain Information")),
              uiOutput("treatment_plain_info_ui")
            ),

            # Group 3: Same-Party Comparison
            wellPanel(
              h5(strong("Group 3: Same-Party Comparison")),
              uiOutput("treatment_same_party_ui"),
              plotOutput("treatment_histogram", height = "350px")
            ),

            # Group 4: Opposite-Party Comparison
            wellPanel(
              h5(strong("Group 4: Opposite-Party Comparison")),
              uiOutput("treatment_opposite_party_ui"),
              plotOutput("treatment_histogram_opposite", height = "350px")
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

        # Page 7: Post-Treatment Questions
        hidden(
          div(
            id = "page7",
            h4("Post-Treatment Survey"),

            sliderInput(
              "turnout_likelihood",
              "How likely are you to vote in the 2027 local elections?",
              min = 0,
              max = 100,
              value = 50
            ),

            br(),

            uiOutput("incumbent_crime_rating_post_ui"),

            br(),

            h5(strong("Your Municipality's Performance (Post-Treatment)")),

            p(
              "After seeing the crime data, drag and drop the following municipalities ",
              "to rank them on their handling of crime, from worst (top) to best (bottom):"
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
                  "Next →",
                  class = "btn-primary btn-lg"
                )
              )
            )
          )
        ),

        # Page 8: Select Reference Municipalities
        hidden(
          div(
            id = "page8",
            h4("Select Reference Municipalities"),

            p(
              "Now we would like you to select some reference municipalities for comparison."
            ),
            uiOutput("reference_instructions_text"),

            selectizeInput(
              "reference_munis_dropdown",
              "Select reference municipalities:",
              choices = NULL,
              multiple = TRUE,
              options = list(
                placeholder = "Type to search and select municipalities..."
              )
            ),

            verbatimTextOutput("selected_munis_display"),

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
                disabled(
                  actionButton(
                    "submit",
                    "Submit Survey",
                    class = "btn-success btn-lg",
                    icon = icon("check")
                  )
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
  # Helper function to find municipality's relative crime rating from bucket inputs
  # Returns "worse", "same", "better", or NA if not ranked
  find_bucket_category <- function(muni_label, prefix = "muni_rank_") {
    categories <- c("worse", "same", "better")
    for (cat in categories) {
      bucket_contents <- input[[paste0(prefix, cat)]]
      if (!is.null(bucket_contents) && muni_label %in% bucket_contents) {
        return(cat)
      }
    }
    NA_character_
  }

  # Reactive values
  selected_map_munis <- reactiveVal(character())
  found_municipality <- reactiveVal(NULL)
  current_page <- reactiveVal(0)
  comparison_municipalities <- reactiveVal(NULL) # Stores comparison muni data for ranking & graph
  comp_munis_opp_rv <- reactiveVal(NULL) # Stores opposite-coalition comparison munis

  # Define the file path for saving responses
  responses_file <- "data/survey_responses.csv"

  # Update municipality dropdown choices on server side
  updateSelectizeInput(
    session,
    "home_muni_dropdown",
    choices = c(" " = "", setNames(d_geo$muni_id, d_geo$mun_state)),
    server = TRUE
  )
  updateSelectizeInput(
    session,
    "reference_munis_dropdown",
    choices = c(" " = "", setNames(d_geo$muni_id, d_geo$mun_state)),
    server = TRUE
  )

  # Home municipality dropdown selection → sets found_municipality
  observeEvent(input$home_muni_dropdown, {
    if (!is.null(input$home_muni_dropdown) && input$home_muni_dropdown != "") {
      found_municipality(input$home_muni_dropdown)
    }
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
    same_coalition <- get_same_coalition_parties(home_party)

    # Get comparison municipalities (same state, same coalition, excluding home)
    comp_munis <- d_geo %>%
      st_drop_geometry() %>%
      filter(
        NOM_ENT == home_state,
        governing_party %in% same_coalition,
        muni_id != home_id
      ) %>%
      select(muni_id, NOMGEO, NOM_ENT)

    # Fallback: if no same-coalition municipalities in state, use any from same state
    if (nrow(comp_munis) == 0) {
      comp_munis <- d_geo %>%
        st_drop_geometry() %>%
        filter(
          NOM_ENT == home_state,
          muni_id != home_id
        ) %>%
        select(muni_id, NOMGEO, NOM_ENT)
    }

    # Randomly select up to 4 comparison municipalities
    if (nrow(comp_munis) > 4) {
      comp_munis <- comp_munis %>% slice_sample(n = 4)
    }

    comparison_municipalities(comp_munis)
  })

  # Set opposite-coalition comparison municipalities when home municipality is found
  observeEvent(found_municipality(), {
    home_id <- found_municipality()
    req(!is.null(home_id))

    home_info <- d_geo %>%
      st_drop_geometry() %>%
      filter(muni_id == home_id)

    home_state <- home_info$NOM_ENT[1]
    home_party <- home_info$governing_party[1]
    opposite_parties <- get_opposite_parties(home_party)

    # Get comparison municipalities (same state, opposite coalition, excluding home)
    comp_munis_opp <- d_geo %>%
      st_drop_geometry() %>%
      filter(
        NOM_ENT == home_state,
        governing_party %in% opposite_parties,
        muni_id != home_id
      ) %>%
      select(muni_id, NOMGEO, NOM_ENT)

    # Fallback: if no opposite-coalition municipalities in state, use any from same state
    if (nrow(comp_munis_opp) == 0) {
      comp_munis_opp <- d_geo %>%
        st_drop_geometry() %>%
        filter(
          NOM_ENT == home_state,
          muni_id != home_id
        ) %>%
        select(muni_id, NOMGEO, NOM_ENT)
    }

    # Randomly select up to 4 comparison municipalities
    if (nrow(comp_munis_opp) > 4) {
      comp_munis_opp <- comp_munis_opp %>% slice_sample(n = 4)
    }

    comp_munis_opp_rv(comp_munis_opp)
  })

  # Show appropriate page
  observe({
    pages <- paste0("page", 0:9)
    lapply(pages, hide)
    show(paste0("page", current_page()))
  })

  # Show/hide home confirmation section based on whether municipality is found
  observe({
    if (!is.null(found_municipality())) {
      show("home_confirmation_section")
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

  observeEvent(input$vote_intention_pre, {
    toggleElement(
      "vote_intention_pre_other",
      condition = input$vote_intention_pre == "other"
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
  # Page 0 → Page 1 (Information Sheet → Find Municipality)
  observeEvent(input$goto_page1_from_0, {
    current_page(1)
  })

  # Page 1 → Page 2 (Find Municipality → Verify Municipality)
  observeEvent(input$goto_page2, {
    current_page(2)
  })

  # Page 2 → Page 1
  observeEvent(input$goto_page1_from_2, {
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
    missing <- c()
    if (is.null(input$party_preference)) missing <- c(missing, "party preference")
    if (is.null(input$last_election_vote)) missing <- c(missing, "last election vote")
    if (length(missing) > 0) {
      showNotification(
        paste("Please complete:", paste(missing, collapse = ", ")),
        type = "warning"
      )
      return()
    }
    current_page(4)
  })

  # Page 4 → Page 3
  observeEvent(input$goto_page3_from_4, {
    current_page(3)
  })

  # Page 4 → Page 5
  observeEvent(input$goto_page5_from_4, {
    if (is.null(input$vote_intention_pre)) {
      showNotification(
        "Please select your vote intention before continuing.",
        type = "warning"
      )
      return()
    }
    current_page(5)
  })

  # Page 5 → Page 4
  observeEvent(input$goto_page4_from_5, {
    current_page(4)
  })

  # Page 5 → Page 6
  observeEvent(input$goto_page6_from_5, {
    missing <- c()
    if (is.null(input$age) || is.na(input$age)) missing <- c(missing, "age")
    if (is.null(input$gender) || input$gender == "") missing <- c(missing, "gender")
    if (is.null(input$attention_check)) missing <- c(missing, "attention check")
    if (length(missing) > 0) {
      showNotification(
        paste("Please complete:", paste(missing, collapse = ", ")),
        type = "warning"
      )
      return()
    }
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

  # Page 7 → Page 8 (Reference Municipalities)
  observeEvent(input$goto_page8_from_7, {
    if (is.null(input$vote_intention_2027)) {
      showNotification(
        "Please select your vote intention before continuing.",
        type = "warning"
      )
      return()
    }
    current_page(8)
  })

  # Page 8 → Page 7
  observeEvent(input$goto_page7_from_8, {
    current_page(7)
  })

  # Clear selection button - resets the found municipality
  observeEvent(input$clear_selection_btn, {
    found_municipality(NULL)
    updateSelectizeInput(session, "home_muni_dropdown", selected = character(0))
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
  output$verify_municipality_info <- renderUI({
    home_id <- found_municipality()
    req(!is.null(home_id))

    home_info <- d_geo %>%
      st_drop_geometry() %>%
      filter(muni_id == home_id)

    home_name <- home_info$NOMGEO[1]
    home_state <- home_info$NOM_ENT[1]

    div(
      style = "background-color: #f8f9fa; padding: 20px; border-radius: 4px; margin: 20px 0;",
      h5(icon("map-marker-alt"), " Your Home Municipality:"),
      tags$p(
        tags$span(
          paste0(home_name, ", ", home_state),
          style = "color: #0072B2; font-size: 1.3em; font-weight: bold;"
        )
      )
    )
  })

  # Dynamic bucket list for municipality crime ranking (relative to home)
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
    # Create labels for comparison municipalities only
    comp_labels <- paste0(comp_munis$NOMGEO, ", ", comp_munis$NOM_ENT)

    tagList(
      p(
        "Compared to ",
        strong(home_info$NOMGEO),
        ", how would you rate the local government's handling of crime in these other municipalities?"
      ),
      bucket_list(
        header = NULL,
        group_name = "muni_rank_bucket",
        orientation = "horizontal",
        add_rank_list(
          text = "Worse at Handling Crime",
          input_id = "muni_rank_worse"
        ),
        add_rank_list(
          text = "Same at Handling Crime",
          input_id = "muni_rank_same"
        ),
        add_rank_list(
          text = "Better at Handling Crime",
          input_id = "muni_rank_better"
        ),
        add_rank_list(
          text = "Unranked",
          labels = comp_labels,
          input_id = "muni_rank_unranked"
        )
      )
    )
  })

  # Dynamic bucket list for post-treatment municipality crime ranking (relative to home)
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
    # Create labels for comparison municipalities only
    comp_labels <- paste0(comp_munis$NOMGEO, ", ", comp_munis$NOM_ENT)

    tagList(
      p(
        "Compared to ",
        strong(home_info$NOMGEO),
        ", how would you rate the local government's handling of crime in these other municipalities?"
      ),
      bucket_list(
        header = NULL,
        group_name = "muni_rank_post_bucket",
        orientation = "horizontal",
        add_rank_list(
          text = "Worse at Handling Crime",
          input_id = "muni_rank_post_worse"
        ),
        add_rank_list(
          text = "Same at Handling Crime",
          input_id = "muni_rank_post_same"
        ),
        add_rank_list(
          text = "Better at Handling Crime",
          input_id = "muni_rank_post_better"
        ),
        add_rank_list(
          text = "Unranked",
          labels = comp_labels,
          input_id = "muni_rank_post_unranked"
        )
      )
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

    home_pct_change <- robo_data %>%
      filter(Cve..Municipio == as.numeric(home_id)) %>%
      pull(pct_change)
    home_pct_change <- ifelse(
      length(home_pct_change) == 0 || is.na(home_pct_change),
      0,
      home_pct_change
    )

    if (home_pct_change > 0) {
      paste0(
        "Robberies in ",
        home_name,
        " increased by ",
        round(abs(home_pct_change), 1),
        "% from 2024 to 2025."
      )
    } else if (home_pct_change < 0) {
      paste0(
        "Robberies in ",
        home_name,
        " decreased by ",
        round(abs(home_pct_change), 1),
        "% from 2024 to 2025."
      )
    } else {
      paste0(
        "Robberies in ",
        home_name,
        " remained unchanged from 2024 to 2025."
      )
    }
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

  # Group 1: Control (Agave)
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

  # Group 2: Plain Information
  output$treatment_plain_info_ui <- renderUI({
    change_text <- home_robbery_change_text()
    tagList(
      p(plain_info_text),
      p(strong(change_text))
    )
  })

  # Group 3: Same-Party Comparison
  output$treatment_same_party_ui <- renderUI({
    home_id <- found_municipality()
    req(!is.null(home_id))

    home_info <- d_geo %>%
      st_drop_geometry() %>%
      filter(muni_id == home_id)

    home_name <- home_info$NOMGEO[1]
    home_state <- home_info$NOM_ENT[1]
    home_party <- home_info$governing_party[1]
    same_coalition <- get_same_coalition_parties(home_party)
    same_label <- get_coalition_label(same_coalition)
    change_text <- home_robbery_change_text()

    tagList(
      p(plain_info_text),
      p(strong(change_text)),
      p(
        paste0(
          "The following graph shows the change in robbery rates from 2024 to 2025 for ",
          home_name,
          " and a sample of other municipalities in ",
          home_state,
          " that are governed by ",
          same_label,
          ", as recorded by the Secretariado Ejecutivo del Sistema Nacional de Seguridad P\u00fablica (SESNSP)."
        )
      ),
      p(
        em(
          "The values shown are percent changes. Positive values indicate ",
          "increases in robbery, negative values indicate decreases."
        )
      )
    )
  })

  # Group 4: Opposite-Party Comparison
  output$treatment_opposite_party_ui <- renderUI({
    home_id <- found_municipality()
    req(!is.null(home_id))

    home_info <- d_geo %>%
      st_drop_geometry() %>%
      filter(muni_id == home_id)

    home_name <- home_info$NOMGEO[1]
    home_state <- home_info$NOM_ENT[1]
    home_party <- home_info$governing_party[1]
    opposite_parties <- get_opposite_parties(home_party)
    opposite_label <- get_coalition_label(opposite_parties)
    change_text <- home_robbery_change_text()

    tagList(
      p(plain_info_text),
      p(strong(change_text)),
      p(
        paste0(
          "The following graph shows the change in robbery rates from 2024 to 2025 for ",
          home_name,
          " and a sample of other municipalities in ",
          home_state,
          " that are governed by ",
          opposite_label,
          ", as recorded by the Secretariado Ejecutivo del Sistema Nacional de Seguridad P\u00fablica (SESNSP)."
        )
      ),
      p(
        em(
          "The values shown are percent changes. Positive values indicate ",
          "increases in robbery, negative values indicate decreases."
        )
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

    tagList(
      sliderInput(
        "party_allegiance_update",
        paste0(
          "After seeing this information, how strongly do you identify with ",
          party_display,
          "?"
        ),
        min = 0,
        max = 100,
        value = 50
      ),
      fluidRow(
        column(
          6,
          p(
            "Not at all",
            style = "color: #6c757d; font-size: 0.85em; margin-top: -15px;"
          )
        ),
        column(
          6,
          p(
            "Very strongly",
            style = "color: #6c757d; font-size: 0.85em; margin-top: -15px; text-align: right;"
          )
        )
      )
    )
  })

  # Combined governance grid for home + comparison municipalities
  output$governance_grid_ui <- renderUI({
    home_id <- found_municipality()
    comp_munis <- comparison_municipalities()

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
            type = "radio",
            name = row$input_name,
            value = party_values[j]
          ))
        })
      )
      row_class <- if (row$is_home) "home-row" else NULL
      do.call(tags$tr, c(cells, list(class = row_class)))
    })

    tagList(
      p(
        "Which party do you believe currently governs each of the following municipalities?"
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

    sliderInput(
      "incumbent_crime_rating",
      paste0(
        "How well do you think the ",
        incumbent_party,
        " government in your municipality handles crime?"
      ),
      min = 0,
      max = 100,
      value = 50
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

    sliderInput(
      "incumbent_crime_rating_post",
      paste0(
        "After seeing this information, how well do you think the ",
        incumbent_party,
        " in general handles crime?"
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

    # Look up pct_change from data
    muni_info <- all_munis %>%
      mutate(Cve..Municipio = as.numeric(muni_id)) %>%
      left_join(robo_data, by = "Cve..Municipio") %>%
      mutate(
        pct_change = ifelse(is.na(pct_change), 0, pct_change),
        is_home = ifelse(muni_id == home_id, "home", "comparison")
      )

    # Sort by pct_change
    muni_info <- muni_info %>%
      arrange(pct_change)

    plot_df <- data.frame(
      municipality = factor(muni_info$NOMGEO, levels = muni_info$NOMGEO),
      pct_change = muni_info$pct_change,
      is_home = muni_info$is_home,
      pct_sign = ifelse(muni_info$pct_change < 0, "negative", "positive")
    )

    # Colorblind-friendly palette:
    # Fill: green (#009E73) for negative z-score (crime decreased), vermillion (#D55E00) for positive (crime increased)
    # Outline: blue (#0072B2) for home, orange (#E69F00) for comparison
    ggplot(
      plot_df,
      aes(x = municipality, y = pct_change, fill = pct_sign, color = is_home)
    ) +
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
      labs(x = NULL, y = "Cambio en Robos (%)") +
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
        color = guide_legend(
          order = 2,
          override.aes = list(fill = NA, linewidth = 2)
        )
      )
  })

  outputOptions(output, "treatment_histogram", suspendWhenHidden = FALSE)

  # Treatment histogram for opposite-party comparisons
  output$treatment_histogram_opposite <- renderPlot({
    home_id <- found_municipality()
    comp_munis <- comp_munis_opp_rv()
    req(!is.null(home_id), !is.null(comp_munis))

    # Combine home municipality with opposite-coalition comparison municipalities
    all_munis <- bind_rows(
      d_geo %>%
        st_drop_geometry() %>%
        filter(muni_id == home_id) %>%
        select(muni_id, NOMGEO),
      comp_munis
    )

    # Look up pct_change from data
    muni_info <- all_munis %>%
      mutate(Cve..Municipio = as.numeric(muni_id)) %>%
      left_join(robo_data, by = "Cve..Municipio") %>%
      mutate(
        pct_change = ifelse(is.na(pct_change), 0, pct_change),
        is_home = ifelse(muni_id == home_id, "home", "comparison")
      )

    # Sort by pct_change
    muni_info <- muni_info %>%
      arrange(pct_change)

    plot_df <- data.frame(
      municipality = factor(muni_info$NOMGEO, levels = muni_info$NOMGEO),
      pct_change = muni_info$pct_change,
      is_home = muni_info$is_home,
      pct_sign = ifelse(muni_info$pct_change < 0, "negative", "positive")
    )

    ggplot(
      plot_df,
      aes(x = municipality, y = pct_change, fill = pct_sign, color = is_home)
    ) +
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
      labs(x = NULL, y = "Cambio en Robos (%)") +
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
        color = guide_legend(
          order = 2,
          override.aes = list(fill = NA, linewidth = 2)
        )
      )
  })

  outputOptions(
    output,
    "treatment_histogram_opposite",
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
        "We are interested in which municipalities you think are the most useful ",
        "comparisons for your municipality. Use the search bar ",
        "below to select these reference municipalities for your home municipality of ",
        muni_name,
        ":"
      )
    ))
  })

  # Enable/disable Submit button on page 8 based on reference selections
  observe({
    has_reference <- length(selected_map_munis()) > 0

    if (has_reference) {
      enable("submit")
    } else {
      disable("submit")
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
      Party_Strength = input$party_strength,
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
      Home_Governing_Party_Belief = ifelse(
        is.null(input$home_governing_party_belief) ||
          length(input$home_governing_party_belief) == 0,
        NA_character_,
        input$home_governing_party_belief
      ),
      Comp_Governing_Party_Belief_1 = {
        val <- input$comp_governing_party_1
        ifelse(is.null(val) || length(val) == 0, NA_character_, val)
      },
      Comp_Governing_Party_Belief_2 = {
        val <- input$comp_governing_party_2
        ifelse(is.null(val) || length(val) == 0, NA_character_, val)
      },
      Comp_Governing_Party_Belief_3 = {
        val <- input$comp_governing_party_3
        ifelse(is.null(val) || length(val) == 0, NA_character_, val)
      },
      Comp_Governing_Party_Belief_4 = {
        val <- input$comp_governing_party_4
        ifelse(is.null(val) || length(val) == 0, NA_character_, val)
      },
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
      # Priors - municipality crime ranking relative to home
      # (home is the fixed reference point)
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
      # Comparison municipality crime categories (relative to home: "worse", "same", "better")
      Crime_Rank_Comp_1 = {
        comp <- comparison_municipalities()
        if (is.null(comp) || nrow(comp) < 1) {
          NA_character_
        } else {
          comp_label <- paste0(comp$NOMGEO[1], ", ", comp$NOM_ENT[1])
          find_bucket_category(comp_label, "muni_rank_")
        }
      },
      Crime_Rank_Comp_2 = {
        comp <- comparison_municipalities()
        if (is.null(comp) || nrow(comp) < 2) {
          NA_character_
        } else {
          comp_label <- paste0(comp$NOMGEO[2], ", ", comp$NOM_ENT[2])
          find_bucket_category(comp_label, "muni_rank_")
        }
      },
      Crime_Rank_Comp_3 = {
        comp <- comparison_municipalities()
        if (is.null(comp) || nrow(comp) < 3) {
          NA_character_
        } else {
          comp_label <- paste0(comp$NOMGEO[3], ", ", comp$NOM_ENT[3])
          find_bucket_category(comp_label, "muni_rank_")
        }
      },
      Crime_Rank_Comp_4 = {
        comp <- comparison_municipalities()
        if (is.null(comp) || nrow(comp) < 4) {
          NA_character_
        } else {
          comp_label <- paste0(comp$NOMGEO[4], ", ", comp$NOM_ENT[4])
          find_bucket_category(comp_label, "muni_rank_")
        }
      },
      Incumbent_Crime_Rating = input$incumbent_crime_rating,
      Turnout_Likelihood_Pre = input$turnout_likelihood_pre,
      Vote_Intention_Pre = ifelse(
        is.null(input$vote_intention_pre) || input$vote_intention_pre == "",
        NA_character_,
        input$vote_intention_pre
      ),
      Vote_Intention_Pre_Other = ifelse(
        is.null(input$vote_intention_pre_other) ||
          input$vote_intention_pre_other == "",
        NA_character_,
        input$vote_intention_pre_other
      ),
      # Treatment outcomes
      Turnout_Likelihood = input$turnout_likelihood,
      Incumbent_Crime_Rating_Post = input$incumbent_crime_rating_post,
      # Post-treatment municipality crime categories (relative to home: "worse", "same", "better")
      Crime_Rank_Comp_1_Post = {
        comp <- comparison_municipalities()
        if (is.null(comp) || nrow(comp) < 1) {
          NA_character_
        } else {
          comp_label <- paste0(comp$NOMGEO[1], ", ", comp$NOM_ENT[1])
          find_bucket_category(comp_label, "muni_rank_post_")
        }
      },
      Crime_Rank_Comp_2_Post = {
        comp <- comparison_municipalities()
        if (is.null(comp) || nrow(comp) < 2) {
          NA_character_
        } else {
          comp_label <- paste0(comp$NOMGEO[2], ", ", comp$NOM_ENT[2])
          find_bucket_category(comp_label, "muni_rank_post_")
        }
      },
      Crime_Rank_Comp_3_Post = {
        comp <- comparison_municipalities()
        if (is.null(comp) || nrow(comp) < 3) {
          NA_character_
        } else {
          comp_label <- paste0(comp$NOMGEO[3], ", ", comp$NOM_ENT[3])
          find_bucket_category(comp_label, "muni_rank_post_")
        }
      },
      Crime_Rank_Comp_4_Post = {
        comp <- comparison_municipalities()
        if (is.null(comp) || nrow(comp) < 4) {
          NA_character_
        } else {
          comp_label <- paste0(comp$NOMGEO[4], ", ", comp$NOM_ENT[4])
          find_bucket_category(comp_label, "muni_rank_post_")
        }
      },
      Party_Allegiance_Update = ifelse(
        is.null(input$party_allegiance_update),
        NA_integer_,
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
    current_page(9)
  })
}

shinyApp(ui = ui, server = server)
