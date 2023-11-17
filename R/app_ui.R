extrap_laws <- c(
  "Kannisto",
  "Kannisto_Makeham",
  "Makeham",
  "Gompertz",
  "GGompertz",
  "Beard",
  "Beard_Makeham",
  "Quadratic"
)

#' The application User-Interface for input page
#'
#' @return A div containing the input page UI elements.
#' @importFrom untheme create_field_set
#' @importFrom shiny downloadButton
#' @noRd
input_page <- function() {
  div(
    class = "ui form",
    # Basic Inputs as in initial setup
    create_field_set("hashtag", "Open Age Group", "input_oanew", seq(60, 100, by = 5), 100),
    create_field_set("hashtag", "Single / Abridged Ages", "input_age_out", c("single", "abridged"), "single"),
    create_field_set("hashtag", "Sex", "input_sex", c("m", "f"), "m"),
    uiOutput("ages_to_use"),
    br(),
    # Advanced Inputs - Initially Hidden
    div(
      id = "advanced_inputs",
      style = "display: none;",
      div(
        class = "ui two column grid",
        div(
          class = "column",
          create_field_set("hashtag", "Age Extrapolate Mortality", "input_extrapFrom", input_selected = 80, numeric_input = TRUE),
          create_field_set("hashtag", "Extrapolation Law", "input_extrapLaw", extrap_laws, extrap_laws[1]),
          create_field_set("hashtag", "Lifetable Radix", "input_radix", input_selected = 100000, numeric_input = TRUE)
        ),
        div(
          class = "column",
          create_field_set("hashtag", "Sex Ratio at Birth", "input_srb", input_selected = 1.05, numeric_input = TRUE),
          create_field_set("hashtag", "a0 Rule", "input_a0rule", c("ak", "cd"), "ak"),
          create_field_set("hashtag", "Ax Method", "input_axmethod", c("un", "pas"), "un")
        )
      )
    ),
    # Dropdown to toggle Advanced Inputs
    action_button("toggle_advanced", "Show Advanced Options", class = "ui button"),
    br(),
    br(),
    uiOutput("download_buttons")
  )
}


#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#' @return A shiny semantic UI for the application.
#' @importFrom shiny div HTML h1 p uiOutput br plotOutput strong h3
#' @importFrom shinyjs useShinyjs hidden
#' @importFrom plotly plotlyOutput
#' @importFrom shiny.semantic main_panel action_button selectInput file_input sidebar_layout sidebar_panel tabset
#' @importFrom untheme fluidUnTheme
#' @importFrom shinycssloaders withSpinner
#' @importFrom rhandsontable rHandsontableOutput
#' @noRd
app_ui <- function(request) {
  fluidUnTheme(
    useShinyjs(),
    main_panel(
      tags$head(
        tags$style(HTML("
            .centered {
                position: absolute;
                top: 35%;
                left: 50%;
                transform: translate(-50%, -50%);
            }
            .semi-centered {
                position: absolute;
                left: 50%;
                transform: translateX(-50%);
            }
            .semi-centered-two {
                position: absolute;
                left: 15%;
            }
            .info-box {
                border: 1px solid #ddd;
                border-radius: 5px;
                padding: 20px;
                margin-bottom: 20px;
                text-align: left;
                color: #555;
            }
            .info-box h1 {
                color: #337ab7;
            }
            .info-box p {
                font-size: 16px;
            }
            .validation-results {
                text-align: left;
                font-size: 15px; /* Adjusted font size */
                color: #555;
                font-family: 'Arial', sans-serif; /* Changed font family */
                font-weight: bold; /* Making text bold */
            }
            .well {
                background-color: #f8d7da; /* Light red for error */
                border-color: #f5c6cb;
                border-radius: 5px;
                padding: 15px;
                box-shadow: 0 2px 4px rgba(0, 0, 0, 0.1); /* Added shadow */
            }
            .well.success {
                background-color: #d4edda; /* Light green for success */
                border-color: #c3e6cb;
            }
        "))
      ),
      div(
        id = "landing_page",
        class = "centered",
        tags$div(
          class = "info-box",
          h1("\xF0\x9F\x9A\x80 Welcome to the Online Demographic Analysis Platform! \xF0\x9F\x8E\xAF"),
          p("\xF0\x9F\x93\x88 Transform your data into insightful forecasts. Begin by uploading your CSV file."),
          p("\xF0\x9F\xA7\x90 Not sure about your file? Here's what we're looking for:"),
          br(),
          rHandsontableOutput("data_table"),
          br(),
          strong(h3("\xF0\x9F\x93\xA4 Ready to get started? Click 'Browse...' to select your file"))
        ),
        div(
          class = "semi-centered",
          br(),
          file_input("file1", ""),
          br(),
          div(
            class = "validation-results",
            uiOutput("validation_results")
          ),
          br(),
          div(class = "semi-centered-two", uiOutput("forward_step2"))
        )
      ),
      tags$head(
        tags$script(HTML("
          $(document).on('click', '#toggle_advanced', function() {
            $('#advanced_inputs').slideToggle();
          });
        "))
      ),
      hidden(
        div(
          id = "step_input",
          div(
            style = "display: flex; gap: 10px;",
            action_button("back_to_landing", "Back", class = "ui grey button"),
            action_button("calculate_lt", "Calculate", class = "ui blue button")
          ),
          br(),
          sidebar_layout(
            list(children = div(input_page()), width = 1.3),
            main_panel(
              uiOutput("tabs")
            )
          )
        )
      ),
      width = NULL
    )
  )
}


#' Add external Resources to the Application
#'
#' This function is internally used to add external
#' resources inside the Shiny application.
#'
#' @importFrom shiny tags
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function() {
  add_resource_path(
    "www",
    app_sys("app/www")
  )

  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys("app/www"),
      app_title = "lifetableprojection"
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
  )
}
