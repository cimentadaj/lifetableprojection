#' Create a field set with a given icon, label, and selectInput
#'
#' @param icon_name Name of the icon.
#' @param label_text Text for the label.
#' @param input_id ID for the select input.
#' @param input_choices Choices for the select input.
#' @param input_selected Selected choice for the select input.
#' @importFrom shiny.semantic icon label selectInput
#' @noRd
create_field_set <- function(icon_name, label_text, input_id, input_choices, input_selected) {
  div(
    class = "field",
    icon(icon_name),
    label(
      class = "main label",
      label_text
    ),
    selectInput(
      input_id,
      NULL,
      choices = input_choices,
      selected = input_selected
    )
  )
}

#' The application User-Interface for input page
#'
#' @return A div containing the input page UI elements.
#' @noRd
input_page <- function() {
  div(
    class = "ui form",
    # Basic Inputs
    create_field_set("hashtag", "OAnew", "input_oanew", c(100), 100),
    create_field_set("hashtag", "Age Output", "input_age_out", c("single", "range"), "single"),
    create_field_set("hashtag", "Sex", "input_sex", c("m", "f"), "m"),

    # Advanced Inputs - Initially Hidden
    div(
      id = "advanced_inputs",
      style = "display: none;",
      create_field_set("hashtag", "Extrap From", "input_extrapFrom", c(80), 80),
      create_field_set("hashtag", "Extrap Fit", "input_extrapFit", c(60, 65, 70, 75, 80), 60),
      create_field_set("hashtag", "Extrap Law", "input_extrapLaw", c("kannisto"), "kannisto"),
      create_field_set("hashtag", "Radix", "input_radix", c(1e+05), 1e+05),
      create_field_set("hashtag", "SRB", "input_srb", c(1.05), 1.05),
      create_field_set("hashtag", "a0 Rule", "input_a0rule", c("ak", "other"), "ak"),
      create_field_set("hashtag", "Ax Method", "input_axmethod", c("un", "other"), "un")
    ),

    # Dropdown to toggle Advanced Inputs
    action_button("toggle_advanced", "Show Advanced Options", class = "ui button")
  )
}

#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#' @return A shiny semantic UI for the application.
#' @importFrom shiny div HTML h1 p uiOutput br plotOutput
#' @importFrom shinyjs useShinyjs hidden
#' @importFrom shiny.semantic main_panel action_button selectInput file_input sidebar_layout
#' @importFrom untheme fluidUnTheme
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
                top: 30%;
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
                left: 30%;
            }
            .info-box {
                border: 1px solid #ddd;
                border-radius: 5px;
                padding: 20px;
                margin-bottom: 20px;
                text-align: center;
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
          style = "text-align: left;",
          class = "info-box",
          h1("Welcome to the Data Uploader! \U0001F680"),
          p("Upload your CSV file with the button below. Make sure it follows the format:"),
          p("\U0001F4C4 Headers on the first row."),
          p("\U0001F522 Numeric and text fields properly arranged."),
          p("If there are any issues with your file, we'll let you know!")
        ),
        div(
          class = "semi-centered",
          file_input("file1", ""),
          rHandsontableOutput("data_table"),
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
            input_page(),
            plotOutput("lt_plt")
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
