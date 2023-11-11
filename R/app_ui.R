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
    create_field_set(
      "globe",
      "Select a country",
      "wpp_country",
      c("Spain", "Netherlands"),
      NULL
    ),
    div(
      class = "two fields",
      create_field_set("calendar", "Starting Year", "wpp_starting_year", 2023:2099, 2023),
      create_field_set("calendar", "Ending Year", "wpp_ending_year", 2024:2100, 2024)
    )
  )
}

#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#' @return A shiny semantic UI for the application.
#' @importFrom shiny div HTML h1 p uiOutput
#' @importFrom shinyjs useShinyjs
#' @importFrom shiny.semantic main_panel action_button selectInput file_input
#' @importFrom untheme fluidUnTheme
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
            .emoji {
                vertical-align: middle;
                font-size: 40px; /* Adjusted emoji size */
            }
        "))
      ),
      div(
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
          div(
            class = "validation-results",
            uiOutput("validation_results")
          )
        ),
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
