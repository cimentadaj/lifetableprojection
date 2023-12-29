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
    create_field_set("", "Desired Open Age Group", "input_oanew", seq(70, 100, by = 5), 100),
    create_field_set("", "Output Age Classes", "input_age_out", c("single", "abridged"), "single"),
    create_field_set("", "Sex", "input_sex", c("Total", "Female", "Male"), "Total"),
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
          create_field_set("", "Extrap. Jump-off Age", "input_extrapFrom", input_selected = 80, numeric_input = TRUE),
          create_field_set("", "Extrapolation Law", "input_extrapLaw", extrap_laws, extrap_laws[1]),
          create_field_set("", "Lifetable Radix", "input_radix", input_selected = 100000, numeric_input = TRUE)
        ),
        div(
          class = "column",
          create_field_set("", "Sex Ratio at Birth", "input_srb", input_selected = 1.05, numeric_input = TRUE),
          create_field_set("", "a(0) Rule", "input_a0rule", c("Andreev-Kingkade", "Coale-Demeny"), "Andreev-Kingkade"),
          create_field_set("", "a(x) Method", "input_axmethod", c("UN (Greville)", "PASEX"), "UN (Greville)")
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
#' @importFrom DT DTOutput formatRound JS
#' @importFrom shinycssloaders withSpinner
#' @importFrom rhandsontable rHandsontableOutput
#' @importFrom shiny.fluent TooltipHost Image
#' @noRd
app_ui <- function(request) {
  fluidUnTheme(
    useShinyjs(),
    latex_pre_tags,
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
            .below-main-panel {
              display: flex;
              flex-direction: column;
              align-items: center;
              justify-content: center;
              margin-top: 20px; /* Adjust as needed */
            }

            /* Keyframe animation for fading in */
            @keyframes fadeIn {
              from { opacity: 0; }
              to { opacity: 1; }
            }

            /* Apply the fadeIn animation to the icon */
            .fade-in-icon {
              animation: fadeIn 2s ease-in-out; /* Animation lasts 2 seconds */
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
          div(
            style = "display: flex; gap: 5px;",
            div(
              style = "",
              rHandsontableOutput("data_table")
            ),
            div(
              TooltipHost(
                content = "Exposures refer to the person-years lived over the same period where Deaths were registered. If Deaths refers to a single year, then sometimes mid-year population can be used to approximate Exposures.",
                delay = 0,
                Image(
                  src = "www/info.png",
                  width = "20px",
                  shouldStartVisible = TRUE
                )
              )
            )
          ),
          br(),
          strong(h3("\xF0\x9F\x93\xA4 Ready to get started? Click 'Browse...' to select your file"))
        ),
        div(
          class = "semi-centered-two",
          br(),
          div(
            style = "display: flex; gap: 10px;",
            file_input("file1", ""),
            div(
              style = "flex: none;",
              action_button(
                "continue_no_data",
                "Use sample data",
                class = "ui blue button",
                width = "100%",
                height = "10%"
              )
            )
          ),
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
      $('#advanced_inputs').slideToggle('fast', function() {
        // This callback function is called after the slideToggle animation completes
        var isVisible = $('#advanced_inputs').is(':visible');
        if(isVisible) {
          // If the advanced inputs are now visible, change button text to 'Hide Advanced Options'
          $('#toggle_advanced').text('Hide Advanced Options');
        } else {
          // If the advanced inputs are now hidden, change button text to 'Show Advanced Options'
          $('#toggle_advanced').text('Show Advanced Options');
        }
      });
    });
  "))
      ),
      hidden(
        div(
          id = "step_input",
          div(
            style = "display: flex; gap: 10px;",
            action_button("back_to_landing", "Back", class = "ui grey button"),
            action_button("calculate_lt", "Calculate", class = "ui blue button"),
            action_button("reset_lt", "Reset Options", class = "ui blue button")
          ),
          br(),
          sidebar_layout(
            list(children = div(input_page()), width = 1.3),
            main_panel(
              uiOutput("tabs")
            )
          ),
          uiOutput("lt_summary_indication"),
          div(
            class = "ui container",
            style = "padding-top: 20px;",
            DTOutput("lt_summary_table")
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
