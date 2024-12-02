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
    uiOutput("sex_to_use"),
    uiOutput("ages_to_use"),
    uiOutput("extrap_from_data"),
    br(),
    # Advanced Inputs - Initially Hidden
    div(
      id = "advanced_inputs",
      style = "display: none;",
      div(
        class = "ui two column grid",
        div(
          class = "column",
          create_field_set("", "Extrapolation Law", "input_extrapLaw", EXTRAP_LAWS, EXTRAP_LAWS[1]),
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
#' @importFrom shiny.semantic main_panel action_button selectInput file_input sidebar_panel tabset
#' @importFrom untheme fluidUnTheme sidebar_layout_responsive
#' @importFrom DT DTOutput formatRound JS
#' @importFrom shinycssloaders withSpinner
#' @importFrom rhandsontable rHandsontableOutput
#' @importFrom shiny.fluent TooltipHost Image
#' @noRd
app_ui <- function(request) {
  fluidUnTheme(
    useShinyjs(),
    LATEX_PRE_TAGS,
    tags$script(JS_CODE_SCREEN_SIZE),
    main_panel(
      tags$head(
        tags$style(HTML("
          /* Base styles with Flexbox */
          .main-content {
              display: flex;
              flex-direction: column;
              align-items: center;
              justify-content: center;
          }

          .info-box {
              border: 1px solid #ddd;
              border-radius: 5px;
              padding: 20px;
              margin: 20px 0; /* Add margin to the top and bottom */
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
              text-align: center;
              font-size: 15px;
              color: #555;
              font-family: 'Arial', sans-serif;
              font-weight: bold;
          }

          .well {
              background-color: #f8d7da;
              border-color: #f5c6cb;
              border-radius: 5px;
              padding: 15px;
              box-shadow: 0 2px 4px rgba(0, 0, 0, 0.1);
          }

          .well.success {
              background-color: #d4edda;
              border-color: #c3e6cb;
          }

          .below-main-panel {
              display: flex;
              flex-direction: column;
              align-items: center;
              justify-content: center;
              margin-top: 20px;
          }

          /* Keyframe animation for fading in */
          @keyframes fadeIn {
              from { opacity: 0; }
              to { opacity: 1; }
          }

          /* Centering container for buttons */
          .button-container {
              display: flex;
              justify-content: center; /* Aligns children (buttons) in the center horizontally */
              align-items: center; /* Aligns children (buttons) in the center vertically */
              gap: 10px; /* Space between buttons */
          }

          /* Centering container for buttons */
          .button-container-file {
              display: flex;
              justify-content: center; /* Aligns children (buttons) in the center horizontally */
              gap: 10px; /* Space between buttons */
          }

          /* Apply the fadeIn animation to the icon */
          .fade-in-icon {
              animation: fadeIn 2s ease-in-out;
          }

          @media (max-width: 1024px) {
              .info-box {
                  margin-top: 40px; /* Increase margin-top to avoid header overlap */
              }

              .content {
                max-width: 90% !important;
              }
          }

          @media (max-width: 768px) {
              .info-box p, .validation-results {
                  font-size: 14px; /* Smaller font size for smaller screens */
              }

              /* Stack elements vertically on small screens */
              .main-content {
                  flex-direction: column;
                  align-items: center;
              }
          }
          @media (max-width: 480px) {
              .info-box p, .validation-results {
                  font-size: 14px; /* Even smaller font size for very small screens */
              }

              .main-content {
                  display: block;
              }

              .info-box h1, h3 {
                  font-size: 18px;
              }

              p div#content-wrapper {
                flex-direction: column !important;
              }

              .plot-container {
                width: 100% !important;
              }

              .table-container {
                width: 100% !important;
              }

              .button-container-file {
                flex-direction: column;
                flex-wrap: wrap;
                gap: 1px;
                width: 100%;
              }

              .button-container-forecast {
                flex-direction: column;
                flex-wrap: wrap;
                align-items: center;
              }

              .button-container-forecast > * {
                width: 100%;
                margin: auto;
                flex-wrap: wrap;
                align-items: center;
              }
          }

          #adjustment_pills {
            margin-bottom: 15px;
          }

          #adjustment_pills .ui.label {
            margin-right: 5px;
            margin-bottom: 5px;
            cursor: default;
          }

          #adjustment_pills .ui.label i.delete.icon {
            cursor: pointer;
          }

      .hero-section {
        text-align: center;
        padding: 3rem 1rem;
        background: linear-gradient(135deg, #f5f7fa 0%, #e4e9f2 100%);
        border-radius: 10px;
        margin-bottom: 2rem;
      }

      .hero-title {
        font-size: 2.5rem;
        color: #2c3e50;
        margin-bottom: 1rem;
      }

      .hero-subtitle {
        font-size: 1.2rem;
        color: #34495e;
        max-width: 600px;
        margin: 0 auto;
      }

      .features-grid {
        display: grid;
        grid-template-columns: repeat(auto-fit, minmax(250px, 1fr));
        gap: 2rem;
        padding: 2rem 0;
      }

      .feature-card {
        background: white;
        padding: 1.5rem;
        border-radius: 8px;
        box-shadow: 0 2px 4px rgba(0,0,0,0.1);
        text-align: center;
        transition: transform 0.2s;
      }

      .feature-card:hover {
        transform: translateY(-5px);
      }

      .feature-card i {
        font-size: 2rem;
        color: #3498db;
        margin-bottom: 1rem;
      }

      .feature-card h3 {
        color: #2c3e50;
        margin-bottom: 0.5rem;
      }

      .feature-card p {
        color: #7f8c8d;
        font-size: 0.9rem;
      }

      .data-format-section {
        background: white;
        padding: 2rem;
        border-radius: 8px;
        box-shadow: 0 2px 4px rgba(0,0,0,0.1);
        margin: 2rem 0;
      }

      .action-section {
        text-align: center;
        padding: 2rem 0;
      }

      @media (max-width: 768px) {
        .hero-title {
          font-size: 2rem;
        }

        .hero-subtitle {
          font-size: 1rem;
        }

        .features-grid {
          grid-template-columns: 1fr;
        }
      }"))
      ),
      div(
        div(
          class = "main-content",
          div(
            id = "initial_landing_page",
            # Hero Section
            div(
              class = "hero-section",
              h1("Life Table Analysis Platform", class = "hero-title"),
              p("Transform mortality data into comprehensive life table analyses with just a few clicks",
                class = "hero-subtitle"
              )
            ),

            # Features Grid
            div(
              class = "features-grid",
              # Upload Feature
              div(
                class = "feature-card",
                icon("upload"),
                h3("Upload Your Data"),
                p("Import your mortality data in CSV format containing Age, Deaths, and Exposures")
              ),
              # Diagnostics Feature
              div(
                class = "feature-card",
                icon("chart line"),
                h3("Run Diagnostics"),
                p("Analyze data quality and identify potential issues with built-in diagnostic tools")
              ),
              # Transform Feature
              div(
                class = "feature-card",
                icon("magic"),
                h3("Transform Data"),
                p("Apply sophisticated smoothing and adjustments by groups")
              ),
              # Results Feature
              div(
                class = "feature-card",
                icon("table"),
                h3("Get Results"),
                p("Download complete life table results and visualizations")
              )
            ),

            # Start Button
            div(
              class = "action-section",
              actionButton("start_button", "Start", class = "ui blue button")
            )
          )
        )
      ),
      div(
        class = "main-content",
        hidden(div(
          id = "landing_page",
          tags$div(
            class = "info-box",
            h1("\xF0\x9F\x9A\x80 Data upload and validation \xF0\x9F\x8E\xAF"),
            p("Begin by uploading your CSV file. Not sure about your file? Here's what we're looking for:"),
            br(),
            div(
              style = "display: flex; gap: 5px;",
              div(
                style = "",
                rHandsontableOutput("data_table")
              ),
              div(
                TooltipHost(
                  content = "Exposures refer to the person-years lived over the same period where Deaths were registered. If Deaths refer to a single year, then sometimes mid-year population can be used to approximate Exposures.",
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
            strong(h3("\xF0\x9F\x93\xA4 Ready? Click 'Browse...' to select your file or start with our sample data."))
          ),
          tags$script(HTML("
            $(document).ready(function() {
              $('div.ui.left.action.input.ui-ss-input').css('display', 'flex');
            });
          ")),
          div(
            br(),
            div(
              class = "button-container-file",
              style = "display: flex; gap: 10px;",
              file_input("file1", "", type = "flex-override"),
              uiOutput("modal_ui"),
              action_button(
                "continue_no_data",
                "Use sample data",
                class = "ui blue button",
                style = "height: 4%;"
              )
            ),
            br(),
            div(
              class = "validation-results",
              uiOutput("validation_results")
            ),
            br(),
            div(
              class = "button-container",
              style = "display: flex; gap: 10px;",
              uiOutput("forward_step2")
            )
          )
        ))
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
          id = "step_adjustment",
          div(
            class = "button-container-adjustment",
            div(
              class = "button-group",
              create_pills_ui(),
              action_button("back_to_diagnostics", "Back", class = "ui grey button"),
              action_button("forward_to_lifetable", "Next", class = "ui blue button")
            )
          ),
          tabset(
            id = "adjustment_tabs",
            list(
              list(
                menu = "Smoothing",
                content = create_adjustment_tab("smoothing", "smoothing_inputs", "smoothing_plot")
              ),
              list(
                menu = "Smoothing Two",
                content = create_adjustment_tab("smoothing_second", "smoothing_second_inputs", "smoothing_second_plot")
              )
            )
          )
        )
      ),
      hidden(
        div(
          id = "step_input",
          div(
            class = "button-container-forecast",
            style = "display: flex; gap: 10px;",
            action_button("back_to_adjustment", "Back", class = "ui grey button"),
            action_button("calculate_lt", "Calculate", class = "ui blue button"),
            action_button("reset_lt", "Reset Options", class = "ui blue button"),
            uiOutput("download_button")
          ),
          br(),
          uiOutput("download_modal"),
          div(
            sidebar_layout_responsive(
              list(children = div(input_page())),
              div(
                id = "tabContent",
                uiOutput("lt_group_select_ui"),
                uiOutput("select_plots"),
                br(),
                uiOutput("render_plots")
              )
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

create_adjustment_tab <- function(tab_name, input_id, plot_id) {
  div(
    br(),
    sidebar_layout_responsive(
      list(
        children = div(
          uiOutput(input_id),
          br(),
          action_button(paste0("execute_", tab_name), "Execute", class = "ui blue button")
        )
      ),
      div(
        uiOutput(paste0("adjustment_group_select_ui_", tab_name)),
        withSpinner(plotlyOutput(plot_id, height = "400px"))
      )
    ),
    div(style = "padding: 10px 0;")
  )
}

# Add this new function to create the pills UI
create_pills_ui <- function() {
  uiOutput("adjustment_pills")
}

# Update the CSS
tags$style(HTML("
  .button-container-adjustment {
    margin-bottom: 20px;
  }

  .button-group {
    display: flex;
    flex-wrap: wrap;
    gap: 10px;
    align-items: center;
  }

  .button-group .ui.button,
  #adjustment_pills .ui.label {
    height: 36px;
    line-height: 34px;
    padding: 0 15px;
    font-size: 14px;
    border-radius: 4px;
    margin: 0;
    box-sizing: border-box;
  }

  #adjustment_pills {
    display: flex;
    flex-wrap: wrap;
    gap: 10px;
  }

  #adjustment_pills .ui.label {
    background-color: #e0e1e2;
    color: rgba(0,0,0,.6);
    font-weight: 700;
    border: 1px solid #d4d4d5;
    display: inline-flex;
    align-items: center;
  }

  #adjustment_pills .ui.label i.delete.icon {
    cursor: pointer;
    margin-left: 8px;
    font-size: 12px;
  }

  /* Ensure consistent sizing for buttons and labels */
  .ui.button,
  .ui.label {
    box-sizing: border-box;
  }
"))
