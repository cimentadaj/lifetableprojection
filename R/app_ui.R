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
#' @importFrom rintrojs introjsUI
#' @noRd
app_ui <- function(request) {
  fluidUnTheme(
    useShinyjs(),
    introjsUI(),
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
      }


        .module-landing {
          min-height: 100vh;
          background: white;
          padding: 2rem;
        }

        .module-header {
          text-align: center;
          margin-bottom: 4rem;
          padding: 2rem 0;
        }

        .module-title {
          font-size: 3rem;
          color: #1a237e;
          font-weight: 700;
          margin-bottom: 1rem;
          letter-spacing: -0.5px;
        }

        .module-subtitle {
          font-size: 1.25rem;
          color: #546e7a;
          max-width: 600px;
          margin: 0 auto;
          line-height: 1.6;
        }

        .modules-grid {
          display: grid;
          grid-template-columns: repeat(auto-fit, minmax(320px, 1fr));
          gap: 2rem;
          max-width: 1200px;
          margin: 0 auto;
        }

        .module-card {
          background: white;
          border-radius: 16px;
          padding: 2rem;
          box-shadow: 0 4px 20px rgba(0,0,0,0.05);
          transition: all 0.3s ease;
          border: 1px solid rgba(0,0,0,0.05);
          cursor: pointer;
        }

        .module-card:hover {
          transform: translateY(-5px);
          box-shadow: 0 8px 30px rgba(0,0,0,0.1);
        }

        .module-icon {
          width: 60px;
          height: 60px;
          background: #e3f2fd;
          border-radius: 12px;
          display: flex;
          align-items: center;
          justify-content: center;
          margin-bottom: 1.5rem;
        }

        .module-icon i {
          font-size: 24px;
          color: #1976d2;
        }

        .module-name {
          font-size: 1.5rem;
          color: #1a237e;
          margin-bottom: 1rem;
          font-weight: 600;
        }

        .module-description {
          color: #546e7a;
          line-height: 1.6;
          margin-bottom: 1.5rem;
          font-size: 1rem;
        }

        .module-status {
          display: inline-flex;
          align-items: center;
          padding: 0.5rem 1rem;
          border-radius: 20px;
          font-size: 0.875rem;
          font-weight: 500;
        }

        .status-active {
          background: #e8f5e9;
          color: #2e7d32;
        }

        .status-coming {
          background: #fff3e0;
          color: #ef6c00;
        }

        @media (max-width: 768px) {
          .module-title {
            font-size: 2.5rem;
          }
          .modules-grid {
            grid-template-columns: 1fr;
            padding: 0 1rem;
          }
        }

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
      ),
      div(
        id = "module_landing_page",
        class = "module-landing",
        div(
          class = "module-header",
          h1("Demographic Analysis Suite", class = "module-title"),
          p("Advanced tools for demographic research and analysis",
            class = "module-subtitle"
          )
        ),
        div(
          class = "modules-grid",
          div(
            class = "module-card",
            div(
              class = "module-icon",
              icon("table")
            ),
            h3("Life Table Analysis", class = "module-name"),
            p("Create comprehensive life tables with advanced smoothing capabilities and mortality analysis tools.",
              class = "module-description"
            ),
            actionButton("goto_lifetable", "Go to module", class = "ui blue button")
          ),
          div(
            class = "module-card",
            style = "opacity: 0.7; pointer-events: none;",
            div(
              class = "module-icon",
              icon("users")
            ),
            h3("Migration Analysis", class = "module-name"),
            p("Analyze migration patterns and demographic impacts.",
              class = "module-description"
            ),
            div(
              class = "module-status status-coming",
              span("Coming Soon")
            )
          )
        )
      ),
      div(
        class = "main-content",
        hidden(
          div(
            id = "lifetable_landing_page",
            # Hero Section
            div(
              class = "hero-section",
              div(
                style = "display: flex; justify-content: flex-start; margin-bottom: 20px;",
                actionButton("back_to_modules", "Back", class = "ui grey button")
              ),
              h1("Life Table Analysis Platform", class = "hero-title"),
              p("Transform mortality data into comprehensive life table analyses with just a few clicks",
                class = "hero-subtitle"
              )
            ),
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
            div(
              class = "action-section",
              actionButton("lifetable_start_button", "Start", class = "ui blue button")
            )
          )
        )
      ),
      div(
        class = "main-content",
        hidden(
          div(
            id = "landing_page",
            div(
              style = "display: flex; justify-content: flex-start; margin-bottom: 20px;",
              actionButton("back_to_lifetable_landing", "Back", class = "ui grey button")
            ),
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
              strong(h3("\xF0\x9F\x93\xA4 Ready? Click 'Browse...' to select your file or start with our sample data.")),
              br(),
              action_button("upload_instructions", "Instructions", class = "ui blue button")
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
                div(id = "file-input", file_input("file1", "", type = "flex-override")),
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
          )
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
          id = "step_adjustment",
          div(
            class = "button-container-adjustment",
            div(
              class = "button-group",
              create_pills_ui(),
              action_button("back_to_diagnostics", "Back", class = "ui grey button"),
              action_button("preprocessing_instructions", "Instructions", class = "ui blue button"),
              action_button("forward_to_lifetable", "Next", class = "ui blue button")
            )
          ),
          tabset(
            id = "adjustment_tabs",
            list(
              list(
                menu = "Smoothing",
                content = create_adjustment_tab("smoothing", "smoothing_inputs", "smoothing_plot")
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
            action_button("lifetable_instructions", "Instructions", class = "ui blue button"),
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
            withSpinner(DTOutput("lt_summary_table"))
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
